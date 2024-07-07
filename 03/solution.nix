# Steps
# 1. Scan matrix for matches of some type
# 2. Scan again for related type
# 3. Associated related when isTouching is true
# 4. Filter func (eg size > 0 or size == 2)
# 5. Agg func

with builtins;

let
  strings = (import <nixpkgs> {}).lib.strings;

  buildMatrix = foldl' (a: b: a ++ [(strings.stringToCharacters b)]) [];

  isPeriod = char: char == ".";  # Period, which is treated as empty space.
  isDigit = char:
    let c = strings.charToInt char;
    in (c >= 48 && c <= 57);  # Digits 0-9.
  isSpecial = char: !(isPeriod char || isDigit char);

  sublist = list: start: end:
    let idx = genList(n: n + start) (end - start + 1);
    in foldl' (a: b: a ++ [(elemAt list b)]) [] idx;

  scanForMatches = matrix: matchFn:
    let
      rowNums = genList (n: n) (length matrix);
      width = length (elemAt matrix 0);
      scan = rowNum:
        let
          row = elemAt matrix rowNum;
          build = start: end:
            let str = strings.concatStrings (sublist row start end);
            in { str = str; row = rowNum; start = start; end = end; };
          iter = found: matchPos: pos:
            let
              match = matchFn (elemAt row pos);
              prevMatch = matchPos != null;
              next = pos + 1;
            in
              if next > width && prevMatch then (found ++ [(build matchPos (pos - 1))]) # EOL with match.
              else if next > width then found # EOL no match.
              else if match && !prevMatch then iter found pos next # Matching started.
              else if match && prevMatch then iter found matchPos next # Matching continued.
              else if prevMatch && !match then iter (found ++ [(build matchPos (pos - 1))]) null next # Matching ended.
              else iter found null next; # No match.
        in iter [] null 0;
    in
      foldl' (acc: row: acc ++ scan row) [] rowNums;

  isTouching = a: b:
    let touching = x: y: x - y >= -1 && x - y <= 1;
    in touching a.row b.row && (touching a.start b.end || touching a.end b.start);

  associateTouching = matches: related:
    let associate = m: filter (r: isTouching m r) related;
    in map (m: m // { associated = associate m; }) matches;

  isPart = m: length m.associated > 0;
  partCalc = m: strings.toIntBase10 m.str;

in
  let
    # Case-specific config.
    matchFn = isDigit;
    relatedFn = isSpecial;
    filterFn = isPart;
    calcFn = partCalc;

    # No updates needed below here.
    lines = filter
      (line: line != "")
      (filter isString (split "\n" (readFile ./input.txt)));
    matrix = buildMatrix lines;
    matches = scanForMatches matrix matchFn;
    related = scanForMatches matrix relatedFn;
    associated = associateTouching matches related;
    filtered = filter (x: filterFn x) associated;
    total = foldl' (a: b: a + calcFn b) 0 filtered;
  in
    total
