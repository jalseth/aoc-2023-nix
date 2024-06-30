with builtins;

let
  strings = (import <nixpkgs> {}).lib.strings;

  buildMatrix = foldl' (a: b: a ++ [(strings.stringToCharacters b)]) [];

  isPeriod = char: char == 46;  # Period, which is treated as empty space.
  isDigit = char: (char >= 48 && char <= 57);  # Digits 0-9.
  isSpecial = char: !(isPeriod char || isDigit char);

  sublist = list: start: end:
    let idx = genList(n: n + start) (end - start + 1);
    in foldl' (a: b: a ++ [(elemAt list b)]) [] idx;

  scanForNumGroups = matrix:
    let
      rowNums = genList (n: n) (length matrix);
      width = length (elemAt matrix 0);
      scan = rowNum:
        let
          row = elemAt matrix rowNum;
          build = start: end:
            let num = strings.toIntBase10 (strings.concatStrings (sublist row start end));
            in { num = num; row = rowNum; start = start; end = end; };
          iter = found: matchPos: pos:
            let
              match = isDigit (strings.charToInt (elemAt row pos));
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

  isPart = matrix: row: start: end:
    let
      rows =
        let l = [(row - 1) row (row + 1)]; # Need to look up and down.
        in filter (n: n >= 0 && n < (length matrix)) l; # Remove impossible y positions.
      cols =
        let l = genList (n: n + start - 1) (end - start + 1 + 2); # Need to look left and right.
        in filter (n: n >= 0 && n < (length (elemAt matrix 0))) l; # Remove impossible x positions.
      coords = 
        let all = map (r: map (c: { col = c ; row = r; }) cols) rows;
        in foldl' (a: b: a ++ b) [] all; # Flatten to 1 dimension.
      check = coord:
        let char = strings.charToInt (elemAt (elemAt matrix coord.row) coord.col);
        in isSpecial char;
    in
      foldl' (ok: c: ok || check c) false coords;

in
  let
    lines = filter
      (line: line != "")
      (filter isString (split "\n" (readFile ./input.txt)));

    matrix = buildMatrix lines;
    possibleParts = scanForNumGroups matrix;
    check = isPart matrix;
    parts = filter (x: check x.row x.start x.end) possibleParts;
  in
   foldl' (a: b: a + b.num) 0 parts
