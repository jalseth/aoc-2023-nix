with builtins;

let
  strings = (import <nixpkgs> {}).lib.strings;

  sublist = list: start: end:
    let idx = genList(n: n + start) (end - start + 1);
    in foldl' (a: b: a ++ [(elemAt list b)]) [] idx;

  parseSeeds = line:
    let sp = filter isString (split ": " line);
    in map strings.toIntBase10 (filter isString (split " " (elemAt sp 1)));

  buildNeedsMaps = lines:
    let
      headingName = h:
        let m = match "(.+) map:" h;
        in if m == null then "" else elemAt m 0;
      headingFrom = h: elemAt (match "(.+)-to-.+" h) 0;
      headingTo = h: elemAt (match ".+-to-(.+)" h) 0;
      headings = foldl'
        (acc: idx: acc ++ (
          let h = headingName (elemAt lines idx);
          in if h != "" then [{ 
            from = headingFrom h;
            to = headingTo h;
            line = idx;
          }] else []))
        [] (genList (n: n) (length lines));
      withStartStop = foldl'
        (acc: idx: acc ++ (
          let
            h = elemAt headings idx;
            n =
              if (idx+1) < (length headings) then elemAt headings (idx+1)
              else { line = length lines; };
          in [(h // { start = h.line + 1; stop = n.line - 1; })]))
        [] (genList (n: n) (length headings));
      buildMap = h: {
        from = h.from;
        to = h.to;
        lookups = foldl' (acc: line: acc ++ (
          let
            sp = filter isString (split " " line);
            dstStart = strings.toIntBase10 (elemAt sp 0);
            srcStart = strings.toIntBase10 (elemAt sp 1);
            range = strings.toIntBase10 (elemAt sp 2);
          in [{
            srcStart = srcStart;
            srcEnd = srcStart + range;
            dstStart = dstStart;
          }])) [] (sublist lines h.start h.stop);
      };
    in foldl' (acc: h: acc // { "${h.from}" = buildMap h; }) {} withStartStop;

  lookupOrSelf = id: col:
    foldl' (a: b:
      if a != id then a
      else (
        if id < b.srcStart || id > b.srcEnd then id
        else b.dstStart + (id - b.srcStart)
      )
    ) id col.lookups;

  findLocations = needs: seeds:
    let
      sl = id: fl id (getAttr "seed" needs);
      fl = id: col:
        let
          nextVal = lookupOrSelf id col;
          nextCol = getAttr col.to needs;
        in
          if col.to == "location" then nextVal
          else fl nextVal nextCol;
    in foldl' (acc: seed: acc ++ [(sl seed)]) [] seeds;

  min = foldl' (a: b: if a == 0 || b < a then b else a) 0;

in
  let
    lines = filter
      (line: line != "")
      (filter isString (split "\n" (readFile ./input.txt)));
    seeds = parseSeeds (head lines);
    needsMaps = buildNeedsMaps (tail lines);
    locations = findLocations needsMaps seeds;
  in
    min locations
