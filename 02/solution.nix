with builtins;

let
  strings = (import <nixpkgs> {}).lib.strings;

  maxCubeCounts = { "red" = 12; "green" = 13; "blue" = 14; };

  getDefault = default: key: set: if hasAttr key set then getAttr key set else default;
  getInt = key: set: getDefault 0 key set;

  parseLines = lines:
    if length lines == 0 then []
    else [(parseLine (head lines))] ++ parseLines (tail lines);

  parseLine = line:
    let
      sp = filter isString (split ":" line);
      id = elemAt (match "Game ([[:digit:]]+).*" (elemAt sp 0)) 0;
      data = elemAt sp 1;
    in {
      "id" = strings.toIntBase10 id;
      "rounds" = parseGame data;
    };

  parseGame = game:
    let
      rounds = filter isString (split ";" game);
      parseRounds = rounds:
        if length rounds == 0 then []
        else [(parseRound (head rounds))] ++ parseRounds (tail rounds);
      parseRound = r: mapAttrs (k: v: extractColorValue r k) maxCubeCounts;
    in
      parseRounds rounds;

  extractColorValue = str: color:
    let extractInt = str: pattern:
      let m = match pattern str;
      in
        if isNull m || length m == 0 then 0
        else strings.toIntBase10 (elemAt m 0);
    in
      extractInt str ".*[[:space:]]+([[:digit:]]+)[[:space:]]+${color}.*";

  validGame = game: all validRound game.rounds;

  validRound = round: all
    (violation: violation == false)
    (attrValues
      (mapAttrs
        (color: _: (getInt color round) > (getInt color maxCubeCounts))
        maxCubeCounts));

  sumIDs = games:
    if length games == 0 then 0
    else (head games).id + sumIDs (tail games);

in
  let
    lines = filter
      (line: line != "")
      (filter isString (split "\n" (readFile ./input.txt)));
    games = parseLines lines;
    valid = filter validGame games;
  in
    sumIDs valid