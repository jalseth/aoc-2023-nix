with builtins;

let
  strings = (import <nixpkgs> {}).lib.strings;

  parseCard = card:
    let
      sp = filter isString (split ": " card);
      cardNum = elemAt (match "Card ([[:digit:]]+).*" (elemAt sp 0)) 0;
      dataSp = filter isString (split " \\| " (elemAt sp 1));
      winning = parseNumsFromStr (elemAt dataSp 0);
      lotto = parseNumsFromStr (elemAt dataSp 1);
    in {
      card = cardNum;
      winning = winning;
      lotto = lotto;
      matched = filter (x: elem x winning) lotto;
    };

  parseNumsFromStr = s:
    let nums = filter (x: x != "") (filter isString (split " " s));
    in foldl' (a: b: a ++ [(strings.toIntBase10 b)]) [] nums;

  scoreCard = card: foldl' (a: _: if a == 0 then 1 else a*2) 0 card.matched;

in
  let
    lines = filter
      (line: line != "")
      (filter isString (split "\n" (readFile ./input.txt)));
    cards = foldl' (a: b: a ++ [(parseCard b)]) [] lines;
  in
    foldl' (a: b: a + scoreCard b) 0 cards
