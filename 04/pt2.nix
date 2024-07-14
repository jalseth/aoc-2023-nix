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
      card = strings.toIntBase10 cardNum;
      matched = filter (x: elem x winning) lotto;
    };

  parseNumsFromStr = s:
    let nums = filter (x: x != "") (filter isString (split " " s));
    in foldl' (acc: n: acc ++ [(strings.toIntBase10 n)]) [] nums;

  sublist = list: start: end:
    let idx = genList (n: n + start) (end - start + 1);
    in foldl' (acc: i: acc ++ [(elemAt list i)]) [] idx;

  countCards = cards:
    let
      initData = map (x: { pos = x.card - 1; count = 1; matches = length x.matched; }) cards;
      addCount = n: list: foldl' (a: b: a ++ [(b // { count = b.count + n; })]) [] list;
      cc = counted: cards:
        let
          current = head cards;
          acc = counted ++ [current];

          remaining = tail cards;
          maxPos = length remaining;
          matchPos = if current.matches > maxPos then maxPos else current.matches;
        in
          # Base case for the last card, just return.
          if length remaining == 0 then acc
          # If the card doesn't have any matches, just move forward.
          else if current.matches == 0 then cc acc remaining
          # Add the current count for the next N cards based on the number of matches, and move forward.
          else cc acc (
            (addCount current.count (sublist remaining 0 (matchPos - 1))) ++
            (sublist remaining matchPos (maxPos - 1)));
    in cc [] initData;
in
  let
    lines = filter
      (line: line != "")
      (filter isString (split "\n" (readFile ./input.txt)));
    cards = foldl' (a: b: a ++ [(parseCard b)]) [] lines;
  in
    foldl' (a: b: a + b.count) 0 (countCards cards)
