
with builtins;

let
  strings = (import <nixpkgs> {}).lib.strings;

  numStrs = genList (x: toString x) 10;

  contains = list : val :
    let h = head list; t = tail list;
    in
      if h == val then true
      else if length t == 0 then false
      else contains t val;

  parseLine = line :
    let numbers = filter (contains numStrs) (split "" line);
    in
      if length numbers == 0 then 0
      else
        let firstAndLast = [ (elemAt numbers 0) (elemAt numbers ((length numbers)-1)) ];
        in strings.toIntBase10 (concatStringsSep "" firstAndLast);

  parseLines = lines :
    if length lines == 0 then 0
    else parseLine (head lines) + parseLines (tail lines);

in
  let contents = filter isString (split "\n" (readFile ./input.txt));
  in parseLines contents
