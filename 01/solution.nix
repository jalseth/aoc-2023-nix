
with builtins;

let
  strings = (import <nixpkgs> {}).lib.strings;

  values =
    let
      nums = genList (x: toString (x + 1)) 9;
      vals = listToAttrs (map (x: { name = x; value = x; }) nums);
    in
      vals // {
        "one"   = "1";
        "two"   = "2";
        "three" = "3";
        "four"  = "4";
        "five"  = "5";
        "six"   = "6";
        "seven" = "7";
        "eight" = "8";
        "nine"  = "9";
      };

  keys = attrNames values;

  parseLine = line :
    let
      lineLen = stringLength line;
      numbers = scan 0 keys [];
      scan = idx: searching: found:
        let
          key = head searching;
          len = stringLength key;
          str = substring idx len line;
          rem = tail searching;
        in
          if idx >= lineLen then found
          else if str == key then scan (idx + len) keys (found ++ [(getAttr key values)])
          else if length rem == 0 then scan (idx + 1) keys found
          else scan idx rem found;
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
