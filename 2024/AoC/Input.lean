import Parser

inductive Pattern where
  | psections : List Pattern → Pattern         -- '{' p1 , p2 , ... '}'
  | plines : Pattern → Pattern                 -- '<' p '>'
  | plist : Pattern → Option String → Pattern  -- '[' p sep? ']'
  | ptup : Pattern → Pattern → Pattern
  | pint : Pattern                             -- 'i'
  | pnat : Pattern                             -- 'n'
  | pchar : Pattern                            -- 'c'

namespace Pattern

open Parser Char

protected abbrev Parser := Parser Unit Substring Char

def parsePattern : Pattern.Parser Pattern := sorry

end Pattern
