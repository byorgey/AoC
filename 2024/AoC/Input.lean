import Parser
import AoC.Grid

@[reducible]
def f : Bool → Type
  | false => Int
  | true => Char

-- p ::= 'i' | 'd' | 'n' | 'c' | 's' (no ws) | '_' (newline) | ' ' (space+)
--   | (p)
--   | p1 p2
--   | '[' p p? ']' -- list
--   | '<' p '>'    -- grid

-- e.g.
--   day 1: [(n n)_]
--   day 2: [[n ]_]
--   day 3: [s_]  (or custom)
--   day 4: <c>
--   day 5: [(n|n)_]_[[n,]_]
--   day 6: <c>

inductive Pattern where
  | pdigit : Pattern                            -- 'd'
  | pint : Pattern                              -- 'i'
  | pnat : Pattern                              -- 'n'
  | pchar : Pattern                             -- 'c'
  | pstr : Pattern                              -- 's'
  | pnewline : Pattern                          -- '_'
  | pspace : Pattern                            -- ' '
  | plit : Char → Pattern
  | pseq : List Pattern → Pattern               -- p1 p2 ...
  | plist : Pattern → Option Pattern → Pattern  -- '[' p p? ']'
--  | pgrid : Pattern → Pattern                   -- '<' p '>'
deriving Inhabited, Repr

mutual

def Pattern.ignored : Pattern → Bool
  | .pdigit => false
  | .pint => false
  | .pnat => false
  | .pchar => false
  | .pstr => false
  | .pnewline => true
  | .pspace => true
  | .plit _ => true
  | .pseq ps => ignoredList ps
  | .plist p _ => p.ignored
--  | .pgrid p => p.ignored

def ignoredList : List Pattern → Bool
  | [] => true
  | p :: ps => p.ignored && ignoredList ps

end

abbrev Pattern.asType : Pattern → Type
  | .pdigit => Nat
  | .pint => Int
  | .pnat => Nat
  | .pchar => Char
  | .pstr => String
  | .pnewline => Unit
  | .pspace => Unit
  | .plit _ => Unit
  | .pseq [] => Unit
  | .pseq (p :: ps) => p.asType × (Pattern.pseq ps).asType
  | .plist p _ => Array p.asType
--  | .pgrid p => Grid (Result p)

-- mutual
--
-- def inhabitedResult : (p : Pattern) → Result p
--   | .pdigit => 0
--   | .pint => 0
--   | .pnat => 0
--   | .pchar => ' '
--   | .pstr => ""
--   | .pnewline => ()
--   | .pspace => ()
--   | .plit _ => ()
--   | .pseq ps => inhabitedSeqResult ps
--   | .plist _ _ => #[]

-- end

#reduce (Pattern.pseq [Pattern.pint, Pattern.pspace, Pattern.pint]).asType

-- #check ((3,(),5,()) : (Pattern.pseq [Pattern.pint, Pattern.pspace, Pattern.pint]).asType)

namespace Pattern

open Parser Char

protected abbrev Parser := Parser Unit Substring Char

instance : Inhabited (Pattern.Parser Pattern) where
  default := pure pint

def mkSeq (ps : Array Pattern) : Pattern :=
  match ps.size with
    | 1 => ps[0]!
    | _ => (pseq ∘ Array.toList) ps

mutual

protected partial def parse : Pattern.Parser Pattern :=
  mkSeq <$> Parser.takeMany1 Pattern.atom

protected partial def atom : Pattern.Parser Pattern :=
      (char '(' *> Pattern.parse <* char ')')
  <|> ((λ _ => pint) <$> char 'i')
  <|> ((λ _ => pnat) <$> char 'n')
  <|> ((λ _ => pchar) <$> char 'c')
  <|> ((λ _ => pstr) <$> char 's')
  <|> ((λ _ => pnewline) <$> char '_')
  <|> ((λ _ => pspace) <$> char ' ')
  <|> (plist <$> (char '[' *> Pattern.atom) <*> (Parser.option? Pattern.atom <* char ']'))
  <|> plit <$> tokenFilter (not ∘ String.contains "[]()_incs ")
--  <|> (pgrid <$> (char '<' *> Pattern.parse <* char '>'))

end

mutual

@[reducible]
def parser : (p : Pattern) → Pattern.Parser p.asType
  | .pdigit => ASCII.digit
  | .pint => ASCII.parseInt
  | .pnat => ASCII.parseNat
  | .pchar => anyToken
  | .pstr => (String.mk ∘ Array.toList) <$> takeMany1 (notFollowedBy ASCII.whitespace *> anyToken)
  | .pnewline => (λ _ => ()) <$> char '\n'
  | .pspace => dropMany space
  | .plit c => (λ _ => ()) <$> char c
  | .pseq ps => parseSeq ps
  | .plist p none => takeMany p.parser
  | .plist p (some sep) => sepEndBy sep.parser p.parser
--  | .pgrid p => _

def parseSeq : (ps : List Pattern) → Pattern.Parser (SeqResult ps)
  | [] => pure ()
  | p :: ps => (λ a b => (a,b)) <$> p.parser <*> parseSeq ps
      -- if p.ignored
      -- then p.parser *> parseSeq ps
      -- else (λ a b => (a,b)) <$> p.parser <*> parseSeq ps
end

end Pattern

@[reducible]
def readPattern (pat : String) : Pattern :=
  match Pattern.parse pat with
    | .ok _ p => p
    | .error _ _ => panic "Pattern did not parse!"

@[reducible]
def ns : Pattern := readPattern "n n"

#check Parser.run ns.parser "3 5"

def foo : Nat × Nat :=
  match Parser.run ns.parser "3 5" with
  | .error _ _ => panic "o no!"
  | .ok _ ((x : Nat), (), (y : Nat), ()) => (x,y)

-- #eval (Parser.run (readPattern "n").parser "5" : Nat)
-- -- #eval match Parser.run ns.parser "3 5" with | (a,b) => a + b

------------------------------------------------------------
