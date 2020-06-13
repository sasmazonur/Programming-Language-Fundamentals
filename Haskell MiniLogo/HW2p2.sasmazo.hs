
--Onur Sasmaz - CS381
--Sources used cited down below
module MiniLogo where

import Data.List
import Prelude

type Numm = Int
type Var = String
type Macro = String
type Prog = [Cmd]

data Mode = Up | Down deriving(Show, Eq)

data Expr = Ref Var | Lit Numm | Add Expr Expr deriving(Show, Eq)

data Cmd = Pen Mode | Move (Expr,Expr) | Define Macro [Var] Prog
                           | Call Macro [Expr] deriving(Show, Eq)

--Define a MiniLogo macro line (x1,y1,x2,y2) that (starting from
--anywhere on the canvas draws a line segment from (x1,y1) to (x2,y2).
--pen up; move (x,y);
--   pen down;
-- move (x+2,y); move (x+2,y+2);
--define square (x,y) {
--      pen up; move (x,y);
--      pen down; move (x+2,y); move (x+2,y+2);
--      move (x,y+2); move (x,y);
--}

--define line (x1,y1,x2,y2){
--      pen up; move(x1 y1);
--      pen down; move(x2 y2);
--}

--rect(0, 0, 100, 100); Rectangle at zero
--rect(0, 50, 50, 0); Horizantal Rectangle
--rect(50, 0, 0, 50); Vertical Rectangle
--Draw Line
--Ref cause its Var. "Var -> Var -> Var -> Var -> Cmd"

line :: Cmd
line = Define "line" ["x1", "y1", "x2", "y2"] [Pen Up, Move(Ref "x1", Ref "y1"), Pen Down, Move(Ref "x2", Ref "y2")]

--Drow X
--Use the line macro you just defined to define a new MiniLogo macro nix (x,y,w,h)
--that draws a big “X” of width w and height h, starting from position (x,y).
--Your definition should not contain any move commands.

--Drow X starting (x,y). with height h and width w
--Not use Move, so we need to use the other data type by calling it.
--(x, y, x+w, y+h) drawing \
--(x, y+h, x+w, y) drawing /

--Prog is [Cmd]
nix :: Cmd
nix = Define "nix" ["x","y","w","h"][Call "line" [Ref "x", Ref "y", Add (Ref "x") (Ref "w"), Add (Ref "y") (Ref "h")], Call "line" [Ref "x", Add (Ref "y") (Ref "h"), Add (Ref "x") (Ref "w"), Ref "y"]]



--line(100, 0, 100, 100) Hortizantal
--line(0, 0, 100, 0) Vertical
-- __
--   |
--    --
--     |
--
--line(100, 100, 0, 100) Hortizantal
--line(0, 100, 0, 0) Vertical =
---- 8. Define the semantics of a StackLang program.
--prog :: Prog -> Domain
--prog []    = \s -> Just s
--prog (c:p) = \s -> case cmd c s of
--                   Just s' -> prog p s'
--                   Nothing -> Nothing


--Define a Haskell function steps :: Int -> Prog that constructs a MiniLogo program
-- that draws a staircase of n steps starting from (0,0).
--Recursively iterate n
--
--lala = (x-1)
steps :: Int -> Prog
steps 0 = []
steps x = steps (x-1) ++[
        Call "line" [Lit  x, Lit x, Lit (x-1), Lit x],
        Call "line" [Lit (x-1), Lit x, Lit (x-1), Lit (x-1)]
                       ]
-- HOW to connect them?


--function macros returns a list of the names of all of the macros that
--are defined anywhere in a given MiniLogo program
macros :: Prog -> [Macro]
macros [] = []
macros (c:p) = case c of
  Define sauce _ _ -> sauce:macros p
  otherwise -> macros p

--Sources:
--https://stackoverflow.com/questions/21354623/haskell-string-function
--https://mail.haskell.org/pipermail/haskell-cafe/2001-December/002417.html
{-
map :: (Char -> Char) -> Text -> Text
map f t is the Text obtained by applying f to each element of t.
>>> let message = pack "I am not angry. Not at all."
>>> T.map (\c -> if c == '.' then '!' else c) message
    "I am not angry! Not at all!"
-}
{-
intercalate :: Text -> [Text] -> Text
function takes a Text and a list of Texts and concatenates the list after
interspersing the first argument between each element of the list.
>>> T.intercalate "NI!" ["We", "seek", "the", "Holy", "Grail"]
    "WeNI!seekNI!theNI!HolyNI!Grail"
-}
{-
intersperse :: Char -> Text -> Text
function takes a character and places it between the characters of a Text.
>>> T.intersperse '.' "SHIELD"
    "S.H.I.E.L.D"
-}

-- transforms the abstract syntax (a Haskell value)
--into nicely formatted concrete syntax (a string of characters
-- lin plus x y = "the sum of" ++ x ++ "and" ++ y   -- English
-- lin plus x y = x ++ "+" ++ y                     -- infix
--Recursively Call the function
pretty :: Prog -> String
pretty [] = ""
pretty (he:rest) = prettystrings he ++ pretty rest
--prettystrings [] = ""
--Sources:
--https://stackoverflow.com/questions/22752556/pretty-printing-expression-structure-in-haskell
prettystrings :: Cmd -> String
prettystrings (Pen Up) = "pen Up"
prettystrings (Pen Down) = "Pen Down"
prettystrings (Move (left,right)) = "Move (" ++ draw2 left ++ ", " ++ draw2 right ++ ")"
prettystrings (Define x y z) = "Define " ++ x ++ "(" ++ intercalate ", " y ++ ")"
prettystrings (Call this that) = "Call " ++ this ++ "(" ++ intercalate ", " (map draw2 that) ++ ")"

draw2:: Expr -> String
draw2 (Lit x) = show x
draw2 (Ref val) = val
draw2 (Add left right) = draw2 left ++ "+" ++ draw2 right

-- Given the expression (2+3)+x, optE should return the expression 5+x.
optE :: Expr -> Expr
optE (Add (Lit x) (Lit y)) = Lit (x + y)
optE blimp = blimp

--optimizes all of the expressions contained in a given program using optE.
optP :: Prog -> Prog
optP [] = []
optP (ct:pt) = case ct of
      Move (left, right) -> Move(optE left, optE right):optP pt
      Call ths tht -> Call ths (map optE tht):optP pt
      Define xs ys zs -> Define xs ys (optP zs):optP pt
      _ -> ct:optP pt
