module HW3 where

import MiniMiniLogo
import Render


--
-- * Semantics of MiniMiniLogo
--
-- Author: Onur Sasmaz

-- NOTE:
--  * MiniMiniLogo.hs defines the abstract syntax of MiniMiniLogo and some
--    functions for generating MiniMiniLogo programs. It contains the type
--    definitions for Mode, Cmd, and Prog.
--  * Render.hs contains code for rendering the output of a MiniMiniLogo
--    program in HTML5. It contains the types definitions for Point and Line.

-- | A type to represent the current state of the pen.
type State = (Mode,Point)

-- | The initial state of the pen.
start :: State
start = (Up,(0,0))

-- | A function that renders the image to HTML. Only works after you have
--   implemented `prog`. Applying `draw` to a MiniMiniLogo program will
--   produce an HTML file named MiniMiniLogo.html, which you can load in
--   your browswer to view the rendered image.
draw :: Prog -> IO ()
draw p = let (_,ls) = prog p start in toHTML ls


-- Semantic domains:
--   * Cmd:  State -> (State, Maybe Line)
--   * Prog: State -> (State, [Line])


-- | Semantic function for Cmd.
--
--   >>> cmd (Pen Down) (Up,(2,3))
--   ((Down,(2,3)),Nothing)
--
--   >>> cmd (Pen Up) (Down,(2,3))
--   ((Up,(2,3)),Nothing)
--
--   >>> cmd (Move 4 5) (Up,(2,3))
--   ((Up,(4,5)),Nothing)
--
--   >>> cmd (Move 4 5) (Down,(2,3))
--   ((Down,(4,5)),Just ((2,3),(4,5)))
--
--- MiniMiniLogo commands
cmd :: Cmd -> State -> (State, Maybe Line)
cmd (Pen updw) (_, x) = ((updw, x), Nothing)
cmd (Move v1 v2) (updw, x) = case updw of
                       Up -> ((updw, (v1,v2)), Nothing)
                       Down -> ((updw, (v1,v2)), Just (x, (v1,v2)))

-- | Semantic function for Prog.
--
--   >>> prog (nix 10 10 5 7) start
--   ((Down,(15,10)),[((10,10),(15,17)),((10,17),(15,10))])
--
--   >>> prog (steps 2 0 0) start
--   ((Down,(2,2)),[((0,0),(0,1)),((0,1),(1,1)),((1,1),(1,2)),((1,2),(2,2))])

-- A program changes the state of the pen and may draw several lines.
--- Source: https://wiki.haskell.org/Let_vs._Where
--- Source2: http://zvon.org/other/haskell/Outputsyntax/letQexpressions_reference.html

prog :: Prog -> State -> (State, [Line])
prog []     str = (str,[])
prog (x:xs) str = case cmd x str of
  (val,Nothing) -> prog xs val
  (val,Just ln)  -> let (val2,lest) = prog xs val in (val2,ln:lest)


--
-- * Extra credit
--
-- | This should be a MiniMiniLogo program that draws an amazing picture.
--   Add as many helper functions as you want.
--- Source: http://duggie.weebly.com/uploads/3/7/3/0/3730287/coord_lists.pdf

--- Helper functions for drawing 'Amazing' picture
--mrbox :: Int -> Int -> Int -> Int -> Prog
--Draw box
mrbox x y w h = [Pen Up, Move x y, Pen Down,
                Move (x+w) y, Move (x+w) (y+h), Move x (y+h), Move x y]
--Draw box-line
--mrbox :: Int -> Int -> Int -> Int -> Prog
mrbox2 x y w = [Pen Up, Move x y, Pen Down,
                Move (x+w) y, Move (x+w) (y), Move x (y), Move x y]
--line :: Int -> Int -> Int -> Int -> Prog
line x1 y1 x2 y2 = [Pen Up, Move x1 y1, Pen Down, Move x2 y2, Pen Up]


amazing :: Prog
amazing = [Pen Up, Move 10 0, Pen Down, Move 10 3, Move 8 3,
              Move 6 5, Move 2 5, Move 1 6, Move 2 6,
              Move 2 11, Move 1 12, Move 1 16, Move 2 17,
              Move 2 26, Move 3 25, Move 6 26,
              Move 7 25, Move 8 26, Move 9 25, Move 10 26, Move 11 25,
              Move 12 26, Move 13 25, Move 14 26, Move 15 25, Move 16 26,
              Move 17 25, Move 18 26, Move 18 15, Move 17 4, Move 16 3,
              Move 16 0,
              Pen Up, Move 6 9, Pen Down, Move 5 8, Move 4 8, Move 3 9,
              Move 3 10, Move 4 11, Move 5 11, Move 6 12, Move 7 12,
              --eye
              Pen Up, Move 3 11, Pen Down, Move 2 11,
              Pen Up, Move 7 5, Pen Down, Move 5 5, Move 12 5, Move 6 5
              ] ++ mrbox 8 13 7 4 ++ mrbox2 15 17 3 ++ mrbox2 15 17 3  ++
              mrbox2 6 15 2 ++ mrbox 0 13 6 4

-- Draw box in box, not pretty enough...
--amazing = mrbox 2 2 10 10 ++ mrbox 4 4 6 6 ++ mrbox 6 6 2 2
--  ++ mrbox 6 6 1 1 ++ mrbox 4 4 1 1 ++ mrbox 2 2 1 1
---  ++ mrbox 2 2 2 2 ++ mrbox 2 2 4 4 ++ mrbox 6 6 6 6
--  ++ mrbox 2 2 8 8 ++ mrbox 2 2 1 1 ++ mrbox 4 4 8 8
--  ++ mrbox 4 4 3 3 ++ mrbox 4 4 4 4 ++ mrbox 4 4 5 5
--  ++ line 2 2 2 2
