# Haskell MiniMiniLogo
## Description
MiniMiniLogo is a simplified version of the MiniLogo language. Which is itself a simplified version of the Logo language for programming simple 2D graphics.

MiniMiniLogo is like MiniLogo, except that it doesn’t have macros, variables, or addition. This leaves only a very simple syntax remaining.

## Tasks:
* 1) Implement cmd, the semantic function for MiniMiniLogo commands (Cmd). Note that a command updates the state of the pen and possibly draws a line. Therefore, the semantic domain is State -> (State, Maybe Line).

* 2) Implement prog, the semantic function for MiniMiniLogo programs (Prog). A program changes the state of the pen and may draw several lines. Therefore, the semantic domain is State -> (State, [Line]).

#### Extra:
* 3) Define the amazing variable in the template file. This should be the MiniMiniLogo program that generates the amazing picture.

* 4) Create a .html file produced by running draw amazing picture. (Bart Simpson wearing sun glasses)
