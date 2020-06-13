-- Onur Sasmaz
-- CS381 - HW5
module KarelSemantics where

import Prelude hiding (Either(..))
import Data.Function (fix)

import KarelSyntax
import KarelState

-- type Prog = (Defs,Stmt)
type Cry = Defs -> World -> Robot -> Result

-- | Valuation function for Test.
test :: Test -> World -> Robot -> Bool
test (Not tst)     wrld rbt = not (test tst wrld rbt)
test (Facing crd)     _ rbt = getFacing rbt == crd
test (Clear drr)   wrld rbt = isClear(relativePos drr rbt) wrld
test Beeper        wrld rbt = hasBeeper(getPos rbt) wrld
test Empty            _ rbt = getBag rbt == 0

{-
    data Stmt
   ---------------------
    Shutdown                 -- end the program
    Move                     -- move forward
    PickBeeper               -- take a beeper
    PutBeeper                -- leave a beeper
    Turn    Dir              -- rotate in place
    Call    Macro            -- invoke a macro
    Iterate Int  Stmt        -- fixed repetition loop
    If      Test Stmt Stmt   -- conditional branch
    While   Test Stmt        -- conditional loop
    Block   [Stmt]           -- statement block
-}
-- Domain is: World -> Robot -> Bool.
-- OK value contains the updated state of the world and robot
-- Done value should only be produced by the Shutdown statement
-- Error value that contains an error message
-- show (Error s) = "Error: " ++ s

-- onOK f (OK w r) = f w r

-- Macro tests use lookup function in the Haskell Prelude
-- lookup table relates keys to values. You look up a value by knowing its
-- key and using the lookup table
-- lookup Eq a => a -> [(a,b)]-> Maybe b

-- use replicate from Param.hs for replication
-- Source:http://www.informatik.uni-bremen.de/~clueth/docs/ZVON/Outputprelude/r
-- eplicate_f.html
-- Ifelse https://en.wikibooks.org/wiki/Haskell/Control_structures
-- While https://stackoverflow.com/questions/27404063/what-is-the-equivalent-
-- statement-of-a-while-loop-in-haskell


-- | Valuation function for Stmt.
stmt :: Stmt -> Cry
stmt Shutdown    _ _    rbt = Done rbt
stmt Move        _ wrld rbt =
                        let pos = relativePos Front rbt
                        in if isClear pos wrld --check if front clear
                        then OK wrld (setPos pos  rbt)
                        else Error ("Blocked at: " ++ show pos)
stmt PickBeeper  _ wrld rbt =
                        let pos = getPos rbt
                        in if hasBeeper pos wrld --check for beeper
                        then OK (decBeeper pos wrld) (incBag rbt)
                        else Error ("No beeper to pick at: " ++ show pos)
stmt PutBeeper   _ wrld rbt =
                        let pos = getPos rbt
                        in if isEmpty rbt --check if bag is empt
                        then Error "No beeper to put."
                        else OK (incBeeper pos wrld) (decBag rbt)
stmt (Turn drr)  _ wrld rbt = OK wrld (setFacing ( --update direction
                        cardTurn drr (getFacing rbt)) rbt)
stmt (Call mcr) mv wrld rbt =
                        case lookup mcr mv of
                        Just st -> stmt st mv wrld rbt
                        Nothing -> Error ("Undefined macro: " ++ mcr)
stmt (Iterate 0 st)   mv wrld rbt = OK wrld rbt
stmt (Iterate itr st) mv wrld rbt =
                        case stmt st mv wrld rbt of
                        OK wr rb  -> stmt (Iterate (itr - 1) st) mv wr rb
                        Error err -> Error err
                        Done  dn  -> Done dn
stmt (If tst x1 x2) mv wrld rbt =
                        if test tst wrld rbt
                        then stmt x1 mv wrld rbt
                        else stmt x2 mv wrld rbt
stmt (While tst st) mv wrld rbt =
                        if test tst wrld rbt
                        then onOK (stmt (While tst st) mv) (stmt st mv wrld rbt)
                        else OK wrld rbt
stmt (Block [])       mv wrld rbt = OK wrld rbt
stmt (Block (hed:tl)) mv wrld rbt = case stmt hed mv wrld rbt of
                        OK wr rb -> stmt (Block tl) mv wr rb
                        Error er -> Error er
                        Done  dn -> Done dn

-- case stmt st mv wrld rbt of
-- OK
-- Helper function for stmt
-- While.hs
-- stmts []     = \r -> r
-- stmts (s:ss) = \r -> stmts ss (stmt s r)
-- Ask in class about semantic and dynamic function!!!!!

--hlpstmt :: [Stmt] -> Cry
--hlpstmt [] _ wld rb = OK wld rb
--hlpstmt (st:nd)     =

-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r
