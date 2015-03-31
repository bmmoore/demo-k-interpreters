{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
import Control.Exception
import System.Environment
import Data.Typeable
import Data.Int
import qualified Data.Map.Strict as Map
import Data.Text

type Var = Text

type AExp = KItem
type BExp = KItem
type Stmt = KItem
data KItem =
   ACon {-# UNPACK #-} !Int64
 | AVar !Var
 | Div !AExp !AExp
 | Add !AExp !AExp
 | BCon !Bool
 | Le !AExp !AExp
 | Not !BExp
 | And !BExp !BExp
 | Assign !Var !AExp
 | If !BExp !Stmt !Stmt
 | While !BExp !Stmt
 | Seq !Stmt !Stmt
 | Skip
 | Pgm ![Var] !Stmt
 | DivL !AExp
 | DivR !AExp
 | AddL !AExp
 | AddR !AExp
 | LeL !AExp
 | LeR !AExp
 | NotF
 | AndL !BExp
 | AssignR !Var
 | IfC !Stmt !Stmt
  deriving Show
data Cfg = Cfg {k :: ![KItem], state :: !(Map.Map Var Int64)}
  deriving Show

aresult :: AExp -> Bool
aresult (ACon _) = True
aresult _ = False

bresult :: BExp -> Bool
bresult (BCon _) = True
bresult _ = False

data Stuck = Stuck Cfg
  deriving (Show, Typeable)
instance Exception Stuck

step c = let withK k = c {k = k} in case c of
  Cfg {k=AVar i : rest, state} ->
    Cfg (ACon (state Map.! i):rest) state
  Cfg {k = Div (ACon i) (ACon j):rest} | j /= 0 ->
    withK (ACon (i `div` j):rest)
  Cfg {k = Add (ACon i) (ACon j):rest} ->
    withK (ACon (i+j):rest)
  Cfg {k = Le (ACon i) (ACon j):rest} ->
    withK (BCon (i <= j):rest)
  Cfg {k = Not (BCon b) : rest} ->
    withK (BCon (not b) : rest)
  Cfg {k = And (BCon True) b : rest} ->
    withK (b : rest)
  Cfg {k = And (BCon False) _ : rest} ->
    withK (BCon False : rest)
  Cfg {k = Assign i (ACon j) : rest, state} ->
    Cfg {k = rest, state = Map.insert i j state}
  Cfg {k = Seq s1 s2 : rest} ->
    withK (s1:s2:rest)
  Cfg {k = Skip : rest} ->
    withK rest
  Cfg {k = If (BCon True) s _ : rest} ->
    withK (s:rest)
  Cfg {k = If (BCon False) _ s : rest} ->
    withK (s:rest)
  Cfg {k = w@(While b s) : rest} ->
    withK (If b (Seq s w) Skip:rest)
  Cfg {k = [Pgm (i : xs) s], state} ->
    Cfg {k = [Pgm xs s], state = Map.insert i 0 state}
  Cfg {k = [Pgm [] s]} ->
    withK [s]
  -- Heating/cooling rules
  -- Heating
  Cfg {k = Div e1 e2:rest} | not (aresult e1) ->
     withK (e1:DivL e2:rest)
  Cfg {k = Div e1 e2:rest} | not (aresult e2) ->
     withK (e2:DivR e1:rest)
  Cfg {k = Add e1 e2:rest} | not (aresult e1) ->
     withK (e1:AddL e2:rest)
  Cfg {k = Add e1 e2:rest} | not (aresult e2) ->
     withK (e2:AddR e1:rest)
  Cfg {k = Le e1 e2:rest} | not (aresult e1) ->
     withK (e1:LeL e2:rest)
  Cfg {k = Le e1 e2:rest} | aresult e1 && not (aresult e2) ->
     withK (e2:LeR e1:rest)
  Cfg {k = Not b:rest} | not (bresult b) ->
     withK (b:NotF:rest)
  Cfg {k = And b1 b2:rest} | not (bresult b1) ->
     withK (b1:AndL b2:rest)
  Cfg {k = Assign i e:rest} | not (aresult e) ->
     withK (e:AssignR i:rest)
  Cfg {k = If b s1 s2:rest} | not (bresult b) ->
     withK (b:IfC s1 s2:rest)
  -- Cooling
  Cfg {k = e@(ACon _):DivL e2:rest} ->
     withK (Div e e2:rest)
  Cfg {k = e@(ACon _):DivR e1:rest} ->
     withK (Div e1 e:rest)
  Cfg {k = e@(ACon _):AddL e2:rest} ->
     withK (Add e e2:rest)
  Cfg {k = e@(ACon _):AddR e1:rest} ->
     withK (Add e1 e:rest)
  Cfg {k = e@(ACon _):LeL e2:rest} ->
     withK (Le e e2:rest)
  Cfg {k = e@(ACon _):LeR e1:rest} ->
     withK (Le e1 e:rest)
  Cfg {k = e@(BCon _):NotF:rest} ->
     withK (Not e:rest)
  Cfg {k = e@(BCon _):AndL e2:rest} ->
     withK (And e e2:rest)
  Cfg {k = e@(ACon _):AssignR i:rest} ->
     withK (Assign i e:rest)
  Cfg {k = e@(BCon _):IfC s1 s2:rest} ->
     withK (If e s1 s2:rest)
  _ -> throw (Stuck c)

sumCode :: Int64 -> KItem
sumCode n =
  Pgm ["n","sum"]
  (Seq (Assign "n" (ACon n))
  (Seq (Assign "sum" (ACon 0))
       (While (Not (Le (AVar "n") (ACon 0)))
         (Seq (Assign "sum" (Add (AVar "sum") (AVar "n")))
              (Assign "n" (Add (AVar "n") (ACon (-1))))))))
{-
  int n, sum;
n = 100;
sum = 0;
while (!(n <= 0)) {
  sum = sum + n;
  n = n + -1;
}
 -}

start :: KItem -> Cfg
start p = Cfg [p] Map.empty

run :: Cfg -> IO Cfg
run c = catch (evaluate (go c)) (\(Stuck c') -> return c')
  where go c = c `seq` go (step c)

main = do
  [size] <- getArgs
  c <- run (start (sumCode (read size)))
  print c
