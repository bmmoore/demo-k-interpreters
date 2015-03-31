functor K(Map : MAP) = struct
open Map.Var

datatype aexp = ACon of int
              | AVar of var
              | Div of aexp * aexp
              | Add of aexp * aexp
datatype bexp = BCon of bool
              | Le of aexp * aexp
              | Not of bexp
              | And of bexp * bexp
datatype stmt = Assign of var * aexp
              | If of bexp * stmt * stmt
              | While of bexp * stmt
              | Seq of stmt * stmt
              | Skip
datatype pgm = Pgm of var list * stmt
datatype freezer = DivL of aexp
                 | DivR of aexp
                 | AddL of aexp
                 | AddR of aexp
                 | LeL of aexp
                 | LeR of aexp
                 | NotF
                 | AndL of bexp
                 | AssignR of var
                 | IfC of stmt * stmt
datatype kitem = AExp of aexp
               | BExp of bexp
               | Stmt of stmt
               | KPgm of pgm
               | Freezer of freezer

type cfg = { k : kitem list, state : int Map.map}

exception Stuck of cfg

fun aresult (a:aexp) : bool =
  case a of
    (ACon _) => true
  | _ => false
fun bresult (b:bexp) : bool =
  case b of
    BCon _ => true
  | _ => false

fun withK k {k = _, state = state} = {k = k, state = state}

fun step (c:cfg) : cfg =
  case c of
    {k = AExp (AVar i) :: rest, state} => 
      (case Map.find (state, i) of
         SOME v => withK (AExp (ACon v) :: rest) c
       | NONE => raise (Stuck c))
  | {k = AExp (Div (ACon i, ACon j)) :: rest, state} =>
    if j = 0 then raise (Stuck c) else
     withK (AExp (ACon (i div j))::rest) c
  | {k = AExp (Add (ACon i, ACon j)) :: rest, state} =>
     withK (AExp (ACon (i+j)) :: rest) c
  | {k = BExp (Le (ACon i, ACon j)) :: rest, state} =>
     withK (BExp (BCon (i <= j)) :: rest) c
  | {k = BExp (Not (BCon b)) :: rest, state} =>
     withK (BExp (BCon (not b)) :: rest) c
  | {k = BExp (And (BCon true, b)) :: rest, state} =>
     withK (BExp b :: rest) c
  | {k = BExp (And (BCon false, _)) :: rest, state} =>
     withK (BExp (BCon false) :: rest) c
  | {k = Stmt (Assign (i,ACon j)) :: rest, state} =>
     {k = rest, state = Map.insert (state,i,j)}
  | {k = Stmt (Seq (s1, s2)) :: rest, state} =>
     withK (Stmt s1::Stmt s2::rest) c
  | {k = Stmt Skip :: rest, state} =>
     withK (rest) c
  | {k = Stmt (If (BCon true, s, _)) :: rest, state} =>
     withK (Stmt s::rest) c
  | {k = Stmt (If (BCon false, _, s)) :: rest, state} =>
     withK (Stmt s::rest) c
  | {k = Stmt (w as While (b, s)) :: rest, state} =>
     withK (Stmt (If (b, Seq (s,w), Skip))::rest) c
  | {k = [KPgm (Pgm (i :: xs, s))], state} =>
     {k = [KPgm (Pgm (xs,s))], state = Map.insert (state,i,0)}
  | {k = [KPgm (Pgm ([], s))], state} =>
     withK ([Stmt s]) c
  (* Heating/cooling rules *)
  (* Heating *)
  | {k = AExp (Div (e1, e2))::rest, state} =>
    if not (aresult e1) then
       withK (AExp e1::Freezer (DivL e2)::rest) c
    else if not (aresult e2) then
       withK (AExp e2::Freezer (DivR e1)::rest) c
    else raise (Stuck c)
  | {k = AExp (Add (e1, e2))::rest, state} =>
    if not (aresult e1) then
       withK (AExp e1::Freezer (AddL e2)::rest) c
    else if not (aresult e2) then
       withK (AExp e2::Freezer (AddR e1)::rest) c
    else raise (Stuck c)
  | {k = BExp (Le (e1, e2))::rest, state} =>
    if not (aresult e1) then
       withK (AExp e1::Freezer (LeL e2)::rest) c
    else if not (aresult e2) then
       withK (AExp e2::Freezer (LeR e1)::rest) c
    else raise (Stuck c)
  | {k = BExp (Not b)::rest, state} =>
    if not (bresult b) then
       withK (BExp b::Freezer NotF::rest) c
    else raise (Stuck c)
  | {k = BExp (And (b1, b2))::rest, state} =>
    if not (bresult b1) then
       withK (BExp b1::Freezer (AndL b2)::rest) c
    else raise (Stuck c)
  | {k = Stmt (Assign (i,e))::rest, state} =>
    if not (aresult e) then
       withK (AExp e::Freezer (AssignR i)::rest) c
    else raise (Stuck c)
  | {k = Stmt (If (b,s1,s2))::rest, state} =>
    if not (bresult b) then
       withK (BExp b::Freezer (IfC (s1,s2))::rest) c
    else raise (Stuck c)
  (* Cooling *)
  | {k = AExp (e as ACon _)::Freezer (DivL e2)::rest, state} =>
     withK (AExp (Div (e,e2))::rest) c
  | {k = AExp (e as ACon _)::Freezer (DivR e1)::rest, state} =>
     withK (AExp (Div (e1,e))::rest) c
  | {k = AExp (e as ACon _)::Freezer (AddL e2)::rest, state} =>
     withK (AExp (Add (e,e2))::rest) c
  | {k = AExp (e as ACon _)::Freezer (AddR e1)::rest, state} =>
     withK (AExp (Add (e1,e))::rest) c
  | {k = AExp (e as ACon _)::Freezer (LeL e2)::rest, state} =>
     withK (BExp (Le (e,e2))::rest) c
  | {k = AExp (e as ACon _)::Freezer (LeR e1)::rest, state} =>
     withK (BExp (Le (e1,e))::rest) c
  | {k = BExp (e as BCon _)::Freezer NotF::rest, state} =>
     withK (BExp (Not e)::rest) c
  | {k = BExp (e as BCon _)::Freezer (AndL e2)::rest, state} =>
     withK (BExp (And (e,e2))::rest) c
  | {k = AExp (e as ACon _)::Freezer (AssignR i)::rest, state} =>
     withK (Stmt (Assign (i,e))::rest) c
  | {k = BExp (e as BCon _)::Freezer (IfC (s1,s2))::rest, state} =>
     withK (Stmt (If (e,s1,s2))::rest) c
  | _ => raise (Stuck c)

fun run c =
  let fun go c = go (step c)
  in go c
  end
  handle Stuck c' => c'

fun sum_pgm size =
  Pgm ([n,sum],
  Seq (Assign (n,ACon size),
  Seq (Assign (sum,ACon 0),
       While (Not (Le (AVar n, ACon 0)),
         Seq (Assign (sum,Add (AVar sum, AVar n)),
              Assign (n, Add (AVar n, ACon (~1))))))))
(*
  int n, sum,
n = 100,
sum = 0,
while (!(n <= 0)) {
  sum = sum + n,
  n = n + -1,
}
 *)

fun start (p : kitem) : cfg =
  {k = [p], state = Map.empty}

fun test size = Map.report (#state (run (start (KPgm (sum_pgm size)))))
end

structure KStr = K(StrMap)

val size =
  case CommandLine.arguments() of
    [input] => valOf (Int.fromString input)
  | _ => 1000000

val _ = KStr.test size
