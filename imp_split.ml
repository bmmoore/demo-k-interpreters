type aexp = ACon of int
          | AVar of string
          | Div of aexp * aexp
          | Add of aexp * aexp
type bexp = BCon of bool
          | Le of aexp * aexp
          | Not of bexp
          | And of bexp * bexp
type stmt = Assign of string * aexp
          | If of bexp * stmt * stmt
          | While of bexp * stmt
          | Seq of stmt * stmt
          | Skip
type pgm = Pgm of string list * stmt
type freezer = DivL of aexp
             | DivR of aexp
             | AddL of aexp
             | AddR of aexp
             | LeL of aexp
             | LeR of aexp
             | NotF
             | AndL of bexp
             | AssignR of string
             | IfC of stmt * stmt
type kitem = AExp of aexp
           | BExp of bexp
           | Stmt of stmt
           | KPgm of pgm
           | Freezer of freezer
type cfg = { k : kitem list; state : (string, int) Hashtbl.t}

exception Stuck of cfg
let aresult (a:aexp) : bool =
  match a with
  | ACon _ -> true
  | _ -> false
let bresult (b:bexp) : bool =
  match b with
  | BCon _ -> true
  | _ -> false

let step (c:cfg) : cfg =
  match c with
  (* language rules *)
  | {k = AExp (AVar i) :: rest; state} ->
    (try {k = AExp (ACon (Hashtbl.find state i)) :: rest; state}
     with Not_found -> raise (Stuck c))
  | {k = AExp (Div (ACon i, ACon j)) :: rest} when j != 0 ->
     {c with k = AExp (ACon (i/j)) :: rest}
  | {k = AExp (Add (ACon i, ACon j)) :: rest} ->
     {c with k = AExp (ACon (i+j)) :: rest}
  | {k = BExp (Le (ACon i, ACon j)) :: rest} ->
     {c with k = BExp (BCon (i <= j)) :: rest}
  | {k = BExp (Not (BCon b)) :: rest} ->
     {c with k = BExp (BCon (not b)) :: rest}
  | {k = BExp (And (BCon true, b)) :: rest} ->
     {c with k = BExp b :: rest}
  | {k = BExp (And (BCon false, _)) :: rest} ->
     {c with k = BExp (BCon false) :: rest}
  | {k = Stmt (Assign (i,ACon j)) :: rest; state} ->
     Hashtbl.replace state i j; {c with k = rest}
  | {k = Stmt (Seq (s1, s2)) :: rest} ->
     {c with k = Stmt s1::Stmt s2::rest}
  | {k = Stmt Skip :: rest} ->
     {c with k = rest}
  | {k = Stmt (If (BCon true, s, _)) :: rest} ->
     {c with k = Stmt s::rest}
  | {k = Stmt (If (BCon false, _, s)) :: rest} ->
     {c with k = Stmt s::rest}
  | {k = Stmt (While (b, s) as w) :: rest} ->
     {c with k = Stmt (If (b, Seq (s,w), Skip))::rest}
  | {k = [KPgm (Pgm (i :: xs, s))]; state} ->
     Hashtbl.add state i 0;
     {c with k = [KPgm (Pgm (xs,s))]}
  | {k = [KPgm (Pgm ([], s))]; state} ->
     {c with k = [Stmt s]}
  (* Heating/cooling rules *)
  (* Heating *)
  | {k = AExp (Div (e1, e2))::rest} when not (aresult e1) ->
     {c with k = AExp e1::Freezer (DivL e2)::rest}
  | {k = AExp (Div (e1, e2))::rest} when not (aresult e2) ->
     {c with k = AExp e2::Freezer (DivR e1)::rest}
  | {k = AExp (Add (e1, e2))::rest} when not (aresult e1) ->
     {c with k = AExp e1::Freezer (AddL e2)::rest}
  | {k = AExp (Add (e1, e2))::rest} when not (aresult e2) ->
     {c with k = AExp e2::Freezer (AddR e1)::rest}
  | {k = BExp (Le (e1, e2))::rest} when not (aresult e1) ->
     {c with k = AExp e1::Freezer (LeL e2)::rest}
  | {k = BExp (Le (e1, e2))::rest} when aresult e1 && not (aresult e2) ->
     {c with k = AExp e2::Freezer (LeR e1)::rest}
  | {k = BExp (Not b)::rest} when not (bresult b) ->
     {c with k = BExp b::Freezer NotF::rest}
  | {k = BExp (And (b1, b2))::rest} when not (bresult b1) ->
     {c with k = BExp b1::Freezer (AndL b2)::rest}
  | {k = Stmt (Assign (i,e))::rest} when not (aresult e) ->
     {c with k = AExp e::Freezer (AssignR i)::rest}
  | {k = Stmt (If (b,s1,s2))::rest} when not (bresult b) ->
     {c with k = BExp b::Freezer (IfC (s1,s2))::rest}
  (* Cooling *)
  | {k = AExp (ACon _ as e)::Freezer (DivL e2)::rest} ->
     {c with k = AExp (Div (e,e2))::rest}
  | {k = AExp (ACon _ as e)::Freezer (DivR e1)::rest} ->
     {c with k = AExp (Div (e1,e))::rest}
  | {k = AExp (ACon _ as e)::Freezer (AddL e2)::rest} ->
     {c with k = AExp (Add (e,e2))::rest}
  | {k = AExp (ACon _ as e)::Freezer (AddR e1)::rest} ->
     {c with k = AExp (Add (e1,e))::rest}
  | {k = AExp (ACon _ as e)::Freezer (LeL e2)::rest} ->
     {c with k = BExp (Le (e,e2))::rest}
  | {k = AExp (ACon _ as e)::Freezer (LeR e1)::rest} ->
     {c with k = BExp (Le (e1,e))::rest}
  | {k = BExp (BCon _ as e)::Freezer NotF::rest} ->
     {c with k = BExp (Not e)::rest}
  | {k = BExp (BCon _ as e)::Freezer (AndL e2)::rest} ->
     {c with k = BExp (And (e,e2))::rest}
  | {k = AExp (ACon _ as e)::Freezer (AssignR i)::rest} ->
     {c with k = Stmt (Assign (i,e))::rest}
  | {k = BExp (BCon _ as e)::Freezer (IfC (s1,s2))::rest} ->
     {c with k = Stmt (If (e,s1,s2))::rest}
  | _ -> raise (Stuck c)

let run c =
  try let rec go c = go (step c)
      in go c
  with Stuck c' -> c'

let sum n =
  Pgm (["n";"sum"],
  Seq (Assign ("n",ACon n),
  Seq (Assign ("sum",ACon 0),
       While (Not (Le (AVar "n", ACon 0)),
         Seq (Assign ("sum",Add (AVar "sum", AVar "n")),
              Assign ("n", Add (AVar "n", ACon (-1))))))))
(*
  int n, sum;
n = 100;
sum = 0;
while (!(n <= 0)) {
  sum = sum + n;
  n = n + -1;
}
 *)

let start (p : pgm) : cfg =
  {k = [KPgm p]; state = Hashtbl.create 10}

let test = run (start (sum (int_of_string (Sys.argv.(1)))))
let _ = Hashtbl.iter (fun k v -> print_string k;print_char ':';print_int v;print_newline ()) test.state
