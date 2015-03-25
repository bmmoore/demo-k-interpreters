module StrMap = Map.Make(String)

type aexp = kitem
and  bexp = kitem
and  stmt = kitem
and  pgm = kitem
and kitem = ACon of int
          | AVar of string
          | Div of aexp * aexp
          | Add of aexp * aexp
          | BCon of bool
          | Le of aexp * aexp
          | Not of bexp
          | And of bexp * bexp
          | Assign of string * aexp
          | If of bexp * stmt * stmt
          | While of bexp * stmt
          | Seq of stmt * stmt
          | Skip
          | Pgm of string list * stmt
          | DivL of aexp
          | DivR of aexp
          | AddL of aexp
          | AddR of aexp
          | LeL of aexp
          | LeR of aexp
          | NotF
          | AndL of bexp
          | AssignR of string
          | IfC of stmt * stmt
type cfg = { k : kitem list; state : int StrMap.t}

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
  | {k = AVar i :: rest; state} ->
    (try {k = ACon (StrMap.find i state) :: rest; state}
     with Not_found -> raise (Stuck c))
  | {k = Div (ACon i, ACon j) :: rest} when j != 0 ->
     {c with k = ACon (i/j) :: rest}
  | {k = Add (ACon i, ACon j) :: rest} ->
     {c with k = ACon (i+j) :: rest}
  | {k = Le (ACon i, ACon j) :: rest} ->
     {c with k = BCon (i <= j) :: rest}
  | {k = Not (BCon b) :: rest} ->
     {c with k = BCon (not b) :: rest}
  | {k = And (BCon true, b) :: rest} ->
     {c with k = b :: rest}
  | {k = And (BCon false, _) :: rest} ->
     {c with k = BCon false :: rest}
  | {k = Assign (i,ACon j) :: rest; state} ->
     {k = rest; state = StrMap.add i j state}
  | {k = Seq (s1, s2) :: rest} ->
     {c with k = s1::s2::rest}
  | {k = Skip :: rest} ->
     {c with k = rest}
  | {k = If (BCon true, s, _) :: rest} ->
     {c with k = s::rest}
  | {k = If (BCon false, _, s) :: rest} ->
     {c with k = s::rest}
  | {k = While (b, s) as w :: rest} ->
     {c with k = If (b, Seq (s,w), Skip)::rest}
  | {k = [Pgm (i :: xs, s)]; state} ->
     {k = [Pgm (xs,s)]; state = StrMap.add i 0 state}
  | {k = [Pgm ([], s)]; state} ->
     {c with k = [s]}
  (* Heating/cooling rules *)
  (* Heating *)
  | {k = Div (e1, e2)::rest} when not (aresult e1) ->
     {c with k = e1::DivL e2::rest}
  | {k = Div (e1, e2)::rest} when not (aresult e2) ->
     {c with k = e2::DivR e1::rest}
  | {k = Add (e1, e2)::rest} when not (aresult e1) ->
     {c with k = e1::AddL e2::rest}
  | {k = Add (e1, e2)::rest} when not (aresult e2) ->
     {c with k = e2::AddR e1::rest}
  | {k = Le (e1, e2)::rest} when not (aresult e1) ->
     {c with k = e1::LeL e2::rest}
  | {k = Le (e1, e2)::rest} when aresult e1 && not (aresult e2) ->
     {c with k = e2::LeR e1::rest}
  | {k = Not b::rest} when not (bresult b) ->
     {c with k = b::NotF::rest}
  | {k = And (b1, b2)::rest} when not (bresult b1) ->
     {c with k = b1::AndL b2::rest}
  | {k = Assign (i,e)::rest} when not (aresult e) ->
     {c with k = e::AssignR i::rest}
  | {k = If (b,s1,s2)::rest} when not (bresult b) ->
     {c with k = b::IfC (s1,s2)::rest}
  (* Cooling *)
  | {k = ACon _ as e::DivL e2::rest} ->
     {c with k = Div (e,e2)::rest}
  | {k = ACon _ as e::DivR e1::rest} ->
     {c with k = Div (e1,e)::rest}
  | {k = ACon _ as e::AddL e2::rest} ->
     {c with k = Add (e,e2)::rest}
  | {k = ACon _ as e::AddR e1::rest} ->
     {c with k = Add (e1,e)::rest}
  | {k = ACon _ as e::LeL e2::rest} ->
     {c with k = Le (e,e2)::rest}
  | {k = ACon _ as e::LeR e1::rest} ->
     {c with k = Le (e1,e)::rest}
  | {k = BCon _ as e::NotF::rest} ->
     {c with k = Not e::rest}
  | {k = BCon _ as e::AndL e2::rest} ->
     {c with k = And (e,e2)::rest}
  | {k = ACon _ as e::AssignR i::rest} ->
     {c with k = Assign (i,e)::rest}
  | {k = BCon _ as e::IfC (s1,s2)::rest} ->
     {c with k = If (e,s1,s2)::rest}
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
  {k = [p]; state = StrMap.empty}

let test = run (start (sum (int_of_string (Sys.argv.(1)))))
let print_binding k v = print_string k;print_char ':';print_int v;print_newline ()
let _ = StrMap.iter print_binding test.state
