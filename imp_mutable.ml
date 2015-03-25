type aexp = kitem
and  bexp = kitem
and  stmt = kitem
and  pgm = kitem
and  kitem = ACon of int
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
           | Pgm of string list ref * stmt
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
type cfg = { mutable k : kitem list; state : (string, int) Hashtbl.t}

let aresult (a:aexp) : bool =
  match a with
  | ACon _ -> true
  | _ -> false
let bresult (b:bexp) : bool =
  match b with
  | BCon _ -> true
  | _ -> false

exception Stuck
let step (c:cfg) : unit =
  match c.k with
  (* language rules *)
  | AVar i :: rest ->
    (try let v = Hashtbl.find c.state i
     in c.k <- ACon v :: rest
     with Not_found -> raise Stuck)
  | Div (ACon i, ACon j) :: rest when j != 0 ->
     c.k <- ACon (i/j) :: rest
  | Add (ACon i, ACon j) :: rest ->
     c.k <- ACon (i+j) :: rest
  | Le (ACon i, ACon j) :: rest ->
     c.k <- BCon (i <= j) :: rest
  | Not (BCon b) :: rest ->
     c.k <- BCon (not b) :: rest
  | And (BCon true, b) :: rest ->
     c.k <- b :: rest
  | And (BCon false, _) :: rest ->
     c.k <- BCon false :: rest
  | Assign (i,ACon j) :: rest ->
     Hashtbl.replace c.state i j; c.k <- rest
  | Seq (s1, s2) :: rest ->
     c.k <- s1::s2::rest
  | Skip :: rest ->
     c.k <- rest
  | If (BCon true, s, _) :: rest ->
     c.k <- s::rest
  | If (BCon false, _, s) :: rest ->
     c.k <- s::rest
  | While (b, s) as w :: rest ->
     c.k <- If (b, Seq (s,w), Skip)::rest
  | [Pgm ({contents = i :: xs} as vars, s)] ->
     Hashtbl.add c.state i 0; vars := xs
  | [Pgm ({contents = []}, s)] ->
     c.k <- [s]
  (* Heating/cooling rules *)
  (* Heating *)
  | Div (e1, e2)::rest when not (aresult e1) ->
     c.k <- e1::DivL e2::rest
  | Div (e1, e2)::rest when not (aresult e2) ->
     c.k <- e2::DivR e1::rest
  | Add (e1, e2)::rest when not (aresult e1) ->
     c.k <- e1::AddL e2::rest
  | Add (e1, e2)::rest when not (aresult e2) ->
     c.k <- e2::AddR e1::rest
  | Le (e1, e2)::rest when not (aresult e1) ->
     c.k <- e1::LeL e2::rest
  | Le (e1, e2)::rest when aresult e1 && not (aresult e2) ->
     c.k <- e2::LeR e1::rest
  | Not b::rest when not (bresult b) ->
     c.k <- b::NotF::rest
  | And (b1, b2)::rest when not (bresult b1) ->
     c.k <- b1::AndL b2::rest
  | Assign (i,e)::rest when not (aresult e) ->
     c.k <- e::AssignR i::rest
  | If (b,s1,s2)::rest when not (bresult b) ->
     c.k <- b::IfC (s1,s2)::rest
  (* Cooling *)
  | ACon _ as e::DivL e2::rest ->
     c.k <- Div (e,e2)::rest
  | ACon _ as e::DivR e1::rest ->
     c.k <- Div (e1,e)::rest
  | ACon _ as e::AddL e2::rest ->
     c.k <- Add (e,e2)::rest
  | ACon _ as e::AddR e1::rest ->
     c.k <- Add (e1,e)::rest
  | ACon _ as e::LeL e2::rest ->
     c.k <- Le (e,e2)::rest
  | ACon _ as e::LeR e1::rest ->
     c.k <- Le (e1,e)::rest
  | BCon _ as e::NotF::rest ->
     c.k <- Not e::rest
  | BCon _ as e::AndL e2::rest ->
     c.k <- And (e,e2)::rest
  | ACon _ as e::AssignR i::rest ->
     c.k <- Assign (i,e)::rest
  | BCon _ as e::IfC (s1,s2)::rest ->
     c.k <- If (e,s1,s2)::rest
  | _ -> raise Stuck

let run c = try while true do step c done with Stuck -> ()

let sum n =
  Pgm (ref ["n";"sum"],
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
  {k = [p]; state = Hashtbl.create 10}

let test n = let c = start (sum n) in
  run c;
  Hashtbl.iter (fun k v -> print_string k;print_char ':';print_int v;print_newline ()) c.state
let _ = test (int_of_string (Sys.argv.(1)))
