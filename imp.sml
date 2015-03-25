structure StringKey : ORD_KEY = struct
  type ord_key = string
  val compare = String.compare
end
structure StrMap = BinaryMapFn(StringKey)

datatype kitem = ACon of int
          | AVar of string
          | Div of kitem * kitem
          | Add of kitem * kitem
          | BCon of bool
          | Le of kitem * kitem
          | Not of kitem
          | And of kitem * kitem
          | Assign of string * kitem
          | If of kitem * kitem * kitem
          | While of kitem * kitem
          | Seq of kitem * kitem
          | Skip
          | Pgm of string list * kitem
          | DivL of kitem
          | DivR of kitem
          | AddL of kitem
          | AddR of kitem
          | LeL of kitem
          | LeR of kitem
          | NotF
          | AndL of kitem
          | AssignR of string
          | IfC of kitem * kitem

type cfg = { k : kitem list, state : int StrMap.map}

exception Stuck of cfg

fun aresult (a:kitem) : bool =
  case a of
    ACon _ => true
  | _ => false
fun bresult (b:kitem) : bool =
  case b of
    BCon _ => true
  | _ => false

fun withK k {k = _, state = state} = {k = k, state = state}

fun step (c:cfg) : cfg =
  case c of
    {k = AVar i :: rest, state} => 
      (case StrMap.find (state, i) of
         SOME v => withK (ACon v :: rest) c
       | NONE => raise (Stuck c))
  | {k = Div (ACon i, ACon j) :: rest, state} =>
    if j = 0 then raise (Stuck c) else
     withK (ACon (i div j)::rest) c
  | {k = Add (ACon i, ACon j) :: rest, state} =>
     withK (ACon (i+j) :: rest) c
  | {k = Le (ACon i, ACon j) :: rest, state} =>
     withK (BCon (i <= j) :: rest) c
  | {k = Not (BCon b) :: rest, state} =>
     withK (BCon (not b) :: rest) c
  | {k = And (BCon true, b) :: rest, state} =>
     withK (b :: rest) c
  | {k = And (BCon false, _) :: rest, state} =>
     withK (BCon false :: rest) c
  | {k = Assign (i,ACon j) :: rest, state} =>
     {k = rest, state = StrMap.insert (state,i,j)}
  | {k = Seq (s1, s2) :: rest, state} =>
     withK (s1::s2::rest) c
  | {k = Skip :: rest, state} =>
     withK (rest) c
  | {k = If (BCon true, s, _) :: rest, state} =>
     withK (s::rest) c
  | {k = If (BCon false, _, s) :: rest, state} =>
     withK (s::rest) c
  | {k = (w as While (b, s)) :: rest, state} =>
     withK (If (b, Seq (s,w), Skip)::rest) c
  | {k = [Pgm (i :: xs, s)], state} =>
     {k = [Pgm (xs,s)], state = StrMap.insert (state,i,0)}
  | {k = [Pgm ([], s)], state} =>
     withK ([s]) c
  (* Heating/cooling rules *)
  (* Heating *)
  | {k = Div (e1, e2)::rest, state} =>
    if not (aresult e1) then
       withK (e1::DivL e2::rest) c
    else if not (aresult e2) then
       withK (e2::DivR e1::rest) c
    else raise (Stuck c)
  | {k = Add (e1, e2)::rest, state} =>
    if not (aresult e1) then
       withK (e1::AddL e2::rest) c
    else if not (aresult e2) then
       withK (e2::AddR e1::rest) c
    else raise (Stuck c)
  | {k = Le (e1, e2)::rest, state} =>
    if not (aresult e1) then
       withK (e1::LeL e2::rest) c
    else if not (aresult e2) then
       withK (e2::LeR e1::rest) c
    else raise (Stuck c)
  | {k = Not b::rest, state} =>
    if not (bresult b) then
       withK (b::NotF::rest) c
    else raise (Stuck c)
  | {k = And (b1, b2)::rest, state} =>
    if not (bresult b1) then
       withK (b1::AndL b2::rest) c
    else raise (Stuck c)
  | {k = Assign (i,e)::rest, state} =>
    if not (aresult e) then
       withK (e::AssignR i::rest) c
    else raise (Stuck c)
  | {k = If (b,s1,s2)::rest, state} =>
    if not (bresult b) then
       withK (b::IfC (s1,s2)::rest) c
    else raise (Stuck c)
  (* Cooling *)
  | {k = (e as ACon _)::DivL e2::rest, state} =>
     withK (Div (e,e2)::rest) c
  | {k = (e as ACon _)::DivR e1::rest, state} =>
     withK (Div (e1,e)::rest) c
  | {k = (e as ACon _)::AddL e2::rest, state} =>
     withK (Add (e,e2)::rest) c
  | {k = (e as ACon _)::AddR e1::rest, state} =>
     withK (Add (e1,e)::rest) c
  | {k = (e as ACon _)::LeL e2::rest, state} =>
     withK (Le (e,e2)::rest) c
  | {k = (e as ACon _)::LeR e1::rest, state} =>
     withK (Le (e1,e)::rest) c
  | {k = (e as BCon _)::NotF::rest, state} =>
     withK (Not e::rest) c
  | {k = (e as BCon _)::AndL e2::rest, state} =>
     withK (And (e,e2)::rest) c
  | {k = (e as ACon _)::AssignR i::rest, state} =>
     withK (Assign (i,e)::rest) c
  | {k = (e as BCon _)::IfC (s1,s2)::rest, state} =>
     withK (If (e,s1,s2)::rest) c
  | _ => raise (Stuck c)

fun run c =
  let fun go c = go (step c)
  in go c
  end
  handle Stuck c' => c'

fun sum n =
  Pgm (["n","sum"],
  Seq (Assign ("n",ACon n),
  Seq (Assign ("sum",ACon 0),
       While (Not (Le (AVar "n", ACon 0)),
         Seq (Assign ("sum",Add (AVar "sum", AVar "n")),
              Assign ("n", Add (AVar "n", ACon (~1))))))))
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
  {k = [p], state = StrMap.empty}


val size =
  case CommandLine.arguments() of
    [input] => valOf (Int.fromString input)
  | _ => 1000000

fun print_binding (k,v) = print (k ^ ":" ^ Int.toString v  ^ "\n")

val _ = case run (start (sum size)) of {k = _, state} => StrMap.appi print_binding state
