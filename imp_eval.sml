(* Handwritten evaluation functions for IMP. *)

functor Eval(Map : MAP) = struct
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

type state = int Map.map

exception Stuck

(* Threading the state through aeval and beval is
   surprisingly a few percent faster than having them
   return only the evaluated result *)
fun aeval e state : int * state =
    case e of
      ACon i => (i,state)
    | AVar v =>
      (case Map.find (state,v) of
         SOME i => (i,state)
       | NONE => raise Stuck)
    | Div (e1,e2) =>
      let val (v1,state') = aeval e1 state
          val (v2,state'') = aeval e2 state'
      in if v2 = 0 then raise Stuck else (v1 div v2, state'')
      end
    | Add (e1,e2) =>
      let val (v1,state') = aeval e1 state
          val (v2,state'') = aeval e2 state'
      in (v1+v2,state'')
      end

fun beval e state : bool * state =
    case e of
      BCon b => (b,state)
    | Not e1 =>
      let val (b,state') = beval e1 state
      in (not b, state')
      end
    | And (e1,e2) =>
      let val (b1,state') = beval e1 state
      in if b1 then beval e2 state' else (false, state')
      end
    | Le (e1,e2) =>
      let val (v1,state') = aeval e1 state
          val (v2,state'') = aeval e2 state'
      in (v1<=v2, state'')
      end

fun seval s state : state =
    case s of
      Assign (i,e) =>
      let val (v,state') = aeval e state
      in Map.insert (state',i,v)
      end
    | If (be,s1,s2) =>
      let val (b,state') = beval be state
      in if b then seval s1 state' else seval s2 state'
      end
    | w as While (b,s) => seval (If (b,Seq(s,w),Skip)) state
    | Seq (s1,s2) => seval s2 (seval s1 state)
    | Skip => state

fun peval p state : state =
    case p of
      Pgm ([],s) => seval s state
    | Pgm (i::xs,s) => peval (Pgm (xs,s)) (Map.insert (state,i,0))

fun sum_pgm size =
  Pgm ([n,sum],
  Seq (Assign (n,ACon size),
  Seq (Assign (sum,ACon 0),
       While (Not (Le (AVar n, ACon 0)),
         Seq (Assign (sum,Add (AVar sum, AVar n)),
              Assign (n, Add (AVar n, ACon (~1))))))))

fun test size = Map.report (peval (sum_pgm size) Map.empty)
end

functor Specialized(Map : MAP) = struct
open Map.Var

fun find (map,v) = valOf (Map.find (map,v))

fun sum_loop state = 
  if not (find (state,n) <= 0)
  then sum_loop
    (let val state' = Map.insert (state,sum,find (state,sum) + find (state,n))
     in Map.insert (state',n,find (state',n) + ~1)
     end)
  else state
fun test1 size = sum_loop
  (Map.insert (Map.insert (Map.insert (Map.insert
    (Map.empty,n,0),sum,0),n,size),sum,0))
fun test size = Map.report (test1 size)
end

structure Hard = struct
fun loop2 (n,sum) = if not (n <= 0) then loop2 (n - 1,sum+n) else
   StrMap.report (StrMap.insert (StrMap.insert (StrMap.empty,"n",n),"sum",sum))
fun test size = loop2 (size,0)
end
