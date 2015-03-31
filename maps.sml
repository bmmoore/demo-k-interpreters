signature VAR = sig
  type var
  val n : var
  val sum : var
end
signature MAP = sig
  structure Var : VAR
  type 'a map
  val empty : 'a map
  val insert : ('a map * Var.var * 'a) -> 'a map
  val find : ('a map * Var.var) -> 'a option 
  val report : int map -> unit
end

structure StringKey = struct
  type ord_key = string
  val compare = String.compare
  type var = string
  val n = "n"
  val sum = "sum"
end

structure StrMapOrd = BinaryMapFn(StringKey)

fun print_binding show_key (k,v) = print (show_key k ^ ":" ^ Int.toString v  ^ "\n")

structure StrMap : MAP = struct
  structure Var = StringKey
  open StrMapOrd
  fun report m = appi (print_binding (fn x => x)) m
end

datatype sumVar = VarN | Sum
fun var_to_string v = case v of VarN => "n" | Sum => "sum"

structure VarKey = struct
  type ord_key = sumVar
  type var = sumVar
  val n = VarN
  val sum = Sum
  fun compare (a,b) =
    case (a, b) of
      (VarN, VarN) => EQUAL
    | (VarN, Sum) => LESS
    | (Sum, Sum) => EQUAL
    | (Sum, VarN) => GREATER
end
structure VarMapOrd = BinaryMapFn(VarKey)

structure VarBinMap : MAP = struct
  structure Var = VarKey
  open VarMapOrd
  fun report m = appi (print_binding var_to_string) m
end

structure VarSlotMap : MAP = struct
  structure Var = VarKey
  type 'a map = ('a option * 'a option)
  fun find ((n,s),v) = case v of VarN => n | Sum => s
  fun insert ((n,s),v,x) = case v of VarN => (SOME x,s) | Sum => (n,SOME x)
  val empty = (NONE, NONE)
  fun appi f (n,s) =
      let val _ = case n of NONE => () | SOME v => f (VarN,v)
      in case s of NONE => () | SOME v => f (Sum,v)
      end
  fun report m = appi (print_binding var_to_string) m
end
