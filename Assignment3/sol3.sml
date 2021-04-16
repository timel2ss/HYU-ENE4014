(* 2019054957 ÀÌ¿ë¿ì *)

datatype pattern = Wildcard
                 | Variable of string
                 | UnitP
                 | ConstP of int
                 | TupleP of pattern list
                 | ConstructorP of string * pattern
        
datatype valu = Const of int
              | Unit
              | Tuple of valu list
              | Constructor of string * valu

(* 1. check_pat *)
fun check_pat p =
    let
        fun check_string_list(p: pattern, sl: string list) =
            case p of
                  Variable(s) => s::sl
                | TupleP(ps) => foldl check_string_list sl ps
                | ConstructorP(_, p) => check_string_list(p, sl)
                | _ => sl

        fun helper(sl: string list) =
            case sl of
                  [] => true
                | s::rest => not(List.exists (fn x => x = s) rest) andalso helper(rest)
    in
        helper(check_string_list(p, []))
    end

val p1 = ConstructorP("what", TupleP([TupleP([UnitP, Wildcard, ConstP(1), TupleP([Variable("b"), Variable("c"), ConstP(3)]), Variable("a")])]))
val p2 = ConstructorP("what", TupleP([TupleP([UnitP, Wildcard, ConstP(1), TupleP([Variable("a"), Variable("a"), ConstP(3)]), Variable("a")])]))
val v1 = Constructor("what", Tuple([Tuple([Unit, Const(1), Const(1), Tuple([Const(4), Unit, Const(3)]), Tuple([Const(5),Const(6)])])]))
val test = check_pat(p1)
val test2 = check_pat(p2)
