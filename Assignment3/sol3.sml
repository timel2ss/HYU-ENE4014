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

(* 2. match *)
fun match(v: valu, p: pattern) =
    case (v, p) of
          (_, Wildcard) => SOME []
        | (_, Variable(s)) => SOME [(s, v)]
        | (Unit, UnitP) => SOME []
        | (Const(i1), ConstP(i2)) => if i1 = i2
                                     then SOME []
                                     else NONE
        | (Tuple(vl), TupleP(pl)) => if length vl = length pl
                                     then
                                         let
                                             val pair = ListPair.zip(vl, pl)
                                             val matchPair = List.filter (fn x => isSome(match(x))) pair
                                         in
                                             if length matchPair = length pl
                                             then SOME(foldl (fn (x, y) => valOf(match(x)) @ y) [] matchPair)
                                             else NONE
                                         end
                                     else NONE
        | (Constructor(s1, v), ConstructorP(s2, p)) => if s1 = s2
                                                       then match(v, p)
                                                       else NONE
        | _ => NONE

(* Test Codes *)
val p1 = ConstructorP("what", TupleP([TupleP([UnitP, Wildcard, ConstP(1), TupleP([Variable("b"), Variable("c"), ConstP(3)]), Variable("a")])]))
val p2 = ConstructorP("what", TupleP([TupleP([UnitP, Wildcard, ConstP(1), TupleP([Variable("a"), Variable("a"), ConstP(3)]), Variable("a")])]))
val v1 = Constructor("what", Tuple([Tuple([Unit, Const(1), Const(1), Tuple([Const(4), Unit, Const(3)]), Tuple([Const(5),Const(6)])])]))
val checkPatTest1 = check_pat(p1)
val checkPatTest2 = check_pat(p2)

val matchTest1 = match(v1, p1)          
val matchTest2 = match(v1, p1)