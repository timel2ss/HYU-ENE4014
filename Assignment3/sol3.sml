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
fun check_pat(p: pattern) =
    let
        fun make_str_list(p: pattern, sl: string list) =
            case p of
                  Variable(s) => s::sl
                | TupleP(ps) => foldl make_str_list sl ps
                | ConstructorP(_, p) => make_str_list(p, sl)
                | _ => sl

        fun distinct(sl: string list) =
            case sl of
                  [] => true
                | s::[] => true
                | s::rest => not(List.exists (fn x => x = s) rest) andalso distinct(rest)
    in
        distinct(make_str_list(p, []))
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
                                             val matchPair = List.filter (fn x => isSome(match(x))) (ListPair.zip(vl, pl))
                                         in
                                             if length matchPair = length vl
                                             then SOME(foldl (fn (x, y) => valOf(match(x)) @ y) [] matchPair)
                                             else NONE
                                         end
                                     else NONE
        | (Constructor(s1, v), ConstructorP(s2, p)) => if s1 = s2
                                                       then match(v, p)
                                                       else NONE
        | _ => NONE


(* 3. Tournament of Rock, Paper, Scissors game *)
type name = string
datatype RSP = ROCK
             | SCISSORS
             | PAPER

datatype 'a strategy = Cons of 'a * (unit -> 'a strategy)
datatype tournament = PLAYER of name * (RSP strategy ref)
                    | MATCH of tournament * tournament

fun onlyOne(one: RSP) = Cons(one, fn() => onlyOne(one))
fun alterTwo(one: RSP, two: RSP) = Cons(one, fn() => alterTwo(two, one))
fun alterThree(one: RSP, two: RSP, three: RSP) = Cons(one, fn() => alterThree(two, three, one))

val r = onlyOne(ROCK)
val s = onlyOne(SCISSORS)
val p = onlyOne(PAPER)
val rp = alterTwo(ROCK, PAPER)
val sr = alterTwo(SCISSORS, ROCK)
val ps = alterTwo(PAPER, SCISSORS)
val srp = alterThree(SCISSORS, ROCK, PAPER)

fun next(strategyRef) =
    let
        val Cons(rsp, func) = !strategyRef
    in
        strategyRef := func();
        rsp
    end

fun whosWinner(t) =
    let
        fun game(p1, p2) =
            case (p1, p2) of
                (PLAYER(n1, s1), PLAYER(n2, s2)) => let
                                                        fun rule(RSP1, RSP2) =
                                                            case (RSP1, RSP2) of
                                                              (ROCK, SCISSORS) => SOME p1
                                                            | (ROCK, PAPER) => SOME p2
                                                            | (SCISSORS, PAPER) => SOME p1
                                                            | (SCISSORS, ROCK) => SOME p2
                                                            | (PAPER, ROCK) => SOME p1
                                                            | (PAPER, SCISSORS) => SOME p2
                                                            | _ => NONE

                                                        val nextRSP1 = next(s1)
                                                        val nextRSP2 = next(s2)
                                                        val winner = rule(nextRSP1, nextRSP2)
                                                    in
                                                        if isSome(winner)
                                                        then valOf(winner)
                                                        else game(p1, p2)
                                                    end

    in
        case t of
              PLAYER(_, _) => t
            | MATCH(t1, t2) => game(whosWinner(t1), whosWinner(t2))
    end

(* (* Test Codes *)
(* Test Data *)
val p1 = Variable("a")
val p2 = ConstructorP("wildcard", Wildcard)
val p3 = ConstructorP("constructor", TupleP([ConstP(17), Variable("a"), TupleP([Variable("b"), Wildcard, Variable("c"), TupleP([UnitP, Variable("d"), Variable("e")])])]))
val p4 = ConstructorP("constructor", TupleP([ConstP(17), Variable("a"), TupleP([Variable("b"), Wildcard, Variable("c"), TupleP([UnitP, Variable("d"), Variable("a")])])]))
val p5 = ConstructorP("constructor", TupleP([ConstP(17), Variable("a"), TupleP([Variable("b"), Wildcard, Variable("c"), TupleP([UnitP, Variable("d"), Variable("e")])]), Variable("f")]))
val p6 = ConstructorP("constructor", TupleP([ConstP(17), Variable("a"), TupleP([Variable("b"), Wildcard, Variable("c"), TupleP([UnitP, Variable("d"), Variable("e")])]), Variable("f"), Variable("d")]))
val v1 = Const(42)
val v2 = Constructor("wild", Unit)
val v3 = Constructor("wildcard", Const(42))
val v4 = Constructor("constructor", Tuple([Const(17), Unit, Tuple([Const(18), Const(19), Const(20), Tuple([Unit, Const(21), Unit])])]))
val v5 = Constructor("constructor", Tuple([Const(17), Unit, Tuple([Const(18), Const(19), Const(20), Tuple([Unit, Const(21), Unit])]), Const(22)]))
val v6 = Constructor("constructor", Tuple([Const(15), Unit, Tuple([Const(18), Const(19), Const(20), Tuple([Unit, Const(21), Unit])]), Const(22)]))
val v7 = Constructor("constructor", Tuple([Const(17), Unit, Tuple([Const(19), Const(20), Tuple([Unit, Const(21), Unit])]), Const(22)]))
val winner1 = PLAYER("rp", ref rp)
val winner2 = PLAYER("Emily", ref srp)

val checkPatTest1 = check_pat(p1) = true
val checkPatTest2 = check_pat(p2) = true
val checkPatTest3 = check_pat(p3) = true
val checkPatTest4 = check_pat(p4) = false
val checkPatTest5 = check_pat(p5) = true
val checkPatTest6 = check_pat(p6) = false

val matchTest1 = match(v1, p1) = SOME [("a", Const(42))] 
val matchTest2 = match(v2, p2) = NONE
val matchTest3 = match(v3, p2) = SOME []
val matchTest4 = match(v4, p3) = SOME [("e", Unit), ("d", Const(21)), ("c", Const(20)), ("b", Const(18)), ("a", Unit)]
val matchTest5 = match(v5, p5) = SOME [("f", Const(22)), ("e", Unit), ("d", Const(21)), ("c", Const(20)), ("b", Const(18)), ("a", Unit)]
val matchTest6 = match(v6, p5) = NONE
val matchTest7 = match(v7, p5) = NONE

val whosWinnerTest1 = whosWinner(MATCH(PLAYER("s", ref s), MATCH(winner1, PLAYER("r", ref r)))) = winner1
val whosWinnerTest2 = whosWinner(MATCH(MATCH(PLAYER("John", ref sr), PLAYER("Steve", ref s)), MATCH(PLAYER("Alice", ref p), MATCH(PLAYER("David", ref r), MATCH(PLAYER("Bill", ref s), winner2))))) = winner2 *)