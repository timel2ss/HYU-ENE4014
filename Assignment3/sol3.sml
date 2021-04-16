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

(* Test Codes *)
val p1 = ConstructorP("what", TupleP([TupleP([UnitP, Wildcard, ConstP(1), TupleP([Variable("b"), Variable("c"), ConstP(3)]), Variable("a")])]))
val p2 = ConstructorP("what", TupleP([TupleP([UnitP, Wildcard, ConstP(1), TupleP([Variable("a"), Variable("a"), ConstP(3)]), Variable("a")])]))
val v1 = Constructor("what", Tuple([Tuple([Unit, Const(1), Const(1), Tuple([Const(4), Unit, Const(3)]), Tuple([Const(5),Const(6)])])]))
val checkPatTest1 = check_pat(p1)
val checkPatTest2 = check_pat(p2)

val matchTest1 = match(v1, p1)          
val matchTest2 = match(v1, p1)

val whosWinnerTest1 = whosWinner(MATCH(PLAYER("s", ref s), MATCH(PLAYER("rp", ref rp), PLAYER("r", ref r))))

