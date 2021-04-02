(* 2019054957 ÀÌ¿ë¿ì *)

(* 1. Simple Eval *)
datatype expr = NUM of int
              | PLUS of expr * expr
              | MINUS of expr * expr

datatype formula = TRUE
                 | FALSE
                 | NOT of formula
                 | ANDALSO of formula * formula
                 | ORELSE of formula * formula
                 | IMPLY of formula * formula
                 | LESS of expr * expr

fun eval(f: formula) =
    let
        fun eval_expr(e: expr) =
            case e of
                  NUM(i) => i
                | PLUS(i1, i2) => eval_expr(i1) + eval_expr(i2)
                | MINUS(i1, i2) => eval_expr(i1) - eval_expr(i2)
    in
        case f of
              TRUE => true
            | FALSE => false
            | NOT(f) => not(eval(f))
            | ANDALSO(f1, f2) => eval(f1) andalso eval(f2)
            | ORELSE(f1, f2) => eval(f1) orelse eval(f2)
            | IMPLY(f1, f2) => not(eval(f1)) orelse eval(f2)
            | LESS(f1, f2) => eval_expr(f1) < eval_expr(f2)
    end

(* 2. Check MetroMap *)
type name = string
datatype metro = STATION of name
               | AREA of name * metro
               | CONNECT of metro * metro

fun checkMetro(m: metro) =
    let
        fun checkStation(areas: name list, m: metro) =
            let
                fun checkName(areas: name list, station: name) =
                    if null areas
                    then false
                    else if hd(areas) = station
                    then true
                    else checkName(tl(areas), station)
            in
                case m of
                      STATION(n) => checkName(areas, n)
                    | AREA(n, m) => checkStation(n::areas, m)
                    | CONNECT(m1, m2) => checkStation(areas, m1) andalso checkStation(areas, m2)
            end
    in
        case m of
          STATION(n) => false
        | AREA(n, m) => checkStation(n::[], m)
        | CONNECT(m1, m2) => checkMetro(m1) andalso checkMetro(m2)

    end
    
(* 3. Lazy List *)
datatype 'a lazyList = nullList
                     | cons of 'a * (unit -> 'a lazyList)

fun seq(first, last) =
    let
        fun res() = seq(first + 1, last)
    in
        if first > last
        then nullList
        else cons(first, res)
    end

fun infSeq(first) =
    let
        fun res() = infSeq(first + 1)
    in
        cons(first, res)
    end

fun firstN(lazyListVal, n) =
    case (lazyListVal, n) of
          (nullList, _) => []
        | (_, 0) => []
        | (cons(first, f), n) => first::firstN(f(), n - 1)

fun Nth(lazyListVal, n) =
    case (lazyListVal, n) of
          (nullList, _) => NONE
        | (_, 0) => NONE
        | (cons(first, f), 1) => SOME first
        | (cons(first, f), n) => Nth(f(), n - 1)
        
fun filterMultiples(lazyListVal, n) =
    case (lazyListVal, n) of
          (nullList, _) => nullList
        | (cons(first, f), n) => let
                                     fun res() = filterMultiples(f(), n)
                                 in
                                     if first mod n = 0
                                     then filterMultiples(f(), n)
                                     else cons(first, res)
                                 end
                                
    
(* Test Codes *)
val evalTest1 = eval(TRUE) = true
val evalTest2 = eval(FALSE) = false
val evalTest3 = eval(NOT(TRUE)) = false
val evalTest4 = eval(NOT(FALSE)) = true
val evalTest5 = eval(ANDALSO(TRUE, TRUE)) = true
val evalTest6 = eval(ANDALSO(TRUE, FALSE)) = false
val evalTest7 = eval(ANDALSO(FALSE, TRUE)) = false
val evalTest8 = eval(ANDALSO(FALSE, FALSE)) = false
val evalTest9 = eval(ORELSE(TRUE, TRUE)) = true
val evalTest10 = eval(ORELSE(TRUE, FALSE)) = true
val evalTest11 = eval(ORELSE(FALSE, TRUE)) = true
val evalTest12 = eval(ORELSE(FALSE, FALSE)) = false
val evalTest13 = eval(IMPLY(TRUE, TRUE)) = true
val evalTest14 = eval(IMPLY(TRUE, FALSE)) = false
val evalTest15 = eval(IMPLY(FALSE, TRUE)) = true
val evalTest16 = eval(IMPLY(FALSE, FALSE)) = true
val evalTest17 = eval(LESS(NUM(1), NUM(2))) = true
val evalTest18 = eval(LESS(PLUS(NUM(1), NUM(2)), NUM(3))) = false
val evalTest19 = eval(LESS(MINUS(NUM(1), NUM(2)), NUM(3))) = true
val evalTest20 = eval(ANDALSO(LESS(MINUS(NUM(1), NUM(2)), NUM(3)), LESS(NUM(5), PLUS(NUM(42), NUM(15))))) = true

val checkMetroTest1 = checkMetro(AREA("a", STATION "a")) = true
val checkMetroTest2 = checkMetro(AREA("a", AREA("a", STATION "a"))) = true
val checkMetroTest3 = checkMetro(AREA("a", AREA("b", CONNECT(STATION "a", STATION "b")))) = true
val checkMetroTest4 = checkMetro(AREA("a", CONNECT(STATION "a", AREA("b", STATION "a")))) = true
val checkMetroTest5 = checkMetro(AREA("a", STATION "b")) = false
val checkMetroTest6 = checkMetro(AREA("a", AREA("a", STATION "b"))) = false
val checkMetroTest7 = checkMetro(AREA("a", AREA("b", CONNECT(STATION "a", STATION "c")))) = false
val checkMetroTest8 = checkMetro(AREA("a", CONNECT(STATION "b", AREA("b", STATION "a")))) = false
val checkMetroTest9 = checkMetro(AREA("a", CONNECT(STATION "a", AREA("b", STATION "c")))) = false
val checkMetroTest10 = checkMetro(STATION("a")) = false
val checkMetroTest11 = checkMetro(CONNECT(STATION("a"), AREA("b", STATION "b"))) = false
val checkMetroTest12 = checkMetro(CONNECT(AREA("a", STATION("a")), AREA("b", STATION "b"))) = true
val checkMetroTest13 = checkMetro(CONNECT(AREA("a", STATION("a")), AREA("b", STATION "a"))) = false

val seqAndfirstNTest1 = firstN(seq(1, 5), 3) = [1, 2, 3]
val seqAndfirstNTest2 = firstN(seq(~5, 5), 3) = [~5, ~4, ~3]
val seqAndfirstNTest3 = firstN(seq(1, 5), 10) = [1, 2, 3, 4, 5]
val seqAndfirstNTest4 = firstN(seq(5, 5), 3) = [5]
val seqAndfirstNTest5 = firstN(seq(5, 1), 3) = []
val seqAndfirstNTest6 = firstN(seq(1, 5), 0) = []

val seqAndNthTest1 = Nth(seq(1, 5), 3) = SOME 3
val seqAndNthTest2 = Nth(seq(~5, 5), 3) = SOME ~3
val seqAndNthTest3 = Nth(seq(5, 5), 3) = NONE
val seqAndNthTest4 = Nth(seq(5, 1), 3) = NONE
val seqAndNthTest5 = Nth(seq(1, 5), 0) = NONE

val infSeqAndfisrtNTest1 = firstN(infSeq(1), 3) = [1, 2, 3]
val infSeqAndfisrtNTest2 = firstN(infSeq(1), 10) = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
val infSeqAndfisrtNTest3 = firstN(infSeq(1), 0) = []
val infSeqAndfisrtNTest4 = firstN(infSeq(~10), 3) = [~10, ~9, ~8]

val infSeqAndNthTest1 = Nth(infSeq(1), 3) = SOME 3
val infSeqAndNthTest2 = Nth(infSeq(1), 0) = NONE
val infSeqAndNthTest3 = Nth(infSeq(1), 1073741823) = SOME 1073741823
val infSeqAndNthTest4 = Nth(infSeq(~100), 3) = SOME ~98

val filterMultiplesTest1 = firstN(filterMultiples(seq(1, 5), 3), 10) = [1, 2, 4, 5]
val filterMultiplesTest2 = firstN(filterMultiples(seq(1, 20), 2), 5) = [1, 3, 5, 7, 9]
val filterMultiplesTest3 = firstN(filterMultiples(seq(~5, 5), 3), 5) = [~5, ~4, ~2, ~1, 1]
val filterMultiplesTest4 = firstN(filterMultiples(seq(~5, 5), 1), 5) = []
val filterMultiplesTest5 = firstN(filterMultiples(seq(5, 1), 5), 5) = []
