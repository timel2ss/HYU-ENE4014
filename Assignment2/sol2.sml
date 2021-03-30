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