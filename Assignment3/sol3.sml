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
fun check_pat f =

(* 2. match *)
fun match f =

(* 3. Tournament of Rock, Paper, Scissors *)
type name = string
datatype RSP = ROCK
             | SCISSORS
             | PAPER

datatype 'a strategy = Cons of 'a * (unit -> 'a strategy)
datatype tournament = PLAYER of name * (RSP strategy ref)
                    | MATCH of tournament * tournament