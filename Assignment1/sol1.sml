(* 2019054957 이용우 *)

fun merge(list1 : int list, list2 : int list) =
    if null list1
    then list2

    else if null list2
    then list1

    else
        if hd(list1) <= hd(list2)
        then hd(list1) :: merge(tl(list1), list2)
        else hd(list2) :: merge(list1, tl(list2))

fun reverse(list : int list) =
    let 
        fun pop_and_push(from : int list, to : int list) =
            if null from
            then to
            else pop_and_push(tl(from), hd(from) :: to)
    in  
        pop_and_push(list, [])
    end

fun pi(a : int, b : int, f : int -> int) =
    if a > b
    then 1
    else f(a) * pi(a + 1, b, f)

fun digits(x : int) = 
    let 
        fun make_list(x : int, list : int list) =
            let
                val digitsList = (x mod 10) :: list
                val division = x div 10
            in 
                if division <= 0
                then digitsList
                else make_list(division, digitsList)
            end
    in
        if x <= 0
        then []
        else make_list(x, [])
    end

fun additivePersistence(x : int) =
    let
        fun sum_digits(x : int) = 
            let 
                fun sum(list: int list) = 
                    if null list
                    then 0
                    else hd(list) + sum(tl(list))

                val digitsList = digits(x)
            in
                sum(digitsList)
            end
        
        val result = sum_digits(x)
    in
        if x < 10   (* check that x is under 10 when initially function called *)
        then 0      (* (ex) additionalPersistence(1) = 0 *)

        else if result < 10
        then 1

        else 1 + additivePersistence(result)
    end

fun digitalRoot(x : int) =
    let
        fun sum_digits(x : int) = 
            let 
                fun sum(list: int list) = 
                    if null list
                    then 0
                    else hd(list) + sum(tl(list))

                val digitsList = digits(x)
            in
                sum(digitsList)
            end
        
        val result = sum_digits(x)
    in
        if result < 10
        then result
        else digitalRoot(result)
    end

(* (* Test Codes *)
val mergeTest1 = merge([1,4,5], [2,6,7]) = [1,2,4,5,6,7];
val mergeTest2 = merge([], []) = [];
val mergeTest3 = merge([], [1, 2]) = [1, 2];
val mergeTest4 = merge([5, 10], []) = [5, 10];
val mergeTest5 = merge([1, 3, 5, 7, 9, 11], [0, 2, 4, 6, 8, 10]) = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11];
val mergeTest6 = merge([~5, ~3, ~1, 7], [~2, 3]) = [~5, ~3, ~2, ~1, 3, 7];

val reverseTest1 = reverse([1, 5, 4]) = [4, 5, 1];
val reverseTest2 = reverse([]) = [];
val reverseTest3 = reverse([1]) = [1];
val reverseTest4 = reverse([1, ~1, 3]) = [3, ~1, 1];
val reverseTest5 = reverse([0, ~3, 5, 4]) = [4, 5, ~3, 0];
val reverseTest6 = reverse([1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0]) = [0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1];

(* Test function of Pi *)
fun test(x : int) = 
    2 * x + 4

val piTest1 = pi(1, 3, test) = 6 * 8 * 10;
val piTest2 = pi(0, 0, test) = 4; (* return just f(0) *)
val piTest3 = pi(0, 1, test) = 4 * 6
val piTest4 = pi(3, ~1, test) = 1 (* if a > b then function pi must return 1 *)
val piTest5 = pi(~5, 1, test) = 0

val digitTest1 = digits(253) = [2,5,3]
val digitTest2 = digits(12) = [1,2]
val digitTest3 = digits(0) = [] (* 0 is not positive number. so it returns empty list *)
val digitTest4 = digits(6) = [6]
val digitTest5 = digits(1010101) = [1,0,1,0,1,0,1]
val digitTest6 = digits(1073741823) = [1,0,7,3,7,4,1,8,2,3]

val additivePersistenceTest1 = additivePersistence(9876) = 2
val additivePersistenceTest2 = additivePersistence(0) = 0
val additivePersistenceTest3 = additivePersistence(6) = 0
val additivePersistenceTest4 = additivePersistence(42) = 1
val additivePersistenceTest5 = additivePersistence(12349) = 3

val digitalRootTest1 = digitalRoot(9876) = 3
val digitalRootTest2 = digitalRoot(0) = 0
val digitalRootTest3 = digitalRoot(6) = 6
val digitalRootTest4 = digitalRoot(42) = 6
val digitalRootTest5 = digitalRoot(12349) = 1 *)
