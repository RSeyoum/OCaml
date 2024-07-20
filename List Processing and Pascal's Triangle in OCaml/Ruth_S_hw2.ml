(*
  Problem 1:

    [odds lst] takes a sublist of [lst] containing only the odd numbers from the argument in the same order. 
    If the given [odds lst] does not have any odd numbers, return []. Otherwise, iterate through the 
    recursive function.

    odds : int list -> int list = <fun>
*)

let rec odds lst =
  match lst with
  | [] -> []
  | h :: t -> if h mod 2 <> 0 then h :: odds t else odds t;;

(* Assert Statements for testing *)
assert (odds [] = []);;
assert (odds [1] = [1]);;
assert (odds [1; 2; 3] = [1; 3]);;
assert (odds [(-2); 2; 0] = []);;
assert (odds [1; 3; 5; 7; 9] = [1; 3; 5; 7; 9]);;
assert (odds [2; 3; (-2)] = [3]);;
assert (odds [2; 3; (-1)] = [3; (-1)]);;
assert (odds [(-7); (-5);(-6); (-2)] = [(-7); (-5)]);;


(*
  Problem 2 :

    [first _index num lst] takes an integer [num] and a int list [lst], and returns the nth element of 
    [lst] is the first occurrence of [num] in [lst].  

    help_find_index : 'a -> 'a list -> int -> int = <fun>
    first_index : 'a -> 'a list -> int = <fun>   
*)

(*
    This helper function uses pattern matching to search for the first occurrence of [num] in [lst].
    If [lst] is not empty, h is compared with [num]. If they are equal, the index is returned. Otherwise,
    the helper function is called recursively passing the [num], t and index arguments.
*)
let rec help_find_index num lst list_index =
  match lst with
  (* If num does not appear in lst, return -1. *)
  | [] -> -1
  | h :: t ->  if h = num then list_index else help_find_index  num t (list_index + 1);;

let first_index num lst =
   (* Consider the index of the first element in [lst] to be zero.*)
   help_find_index num lst 0;;

(* Assert Statements for testing *)
assert (first_index 0 [] = -1);;
assert (first_index 1 [20; 15; 20] = -1);;
assert (first_index 2 [2] = 0);;
assert (first_index 20 [20; 15; 20] = 0);;
assert (first_index 15 [20; 15; 20] = 1);;
assert (first_index 20 [1; 15; 20; 25; 30] = 2);;
assert (first_index 35 [30; 35; 40] = 1);;


(*
  Problem 3 :

    [partial_sum lst] takes a list of numbers [lst] and returns a list of the partial sums
    of these numbers.

    sum_of_two_values : int -> int -> int = <fun>
    helper_sum : int -> int list -> int list = <fun>
    partial_sum : int list -> int list = <fun>
*)

(*
    This helper function adds [value1] and [value2]
*)

let sum_of_two_values value1 value2 = 
     value1 + value2;;

(*
    This helper function uses pattern matching. If [lst] is not empty, then the helper function is called 
    recursively passing our helper [sum_of_two_values value1 value2] with the [index] and [h].
*)
let rec helper_sum index lst =
    match lst with
    | [] -> []
    | h :: t -> (sum_of_two_values index h) :: helper_sum (sum_of_two_values index h) t ;;

(* Use helper function with given input value, the first [index] is always 0*)
let partial_sum lst =
  helper_sum 0 lst;;

(* Assert Statements for testing *)
assert( partial_sum [] = []);;
assert( partial_sum [1] = [1]);;
assert( partial_sum [1; 1] = [1; 2]);;
assert( partial_sum [1; 1; 1] = [1; 2; 3]);;
assert( partial_sum [1; 2; 3] = [1; 3; 6]);;
assert( partial_sum [1; (-7)] = [1; (-6)]);;
assert( partial_sum [2; (-7); 100] = [2; (-5); 95]);;


(*
  Problem 4 :
    
    [pascal_tr j k] given the current [j] and [k] , returns the sum of two numbers diagonally above it.
    Requires: [j k are integers] [the first and last number are 1] 
   
    triangle_sum : int -> int -> int -> int = <fun>
    pascal_tr : int -> int -> int = <fun>
*)

(* 
    The element at an intermediate position [row] [col] is the sum of the element [row - 1] [col], 
    the upper neighbor, and the element ([row - 1],  [col - 1]), the upper left  neighbor. [acc] keeps track of the 
    of each result and it gets updated on each iteration.
*)
let rec pascal_helper row col acc = 
    if row = 0 then 1
    else if row = col || col = 0 then acc + 1
    else pascal_helper (row - 1) col (pascal_helper (row - 1) (col - 1) acc)

(* Use helper function with given input value*)
let pascal_tr j k  =   
  pascal_helper j k 0;;


(* 12  Assert Statements for testing *)
assert (pascal_tr 0 0 = 1);;
assert (pascal_tr 1 0 = 1);;
assert (pascal_tr 1 1 = 1);;
assert (pascal_tr 2 0 = 1);;
assert (pascal_tr 2 1 = 2);;
assert (pascal_tr 2 2 = 1);;
assert (pascal_tr 3 0 = 1);;
assert (pascal_tr 3 1 = 3);;
assert (pascal_tr 3 2 = 3);;
assert (pascal_tr 3 3 = 1);;
assert (pascal_tr 5 0 = 1);;
assert (pascal_tr 5 2 = 10);;


