open Deque

let q1 = TwoListDeque.empty;;
assert(0=(TwoListDeque.size q1));;
assert(0=(TwoListDeque.size q1));;

let deque_empty = TwoListDeque.empty;;
let deque_1 = TwoListDeque.(add_first 1 deque_empty);;
let deque_2 = TwoListDeque.(add_first 2 deque_1);;

assert(2 = TwoListDeque.peek_first deque_2);;

assert(TwoListDeque.((to_list deque_empty) = []));;

let is_empty_1 = TwoListDeque.(is_empty deque_empty);;
assert(is_empty_1);;

let is_empty_2 = TwoListDeque.(is_empty deque_1);;
assert(not (is_empty_2));;

let add_first_1 = TwoListDeque.(add_first 1 deque_empty |> to_list);;
assert([1] = add_first_1);;


(* Dillon's Tests *)


let ah_hi_bye_check = TwoListDeque.(empty |> add_first "hi" |> add_first "somewhere in between" |> add_first "bye");;
assert("bye" = TwoListDeque.peek_first ah_hi_bye_check);;

let q1 = TwoListDeque.empty;;
assert(0=(TwoListDeque.size q1));;


let test_q = TwoListDeque.add_first 1 q1;;
assert(1 = TwoListDeque.size test_q);;



(***
Added a test case to test the add_first function of TwoListDeque
***)
let q2 = TwoListDeque.add_first 1 q1;;
assert(TwoListDeque.empty = TwoListDeque.remove_first q2);;

(***
Added a test case to test the add_last function of TwoListDeque
***)
let q3 = TwoListDeque.add_last 1 q2;;
assert([1] = TwoListDeque.to_list (TwoListDeque.remove_last q3));;
(***
add_first test case
***)
let q4 = TwoListDeque.add_first 1 q3;;
assert(1 = TwoListDeque.peek_first q4);; 
assert(1 = TwoListDeque.peek_last q4);;
assert([1;1;1] = TwoListDeque.to_list q4);;

(***
add_last test case
***)
let q5 = TwoListDeque.add_last 1 q3;;
assert(1 = TwoListDeque.peek_last q5);;
assert([1;1;1] = TwoListDeque.to_list q5);;

(* **
remove_first test case
***)
let q6 = TwoListDeque.remove_first q5;;
assert([1;1] = TwoListDeque.to_list  q6);;
assert([1] = TwoListDeque.to_list (TwoListDeque.remove_first q6));;

(***
map test case
***)
let q7 = TwoListDeque.map (fun x -> x+1) q6;;
assert(2 = TwoListDeque.peek_first q7);;

let q10 = TwoListDeque.map (fun x -> x+1) q7;;
assert(3 = TwoListDeque.peek_first q10);;



(***
add_first two new elements, check peek_first and peek_last
***)
let q11 = TwoListDeque.empty;;

let q12 = TwoListDeque.add_first 1 q11;;
assert(1 = TwoListDeque.peek_first q12);;

let q13 = TwoListDeque.add_last 100 q12;;
assert(100 = TwoListDeque.peek_last q13);;


(*jonahs tests start*)
(* testing size 2 *)
let jqueue = TwoListDeque.empty;;
let j1 = TwoListDeque.add_first "no" jqueue;;
let j2 = TwoListDeque.add_last "hi" j1;;
assert(2 = (TwoListDeque.size j2));;
assert("hi" = (TwoListDeque.peek_last j2));;
let j3 = TwoListDeque.remove_first j2;;
assert(1 = (TwoListDeque.size j3));;
assert("hi" = (TwoListDeque.peek_last j3));;
assert("hi" = (TwoListDeque.peek_first j3));;
(* jonahs test end*)


let empty = TwoListDeque.empty;;
let test_add_first = TwoListDeque.add_first 3 empty;;
assert((TwoListDeque.peek_first test_add_first)=3);;

let test_add_last = TwoListDeque.add_last 4 test_add_first;;
assert((TwoListDeque.peek_first test_add_last)=3);;
assert((TwoListDeque.peek_last test_add_last)=4);;
assert((TwoListDeque.to_list test_add_last)=[3;4]);;

let test_remove_first = TwoListDeque.remove_first test_add_last;;
assert((TwoListDeque.peek_first test_remove_first) = 4);;

let test_remove_last = TwoListDeque.remove_last test_remove_first;;
assert((TwoListDeque.is_empty test_remove_last)=true);;


assert(TwoListDeque.is_empty q1 = true);;



(* ! lev's tests ! *)
(* ? testing empty/is_empty *)
let test = TwoListDeque.empty

let t1 = assert(TwoListDeque.is_empty test = true)

let test = TwoListDeque.add_first 3 test

let t2 = assert(TwoListDeque.is_empty test = false)

(* ? testing add/peek *)
let t3 = assert(TwoListDeque.peek_first test = 3)

let t4 = assert(TwoListDeque.peek_last test = 3)

let test = TwoListDeque.add_last 4 test

let t5 = assert(TwoListDeque.peek_first test = 3)

let t6 = assert(TwoListDeque.peek_last test = 4)
(* ! lev's test end ! *)


let tldq = TwoListDeque.empty;;
assert(0=(TwoListDeque.size tldq));;
assert(TwoListDeque.is_empty tldq);;

let tldq = TwoListDeque.(empty |> add_first 1 |> add_first 2 |> add_first 3);; (* [3,2,1]*)
assert(3 = TwoListDeque.size tldq);;
assert(3 = TwoListDeque.peek_first tldq);;
assert(1 = TwoListDeque.peek_last tldq);;
let tldq = tldq |> TwoListDeque.add_last 5;; (* [3,2,1,5]*)
assert(5 = TwoListDeque.peek_last tldq);;

let tldq = tldq |> TwoListDeque.remove_first;; (* [2,1,5]*)
assert(2 = TwoListDeque.peek_first tldq);;
assert(5 = TwoListDeque.peek_last tldq);;
assert( [2;1;5] = TwoListDeque.to_list tldq);;

let tldq = tldq |> TwoListDeque.remove_last;; (* [2,1]*)
assert(2 = TwoListDeque.peek_first tldq);;
assert(1 = TwoListDeque.peek_last tldq);;
assert(2 = TwoListDeque.size tldq);;
assert( [2;1] = TwoListDeque.to_list tldq);;

let tldq = tldq |> TwoListDeque.add_first 7 |>  TwoListDeque.add_last 5 |> TwoListDeque.add_last 9 |> TwoListDeque.add_last 101;; (* [7;2;1;5;9;101]*)
assert(7 = TwoListDeque.peek_first tldq);;
assert(101 = TwoListDeque.peek_last tldq);;
assert(6 = TwoListDeque.size tldq);;
assert([7;2;1;5;9;101] = TwoListDeque.to_list tldq);;

let tldq = tldq |> TwoListDeque.map (fun mb -> mb + 2);; (* [9;4;3;7;11;103]*)
assert(9 = TwoListDeque.peek_first tldq);;
assert(103 = TwoListDeque.peek_last tldq);;
assert(6 = TwoListDeque.size tldq);;
assert([9;4;3;7;11;103] = TwoListDeque.to_list tldq);;

let tldq = tldq |> TwoListDeque.filter (fun mb -> mb mod 3 = 0);; (* [9;3]*)
assert(9 = TwoListDeque.peek_first tldq);;
assert(3 = TwoListDeque.peek_last tldq);;
assert(2 = TwoListDeque.size tldq);;
assert([9;3] = TwoListDeque.to_list tldq);;

let tldq = tldq |> TwoListDeque.add_first 1 |>  TwoListDeque.add_last 0 |> TwoListDeque.add_last 0 |> TwoListDeque.add_last 87;; (* [1;9;3;0;0;87]*)
assert(1 = TwoListDeque.peek_first tldq);;
assert(87 = TwoListDeque.peek_last tldq);;
assert(6 = TwoListDeque.size tldq);;
assert( 100 = TwoListDeque.fold_from_first_forward (+) 0 tldq);;
assert( 100 = TwoListDeque.fold_from_last_backward (+) tldq 0);;


let add_last_test1 = TwoListDeque.empty;;
let add_last_test1' = TwoListDeque.add_last 1 q1;;
let () = assert ([1]= TwoListDeque.to_list add_last_test1');;


let last_backward_test1 = TwoListDeque.empty;;
let last_backward_test1' = TwoListDeque.add_first 1 last_backward_test1;;
let last_backward_test12' = TwoListDeque.add_last 2 last_backward_test1';;
assert([1;2] = TwoListDeque.to_list last_backward_test12');;
assert( 3 = TwoListDeque.fold_from_last_backward (+) last_backward_test12' 0);;


let tldq123 = TwoListDeque.empty;;
let tldq123 = tldq123 |> TwoListDeque.add_first "hi" |> TwoListDeque.add_last "ruth" |> TwoListDeque.add_last "seyoum";; 
assert("seyoum" = TwoListDeque.peek_last tldq123);;


