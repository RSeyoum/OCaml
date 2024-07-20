open Hw4
open Json


(* This file provides one test case for each problem.
 * You will surely want to add more! Also, remember to add asserts.
 * These tests do not guarantee that your code is correct 
 * or will pass our tests.
 *
 * Similar to the last assignment, as you
 * work on implementing the homework functions:
 *   1. Implement the changes on hw4.ml
 *   2. Run `dune build` to make sure everything compiles
 *   3. Uncomment the corresponding test
 *   4. Add more test cases
 *   5. Run `dune test` to build and run your tests.
 * If working correctly, `dune test` will complete with no error messages
 *)

(* Any order of the result list is allowed by the specification, 
   so it's ok to fail this test because of a different order of your result. 
   You can edit this test to match your implementation's order. *)

let json_obj   = Object [("foo", json_pi); ("bar", json_array); ("ok", True)];;
let json_obj1   = Object [];;
let json_obj2   = Object [("ruth", json_pi)];;
let json_obj3   = Object [("foo", json_pi); ("bar", json_array); ("ok", True); ("hi", True); ("welcome", True)];;
let test1_01 = one_fields json_obj = List.rev ["foo";"bar";"ok"] ;;
let test1_02 = one_fields json_obj1 = List.rev [];;
let test1_03 = one_fields json_obj2 = List.rev ["ruth"];;  
let test1_04 = one_fields json_obj3 = List.rev ["foo";"bar";"ok";"hi";"welcome"];; 

assert(test1_01);;
assert(test1_02);;
assert(test1_03);;
assert(test1_04);;



let test2_01 = not (no_repeats ["foo";"bar";"foo"]);; 
let test2_02 = not (no_repeats ["foo";"foo";"foo"]);;
let test2_03 = (no_repeats []);; 
let test2_04 = (no_repeats ["ruth";"hi";"hii"]);; 

assert(test2_01);;
assert(test2_02);;
assert(test2_03);;
assert(test2_04);;


(* recursive_no_field_repeats: false *)       
let nest0 = Array [Object [];
                  Object[("a",True);
                         ("b",Object[("foo",True);
                                     ("foo",False)]);
                         ("c",True)];
                  Object []]

(* recursive_no_field_repeats: true *)        
let nest1 = Object[("try", 
  Array [Object [];
        Object[  ("a",True);        
                 ("b",Object[("foo",True);("hello",True)]);
                 ("foo",True)];
        Object []]
    )]

(* recursive_no_field_repeats: true *)        
let nest2 = Array [
  True;
  True;
  Object [];
  Object[("a",True);
        ("b",Object[("foo",True);("hello",True)]);
        ("foo",True)];
  Object []
  ]


let test3_00 = not (recursive_no_field_repeats nest0);;
let test3_01 =  (recursive_no_field_repeats nest1);;
let test3_02 =  (recursive_no_field_repeats nest2);;

assert(test3_01);;
assert(test3_01);;
assert(test3_02);;


(* Any order of the result list is allowed by the specification, 
so it's ok to fail this test because of a different order of your result. 
You can edit this test to match your implementation's order. *)
let test4_01 = count_occurrences (["a"; "a"; "b"], (Failure "")) = [("b",1);("a",2)];; 
let test4_02 = count_occurrences ([], (Failure "")) = [];;
let test4_03 = count_occurrences (["a"], (Failure "")) = [("a",1)];;
let test4_04 = count_occurrences (["a"; "a"; "a"; "b"; "c"], (Failure "")) = [("c",1);("b",1);("a",3)];;

(* test to see that an exception is thrown when the input list is not sorted *)
let test4_05 = try count_occurrences (["b"; "a"; "b"], (Failure "")) = []
            with Failure _ -> true ;;

assert(test4_01);;
assert(test4_02);;
assert(test4_03);;
assert(test4_04);;
assert(test4_05);;
   

let test5_01 = only_lowercase ["aBc"; "Abc"; "f"; "D"] =["aBc"; "f"];; 
let test5_02 = only_lowercase ["a"; "A";"D"] =["a"];;
let test5_03 = only_lowercase [] =[];; 
let test5_04 = only_lowercase ["Abc"] = [];;

assert(test5_01);;
assert(test5_02);;
assert(test5_03);;
assert(test5_04);;


let test6_01 =  longest_string ["a"; "good"; "will"; "Gesture"; "."] = "Gesture";; 
let test6_02 =  longest_string [] = "" ;;
let test6_03 =  longest_string ["a"] = "a";; 
let test6_04 =  longest_string ["a"; "good"; "will"] = "good";;

assert(test6_01);;
assert(test6_02);;
assert(test6_03);;
assert(test6_04);;

let test7_01 =  longest_lowercase ["a"; "good"; "will"; "Gesture"; "."] = "good"
let test7_02 =  longest_lowercase ["Gesture"] = "";; 
let test7_03 =  longest_lowercase [] = "" ;;
let test7_04 =  longest_lowercase ["a"] = "a";; 
let test7_05 =  longest_lowercase ["a"; "good"; "will"] = "good";;

assert(test7_01);;
assert(test7_02);;
assert(test7_03);;
assert(test7_04);;
assert(test7_05);;

let test8_01 =  caps_no_X_string "aBxXXxDdx" = "ABDD";; 
let test8_02 =  caps_no_X_string "xXXx" = "";;
let test8_03 =  caps_no_X_string "a" = "A";;
let test8_04 =  caps_no_X_string "BxXd" = "BD";;

assert(test8_01);;
assert(test8_02);;
assert(test8_03);;
assert(test8_04);;
