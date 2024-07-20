open Hw3
open Json

(* This file provides a list of basic tests for your homework.
 * You will surely want to add more! These tests do not guarantee that your code
 * is correct or will pass our tests.
 * Notice that currently calling any of the functions on hw3.ml will fail,
 * as such, all test functions are commented by default. As you
 * work on implementing the homework functions:
 *   1. Implement the changes on hw3.ml
 *   2. Run `dune build` to make sure everything compiles
 *   3. Uncomment the corresponding test
 *   4. Add more test scenarios
 *   5. Run `dune test` to build and run your tests.
 * If working correctly, `dune test` will complete with no error messages
 *)

(* We leave the first test uncommented to get you started. Until make_silly_json
 * gets implemented, calling `dune test` or `#use "hw3test.ml"` from dune utop
 * will print a Failure message: *)
 
type json =
| Num of float
| String of string
| False
| True
| Null
| Array of json list
| Object of (string * json) list;;

let test1_1 =
  make_silly_json 1
  =
  Array
    [Object [("n", Num 1.); ("b", True)]];;

let test1_2 =
      make_silly_json 2
      =
      Array
        [Object [("n", Num 2.); ("b", True)];
        Object [("n", Num 1.); ("b", True)]];;

let test1_3 =
      make_silly_json 3
      =
      Array
        [Object [("n", Num 3.); ("b", True)];
        Object [("n", Num 2.); ("b", True)];
        Object [("n", Num 1.); ("b", True)]];;

assert(test1_1);;
assert(test1_2);;
assert(test1_3);;
  


let test2 = concat_with (";", ["1"; "2"]) = "1;2";;
assert(test2);;


let test3 = quote_string "hello" = "\"hello\"";;
assert(test3);; 


let test4 = string_of_json (Object [("foo", Num 3.14159); ("bar", (Array [Num 1.0; String "world"; Null])); ("ok", True)]) = "{\"foo\":3.14159,\"bar\":[1,\"world\",null],\"ok\":true}";;
assert(test4)
 
let test5_1 = take (3, []) = [];;
let test5_2 = take  (1, [1; 2; 3]) = [1];;
let test5_3 = take (2, [4; 5; 6; 7]) = [4; 5] ;;
assert(test5_1);;
assert(test5_2);;
assert(test5_3);;

let test6 = firsts [(1,2); (3,4)] = [1; 3] ;;
assert(test6);;

(** Added comment on hw3.ml **)


let test8 = assoc ("foo", [("bar",17);("foo",19)]) = Some 19;;
let test8_1 = assoc ("ruth", [("ruth",1);("seyoum",2)]) = Some 1;;
let test8_2 = assoc ("hi", [("welcome",1);("world",2)]) = None;;
let test8_3 = assoc ("", []) = None;;
let test8_4 = assoc ("h", [("hi", 2022)]) = None;;
assert(test8);;
assert(test8_1);;
assert(test8_2);;
assert(test8_3);;
assert(test8_4);;



let test9 = dot (Object [("foo", Num 3.14159); ("bar", Array [Num 1.0; String "world"; Null]); ("ok", True)], "ok") = Some True;;
let test9_1 = dot (Object [("foo", Num 3.14159); ("bar", Array [Num 1.0; String "world"; Null]); ("ok", True)], "foo") = Some (Num 3.14159);;
let test9_2 = dot (Object [("foo", Num 3.14159); ("bar", Array [Num 1.0; String "world"; Null]); ("ok", True)], "bar") = Some (Array [Num 1.0; String "world"; Null]);;
let test9_3 = dot (Object [("foo", Num 3.14159); ("bar", Array [Num 1.0; String "world"; Null]); ("ok", True)], "baz") = None;;
let test9_4 = dot ((Num 42.0), "foo") = None;;
let test9_5 = dot (Null, "foo") = None;;
assert(test9);;
assert(test9_1);;
assert(test9_2);;
assert(test9_3);;
assert(test9_4);;
assert(test9_5);;



let test10 = dots (Object [("f", Object [("g", String "gotcha")])], ["f"; "g"]) = Some (String "gotcha");; 
assert(test10);;
