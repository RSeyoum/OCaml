
(* This is from file json.ml in this directory. json.ml
 * contains the main datatype definition we will use throughout the
 * assignment. You will want to look over this file before starting. *)
include Json


(* These come from the parsed_*_bus.ml.
   Each file binds one variable: small_bus_positions (10 reports),
   medium_bus_positions (100 reports), and complete_bus_positions (~1000 reports),
   respectively with the data that you will need to implement
   your homework.
*)
open Json_structures.Parsed_complete_bus
open Json_structures.Parsed_medium_bus
open Json_structures.Parsed_small_bus

(* provided helper function that deduplicates a list *)
let dedup xs = List.sort_uniq compare xs

(* provided helper function that sorts a given list *)
let sort xs = List.sort compare xs

(* provided helper function to convert a float to a string *)
(* OCaml's string_of_float is not quite RFC compliant due to its tendency
   to output whole numbers with trailing decimal points without a zero.
   But, printf does the job how we want. *)
let json_string_of_float f =
  Printf.sprintf "%g" f

(* Variant type for json : *)
type json =
  | Num of float
  | String of string
  | False
  | True
  | Null
  | Array of json list
  | Object of (string * json) list;;

(* 
   PROBLEM 1 

    [helper_function json_object] this helper function takes an int i 
    and returns a [json_object]. Every object in the array has two fields, 
    "n" and "b". Every object's "b" field should hold true. While the first 
    object in the array should have an "n" field holding the JSON number.  
    
              val helper_function : int -> json list = <fun>

    [make_silly_json i] takes an int [i] and returns a json array of json objects
    from our helper function [helper_function json_object].

              val make_silly_json : int -> json = <fun>

    Eg. [make_silly_json 1 = Array [Object [("n", Num 1.); ("b", True)]]]
*)
let rec helper_function json_object =
    match json_object with
    | 0 -> []
    (*
       The next object should have an "n" field holding (json_object - 1) 
       and so on where the last object in the array has an "n" field 
       holding 1.
     *)
    | json_object -> [Object [("n", Num (float_of_int json_object)); ("b", True)]] @ helper_function (json_object - 1);;

let make_silly_json i =
      Array (helper_function i);;

(* 
  PROBLEM 2 
    [concat_with sep ss] takes a separator string [sep] and
    a list of strings [ss].
    
            val concat_with : string * string list -> string = <fun>

    returns: a new string that consists of all the strings in the
    list [ss] concatenated together, separated by the separator [sep].
    
    Eg. [concat_with (";", ["1"; "2"]) = "1;2"]
*)
let rec concat_with (sep, ss) =
  match ss with 
  | [] -> ""
  (*
    If there is only one string in the list [ss], then we simply return
    that value.
  *)
  | h :: [] -> h
  | h :: t -> h ^ sep ^ concat_with (sep, t);;

(* 
  PROBLEM 3
    [quote_string s] takes a string [s].

            val quote_string : string -> string = <fun>

    returns: the same string [s] with just an additional "\"" at 
    the beginning and end.

    requires: [s] has to be a string

    Eg. [quote_string "hello" = "\"hello\""]
*)

let quote_string s =
 "\"" ^ s ^ "\"";;

(* PROBLEM 4 
    [string_of_json j] takes a json [j] and converts it into the proper 
    string encoding in terms of the syntax described below.  

        val string_of_json : json -> string = <fun>

    These two functions use mutual recursion and are used as helper functions:
        [convert_array json_value] takes a [json_value] and concatenates the 
        string representation of [h] with a comma separtor and the string 
        representation of the rest of the list [t]

            val convert_array : json list -> string = <fun>

        [convert_object json_object] takes a [json_object] which is a 
        sequence of field/value pairs and concatenates the string representation
        of the field/value pair by converting the (field, value) to a string 
        using [quote_string field] and [string_of_json value] with a colon in
        between, and then concatenated with a comma and a recursive call with the rest
        of the list [t].

            val convert_object : (string * json) list -> string = <fun>

    Eg. [string_of_json json_obj = "{\"foo\" : 3.14159, \"bar\" : [1, \"world\", null], \"ok\" : true}"]
*)

let rec string_of_json j =
  
  (* A JSON value is one of seven things:

      1. A number (floating point, e.g., 14 or -0.2)
      2. A string (e.g., "hello")
      3. false
      4. true
      5. null
      6. An array of JSON values, written between square brackets and commas (e.g., [1, "world", null])
      7. An “object,” which is a sequence of field/value pairs
          - A field is always a string literal (e.g., "foo")
          - A value is any JSON value
          - Objects are written with curly braces, commas, and colons, e.g.,
  *)
  match j with
  | Num json_number -> json_string_of_float json_number
  | String json_string -> quote_string json_string
  | False -> "false"
  | True -> "true"
  | Null -> "null"
  | Array json_value -> "[" ^ convert_array json_value ^ "]"
  | Object json_object -> "{" ^ convert_object json_object ^ "}"

  
  and convert_array = function
  | [] -> ""
  | [h] -> string_of_json h 
  | h :: t -> string_of_json h  ^ "," ^ convert_array t

  (* 
    If there is only one field / value pair then using [quote_string field] [field] is wrapped 
    in double quotes and [string_of_json value] converts the value to a string representation 
    with a colon seperating the two string representations. Otherwise, concatenate a comma with 
    the rest of the list [t] 
  *)
  and convert_object = function
    | [] -> ""
    | [(field, value)] -> quote_string field ^ ":" ^ string_of_json value
    | (field, value) :: t -> quote_string field ^ ":" ^ string_of_json value ^ "," ^ convert_object t;;

(* 
  PROBLEM 5
    [take n xs] takes an int [n] and a list [xs]. 

          val take : int * 'a list -> 'a list = <fun>

    returns:  [take n xs] is the first [n] elements of [xs], or
    just [xs] if [xs] has fewer than [n] elements. 
    
    requires: [n >= 0]

    Eg. [take (1, [1; 2; 3]) = [1]]

    Solution to problem 5 was copied from our textbook, 
    more specifically from chapter 3.14 exercises.
*)
let rec take (n, xs) =
  if n = 0 then [] else match xs with
    | [] -> []
    | h :: t -> h :: take ((n - 1), t);;


(* 
  PROBLEM 6 
    [firsts xs] takes a list of pairs [xs].

          val firsts : ('a * 'b) list -> 'a list = <fun>

    returns: a list of all the first components of the pairs in 
    the same order as the argument list [xs].
    
    Eg. [firsts [(1,2); (3,4)] = [1; 3]]
*)
let rec firsts xs = 
  match xs with
  | [] -> []
  (* Extracts the first pair from [xs], and recursively iterates through
    the  rest of the list [t] *)
  | (h, _) :: t -> h :: firsts t;;

(* PROBLEM 7 *)

(* 
   The two functions [firsts xs] and [take n xs] always evaulate to the same value. 
   [first xs] extracts the first components of each pairs in [xs] which is equivalent
   to the operation of the second function [take n xs], as both start with extracting 
   the first components in [xs]. Therefore, we can actually rewrite [take n xs] to be 
   [take n [first xs]]*
   
   For example:
    xs = [(12, 21); (33, 100)]
    n = 1

    firsts (xs) = [12];;
    take (n, xs) = [12];;

    I would say that [take (n, xs)] is faster because it only iterates and extracts the
    first [n] elements. Whereas, [firsts  xs] needs to iterate throught the entire list
    [xs] and return the first components of each pair. Therefore, the operation of [take (n, xs)]
    is faster.
*)


(** 
  PROBLEM 8

    [assoc k xs] is [Some v] if association list [xs] binds key [k] to
    value [v]; and is [None] if [xs] does not bind [k]. 

           val assoc : 'a * ('a * 'b) list -> 'b option = <fun>

    returns: [Some v] if (k', v) is a pair in the list closest to the 
    beginning of the list for what k and k' are equal. If there is no
    such pair, assoc returns [None].

    Eg. [assoc ("foo", [("bar",17);("foo",19)]) = Some 19]
      
    Solution to problem 8 was copied from our textbook, 
    more specifically from 3.8. Association Lists
*)

let rec assoc (k, xs) =
  match xs with
  | [] -> None
  | (k', v) :: t -> if k = k' then Some v else assoc (k, t);;
  

(* PROBLEM 9
    [dot (j, f)] take a json [j] and a string [f]. This function use the
    previous function [assoc (k, xs)] to determine if [j] is an object 
    that has a field named [f]. 

            val dot : json * string -> json option = <fun>

    returns: takes the two arguments [j] and [f] and returns a json 
    option based on our helper function [assoc (k, xs)]. If [j] has the
    key [f], then [Some V] is returned where [v] is the contents of that 
    field. If [j] is not an object or does not contain a field [f], then 
    return None. 
    
    Eg. [dot (json_obj, "ok") = Some True]
*)
let dot (j, f) = 
  match j with
  | Object j -> assoc (f, j)
  | _ -> None;;


(* PROBLEM 10 
   [dots (j, fs)] takes a json [j] and a string list [fs] that represents
   an access path, or in other words, a list of field names. This function
   determines if we have a pattern.

          val dots : json * string list -> json option = <fun>

   returns: a json option by recursively accessing the fields in [fs], 
   starting at the beginning of the list. If any of the field accesses
   occur on non-ojects, or to fields that do not exist, return None. Otherwise, 
   return [Some v] where v is the value of the field "pointed to" by the 
   access path.

   Eg. [dots (Object [("f", Object [("g", String "gotcha")])], ["f"; "g"]) = Some (String "gotcha")]

*)
let rec dots (j, fs) =

  (* 
     Recursively access the fields in [fs], if [fs] is empty then return [Some json]
     object [j]. Otherwise, we do a pattern-match using our previous function [dot (j, f)]
     starting at the beginning of the list. Which in our case the key (beginning)
     is [h] (head). 
   *)
  match fs with
  | [] -> Some j
  | h :: t ->
    match dot (j, h) with
      (* If any of the field accesses occur on non-objects, or to fields that do not
      exit, return None*)
      | None -> None 
      (* Otherwise, return [Some (v or value_field)] where [v or value_field] is the 
      value of the fields "pointed to" by the access path*)
      | Some value_field -> dots (value_field, t);;
    

(*
 type json =
  | Num of float
  | String of string
  | False
  | True
  | Null
  | Array of json list
  | Object of (string * json) list;;

let json_string_of_float f =
  Printf.sprintf "%g" f;;

let rec concat_with (sep, ss) =
  match ss with 
  | [] -> ""
  | h :: [] -> h
  | h :: t -> h ^ sep ^ concat_with (sep, t);;

let quote_string s =
  "\"" ^ s ^ "\"";;

let rec string_of_json json =

  let rec make_key_value (key, value) =
    quote_string key ^ " : " ^ string_of_json value
  in
  let rec string_of_json_array arr =

    let rec string_of_array sub_objs acc =
        match sub_objs with
        | [] -> "[" ^ acc ^ "]"
        | hd :: tl ->
            let sep = if acc = "" then "" else ", " in
            string_of_array tl (acc ^ sep ^ string_of_json hd)
      in
      string_of_array arr ""

  in

  let rec string_of_json_object obj =

    let rec string_of_object sub_objs acc =
        match sub_objs with
        | [] -> "{" ^ acc ^ "}"
        | hd :: tl ->
            let sep = if acc = "" then "" else ", " in
            string_of_object tl (acc ^ sep ^ make_key_value hd)
      in
      string_of_object obj ""

  in

match json with
  | Num num -> json_string_of_float num
  | String str -> quote_string str
  | False -> "false"
  | True -> "true"
  | Null -> "null"
  | Array sub_objs -> string_of_json_array sub_objs 
  | Object sub_objs -> string_of_json_object sub_objs ;;

let json_pi    = Num 3.14159;;
let json_hello = String "hello";;
let json_false = False;;
let json_array = Array [Num 1.0; String "world"; Null];;
let json_obj   = Object [("foo", json_pi); ("bar", json_array); ("ok", True)];;

let test4 = string_of_json json_obj = "{\"foo\" : 3.14159, \"bar\" : [1, \"world\", null], \"ok\" : true}";;   

*)

(*
type json =
  | Num of float
  | String of string
  | False
  | True
  | Null
  | Array of json list
  | Object of (string * json) list;;

let json_string_of_float f =
  Printf.sprintf "%g" f;;


let rec concat_with (sep, ss) =
  match ss with 
  | [] -> ""
  | h :: [] -> h
  | h :: t -> h ^ sep ^ concat_with (sep, t);;

let quote_string s =
 "\"" ^ s ^ "\"";;

let rec string_of_json obj =

  let rec make_key_value key_value =
    match key_value with
    | (key, value) -> quote_string key ^ " : " ^ string_of_json value
  in

  let sep acc = 
     if acc = "" then "" else ", " 
  in
  
  let rec string_of_array sub_objs acc =
        match sub_objs with
        | [] -> "[" ^ acc  ^ "]"
        | hd :: tl -> string_of_array tl (acc ^ (sep acc) ^ string_of_json hd)
  in

  let rec string_of_object sub_objs acc =
        match sub_objs with
        | [] -> "{" ^ acc ^ "}"
        | hd :: tl -> string_of_object tl (acc ^ (sep acc) ^ make_key_value hd)
  in

  match obj with
  | Num num -> json_string_of_float num
  | String str -> quote_string str
  | False -> "false"
  | True -> "true"
  | Null -> "null"
  | Array sub_objs -> string_of_array sub_objs ""
  | Object sub_objs -> string_of_object sub_objs "";;


let json_pi    = Num 3.14159;;
let json_hello = String "hello";;
let json_false = False;;
let json_array = Array [Num 1.0; String "world"; Null];;
let json_obj   = Object [("foo", json_pi); ("bar", json_array); ("ok", True)];;

let test4 = string_of_json json_obj = "{\"foo\" : 3.14159, \"bar\" : [1, \"world\", null], \"ok\" : true}";;   
*)