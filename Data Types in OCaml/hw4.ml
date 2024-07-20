(* This is from file json.ml in this directory. json.ml
 * contains the main datatype definition we will use throughout the
 * assignment. You will want to look over this file before starting. *)
include Json

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

(* provided helper operator that does function composition.*)  
let ( @@ ) f g x = f (g x)


(* 
   PROBLEM 1 
    [one_fields j] takes a json argument and determines if it is an object, if
    so then we call our helper function and return a string list with only the
    keys.
            val one_fields : json -> string list = <fun> 

    returns: if [j] is an object, we call our helper function [helper key_value_pair accumulator].
    Otherwise, if [j] is not an object then an empty list is returned.

    Eg. [one_fields Object [("foo", json_pi); ("bar", json_array); ("ok", True)] = List.rev ["foo";"bar";"ok"]]

    Helper function:

    [helper key_value_pair accumulator] takes two arguments [key_value_pair] 
    and [accumulator]. The [key_value_pair] is a list of key-value pairs, 
    while [accumulator] is an accumulator list. 

            val helper : ('a * 'b) list -> 'a list -> 'a list = <fun>

    returns: if [key_value_pair] is empty, then return the [accumulator] list.
    Otherwise, only extract the first key from the [key_value_pair] and recursively
    call this helper function with the tail [t] and an updated [accumulator] with
    the key value cons to it. 
   
   
*)
let one_fields json =
  let rec helper key_value_pair accumulator =
    match key_value_pair with
    | [] -> accumulator
    | (key, _) :: t -> helper t (key :: accumulator)
  in
  match json with
  | Object json_obj -> helper json_obj []
  | _ -> [];;


(* 
   PROBLEM 2 
    [no_repeats lst] takes a string list. This problem uses the helper 
    function dedup (returns its argument without duplicates) together
    with standard library function List.length.

          val no_repeats : 'a list -> bool = <fun>


    returns: a bool value, true if and only if no string appears more 
    than once in the input.

    Helper function was already provided:

          debup : val dedup : 'a list -> 'a list = <fun>
    
    Eg. [no_repeats ["foo";"bar";"foo"] = true] 
   
*)

let no_repeats lst =
  let size = List.length lst in
    size = List.length (dedup lst);;

(* 
  PROBLEM 3

   [recursive_no_field_repeats j] takes a json and determines if the fields
   are repeated. It is not relevant that different objects may have field 
   names in common. This function uses these mutually recursive helper functions:
   find_key_repeats_array, find_key_repeats_object and find_fields_repeats_obj_helper
   to check Array and Object.

          val recursive_no_field_repeats : json -> bool = <fun>

    returns: a bool value, true if and only if no object anywhere "inside"
    (arbitrarily nested) the json argument has repeated field names. If [j] 
    is an array we call [find_key_repeats_array lst]. Otherwise, if [j]
    is an object call [find_key_repeats_object lst]. 

    Eg. [not (recursive_no_field_repeats Array [Object [];
            Object[("a",True);
                    ("b",Object[("foo",True);
                                ("foo",True)]);
                    ("c",True)];
            Object []]) ]

    Helper functions:

    [find_key_repeats_array lst] we use this function when we deal with json
    arrays. It is a mutually recursive function that returns true if and only 
    if the list is empty. Otherwise, it recursively calls [recursive_no_field_repeats]
    with [h] as the json argument, and then calls itself with the rest of the list [t]. 
    
              val find_key_repeats_array : json list -> bool = <fun>

    [find_key_repeats_object lst] we use this function when we deal with json
    objects. It is a mutually recursive function that returns true if and only
    if the list is empty. Otherwise, it extracts a key-value pair [(key, value)]
    from the object and calls our third helper function with just the key we 
    just extracted with the rest of the list. We use && here again to recursively 
    call itself and [recursive_no_field_repeats j] on just the value. This function
    acts as what is known in many programming languages a "for-loop".

              val find_key_repeats_object : (string * json) list -> bool = <fun>

    [find_fields_repeats_obj_helper extracted_key lst] we use this function only after
    [find_key_repeats_object lst] is called with a [lst] that is not empty. Returns true
    if and only if the [lst] is empty. Otherwise, we compare if the [key] and the [extracted_key]
    match. If they do then, we do have a repeated key / field therefore, we return false. Otherwise,
    recursively call itself with the rest of object [t]. 

            val find_key_repeats_object_helper : string -> (string * json) list -> bool = <fun>
*)

let rec recursive_no_field_repeats j = 

  match j with
  | Num json_number -> true
  | String json_string -> true
  | False -> true
  | True -> true
  | Null -> true
  | Array json_value -> (find_key_repeats_array json_value)
  | Object json_object -> (find_key_repeats_object json_object)
  
  and find_key_repeats_array lst =
   match lst with
    | [] -> true
    | h :: t -> (recursive_no_field_repeats h) && (recursive_no_field_repeats (Array t))

  and find_key_repeats_object_helper extracted_key lst =
    match lst with
    | [] -> true
    | (key, _) :: t ->
      match key = extracted_key with
      | false -> (find_key_repeats_object_helper extracted_key t)
      | true -> false

  and find_key_repeats_object lst =
    match lst with
    | [] -> true
    | (key, value) :: t -> (find_key_repeats_object_helper key t) && (find_key_repeats_object t) && (recursive_no_field_repeats value);;

(* 
  PROBLEM 4 

   [count_occurrences (xs, e)] takes a string list and an exception. This function
   counts the occurences of each string in a list of strings, and returns a tuple
   representing each string with its count. 

          val count_occurrences : 'a list * exn -> ('a * int) list = <fun>

   returns: if [xs] is empty then we simply return an empty list. Otherwise, 
   call our helper function [count_helper accumulator current_string current_count].
   Using the helper function, we traverse the string list with the head [h] being 
   the [current_string], while [current_count] is 1 and [t] is the tail. 
   
   Eg. [count_occurrences (["a"; "a"; "b"], (Failure "")) = [("b",1);("a",2)]]


   Helper function:

   [count_helper accumulator current_string current_count lst] takes four arguments:
   [current_string] is the current string that is being counted, [current_count]
   counts the occurrences of [current_string], [lst] and [accumulator]. If the list [lst]
   is empty, then we just add the [current_string] and [current_count] to the
   [accumulator] and return it. Otherwise, check whether [current_string] is in
   lexicographical order. If, [h] and the [current_string] are equal then increment
   the [current_count] by one and do a recursive call with [t]. If [h] is less than
   [current_string], then we know that our list is not sorted so we raise an exception
   [e]. However, if [h] > [current_string] then add [current_string] and [current_count]
   to the [accumulator] and do a recursive call with [h] being the [current_string] with
   [current_count] being 1. 

   returns: if the string list is stored, a list where each string 
   is paired with the number of times it occurs. If not, then an [e] 
   argument is raised.

*)


let count_occurrences (xs, e) =
  
  let rec count_helper lst accumulator current_string current_count = 
    match lst with
    | [] -> (current_string, current_count) :: accumulator
    | h :: t when h = current_string -> count_helper t accumulator current_string (current_count + 1) 
    | h :: t when h < current_string -> raise e
    | h :: t  -> count_helper t ((current_string, current_count) :: accumulator) h 1 
  
  in
  match xs with
  | [] -> []
  | h :: t -> count_helper t [] h 1 ;;



(* PROBLEM 5 

   [only_lowercase] takes a string list and assumes all strings have atleast
   1 character. In this function we use List.filter, Char.lowercase_ascii,
   and string index access (str.[pos]). 

            val only_lowercase : string list -> string list = <fun>

   returns: a string list that has only the strings in the argument that 
   start with a lowercase letter. 

   Eg. [only_lowercase ["aBc"; "Abc"; "f"; "D"] = ["aBc"; "f"]]
   
*)

let only_lowercase =
  List.filter (fun string_list -> Char.lowercase_ascii string_list.[0] = string_list.[0]);;

(* PROBLEM 6 
   
   [longest_string] takes a string list. Uses List.fold_left and String.length
   to determine which string is the longest within our string list.

            val longest_string : string list -> string = <fun>

    returns: the longest string in the list by comparing string1 against
    string2. However, if the list is empty it will return "".

    Eg. [longest_string ["a"; "good"; "will"; "Gesture"; "."] = "Gesture"]

    Helper function:

    [compare_length string1 string2] is the helper function that takes two 
    arguments [string1] and [string2]. In this function all that we doing is 
    compare [string1] with [string2]. If the length of [string1] > [string2] 
    than it is true and we return [string1] as we want to return the string 
    that is closest to the beginning of the list. If [string1] and [string2] 
    are equal we return the [string1]. Otherwise, if [string1] < [string2] 
    then [string2]. 

          val compare_length : string -> string -> string = <fun>

*) 

let longest_string = (fun string_list ->
  match string_list with
  | [] -> ""
  | [h] -> h
  | h :: t ->
    let compare_length string1 string2 =
      match String.length string1 > String.length string2 with
      | true -> string1
      | false -> match String.length string1 = String.length string2 with
          | true -> string1  
          | false -> string2
  in (List.fold_left compare_length h t));;


(* 
  PROBLEM 7

   [longest_lowercase] takes a string list. We can assume all strings have 
   atleast 1 character. In this function we use our previous two functions:
   [only_lowercase] and [longest_string]. By using pipline we first only 
   return the strings in [lst] that start with a lowercase letter then 
   determine which string is the longest.

          val longest_lowercase : string list -> string = <fun>

   returns: the longest string in the list that begins with a lowercase letter, 
   or "" if there are no such strings. 

   Eg. [longest_lowercase ["a"; "good"; "will"; "Gesture"; "."] = "good"]
*) 

let longest_lowercase = (fun lst -> lst |> only_lowercase |> longest_string);;

(*
   PROBLEM 8 
   
   [caps_no_X_string s] takes a string and uses pipeline and a few functions 
   from String (String.split, String.concat & String.uppercase_ascii). 
   String.uppercase_ascii is first used to capitalize the entire [s], then
   String.split_on_char 'X' splits [s] into substring wherever there is "X". 
   This however, gives us different substrings of s, that we need to 
   concatenate using String.concat in order to return one string output.  

            val caps_no_X_string : string -> string = <fun>

   returns: a string similiar to the input except every letter is capitalized
   and every 'x' or 'X' is removed.

   Eg. [caps_no_X_string "aBxXXxDdx" = "ABDD"] 
   
*) 

let caps_no_X_string s =
  String.uppercase_ascii s |> String.split_on_char 'X' |> String.concat "";;