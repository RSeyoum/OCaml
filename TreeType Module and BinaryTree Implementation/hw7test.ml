open Bstmap

module IntMap = MakeBSTMap(struct
  type t = int
  let compare first_element second_element =
    if first_element = second_element then 0
    else if first_element > second_element then 1
    else -1
end)

(** Test add_basic: *)

let tree_test = IntMap.add_basic 0 "Ruth" IntMap.Empty;;
assert(IntMap.in_order_list tree_test = [(0, "Ruth")]);;

let tree_test = IntMap.add_basic 1 "Negussie" tree_test;;
assert(IntMap.in_order_list tree_test = [(0, "Ruth"); (1, "Negussie")]);;

let tree_test = IntMap.add_basic 2 "Hi" tree_test;;
assert(IntMap.in_order_list tree_test = [(0, "Ruth"); (1, "Negussie");(2, "Hi")]);;

let tree_test = IntMap.add_basic 4 "HI" tree_test;;
let tree_test = IntMap.add_basic 3 "WELCOME" tree_test;;

assert(IntMap.in_order_list tree_test = [(0, "Ruth"); (1, "Negussie");(2, "Hi"); (3, "WELCOME");(4, "HI");]);;

assert (IntMap.find 0 tree_test = "Ruth");;
assert (IntMap.find 1 tree_test = "Negussie");;
assert (IntMap.find 2 tree_test = "Hi");;

let tree_test1 = IntMap.(Empty |> add_basic 3 "test" |> add_basic 0 "test" |> add_basic 1 "test" |> add_basic 1000 "test" |> add_basic 6 "test");;

(** Test find: *)

assert (IntMap.find 3 tree_test1 = "test");;
assert (IntMap.find 1000 tree_test1 = "test");;
assert (IntMap.find 6 tree_test1 = "test");;


(** Test Order: *)

let basic_in = IntMap.in_order_list tree_test1;;
let basic_pre = IntMap.pre_order_list tree_test1;; 
let basic_pos = IntMap.post_order_list tree_test1;;

assert(basic_in = [(0, "test");(1,"test"); (3, "test"); (6, "test");(1000, "test")]);;
assert(basic_pre = [(3, "test");(0,"test"); (1, "test"); (1000, "test");(6, "test")]);;
assert(basic_pos = [(1, "test");(0,"test"); (6, "test"); (1000, "test");(3, "test")]);;


(** Test add_avl: *)

let tree_test2 = IntMap.add_avl 0 "Ruth" IntMap.Empty;;
let tree_test2 = IntMap.add_avl 1 "Negussie" tree_test2;; 
let tree_test2 = IntMap.add_avl 2 "Hi" tree_test2;; 


(** Test find: *)

let find0 = IntMap.find 0 tree_test2;;
let find1 = IntMap.find 1 tree_test2;;
let find2 = IntMap.find 2 tree_test2;;

assert (find0 = "Ruth");;
assert (find1 = "Negussie");;
assert (find2 = "Hi");;

let tree_test2 = IntMap.add_avl 1 "new Negussie" tree_test2;; 
assert (IntMap.find 1 tree_test2 = "new Negussie");;

let avl_tree_test = IntMap.(Empty |> add_avl 3 "test2" |> add_avl 0 "test2" |> add_avl 1 "test2" |> add_avl 100 "test2" |> add_avl 6 "test2");;

(** Test Order: *)


let balanced_in = IntMap.in_order_list avl_tree_test;;
let balanced_pre = IntMap.pre_order_list avl_tree_test;; 
let balanced_pos = IntMap.post_order_list avl_tree_test;; 

assert(balanced_in = [(0, "test2");(1,"test2"); (3, "test2"); (6, "test2");(100, "test2")]);;
assert(balanced_pre = [(3, "test2");(0,"test2"); (1, "test2"); (100, "test2");(6, "test2")]);;
assert(balanced_pos = [(1, "test2");(0,"test2"); (6, "test2"); (100, "test2");(3, "test2")]);;


assert(IntMap.depth 3 avl_tree_test = 1);;

let avl_tree_test_unb = IntMap.(Empty |> add_avl 4 "test3" |> add_avl 0 "test3" |> add_avl 1 "test3" |> add_avl 15 "test3" |> add_avl 5 "test3");;

let unbalanced_in = IntMap.in_order_list avl_tree_test_unb;;
let unbalanced_pre = IntMap.pre_order_list avl_tree_test_unb;; 
let unbalanced_pos = IntMap.post_order_list avl_tree_test_unb;; 

assert(unbalanced_in = [(0, "test3");(1,"test3"); (4, "test3"); (5, "test3");(15, "test3")]);;
assert(unbalanced_pre = [(4, "test3");(0,"test3"); (1, "test3"); (15, "test3");(5, "test3")]);;
assert(unbalanced_pos = [(1, "test3");(0,"test3"); (5, "test3"); (15, "test3");(4, "test3")]);;


assert(IntMap.depth 15 avl_tree_test_unb = 2);;

let avl_tree_test_unb = avl_tree_test_unb |> IntMap.add_avl 2 "test3" |> IntMap.add_avl 3 "test3";;

let unbalanced_in_update = IntMap.in_order_list avl_tree_test_unb;;
let unbalanced_pre_update = IntMap.pre_order_list avl_tree_test_unb;; 
let unbalanced_pos_update = IntMap.post_order_list avl_tree_test_unb;; 


assert(unbalanced_in_update = [(0, "test3") ; (1,"test3") ; (2, "test3") ; (3,"test3");(4, "test3") ; (5, "test3") ; (15, "test3")]);;
assert(unbalanced_pre_update =  [(4, "test3"); (0, "test3"); (1, "test3"); (2, "test3"); (3, "test3"); (15, "test3"); (5, "test3")]);;
assert(unbalanced_pos_update = [(3, "test3"); (2, "test3"); (1, "test3"); (0, "test3"); (5, "test3"); (15, "test3"); (4, "test3")]);;


assert(IntMap.depth 5 avl_tree_test_unb = 3);;




