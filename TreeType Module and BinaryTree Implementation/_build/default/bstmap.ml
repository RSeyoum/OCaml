
(** This assignment is about creating maps with basic BSTs or AVL trees, 
    doing tree traversals, and computing node depths.

    Parts of the comments and code below are copied from the OCaml
    library files for its map module on github, specifically
    https://github.com/lucasaiu/ocaml/blob/master/stdlib/map.mli
    https://github.com/lucasaiu/ocaml/blob/master/stdlib/map.ml

    The OCaml map library is implemented with Red Balck Trees. 
*)

(** Association tables over ordered types.

   This module implements several functions of applicative association 
   tables, also known as finite maps or dictionaries, given a total ordering 
   function over the keys.
   
   All operations over maps are purely applicative (no side-effects).

   The implementation uses AVL binary trees, and therefore searching
   and insertion take time logarithmic in the size of the map.
*)

module type OrderedType =
  sig
    (** The type of the map keys. *)
    type t
      
    (** A total ordering function over the keys.
          This is a two-argument function [f] such that
          [f e1 e2] is zero if the keys [e1] and [e2] are equal,
          [f e1 e2] is strictly negative if [e1] is smaller than [e2],
          and [f e1 e2] is strictly positive if [e1] is greater than [e2].
          Example: a suitable ordering function is the generic structural
          comparison function {!Pervasives.compare}. *)
    val compare : t -> t -> int
      
  end
(** Input signature of the functor MakeBSTMap. *)


module type S =
  sig
    (** The type of the map keys. *)
    type key

    (** The type of maps from type [key] to type ['a]. *)
    type (+'a) t
    
    (** The empty map. *)
    val empty: 'a t
    
    (** Test whether a map is empty or not. *)
    val is_empty: 'a t -> bool
    
    (** [find x m] returns the current binding of [x] in [m],
       or raises [Not_found] if no such binding exists. *)
    val find: key -> 'a t -> 'a
    
    (** [add_basic x y m] returns a basic BST, with no balancing, that 
       contains the same bindings as [m], plus a binding of [x] to [y]. 
       If [x] was already bound in [m], its previous binding disappears 
       and the key [x] is bound to the new value [y]. 
    *)   
    val add_basic: key -> 'a -> 'a t -> 'a t

    (** [add_avl x y m] returns an AVL  BST, with proper balancing, that 
       contains the same bindings as [m], plus a binding of [x] to [y]. 
       If [x] was already bound in [m], its previous binding disappears 
       and the key [x] is bound to the new value [y].
    *)
    val add_avl: key -> 'a -> 'a t -> 'a t
    
    (** [in_order_list m] returns the list of all bindings of the given 
        BST map [m], with (key, value) pairs listed in the order of 
        an in-order traversal of the tree [m].
     *)
    val in_order_list: 'a t -> (key * 'a) list
    
    (** [pre_order_list m] returns the list of all bindings of the given 
        BST map [m], with (key, value) pairs listed in the order of 
        an pre-order traversal of the tree [m].
     *)  
    val pre_order_list: 'a t -> (key * 'a) list
    
    (** [post_order_list m] returns the list of all bindings of the given 
        BST map [m], with (key, value) pairs listed in the order of 
        an post-order traversal of the tree [m].
     *)  
    val post_order_list: 'a t -> (key * 'a) list
    
    (** [depth x m] returns the depth of the node with [x] in [m],
       or raises [Not_found] if no such binding exists. The depth
       of the root is 1. *)
    val depth : key -> 'a t -> int
    
  end


module MakeBSTMap(Ord: OrderedType) = struct

  type key = Ord.t

  type 'a t =
      Empty
    | Node of 'a t * key * 'a * 'a t * int

  let empty = Empty

  let is_empty = function Empty -> true | _ -> false

  let rec find x = function
      Empty ->
        raise Not_found
    | Node(l, v, d, r, _) ->
        let c = Ord.compare x v in
        if c = 0 then d
        else find x (if c < 0 then l else r)
  

  let rec add_basic new_key new_value = function
  | Empty -> Node (Empty, new_key, new_value, Empty, 1)
  | Node (left, current_key, current_value, right, h) ->
      if new_key = current_key then Node (left, current_key, new_value, right, h)
      else if new_key < current_key then Node ((add_basic new_key new_value left), current_key, current_value, right, h + 1)
      else Node (left, current_key, current_value, (add_basic new_key new_value right), h + 1)  

  (**
    [height tree_node], [create l x d r] and [bal l x d r] were taken from:
    
      https://github.com/lucasaiu/ocaml/blob/master/stdlib/map.ml
  *)    


  (**
    [height tree_node] calculates the height of the given [tree_node]. If [tree_node]
    is empty then the height is 0. Otherwise, the height of the [tree_node] is 
    extracted and returned.
  *)    
  let height = function
      Empty -> 0
    | Node(_,_,_,_, height) -> height

  (**
   [create l x d r] takes four arguments and creates a new BST node. The last argument 
   is used to calculate the height of the node. It is calculated by comparing the
   height of the left subtree and right subtree and adds 1 to get the new height 
   of the new node.  
  *)  
  let create l x d r =
    let hl = height l and hr = height r in
    Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))    

  (**
   [bal l x d r] takes four arguments and returns a BST that is balanced
   even after insertion. We first calculate and extract 
   the height of the left and right subtree. If the height of the 
   left subtree is greater then the right subtree plus 2 than we need to
   rebalance the node by doing a right rotation or adding a new
   node to the left. However, if the height of the right subtree is greater
   than the left subtree plus 2 then we need to rebalance the node by 
   doing a left rotation or adding a new node to the right. 

   However, if the height of the subtree is already balanced. A new node
   is created. The height is computed based on the height of its children
   nodes. 
  *)    
  let bal l x d r =
    let hl = match l with Empty -> 0 | Node(_,_,_,_,h) -> h in
    let hr = match r with Empty -> 0 | Node(_,_,_,_,h) -> h in
    if hl > hr + 2 then begin
      match l with
        Empty -> invalid_arg "Map.bal"
      | Node(ll, lv, ld, lr, _) ->
          if height ll >= height lr then
            create ll lv ld (create lr x d r)
          else begin
            match lr with
              Empty -> invalid_arg "Map.bal"
            | Node(lrl, lrv, lrd, lrr, _)->
                create (create ll lv ld lrl) lrv lrd (create lrr x d r)
          end
    end else if hr > hl + 2 then begin
      match r with
        Empty -> invalid_arg "Map.bal"
      | Node(rl, rv, rd, rr, _) ->
          if height rr >= height rl then
            create (create l x d rl) rv rd rr
          else begin
            match rl with
              Empty -> invalid_arg "Map.bal"
            | Node(rll, rlv, rld, rlr, _) ->
                create (create l x d rll) rlv rld (create rlr rv rd rr)
          end
    end else
      Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))
    
  
  let rec add_avl new_key new_value = function
  | Empty -> Node (Empty, new_key, new_value, Empty, 1)
  | Node (left, current_key, current_value, right, h) ->
      if new_key = current_key then Node (left, current_key, new_value, right, h)
      else if new_key < current_key then bal (add_basic new_key new_value left) current_key current_value right
      else bal left current_key current_value (add_basic new_key new_value right)   
 

  let rec in_order_list = function
    | Empty -> []
    | Node (left, key, value, right, _) ->
      (in_order_list left) @ [(key, value)] @ (in_order_list right) 
    
  let rec pre_order_list = function
    | Empty -> []
    | Node (left, key, value, right, _) ->
      [(key, value)] @ (pre_order_list left) @ (pre_order_list right)

  let rec post_order_list = function
    | Empty -> []
    | Node (left, key, value, right, _) ->
          (post_order_list left) @ (post_order_list right) @ [(key, value)]    

  let rec depth search_node tree = 
    let rec find_depth accumulator = function
      | Empty -> raise Not_found
      | Node (left, key, _, right, h) ->
        if search_node = key then accumulator
        else if search_node < key then find_depth (accumulator + 1) left
        else find_depth (accumulator + 1) right
      in 
      find_depth 1 tree        
       
end



