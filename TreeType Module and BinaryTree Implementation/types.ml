
(** Develop a minimum signature, i.e. a minimum set of functions, for this module 
    type that will allow the MkForest functor and the client code given at 
    the end of the file work.

    For each function, write its specification comments, following the style 
    guidelines in the book. Refer to book sections 6.1 to 6.3 and related 
    vidoes for information and examples.  
 *)

(** To Do: add comments for this module type. 
    Remember to specify representation invariants, if there are any.
*)
  module type TreeType =
    sig
      
      (** ['a tree] represents a tree whose elements are of type ['a]. *)
      type 'a tree 
  
      (** 
        [cmpFun] is a comparison function that takes two values of the same 
        type 'a and return an integer value. The integer value is used 
        to interpret the comparsion. If the keys [e1] and [e2] are equal, 
        [f e1 e2] is strictly negative if [e1] is smaller than [e2],
        and [f e1 e2] is strictly positive if [e1] is greater than [e2].
      *)
      type 'a cmpFun = 'a -> 'a -> int
  
      (** This exception is raised when the leaf is empty*)
      exception NotFound
  
       (** The empty tree *)
      val empty : unit -> 'a tree

      (** 
      [insert cpmFun t x] returns a basic BST. It takes a BST [t], the [cmpFun] functions
      and a value [x] that is going to be inserted into our BST [t]. The comparison 
      function [cmpFun] is used to indicate the position of the new insertion value 
      [x]. If the BST is empty, then a new node with the value [x] becomes the root.
      Otherwise, we recursively perform a comparsion until we find an empty 
      leaf node to insert the new value [x] 
      *)
      val insert : ('a -> 'a -> int) -> 'a tree -> 'a -> 'a tree

      (** 
      [remove cmpFun t x] returns a basic BST. It takes a BST [t], the [cmpFun] function
      and a value [x] that is going to be removed from the BST [t]. The comparsion function
      [cmpFun] is used to determine the correct poistion. If the BST [t] is empty 
      then an empty node is returned. Otherwise, we recursively perform a comparsion
      until it finds the node containing the value [x]. When the value [x] is found 
      an empty node is returned removing the node from the BST [t]. If [x] is less than
      [k] then we go to the left subtree. Otherwise, we go to the right subtree.    
      *)
      val remove : ('a -> 'a -> int) -> 'a tree -> 'a -> 'a tree

      (** 
      [isIN cmpFun t x] returns a bool. It takes a BST [t], the [cmpFun] function
      and a value [x] that we are trying to find in the BST. If the BST is empty
      it should return false because the value [x] doesn't exist in [t]. Otherwise,
      use [cmpFun] to determine if we should go to the left or right subtree. If 
      we are able to find a node containing [x] then we return true as we have found
      the value [x]. Otherwise, if none of the nodes contain the value [x] false is
      returned, as the value [x] does not exist in [t].     
      *)
      val isIn : ('a -> 'a -> int) -> 'a tree -> 'a -> bool


      (**
      [min t] takes a BST [t] and returns the minimum value of this BST. If the BST
      [t] is empty the NotFound exception is raised. Otherwise, find the node with
      the smallest value. This is done by traversing the left subtree of the BST [t]
      until a Leaf node is found. The Leaf node is the minimum value therefore, 
      it is returned.  
      *)
      val min : 'a tree -> 'a

      (**
      [findRoot t] takes a BST [t] and returns the root of the BST. If the BST is empty
      the NotFound exception is raise. Otherwise, node is pattern matched and 
      the root [k] is extracted and returned.
      *)
      val findRoot : 'a tree -> 'a

  
    end
    

  module BinaryTree : TreeType =
    struct
  
      type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree
      
      type 'a cmpFun = 'a -> 'a -> int
  
      
      exception NotFound
  
      let empty () = Leaf
  
      let rec insert (cmp : 'a cmpFun) t x = 
        match t with
          | Leaf -> Node (Leaf, x, Leaf)
          | Node (l, k, r) ->
              let cmp_rst = cmp x k in
                match (cmp_rst < 0) with
                  | true -> Node (insert cmp l x, k, r)
                  | false -> Node (l, k, insert cmp r x)
  
  
      let rec remove (cmp : 'a cmpFun) t x = 
        match t with
          | Leaf -> Leaf
          | Node (l, k, r) ->
              let cmp_rst = cmp x k in
                if(cmp_rst = 0) 
                then Leaf
                else if (cmp_rst < 0)
                    then Node (remove cmp l x, k, r)
                    else Node (l, k, remove cmp r x)
  
  
      let rec isIn (cmp : 'a cmpFun) t x = 
        match t with
          | Leaf -> false
          | Node (l, k, r) ->
              let cmp_rst = cmp x k in
                if(cmp_rst = 0) 
                then true
                else if (cmp_rst < 0)
                    then isIn cmp l x
                    else isIn cmp r x
  
  
      let rec min t = 
        match t with
          | Leaf -> raise NotFound
          | Node (l, k, r) ->
            match l with
            | Leaf -> k
            | _ -> min l
  
  
      let findRoot = function
        | Leaf -> raise NotFound
        | Node (l, k, r) -> k
  
    end
  
  
  (** Develop a minimum signature, i.e. a minimum set of functions, for this module 
      type that will allow the MkForest functor and the client code given at 
      the end of the file work.
  
      For each function, write its specification comments, following the style 
      guidelines in the book. Refer to book sections 6.1 to 6.3 and related 
      vidoes for information and examples.  
   *)
  module type ForestType =
    sig
      
      module T : TreeType 
  
      type 'a forest 

      (** This exception is raised when the forest is full*)
      exception Full
      (** This exception is raised when not in forest*)
      exception NotInForest
      (** This exception is raised when the forest is empty*)
      exception Empty
  
      val limit: int

      (** The empty forest *)
      val empty : unit -> 'a forest

      (**
      [addToForest (ts, i) tree] takes a tuple list of trees [ts], an int [i] and 
      a tree [tree]. [addToForest] performs an insertion that appends [tree] to 
      the list of trees [ts], while also incrementing [i] each time an insertion 
      is made indicating a new tree has been added to the forest. 
      *)
      val addToForest : 'a forest -> 'a T.tree -> 'a forest
 
      (**
      [getRoot t x] takes a BST [t] and returns the root of [t]. This function
      uses the [findRoot t] from module T. It is applied to extract the 
      root value of [t] by extracting [k] from the node. 
      *)
      val getRoot : 'a T.tree -> 'b -> 'a

      (**
      [iterateForest g f tlist x] iterates through each tree in the BST list [tlis].
      If [g] returns true for a tree [h] and value [x], then [f] is applied to [h]
      and [x]. However, if it is false we recursively move onto the next tree by 
      calling the tail [t] and applying the value [x]. However, if the [tlist] is a 
      empty an Empty exception is raised indicating the forest is empty.   
      *)
      val iterateForest : ('a T.tree -> 'b -> bool) -> ('a T.tree -> 'b -> 'b) -> 'a T.tree list -> 'b -> 'b

      (**
      [findRoot cmp forest x] takes a [cpm] function, BST forest [forest] and a value
      [x]. This function finds the root which is the value that contains [x]. A 
      search is performed on each tree in the forest. If the forest is empty
      an NotInForest exception is raised. Otherwise, [iterateForest g f tlist x] is called
      to determine if the given value [x] is present in the given tree. The [getRoot t x]
      is also called to return the root of the node of that specific tree. Using these
      two functions as helper functions we can determine if there is a tree in the forest
      that contains the value [x]. If not, an NotInForest exception is raised.    
      *)
      val findRoot : ('a -> 'a -> int) -> 'a forest -> 'a -> 'a

      (**
      [accForest g f tlist key x] iterates through each tree in [tlist] and determines
      if the [key] is in the current tree [h]. If so, [f] is applied to 
      the current tree [h] with the value [x] and added to the list of [ts]. Otherwise, 
      add the current tree and repeat the process recursively for the rest of [tlist].
      If [tlist] is empty, raise an Empty exception. 
      *)
      val accForest :('a -> 'b -> bool) -> ('a -> 'c -> 'a) -> 'a list -> 'b -> 'c -> 'a list

      (**
      [addToTree cmp forest key x] takes a comparsion function [cmp], a [forest], 
      [key] and a value [x]. If the [forest] is empty then a NotInForest exception is
      raised. Otherwise, the previous function [accForest g f tlist key x] is called 
      with [isIN cmpFun t x] and [insert cpmFun t x] from module T. [accForest] either
      returns the new tree or adds a new value to an existing tree in [tlist]. [forest]
      is then updated. 
      *)
      val addToTree : ('a -> 'a -> int) -> 'a forest -> 'a -> 'a -> 'a forest

      (**
      [removeFromTree cmp forest x] takes a comparsion function [cmp], a [forest]
      and a key [x]. [x] is the value we are trying to remove from a tree in the
      forest [forest]. If [forest] is empty then the NotInForest exception is raised. 
      Otherwise, it iterates through each tree in [forest] using [accForest g f tlist key x].
      If the key [x] is found then a new [forest] is returned.
      *)
      val removeFromTree : ('a -> 'a -> int) -> 'a forest -> 'a -> 'a forest

  
    end
  
  (** [IntBoundType] specifies an int bound as its field.*)
  module type IntBoundType =
    sig 
      val v : int
    end
  
  
  module MkForest (Tree : TreeType) (Bound : IntBoundType) : ForestType =
    struct
  
      module T = Tree
  
      type 'a forest = 'a Tree.tree list * int
  
      exception Full
      exception NotInForest
      exception Empty
  
      let limit = Bound.v
  
      let empty () = ([], 0)
  
      let addToForest (ts, i) tree =
        if i >= limit then raise Full else (tree::ts, i + 1)
  
      let getRoot t x = T.findRoot t
  
      let rec iterateForest g f tlist x = 
        match tlist with 
          | [ ] -> raise Empty
          | h::t ->
            if g h x 
            then f h x 
            else iterateForest g f t x
    
      let findRoot cmp forest x =
        match forest with 
        | ([], _) ->  raise NotInForest
        | (tlist, _) ->
            try (iterateForest (T.isIn cmp) getRoot tlist x)
            with
              | Empty  -> raise NotInForest
              | _ -> failwith "not reachable"
  
      let rec accForest g f tlist key x = 
        match tlist with 
          | [ ] -> raise Empty
          | t::ts ->
            if g t key 
            then (f t x)::ts 
            else t::(accForest g f ts key x)
  
      let addToTree cmp forest key x = 
        match forest with
        | ([], _) -> raise NotInForest
        | (ts, i) ->
            try
               let nts = accForest (T.isIn cmp) (T.insert cmp) ts key x
               in
                 (nts, i)
            with 
            | Empty -> raise NotInForest
            | _ -> failwith "not reachable"
          
      let removeFromTree cmp forest x = 
        match forest with
        | ([], _) -> raise NotInForest
        | (ts, i) ->
            try
              let nts = accForest (T.isIn cmp) (T.remove cmp) ts x x
              in (nts, i)
            with 
              | Empty -> raise NotInForest
              | _ -> failwith "not reachable"
  
    end;;
  
  
  (** [Limit4] module defines a value v as an integer 4.*)
  module Limit4 =
    struct 
      let v = 4
    end;;
  
  
  (** The following code must run after you complete the module types,
    * and they need to be minimum.
  *)
  module Forest = MkForest (BinaryTree) (Limit4);;
  
  assert (4 = Forest.limit);;
  
  let cmp = Int.compare
  
  let f t x = Forest.T.insert cmp t x
  
  let t0 = List.fold_left f (Forest.T.empty ())
                              [14; 56; 8; 3; 9; 0]
  
  let t1 = List.fold_left f (Forest.T.empty ())
                              [1; 9; 0; 5; 3]
  
  let fe : int Forest.forest = Forest.empty ()
  
  let f1 = Forest.addToForest fe t0
  
  let f2 = Forest.addToForest f1 t1
  
  let f3 = try 
              Forest.addToTree cmp f2 3 15
              with 
              | Forest.Full -> f2
  
  let f4 = Forest.addToTree cmp f3 9 20
  
  let root_rst = try 
                      Some (Forest.findRoot cmp f4 20)
                  with 
                      | Forest.NotInForest -> None
  
  let remove_rst = try
                      Some (Forest.removeFromTree cmp f4 10)
                      with 
                      Forest.NotInForest -> None
  
  let cmp_str = String.compare
  
  let f_str t x = Forest.T.insert cmp_str t x
  
  let t2 = List.fold_left f_str (Forest.T.empty ())
  
                              ["Clark"; "Freud"; "CMACD"; "Lasry"; "Sackler"]
  
  let fe2 : string Forest.forest = Forest.empty ()
  
  let f5 = Forest.addToForest fe2 t2
  
  let root_rst2 = Forest.findRoot cmp_str f5 "CMACD"