
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
  
      (** To Do: add specification, and delete this line*)
      type 'a cmpFun = 'a -> 'a -> int
  
      (** To Do: add specification, and delete this line*)
      exception NotFound
  
      val empty : unit -> 'a tree

      (** To Do: add function headers and specifications, delete this line*)
      val insert : ('a -> 'a -> int) -> 'a tree -> 'a -> 'a tree

      val remove : ('a -> 'a -> int) -> 'a tree -> 'a -> 'a tree

      val isIn : ('a -> 'a -> int) -> 'a tree -> 'a -> bool

      val min : 'a tree -> 'a

      val findRoot : 'a tree -> 'a

  
    end
    
  (** To Do: add comments about this module.
   *  
   * Also add comments about functions, when appropriate, following the 
   * guidelines in the book.
  *)
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

      exception Full
      exception NotInForest
      exception Empty
  
      val limit: int

      val empty : unit -> 'a forest

      val addToForest : 'a forest -> 'a T.tree -> 'a forest
 
      val getRoot : 'a T.tree -> 'b -> 'a

      val iterateForest : ('a T.tree -> 'b -> bool) -> ('a T.tree -> 'b -> 'b) -> 'a T.tree list -> 'b -> 'b

      val findRoot : ('a -> 'a -> int) -> 'a forest -> 'a -> 'a

      val accForest :('a -> 'b -> bool) -> ('a -> 'c -> 'a) -> 'a list -> 'b -> 'c -> 'a list

      val addToTree : ('a -> 'a -> int) -> 'a forest -> 'a -> 'a -> 'a forest

      val removeFromTree : ('a -> 'a -> int) -> 'a forest -> 'a -> 'a forest

  
    end
  
  (** [IntBoundType] specifies an int bound as its field.*)
  module type IntBoundType =
    sig 
      val v : int
    end
  
  
  (** To Do: add comments about this module.
   *  
   * Also add comments about functions, when appropriate, following the 
   * guidelines in the book.
  *)
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
  
  
  (** To Do: add comments*)
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