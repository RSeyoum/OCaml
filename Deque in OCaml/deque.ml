module type Deque = sig
  (** An ['a t] is a double-ended queue whose elements have type ['a]. *)
  type 'a t

  (** Raised if [peek_first], [peek_last], [remove_first] or [remove_last] 
      is applied to the empty queue. *)
  exception Empty

  (** [empty] is the empty queue. *)
  val empty : 'a t

  (** [is_empty q] is whether [q] is empty. *)
  val is_empty : 'a t -> bool

  (** [add_first x q] is a queue with [x] as the first element, 
      followed by the elements [q]. *)
  val add_first : 'a -> 'a t -> 'a t
  
  (** [add_last x q] is a queue with the elements of the queue [q], 
      followed by [x] as the last element.*)
  val add_last : 'a -> 'a t -> 'a t

  (** [peek_first q] is the first element of the queue [q]. 
  Raises [Empty] if [q] is empty. *)
  val peek_first : 'a t -> 'a

  (** [peek_last q] is the last element of the queue [q]. 
  Raises [Empty] if [q] is empty. *)
  val peek_last : 'a t -> 'a

  (** [remove_first q] is the queue containing all the elements of [q] 
  except the first element of [q]. Raises [Empty] is [q] is empty. *)
  val remove_first : 'a t -> 'a t

  (** [remove_last q] is the queue containing all the elements of [q] 
  except the last element of [q]. Raises [Empty] is [q] is empty. *)
  val remove_last : 'a t -> 'a t

  (** [size q] is the number of elements in [q]. *)
  val size : 'a t -> int
  
  (** [to_list q] is a list containing the elements of [q] in order from
  the first element to the last element. *)
  val to_list : 'a t -> 'a list

  (** [filter f q] is a deque with all the elements of the deque [q] 
      that satisfy the predicate [f]. The order of the elements in the 
      argument [q] is preserved. 
  *)
  val filter : ('a -> bool) -> 'a t -> 'a t


  (** [map f q] applies function f to the elements of [q] and builds a deque 
   with the results returned by f.   
  *)
  val map : ('a -> 'b) -> 'a t -> 'b t


 (** [fold_from_first_forward f init q] uses [f] and [init] to combine
     the deque [q], starting with its first element and going forward,
     in a sense similar to List.fold_left.
     As an example, if [q] is an int deque with elements 1, 10, 20, 
     with 1 being the first element and 20 being the last,
     [f] is the int substraction ( - ), init is 0, then the result 
     of fold_from_first_forward should be -31.
  *)
  val fold_from_first_forward : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

 (** [fold_from_last_backward f q init] uses [f] and [init] to combine
     the deque [q], starting with its last element and going backward,
     in a sense similar to List.fold_right.
     As an example, if [q] is an int deque with elements 1, 10, 20, 
     with 1 being the first element and 20 being the last,
     [f] is the int substraction ( - ), init is 0, then the result 
     of fold_from_last_backward is 11.
  *)
  val fold_from_last_backward : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
end

module TwoListDeque : Deque = struct

  (** [{front; back; size}] represents the deque [front @ List.rev back]
  along with its size -- the number of elements in the deque. 
  For example, [{front = [10; 20]; back = [50; 40; 30]; size = 5}] 
  represents the conceptual queue 10, 20, 30, 40, 50, where [10] is 
  the first element and [50] is the last. 
  To avoid ambiguity about emptiness, whenever only one of the 
  lists is empty, it must be [back]. For example,
  [{front = [10]; back = []; size = 1}] is a legal representation, but 
  [{front = []; back = [10]; size =1 }] is not. This implies that if
  [front] is empty, [back] must also be empty. *)

  type 'a t = {front : 'a list; back : 'a list; size: int}

  exception Empty

  (* provide all the funcions defined in the Deque module type,
     and define helper methods if needed
  *)

  (** [empty] is the empty queue. *)  
  let empty = {front = []; back = []; size = 0}

  (** [is_empty q] is whether [q] is empty. *)
  let is_empty = function
    |{front = []; back; size} -> true
    | _ -> false

  (** [add_first x q] is a queue with [x] as the first element, 
      followed by the elements [q]. *)
  let add_first x = function
    | {front = []; back; size} -> {front = [x]; back = []; size = 1}
    | {front; back; size} -> {front = x :: front; back; size = size + 1};;

  (** [add_last x q] is a queue with the elements of the queue [q], 
      followed by [x] as the last element.*)  
  let add_last x = function
  | {front = []; back = []; size} -> {front = [x]; back = []; size = 1}
  | {front; back; size} -> {front; back = back @ [x]; size = size + 1};;

  (** [peek_first q] is the first element of the queue [q]. 
  Raises [Empty] if [q] is empty. *)
  let peek_first = function
  | {front = []; _} -> raise Empty
  | {front = h :: _; back; size} -> h;;

  (** [peek_last q] is the last element of the queue [q]. 
  Raises [Empty] if [q] is empty. *)
  let peek_last = function
  | {front = []; _} -> raise Empty
  | {front= [x]; back = []; _} -> x
  | {front; back = [x]; _} -> x
  | {front = h :: t; back = []; _} -> List.hd (List.rev t)
  | {front; back = h :: t ; _} -> List.hd (List.rev t);;

  (** [remove_first q] is the queue containing all the elements of [q] 
  except the first element of [q]. Raises [Empty] is [q] is empty. *)
  let remove_first = function
    | {front = []; _} -> raise Empty
    | {front = [_]; back; size} -> {front = List.rev back; back = []; size = size - 1}
    | {front = _ :: t; back; size} -> {front = t; back; size = size - 1}

  (** [remove_last q] is the queue containing all the elements of [q] 
  except the last element of [q]. Raises [Empty] is [q] is empty. *)  
  let remove_last = function
    | { front = []; _ } -> raise Empty
    | { front = [_]; back = []; _ } -> { front = []; back = []; size = 0 }
    | { front; back = [_]; size } -> { front; back = []; size = size - 1 }
    | { front; back = h :: t; size } -> { front; back = h :: (List.tl (List.rev t)); size = size - 1 }
    | { front = h :: t; back = []; size } -> { front = h :: (List.tl (List.rev t)); back = []; size = size - 1 };;
    

  (** [size q] is the number of elements in [q]. *)
  let size q = q.size
  
  (** [to_list q] is a list containing the elements of [q] in order from
  the first element to the last element. *)
  let to_list {front; back; _} = front @ back

  (** [filter f q] is a deque with all the elements of the deque [q] 
      that satisfy the predicate [f]. The order of the elements in the 
      argument [q] is preserved. 
  *)
  let filter p = function 
  | {front; back; size} -> 
    { front = List.filter p front; 
    back = List.filter p back; 
    size = List.length (List.filter p front) + List.length (List.filter p back) };;


  (** [map f q] applies function f to the elements of [q] and builds a deque 
   with the results returned by f.   
  *)
  let map f = function
  | {front; back; size} -> { front = List.map f front; back = List.map f back; size };;


 (** [fold_from_first_forward f init q] uses [f] and [init] to combine
     the deque [q], starting with its first element and going forward,
     in a sense similar to List.fold_left.
     As an example, if [q] is an int deque with elements 1, 10, 20, 
     with 1 being the first element and 20 being the last,
     [f] is the int substraction ( - ), init is 0, then the result 
     of fold_from_first_forward should be -31.
  *)
  let fold_from_first_forward f init = function
    | { front = []; _ } -> init
    | { front; back; _ } -> let fold_front_deque = List.fold_left f init front in
      List.fold_left f fold_front_deque (List.rev back);;


 (** [fold_from_last_backward f q init] uses [f] and [init] to combine
     the deque [q], starting with its last element and going backward,
     in a sense similar to List.fold_right.
     As an example, if [q] is an int deque with elements 1, 10, 20, 
     with 1 being the first element and 20 being the last,
     [f] is the int substraction ( - ), init is 0, then the result 
     of fold_from_last_backward is 11.
  *)
 
  let fold_from_last_backward f q init =
    match q with
    | {front = []; _ } -> init
    | { front; back; size } -> let fold_back_deque = List.fold_right f back init in
        List.fold_right f front fold_back_deque;;

end
