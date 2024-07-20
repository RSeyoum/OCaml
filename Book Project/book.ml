(** module type of books*)
module type Book_t =
sig
  type page = string (* concrete *)
  type direction = Forward | Backward
  type book (* abstract *)

  exception OutOfBounds


  (** [bind lst] is a book that includes all pages in [lst], 
     preserving the order of the pages, and is opened to
     the first page, if one exsits.
  *)
  val bind : page list -> book


  (** [unbind b] is a list of all pages in b, with the 
      order preserved. Note that unbind (bind lst) = lst. 
  *)
  val unbind : book -> page list

  (**
   *   [currentPage b] returns a tuple, with the first element indicating
   *   whether the book is opened at the beginning, and the second
   *   element indicate the current page if there is one, or None otherwise.
   *   More specifically, it should return the following values  
   *      (true,  None) iff [b] is empty
   *      (true,  Some p) iff [b] is not empty and opened to its first page [p]
   *      (false, None) iff b is non-empty and opened to the the end of the book
   *      (false,  Some p) iff b is opened to page [p] in the middle of the book 
   *
   *)
  val currentPage : book -> bool * page option

  (* [flip dir b] returns a book that is flipped the given book
   * [b] in the direction [dir] by one page, if that is possible;
   * or reaise OutOfBounds otherwise.
   * More Specifically,
   *   flip Forward b raises OutOfBounds iff
   *     currentPage b is the end of b
   *
   *   flip Forward b = b' otherwise,
   *     where currentPage b' is the next page in b
   *
   *   flip Backward b raises OutOfBounds iff
   *     currentPage b is the beginning of b
   *
   *   flip Backward b = b' otherwise,
   *     where currentPage b' is the previous page in b
   *)
  val flip : direction -> book -> book
end

module Book : Book_t = struct
  
  type page = string
  type direction = Forward | Backward
  type book = {all_pages: page list; current_page: int}

  exception OutOfBounds

  (* 
    [bind lst] is a book containing [lst], it is initally opened to the 
    first page, if one exists. 
  *)
  let bind lst =
    match lst with
    | [] -> {all_pages = []; current_page = 0}
    | _ -> {all_pages = lst; current_page = 1}

  (* [unbind book] is a list of all pages in [book]. *)
  let unbind book = book.all_pages
  

  (* What is the worse case running time complexity of your function? Why?
      The worst case running time complexity for my function is O(n). This
      is because of List.length and List.nth.Which is called for all the 
      pages in the book. 
  *)

  (* [currentPage book] returns a tuple, indicating whether the book is 
     opened at the beginning, middle or end. It also indicates the 
     current page if there is one, or None otherwise*)
 
  (* Process:
     1. (true,  None) iff [b] is empty
     2. (true,  Some p) iff [b] is not empty and opened to its first page [p]
     3. otherwise, subtract 1 from the current_page and use that value as the index
        for List.nth with the total page returning the element at that index. 
  *)

  let currentPage book =
    if book.current_page = 0 then (true, None)
    else if book.current_page = 1 then (true, Some (List.hd book.all_pages)) 
    else if (List.length book.all_pages) <= (book.current_page - 1 )  then  (false, None)
    else (false, Some (List.nth book.all_pages (book.current_page - 1)))
  
  
  (* What is the worse case running time complexity of your function? Why?
      The worst case running time complexity for my function is O(n).
      As it iterates the favorite word linearly using the two helper functions.    
  *)  

  (*
    What is the worse case running time complexity of your function? Why?
      The worst case running time complexity for my function is O(1). This
      is because everything in this functio is done in constant
      time operation. This function also doesn't perform any recursion or 
      iteration of all pages in the book.    
  *)

  (* [flip direction book] flips the given [book] in the [direction] determined 
     by the currentPage or raise OutOfBounds. *)

  (*
    Process:
    1. flip Backward b raises OutOfBounds iff currentPage b is the beginning of b.
       Otherwise, currentPage b' is the previous page in b
    2. flip Forward b raises OutOfBounds iff currentPage b is the end of b. 
       Otherwise, currentPage b' is the next page in b    
  *)
  
  let flip direction book =
    match direction with
    | Backward -> if book.current_page = 0 then raise OutOfBounds
                  else {book with current_page = book.current_page - 1}
    | Forward -> if book.current_page > List.length book.all_pages then raise OutOfBounds
                  else {book with current_page = book.current_page + 1}
                                   
end
