open Book

(**module type of like criteria*)
module type Like_Criteria_t = sig
  val favoriteWord : string
  val minCount : int
end

(**module type of students*)
module type Student_t =
sig
  type thoughts (* abstract *)
  type status (* concrete *)
    = Yes
    | No
    | Maybe of thoughts * Book.direction


  val start : thoughts  (* the student's starting thoughts *)

  (**  [think t p] returns the student's like [status]
   *   of the book, considering the given thoughts [t] and 
   *   the information about the current page [p]. More
   *   specifically, 
   *     [think t p] returns [Yes] iff the student "likes" the book
   *     [think t p] returns [No] iff the student "dislikes" the book
   *     [think t p] returns [Maybe (t',d)] iff the student remains
   *       undecided, with updated thoughts [t'] and a direction
   *       [d] in which to flip the book in future steps
   *)

   val think : thoughts -> (bool * Book.page option) -> status

   (* [read b] is true iff Student's status is [Yes]
    *     upon thinking repeatedly about the pages of [b]
    *      until seeing [minCount] occurrences of the [favoriteWord],
    *  or false iff Student's status is [No]
    *  upon thinking repeatedly about the pages of [b]
    *      until reaching the end of the book [b] and still not
    *      seeing [minCount] of the [favoriteWord],
    *  starting from the first page and going forward
    *)
    val read : Book.book -> bool
end

(** functor for creating a student module from the given [Criteria]*)
module MkStudent (Criteria: Like_Criteria_t) : Student_t = struct
  
  type thoughts = {favoriteWordCounter: int};;

  type status
    = Yes
    | No
    | Maybe of thoughts * Book.direction;;

  (* the student's starting thoughts *)  
  let start = {favoriteWordCounter = 0} ;;

  
  (* What is the worse case running time complexity of your function? Why?
      The worst case running time complexity for my function is O(n).
      As it iterates the favorite word linearly using the two helper functions.    
  *)

  (* 
    [find_favorite_word page] where [page] is the current page. This helper 
    function finds and counts the number of occurrences of the favorite word
    in the current page. 

    [like_or_dislike favorite_word_counter word_count] this is the second helper
    function. It takes [favorite_word_counter] and [favorite_word_counter] to help
    us determine if the student "likes", "dislikes" or remains undecided.
    
    [think student_thought page] returns the student's like [status] of the book, 
    considering the given [thoughts] and the information about the current page 
    [page].   
   *)


  let find_favorite_word page =
    String.split_on_char ' ' page
    |> List.filter (fun favorite -> String.equal favorite Criteria.favoriteWord)
    |> List.length

  let like_or_dislike favorite_word_counter word_count = 
    match favorite_word_counter.favoriteWordCounter + word_count >= Criteria.minCount with
    | true -> Yes
    | false -> No 

  let think student_thought = function
    | (_, None) -> like_or_dislike student_thought 0
    | (_, Some page) -> like_or_dislike student_thought (find_favorite_word page)
           

  (*
  * What is the worse case running time complexity of your function? Why?
    I believe the worst-case running time complexity of this function is O(n^2), 
    where n is the number of pages in the book. It is O(n^2) because our helper
    function [helper_read counter book] iterates through each page.
  *)

  (*
    [helper_read counter book] is a helper function that is used to read 
    the pages of [book] and [counter] the student's thoughts about the 
    book. [counter] is used to keep track of the total number of favorite
    words. While [book] is a list of all the pages in the book. If the 
    [current_page] is [None] then we call our think function with [counter]
    and [current_page] to determine the students thoughts of the book.
    Whereas, is there is [Some page] then we have to update [counter] by
    adding the count of the favorite word. Here we also create flip to
    the next page using [next_page] that calls [Book.flip Book.Forward book]
    and do the same thing with the next_page.
    
  *)

  (*
    [read book] is true iff Student's status is [Yes], otherwise, it is 
    false iff Student's status is [No]. Here we call our helper function
    [helper_read counter book] that helps us determine if we have reached the 
    end of the [book] as well as if we have seen [minCount] occurrences 
    of the [favoriteWord]
  *)  
  
  let rec helper_read counter book =
    let current_page = Book.currentPage book in
    match current_page with
    | (_, None)   -> think counter current_page
    | (_, Some page) ->
      let new_counter = counter.favoriteWordCounter + (find_favorite_word page) in
      let next_page = Book.flip Book.Forward book in
      next_page|> helper_read {favoriteWordCounter = new_counter}  

  let read book=
    match helper_read {favoriteWordCounter = 0} book with
    | Yes -> true
    | _   -> false

end
