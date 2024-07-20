open OUnit2
open Book
open Student

let book_empty = Book.bind []

let book_1page = Book.bind ["hello"]

let book_2pages = Book.bind ["lambda";"function"]

let book_2pages_fd = (book_2pages |> Book.flip Book.Forward)

let book_3pages = Book.bind ["first"; "second"; "third"]

let book_3pages_fd = (book_3pages |> Book.flip Book.Forward)

let book_4pages = Book.bind ["hello"; "ruth"; "morning"; "ruth"; "ruth"]

let book_4pages_fd2 = (book_4pages |> Book.flip Book.Forward |> Book.flip Book.Forward)



module Lambda_1 : Like_Criteria_t = struct
  let favoriteWord  = "lambda" 
  let minCount = 1
end

module Lambda_2: Like_Criteria_t = struct
  let favoriteWord  = "ruth" 
  let minCount = 2
end



module Student_lambda_1 = MkStudent(Lambda_1)
module Student_lambda_2 = MkStudent(Lambda_2)

let tests = "test suite for Book" >::: [

  "book empty, unbind" >:: 
    (fun _ -> assert_equal [] (Book.unbind book_empty));

  "book 2pages at the beginning, unbind" >:: 
    (fun _ -> assert_equal ["lambda";"function"]  
                           (Book.unbind book_2pages));

  "book 2pages forwarded, unbind" >:: 
    (fun _ -> assert_equal ["lambda";"function"] 
                           (Book.unbind book_2pages_fd));

  "book empty, current page" >:: 
    (fun _ -> assert_equal (true, None)  
                           (Book.currentPage book_empty));

  "book 2pages at the beginning, current page" >:: 
    (fun _ -> assert_equal (true, Some "lambda")  
                           (Book.currentPage book_2pages));

  "book 2pages forwarded, current page" >:: 
    (fun _ -> assert_equal (false, Some "function") 
                           (Book.currentPage book_2pages_fd));

  "book empty, flip backward" >:: 
    (fun _ -> assert_raises (Book.OutOfBounds)  
                     (fun () -> Book.(flip Backward book_empty))); 

  "student lambda 1, read book_empty " >::
  (fun _ -> assert_equal false (Student_lambda_1.read book_empty));

  "student lambda 1, read book_2pages " >::
    (fun _ -> assert_equal true (Student_lambda_1.read book_2pages));
    
  "student lambda 1, read book_2pages_fd " >::
    (fun _ -> assert_equal false (Student_lambda_1.read book_2pages_fd));
 
  "book 4pages at the beginning, unbind" >:: 
    (fun _ -> assert_equal ["hello"; "ruth"; "morning"; "ruth"; "ruth"]  
                           (Book.unbind book_4pages));
  
  "book 4pages at the beginning, current page" >:: 
  (fun _ -> assert_equal (true, Some "hello")  
                        (Book.currentPage book_4pages));

  "book 4pages forwarded, current page" >:: 
  (fun _ -> assert_equal (false, Some "morning") 
                           (Book.currentPage book_4pages_fd2));

  "student lambda 4, read book_4pages" >::
  (fun _ -> assert_equal true (Student_lambda_2.read book_4pages));

  "student lambda 4, read book_4pages_fd2" >::
    (fun _ -> assert_equal true (Student_lambda_2.read book_4pages_fd2));

  "student lambda 4, read book_3pages" >::
    (fun _ -> assert_equal false (Student_lambda_2.read book_3pages));

  "student lambda 4, read book_3pages_fd" >::
    (fun _ -> assert_equal false (Student_lambda_2.read book_3pages_fd));

  "student lambda 4, read book_1page" >::
    (fun _ -> assert_equal false (Student_lambda_2.read book_1page));
                  
]


let _ = run_test_tt_main tests