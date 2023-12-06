(* 1. Write a function last : 'a list -> 'a option that returns the last element of a list. (easy) *)
let rec last = function
  | [] -> None
  | [x] -> Some x
  | _ :: t -> last t;;

last [1; 2; 3]

(* 2. Find the last but one (last and penultimate) elements of a list. (easy) *)
let rec last = function
  | [] -> None
  | [a] -> None
  | [a ; b] -> Some (a, b)
  | _ :: t -> last t;;

last [1; 2; 3; 4];;
last [1];;
last [];;

(* 3. Find the K'th element of a list. (easy) *)
let rec find_k k l = 
  match k, l with
  | _, [] -> None
  | 0, h::_ -> Some h
  | k, _::t -> find_k (k-1) t;;

find_k 3 ["a"; "b"; "c"; "d"; "e"];;
(* 
let rec at k = function
  | [] -> None
  | h :: t -> if k = 1 then Some h else at (k - 1) t;; *)



(* 4. Find the number of elements of a list. (easy) *)
let rec length list =
  match list with
  | [] -> 0
  | _::t -> 1 + length t;;

length [1;2;3;4;5];;

(* 5. Reverse a list. (easy) *)
let rec rev_2 list =
  match list with
  | [] -> []
  | a::k -> a :: rev_2 k;;
  
let rev list =
  let rec aux acc = function
  | [] -> acc
  | h :: t -> aux ( h :: acc) t in
  aux [] list;;

rev ["a"; "b"; "c"];;


(* 6. Find out whether a list is a palindrome. (easy) *)
let is_palindrome list =
  let rec rev list =
    match list with
    | [] -> []
    | a::k -> rev k @ [a] in
  list = rev list ;;

is_palindrome ["x"; "a"; "m"; "a"; "x"];;

(* 7. Flatten a nested list structure. (medium) *)
type 'a node =
    | One of 'a 
    | Many of 'a node list;;

let flatten list =
  let rec sub_flatten acc = function
    | [] -> acc
    | One x :: t -> sub_flatten (x :: acc) t
    | Many l :: t -> sub_flatten (sub_flatten acc l) t in
  List. rev (sub_flatten [] list);;

flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]];;


(* 8. Eliminate consecutive duplicates of list elements. (medium) *)
let compress list = 
  let rec sub acc = function
  | [] -> List.rev acc
  | x :: [] -> List.rev (x::acc)
  | h :: h2 :: t -> if h = h2 then sub acc (h2::t) else sub (h :: acc) ( h2 :: t ) in
  sub [] list
;;
let rec compress = function
  | a:: (b:: _ as t) -> if a = b then compress t else a :: compress t
  | smaller -> smaller;;

compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;

(* 9. Pack consecutive duplicates of list elements into sublists. (medium) *)
let pack list =
  let rec aux current acc = function
    | [] -> []
    | [x] -> (x :: current) :: acc
    | a :: (b :: _ as t) -> if a = b then aux (a :: current) acc t
                            else aux [] ((a :: current) :: acc) t  in
  List.rev (aux [] [] list);;

pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"];;



(* 10. Run-length encoding of a list. (easy) *)
let encode list = 
  let rec sub num letter acc = function
    | [] -> (num, letter)::acc
    | a :: t -> if a = letter then sub (1 + num) a acc t
                                        else sub 1 a ((num, letter)::acc) t in
    List.rev (sub 0 (List.hd list) [] list)
;;

let encode list =
  let rec aux count acc = function
    | [] -> []
    | [x] -> (count + 1, x) :: acc
    | a :: (b :: _ as t) -> if a = b then aux (count + 1) acc t
                            else aux 0 ((count + 1, a) :: acc) t in
  List.rev (aux 0 [] list);;
  
encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"; "f"];;

(* 11. Modified run-length encoding. (easy) *)
type 'a rle =
  | One of 'a
  | Many of int * 'a;;

let encode list =
  let rec aux count acc = function
    | [] -> []
    | [x] -> if count = 1 then (One x :: acc)
                        else (Many ((count + 1), x) :: acc)
    | a :: (b :: _ as t) -> if a = b then aux (count + 1) acc t
                            else if count = 0 then aux 0 (One a :: acc) t
                                              else aux 0 ((Many ((count + 1), a)) :: acc) t in
  List.rev (aux 0 [] list);;
  
  let encode list = 
    let rec sub num letter acc = function
      | [] -> if num = 1 then (One letter)::acc
                         else (Many (num, letter)::acc)
      | a :: t -> if a = letter then sub (1 + num) a acc t
              else if num = 1 then sub 1 a ((One letter)::acc) t 
              else sub 1 a (Many (num, letter)::acc) t 
              in
      List.rev (sub 0 (List.hd list) [] list)
  ;;

encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;


(* 12. Decode a run-length encoded list. (medium) *)
let decode list =
  let rec sum k l acc =
  if k = 0 then acc else sum (k - 1) l (l::acc)
  in
  let rec sub acc = function
  | [] -> acc 
  | Many (k, l) :: t  -> sub (sum k l acc) t
  | One l :: t -> sub (l::acc) t
in List.rev (sub [] list);;

decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")];;

(* 13. Run-length encoding of a list (direct solution). (medium) *)
let encode list =
  let rle count x = if count = 0 then One x else Many (count + 1, x) in
  let rec aux count acc = function
    | [] -> [] (* Can only be reached if original list is empty *)
    | [x] -> rle count x :: acc
    | a :: (b :: _ as t) -> if a = b then aux (count + 1) acc t
                            else aux 0 (rle count a :: acc) t
  in
    List.rev (aux 0 [] list);;
encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;

(* 14. Duplicate the elements of a list. (easy) *)
let duplicate list = 
  let rec sub acc = function
    |[] -> acc
    | a::b -> sub (a::a::acc) b in
    List.rev (sub [] list);;

duplicate ["a"; "b"; "c"; "c"; "d"];;

(* 15. Replicate the elements of a list a given number of times. (medium) *)
let replicate list num =
  let rec count l acc num =
    if num = 0 then acc else count l (l:: acc) (num - 1) in
  let rec sub acc = function
    | [] -> acc
    | a::b -> sub (count a acc num) b in
  List.rev (sub [] list);;

replicate ["a"; "b"; "c"] 3;;

(* 16. Drop every N'th element from a list. (medium) *)
let drop list num =
  let rec sub acc i = function
    | [] -> acc
    | a::b -> if i == num then sub acc 1 b else sub (a::acc) (i + 1) b in
    List.rev ( sub [] 1 list);;

let drop list num =
    let rec sub i = function
    | [] -> []
    | a::b -> if i = num then sub 1 b else a :: sub (i+1) b in
    sub 1 list;;

drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;;

(* 17. Split a list into two parts; the length of the first part is given. (easy) *)
let split list num =
  let rec sub acc num = function
    | [] -> List.rev acc, []
    | a :: b -> if num = 0 then List.rev acc, (a::b)
                           else sub (a::acc) (num - 1) b
in sub [] num list;;

split ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;;

(* 18. Extract a slice from a list. (medium) *)

let slice list s e =
  let rec  move s = function
  | [] -> []
  | a::b -> if s = 0 then a::b else move (s-1) b in
  let rec sub acc e = function
  | [] -> List.rev acc
  | a :: b -> if e = 0 then List.rev acc else sub (a::acc) (e-1) b
in sub [] (e - s + 1) (move s list);;

slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6;;

(* 19. Rotate a list N places to the left. (medium) *)
let rotate list n =
  let rec len =  function
    | [] -> 0
    | a::b -> 1 + len b in
  let rec sub acc n l = function
    | [] -> List.rev acc
    | a::b -> if n = 0 then (a::b)@(List.rev acc)
              else if n > 0 then sub (a::acc) (n - 1) l b 
              else sub (a::acc) (n + l - 1) l b 
  in sub [] n (len list) list;;

rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;
rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2);;

(* 20. Remove the K'th element from a list. (easy) *)
let remove_at n list = 
  let rec sub acc n = function
    | [] -> acc
    | a::b -> if n == 0 then sub acc (n - 1) b else sub (a::acc) (n-1) b in
    List.rev (sub [] n list)
;;

let rec remove_at n = function
  | [] -> []
  | a::b -> if n = 0 then b else a :: remove_at (n - 1) b;;

remove_at 1 ["a"; "b"; "c"; "d"];;
