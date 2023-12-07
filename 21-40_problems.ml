(* 21. Insert an element at a given position into a list. (easy) *)
let rec insert_at word n = function
  | [] -> [word]
  | a::b -> if n = 0 then (word::a::b) else a :: insert_at word (n-1) b;;

insert_at "alfa" 1 ["a"; "b"; "c"; "d"];;
insert_at "alfa" 3 ["a"; "b"; "c"; "d"];;
insert_at "alfa" 4 ["a"; "b"; "c"; "d"];;

(* 22. Create a list containing all integers within a given range. (easy) *)
let range a b =
  let rec sub a b acc =
    if a = b then a::acc 
    else if a < b then a :: sub (a + 1) b acc
    else b :: sub a (b + 1) acc
in sub a b [];;

range 4 9;;
range 9 4;;

(* 23. Extract a given number of randomly selected elements from a list. (medium) *)
let rand_select list n =
  let len = List.length list in
  let rec sub index = function
    | [] -> raise (Failure "Empty list")
    | a::b -> if index = 0 then a else sub (index - 1) b 
  in let rec create_list i acc = 
    if i = 0 then acc 
    else create_list (i - 1) ((sub (Random.int len) list)::acc)
  in 
  create_list n [];;

rand_select ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;; 

let result = rand_select ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;

(* 24. Lotto: Draw N different random numbers from the set 1..M. (easy) *)
let rec lotto_select n m =
  if n = 0 then [] else (Random.int m) :: lotto_select (n-1) m
;;

lotto_select 6 49;;

(* 25. Generate a random permutation of the elements of a list. (easy) *)
(* let permutation list =
  let len = List.length list in
  let rec sub acc k = function
    | [] -> raise (Failure "Empty list")
    | a :: b -> if k = 0 then (a, acc@b) else sub (a::acc) (k-1) b in
  let rec perm len acc a b =
    if len = 0 then acc else perm (len - 1) (acc@sub [] (Random.int len) list)
  ;;
    (* sub [] (Random.int len) list *)

permutation ["a"; "b"; "c"; "d"; "e"; "f"];; *)
