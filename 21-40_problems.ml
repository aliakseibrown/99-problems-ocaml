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
(* let rand_select list n =
  let len = List.length list in
  let ran l = Random.int l in
  let rec sub index = function
    | [] -> "0"
    | a::b -> if index = 0 then a else sub (index - 1) b 
(* in sub (ran len) list;; *)
  in let rec create_list i acc = 
    if i = 0 then acc else create_list (i - 1) ((sub (ran (len - 1)) list)@acc)
  in 
  create_list n [];;

rand_select ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;; *)
