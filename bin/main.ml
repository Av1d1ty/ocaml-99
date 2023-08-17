let rec last = function
  | [] -> None
  | [ x ] -> Some x
  | _ :: tl -> last tl
;;

let rec last_two = function
  | [] | [ _ ] -> None
  | [ x; y ] -> Some (x, y)
  | _ :: tl -> last_two tl
;;

let rec list_nth lst n =
  match lst, n with
  | [], _ -> raise (Failure "nth")
  | hd :: _, 0 -> hd
  | _ :: tl, i -> list_nth tl (i - 1)
;;

let rec at k = function
  (* Provided solution to the previous one *)
  | [] -> None
  | h :: t -> if k = 0 then Some h else at (k - 1) t
;;

let rec list_length = function
  | [] -> 0
  | _ :: tl -> 1 + list_length tl
;;

let list_rev lst =
  let rec sub out = function
    | [] -> out
    | hd :: tl -> sub (hd :: out) tl
  in
  sub [] lst
;;

let is_palindrome lst = lst = list_rev lst

type 'a node =
  | One of 'a
  | Many of 'a node list

let flatten lst =
  let rec aux out = function
    | [] -> list_rev out
    | One x :: tl -> aux (x :: out) tl
    | Many nodes :: tl -> aux out (nodes @ tl)
  in
  aux [] lst
;;

let rec compress = function
  | hd :: (hd' :: _ as tl) ->
    if hd = hd' then compress tl else hd :: compress tl
  | smaller -> smaller
;;

let pack lst =
  let rec aux out lst =
    match lst, out with
    | [], _ -> out
    | hd :: tl, [] -> aux ((hd :: []) :: []) tl
    | hd :: tl, hd' :: tl' ->
      if hd = List.hd hd'
      then aux ((hd :: hd') :: tl') tl
      else aux ((hd :: []) :: out) tl
  in
  aux [] lst |> list_rev
;;
