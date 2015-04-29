open List

exception Error of string;;

let error s = raise @@ Error s;;

(** Parse Input **)
let h, w =
  let str = read_line() in
  let s = Str.split_delim (Str.regexp " ") str in
  if length s == 2
  then (int_of_string @@ nth s 0), (int_of_string @@ nth s 1)
  else error "Error in description of height or width."
;;

let itos = string_of_int;;
let rec range a b = if a >= b then [] else a :: range (a + 1) b;;

let board =
  let func i =
    let s = read_line() in
    if String.length s == w
    then map (fun j -> i, j, String.get s j) (range 0 w)
    else error "The size of board is wrong." in
  concat @@ map func (range 0 w)
;;

(** Translate to CSP description **)
let variable (i, j, _) = "x_" ^ itos i ^ "_" ^ itos j

let basic_rule b =
  let f (_, _, x) = if x == '.' then "0 1" else "0" in
  let func t = "(int " ^ variable t ^ " " ^ f t ^ ")" in
  map func b
;;

let bomb_rule b =
  let is_near (i, j, _) (ii, jj, _) =
    max (abs @@ i - ii) (abs @@ j - jj) == 1 in
  let near t = filter (fun x -> is_near t x) b in
  let f t = String.concat "" (map (fun x -> " " ^ variable x) (near t)) in
  let func (i, j, x) =
    if x == '.' then ""
    else"(= (+" ^ f (i, j, x) ^ ") " ^ Char.escaped x ^ ")" in
  filter (fun x -> not (x = "")) @@ map func b
;;

let print_board b = iter (fun (i, j, x) -> print_string @@
  itos i ^ " " ^ itos j ^ " " ^ Char.escaped x ^ "\n") b
;;

(** SAT Solving **)
let print_rules rules =
  let ch = open_out "tmp" in
  output_string ch "; Bomber Puzzle\n";
  iter (fun x -> output_string ch @@ x ^ "\n") rules;
  output_string ch "; END\n";
  flush ch;
  close_out ch
;;

print_rules @@ append (basic_rule board) (bomb_rule board);;
Sys.command "sugar -c tmp > out";;

(** Translate the result of SAT solver **)

let answer =
  let ch = open_in "out" in
  let result =
    let str = input_line ch in
    let s = Str.split_delim (Str.regexp " ") str in
    nth s 1 in
  if result = "SATISFIABLE" then
    let str = input_line ch in
    let s = Str.split_delim (Str.regexp " ") str in
    ( close_in ch;
      map2 (fun (i, j, x) y -> if y = "1" then (i, j, '@') else (i, j, x)) board (tl s) )
  else
    ( close_in ch;
      error "NOT SATISFIABLE" )
;;
    
let output_answer ans =
  let f p (i, j, x) =
    print_string @@ Char.escaped x;
    if (p mod w == w - 1) then print_string "\n" in
  iteri f ans
;;
  
output_answer answer;;
