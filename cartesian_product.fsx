// By Rolf Kinder Gilet 
// takes as input two lists xs and ys and returns 
// a list of pairs that represents the Cartesian product of xs and ys.

// version 1
let rec cartesian xs ys = 
  match xs, ys with
  | xs, [] -> []
  | [], ys -> []
  | x::xs, ys -> (List.map (fun y -> x, y) ys) @ (cartesian xs ys);;

// version 2
let rec cartesian2  = function
    | ([], []) -> []
    | ([], ys) -> []
    | (xs, []) -> []
    | (x::xs, ys) -> (List.map (fun y -> x, y) ys) @ (cartesian2 (xs, ys));;

cartesian ['a';'b';'c'] [0;1;2;3];;
cartesian2 (['a';'b';'c'], [0;1;2;3]);;
