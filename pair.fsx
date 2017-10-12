// By Rolf Kinder Gilet 
// Pairs the elements from two lists into a new list:

// version 1
let rec pair = function
    | ([], []) -> []
    | ([], ys) -> []
    | (xs, []) -> []
    | (x::xs , y::ys) -> (x, y):: pair(xs, ys);;

// version 2
let rec pair2 xs ys =
    match xs, ys with
    | [],[] -> []
    | [], ys -> []
    | xs, [] -> []
    | x::xs , y::ys -> (x, y):: (pair2 xs ys);;

pair (['a';'b';'c'], [1;2;3]);;
pair2 ['a';'b';'c'] [1;2;3];;
