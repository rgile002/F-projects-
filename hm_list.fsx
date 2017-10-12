// By Rolf Kinder Gilet 

// homogenous lists
type mix = Int of int | Str of string | Boo of bool;;

[Int 5; Boo true; Str"abc"; Int 10];;

let rec add = function 
| [] -> 0
| Int n :: xs -> n + add xs 
| _ :: xs -> add xs;;

add [Int 5; Boo true; Str"abc"; Int 10];;
