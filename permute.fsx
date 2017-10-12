// By Rolf Kinder Gilet 
// generate the permutation of the elements of a list

let rec insert x ys =
    match ys with
    | [] -> [[x]]
    | y::ys -> (x::y::ys):: List.map (fun n -> y::n)(insert x ys);;

let rec map f xs =
    match xs with 
    | [] -> []
    | x::xs -> f x @ map f xs;;


let rec permute xs = 
    match xs with
    | [] -> [[]]
    | x::xs -> map (insert x) (permute xs);;

permute [1;2;3];;
