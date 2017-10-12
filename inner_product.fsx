// By Rolf Kinder Gilet 
// takes two vectors represented as int lists and returns their inner product
//Assuming the two lists are of equal length

let rec innerproduct xs ys=
    match xs,ys with
    |[],[] -> 0
    |[],ys -> 0
    |xs,[] -> 0
    |x::xs, y::ys -> (x*y) + innerproduct xs ys;;

innerproduct [1;2;3][4;5;6];;
