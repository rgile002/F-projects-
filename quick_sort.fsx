// By Rolf Kinder Gilet 
//quick sort a given list 

let rec split pivot = function
| [] -> ([],[])
| x::xs -> let (left,right) = split pivot xs
           if x <= pivot then (x::left, right) else (left, x::right);;


let rec qsort = function 
| [] ->[]
| [x] -> [x]
| x::xs -> let (left, right) = split x xs // x is the pivot
           qsort left @ x :: qsort right;;

qsort [3;1;4;7;5;9;2;6;5];;
