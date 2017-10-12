// By Rolf Kinder Gilet 

// binary search tree
type 'a tree = Lf | Br of 'a * 'a tree * 'a tree;;

//search an element
let rec element n = function
| Lf -> false
| Br(m, t1, t2) -> if n = m then true
                   elif n < m then element n t1
                   else element n t2;;

let rec insert n = function
| Lf -> Br(n, Lf, Lf)
| Br(m,t1,t2)-> if n<m then Br(m,insert n t1,t2)
                else Br(m, t1, insert n t2);;

let rec buildtree = function
|[] ->Lf
| x::xs -> insert x (buildtree xs);;


let rec sum = function
| Lf -> 0
| Br(m, t1, t2) -> m + sum t1 + sum t2;;

let t = buildtree [3;1;4];;

element 3 t;;

element 6 t;;

sum t;;
