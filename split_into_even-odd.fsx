// By Rolf Kinder Gilet 
// split into even and odd

let rec split2 = function
    | [] -> ([],[])
    | x::xs -> let (xs1, xs2) = split2 xs
               if (x%2 = 0)then (x::xs1, xs2)
                           else (xs1, x::xs2) ;;

split2 [1;8;2;3;4;5;6;7];;
