// By Rolf Kinder Gilet 
// splits list x into two lists y and z 

let rec split = function
    | [] -> [],[]
    | [x]-> [x],[]
    | x1::x2::xs -> let xs1, xs2 = split xs
                    x1::xs1, x2::xs2 ;;
