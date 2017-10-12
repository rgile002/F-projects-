// By Rolf Kinder Gilet 

// returns the set of all subsets of set

let rec powerset = function
   | [] -> [[]]
   | (x::xs) -> let xss = powerset xs 
                List.map (fun xs' -> x::xs') xss @ xss;;

powerset [1;2;3];;
