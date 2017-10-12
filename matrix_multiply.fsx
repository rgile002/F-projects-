// By Rolf Kinder Gilet 
// do matrix multiplication

//Assuming the dimensions of two given matrices are appropriate

let rec transpose = function
| [] -> []
| []::xs -> [] 
| xs -> List.map List.head xs :: transpose (List.map List.tail xs);;

let rec innerproduct xs ys=
    match xs,ys with
    |[],[] -> 0
    |[],ys -> 0
    |xs,[] -> 0
    |x::xs, y::ys -> (x*y) + innerproduct xs ys;;

let multiply (xs, ys) = 
    let rec compute xs ys =
        match xs with
        | [] -> []
        | x::xs -> List.map (fun f -> innerproduct f x) ys :: compute xs ys in compute xs (transpose ys);;
                                    
multiply ([[1;2;3];[4;5;6]], [[0;1];[3;2];[1;2]]);;
