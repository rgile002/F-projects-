// compute the transpose of an m-by-n matrix

let rec transpose = function
| [] -> []
| []::xs -> [] 
| xs -> List.map List.head xs :: transpose (List.map List.tail xs);;

transpose[[1;4];[2;5];[3;6]];;

transpose [[1; 2; 3]; [4; 5; 6]];;
