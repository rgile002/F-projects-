// By Rolf Kinder Gilet 

// calculate the gcd of x
let rec gcd  = function
    | (x, 0) -> x
    | (x, y) -> gcd (y, x%y);;

gcd (10, 8);;
