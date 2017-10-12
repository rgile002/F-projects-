// By Rolf Kinder Gilet 

// stream  example 

type 'a stream = Cons of 'a * (unit -> 'a stream);;

let rec upfrom n = Cons(n, fun () -> upfrom(n+1));; 
// natural number
let nats = upfrom 0;;

let rec take n (Cons(x, xsf)) =
      if n = 0 then []
               else x :: take (n-1) (xsf());;

take 10 nats;;


let rec drop n (Cons (x, xsf)) =
      if n = 0 then Cons (x, xsf)
               else drop (n-1) (xsf());;


let rec filter p (Cons(x, xsf)) =
      if p x then Cons(x, fun () -> filter p (xsf()))
             else filter p (xsf());;

let rec eratosthenes (Cons(x, xsf)) =
      Cons(x, fun () -> eratosthenes (filter (fun n -> n%x <> 0) (xsf())));;

let primes = eratosthenes (upfrom 2);;

take 10 primes;;


[15]::[];;
