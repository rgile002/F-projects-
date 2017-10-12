// By Rolf Kinder Gilet 

// evaluate arithmetic expressions written 
// in the language given by the following context-free grammar:
// E -> n | -E | E + E | E - E | E * E | E / E | (E)

type Exp = Num of int| Prod of Exp * Exp | Quot of Exp * Exp | Neg of Exp | Sum of Exp * Exp| Diff of Exp * Exp;;

type 'a option = None | Some of 'a;;

let rec evaluate = function
    |Num n -> Some n
    |Neg e -> match evaluate e with
              | Some n -> Some (-n)
              | None -> None
    |Sum (e1,e2) -> match (evaluate e1, evaluate e2) with
                    | Some a, Some b -> Some (a+b)
                    | _ -> None
    |Diff (e1,e2) -> match (evaluate e1, evaluate e2) with
                     | Some a, Some b -> Some (a-b)
                     | _ -> None
    |Prod (e1,e2) -> match (evaluate e1, evaluate e2) with
                     | Some a, Some b -> Some (a*b) 
                     | _ -> None
    |Quot (e1,e2) -> match (evaluate e1, evaluate e2) with
                     | Some a, Some 0 -> None
                     | Some a, Some b -> Some (a/b);;


evaluate (Prod(Num 3, Diff(Num 5, Num 1)));;

evaluate (Diff(Num 3, Quot(Num 5, Prod(Num 7, Num 0))));;
