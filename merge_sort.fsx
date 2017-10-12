// By Rolf Kinder Gilet 

// Divide the unsorted list into two sublists
// Sort the two sublists;
// Merge two sorted sublists to produce a new sorted list


//split a given list
let rec split = function
    | [] -> [],[]
    | [x]-> [x],[]
    | x1::x2::xs -> let xs1, xs2 = split xs
                    x1::xs1, x2::xs2 ;;

//merge two given lists
//do the actual sorting
let merge xss yss =
  let rec mergelist f xss yss = 
    match xss, yss with
    | xss, [] -> f xss
    | [], yss -> f yss
    | x1::xs, y1::ys -> if x1<y1 then mergelist (fun a -> f(x1::a)) xs yss 
                                 else mergelist (fun a -> f(y1::a)) xss ys
  mergelist (fun x -> x) xss yss;;

//uses the split function to obtain two unsorted lists from one unsorted list
//recursively calls itself on the two unsorted sublists
//then uses merge to get the final sorted list
let mergesort xs = 
  let rec mergesortlist xs f =
    match xs with
    | [] -> f([])
    | [x] -> f([x])
    | y -> let list1, list2 = split y
           mergesortlist list1 (fun a ->
           mergesortlist list2 (fun b -> f(merge a b)))
  mergesortlist xs (fun x -> x);;

mergesort [1;8;0;2;10;72;3;42;5;26];;
