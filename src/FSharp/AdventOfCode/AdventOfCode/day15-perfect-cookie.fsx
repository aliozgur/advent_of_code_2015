(* 
    Sprinkles: capacity 5, durability -1, flavor 0, texture 0, calories 5
    PeanutButter: capacity -1, durability 3, flavor 0, texture 0, calories 1
    Frosting: capacity 0, durability -1, flavor 4, texture 0, calories 6
    Sugar: capacity -1, durability 0, flavor 0, texture 2, calories 8
*)
open System
let myIngredientsNoCalories = 
    [|[|5L; -1L; 0L; 0L|];
      [|-1L; 3L; 0L; 0L|];
      [|0L; -1L; 4L; 0L|];
      [|-1L; 0L; 0L; 2L|]|]

let myIngredients = 
    [|[|5L; -1L; 0L; 0L; 5L|];
      [|-1L; 3L; 0L; 0L; 1L|];
      [|0L; -1L; 4L; 0L; 6L|];
      [|-1L; 0L; 0L; 2L; 8L|]|]



(*
Frosting: capacity 4, durability -2, flavor 0, texture 0, calories 5
Candy: capacity 0, durability 5, flavor -1, texture 0, calories 8
Butterscotch: capacity -1, durability 0, flavor 5, texture 0, calories 6
Sugar: capacity 0, durability 0, flavor -2, texture 2, calories 1
First Step = 18965440 
Second Step = 15862900 
*)
(*
let myIngredientsNoCalories = 
    [|[|4L; -2L; 0L; 0L|];
      [|0L; 5L; -1L; 0L|];
      [|-1L; 0L; 5L; 0L|];
      [|0L; 0L; -2L; 2L|]|]

let myIngredients = 
    [|[|4L; -2L; 0L; 0L;5L|];
      [|0L; 5L; -1L; 0L;8L|];
      [|-1L; 0L; 5L; 0L;6L|];
      [|0L; 0L; -2L; 2L;1L|]|]
*)

let superSet = 
    [for i in 1L..100L -> i]

let rec combination n l = 
    match n, l with
    | 0, _ -> [[]]
    | _, [] -> []
    | k, (head :: tail) -> 
        List.map ((@) [head]) (combination (k - 1) tail) @ combination k tail

let rec insertions x = 
    function 
    | [] -> [[x]]
    | (y :: ys) as l -> 
        (x :: l) :: (List.map (fun x -> y :: x) (insertions x ys))

let rec permutations = 
    function 
    | [] -> seq [[]]
    | x :: xs -> Seq.concat(Seq.map (insertions x) (permutations xs))

let amountCombinations = 
    combination 4 superSet 
    |> List.filter(fun amounts -> (amounts |> List.sum) = 100L)

let allAmountCombinations = 
    amountCombinations
    |> List.map(fun amounts -> amounts |> permutations)
    |> List.map(fun m -> m |> List.ofSeq)

let amounts = 
    seq {
        for ll in allAmountCombinations do
            for lm in ll do
                yield lm
    }

let useCalories = true

let problemConfig = 
    match useCalories with
    | false -> (myIngredientsNoCalories, [|0L; 0L; 0L; 0L|])
    | _ -> (myIngredients, [|0L; 0L; 0L; 0L; 0L|])

let ing, foldSum = problemConfig

let allSums = 
  amounts
  |> Seq.map
         (fun amounts -> 
         amounts |> List.mapi(fun i v -> ing.[i] |> Array.map(fun m -> m * v)))
  |> Seq.map
         (fun g -> 
         g 
         |> List.fold (fun acc el -> acc |> Array.mapi(fun i e -> e + el.[i])) 
                foldSum)
allSums
  |> Seq.filter
         ( fun prods -> 
         ( not(useCalories) || prods.[4] = 500L) )
  |> Seq.map ( fun prods ->  
       prods |> Array.take 4 |> Array.reduce(fun acc el -> acc * ( max 0L el)) )
  |> Seq.max
