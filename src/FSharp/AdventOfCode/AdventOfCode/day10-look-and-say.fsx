
// "Elves Look, Elves Say" - Day 10 - Advent of Code http://adventofcode.com/day/10 #AdventOfCode
let lookAndSay(input : string array) = 
    input
    |> Array.fold (fun acc el -> 
           match acc with
           | [||] -> [|(1, el)|]
           | _ -> 
               let count, item = acc.[acc.Length - 1]
               if el = item then 
                   acc.[acc.Length - 1] <- (count + 1, el)
                   acc
               else [|(1, el)|] |> Array.append acc) Array.empty
    |> Array.map ( fun (count, el) -> [| (count |> string) ;el|] )
    |> Array.concat

let rec iterate (numOfIterations : int) (count : int) (input : string array) = 
    if count = numOfIterations - 1 then (lookAndSay input)
    else lookAndSay input |> iterate (numOfIterations) (count + 1)

