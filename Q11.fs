module q11

open System
open System.IO
open utils

let inputString = File.ReadAllText "11.txt"

let getExpansionRows (grid: Char[,]) =
    seq {
        for i in [0..Array2D.length1 grid - 1] do
            let row = grid.[i,*]
            if Array.forall (fun c -> c = '.') row then
                yield i         
    } |> List.ofSeq
    
let getExpansionColumns (grid: Char[,]) =
    seq {
        for i in [0..Array2D.length2 grid - 1] do
            let row = grid.[*,i]
            if Array.forall (fun c -> c = '.') row then
                yield i         
    }|> List.ofSeq
    
let getHashLocations (grid: Char[,]) =
    seq {
        for x in [0..Array2D.length1 grid - 1] do
            for y in [0..Array2D.length2 grid - 1] do
                if grid[x,y] = '#' then
                    yield (x,y) } |> Seq.toList

let distance ((x1,y1) : int * int ,(x2,y2) : int * int) =
    Math.Abs(x1 - x2) + Math.Abs(y2 - y1)

let calculateExpansionsCrossed expansions (i1: int) (i2: int) =
    expansions
    |> List.filter (fun r -> r > Math.Min(i1, i2) && r < Math.Max(i1, i2))
    |> List.length
    
let grid = Array2D.createGrid inputString
let locations = getHashLocations grid
let expansionRows = getExpansionRows grid
let expansionColumns = getExpansionColumns grid
let calculateDistance multiplier ((x1: int, y1: int), (x2: int, y2: int)) =
        let dX = int64 <| Math.Abs(x1 - x2) 
        let dY = int64 <| Math.Abs(y1 - y2)
        let crossedExpansionRows = int64 <| calculateExpansionsCrossed expansionRows x1 x2
        let crossedExpansionColumns = int64 <| calculateExpansionsCrossed expansionColumns y1 y2
        dX + dY + (crossedExpansionColumns * multiplier) + (crossedExpansionRows * multiplier) - crossedExpansionColumns - crossedExpansionRows

let allPairs =  seq {
        for i in [0..locations.Length - 2] do
            for j in [i..locations.Length - 1] do
                (locations[i], locations[j])    }
                    |> List.ofSeq
let q11a () =
    let result =
        allPairs
        |> List.map (calculateDistance 2) 
        |> List.sum
        
    Console.WriteLine $"11a: {result}"
    
let q11b () =
    let multiplier = 1_000_000L

    let result =
        allPairs
        |> List.map (calculateDistance multiplier) 
        |> List.sum
    
    Console.WriteLine $"11b: {result}"
    
    