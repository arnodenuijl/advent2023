module q21

open System
open System.Collections.Generic
open System.IO
open Microsoft.FSharp.Core
open utils

let testinput = """...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
..........."""

type Coordinate = int * int
let findIndex (c: Char) (grid : Char[,]) : Coordinate =
    seq {
        for x in [0..Array2D.length1 grid - 1] do
            for y in [0..Array2D.length2 grid - 1] do
                if grid[x,y] = c
                then
                    yield (x,y)
    } |> Seq.head

let rec bounded (max: int) (x: int) : int =
    if x >= 0 then x % max
    else
        let timesMaxToGetOutOfNegativity = 1 + (Math.Abs(x) / max)
        (x + (timesMaxToGetOutOfNegativity * max)) % max
    
let getPossibleNextSteps (grid: Char[,]) (currentLocation: Coordinate): (int * int) Set =
    let maxLength1 = Array2D.length1 grid
    let maxLength2 = Array2D.length2 grid
    seq {
        yield ((fst currentLocation - 1), snd currentLocation) // up
        yield ((fst currentLocation + 1), snd currentLocation) // down
        yield (fst currentLocation, (snd currentLocation - 1)) // up
        yield (fst currentLocation, (snd currentLocation + 1)) // down
    }
    |> Seq.filter (fun (x,y) ->
        let testX = bounded maxLength1 x
        let testY = bounded maxLength2 y
        not <| (grid[testX,testY] = '#'))
    |> Set.ofSeq
    
let cache2 = Dictionary()

let rec allPointsAtStep (grid: Char[,]) (startPoint : Coordinate) (step:int) =
    match cache2.TryGetValue step with
    | true, x -> x
    | false, _ -> 
        match step with
        | -1 -> Set.empty
        | 0 -> Set.singleton startPoint
        | 1 -> getPossibleNextSteps grid startPoint
        | _ ->
            let previous = allPointsAtStep grid startPoint (step - 1)
            let twoBack = allPointsAtStep grid startPoint (step - 2)    
            let threeBack = allPointsAtStep grid startPoint (step - 3)
            let toCheck = Set.difference previous threeBack
            let possibleNew =
                toCheck
                |> Seq.map (fun p -> getPossibleNextSteps grid p)
                |> Set.unionMany
            let allForThisStep = Set.union twoBack possibleNew
            cache2.Add(step, allForThisStep)
            allForThisStep
                
let q21a () =
    let input = File.ReadAllText "21.txt"
    let grid = Array2D.createGrid input
    let s = findIndex 'S' grid
    let all = allPointsAtStep grid s 64
    Console.WriteLine $"21a: {all.Count} "

let q21b () =
    let input = File.ReadAllText "21.txt"
    let grid = Array2D.createGrid input
    let s = findIndex 'S' grid

    [0..1000]
    |> Seq.map (fun i -> (i, allPointsAtStep grid s i))
    |> Seq.filter (fun (i, xs) -> (((131 - 65) + i) % 131) = 0) // the pattern repeats every 131. Start at 65 to get to exactly 26501365
    |> Seq.iter (fun (i, xs) -> Console.WriteLine $"{i} {xs.Count}")
    
    // print numbers and find the repeats. Ends up being a Triangular increase * 29742
    // x = 3776 + (i * 29876) + (( (i-1)^2 + (i-1) ) / 2) * 29742    
    Console.WriteLine $"21b: 608603023105276 (calculated with excel)"