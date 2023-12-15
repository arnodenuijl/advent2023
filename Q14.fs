module q14

open System
open System.IO
open utils

let testInput = """O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#...."""

let input = File.ReadAllText "14.txt"
let inputGrid = Array2D.createGrid testInput

let turnCounterClockwise (grid : 'a list list) : 'a list list =
    let transposed = Array2D.create (grid.Length) (grid[0].Length) Unchecked.defaultof<'a>
    for x in [0.. grid.Length - 1] do
        for y in [0..grid[0].Length - 1] do
            transposed[grid.Length - 1 - y,x] <- grid[x][y]
    transposed
    |> Array2D.rowsAsList
    
let turnClockwise (grid : 'a list list) : 'a list list =
    let transposed = Array2D.create (grid.Length) (grid[0].Length) Unchecked.defaultof<'a>
    for x in [0.. grid.Length - 1] do
        for y in [0..grid[0].Length - 1] do
            transposed[y,grid.Length - 1 - x] <- grid[x][y]
    transposed
    |> Array2D.rowsAsList
    
let processWest(cs : char list list) : char list list =
    let moveZeroes (cs: char list) : char list =
        cs
        |> List.fold
            (fun (zeroes, dots, reconstructed) c ->
                    match c with
                    | '#' -> (0, 0, reconstructed @ List.replicate zeroes 'O' @ (List.replicate dots '.') @ ['#'])
                    | '.' -> (zeroes, dots + 1, reconstructed )
                    | 'O' -> (zeroes + 1, dots, reconstructed)
                    | _ -> failwith $"{c} is not valid"
                    )
            (0,0,[])
        |> fun (zeroes, dots, reconstructed) -> reconstructed @ List.replicate zeroes 'O' @ List.replicate dots '.'
    cs
    |> List.map moveZeroes

let processNorth (cs : char list list) : char list list =
    cs
    |> turnCounterClockwise
    |> processWest
    |> turnClockwise
    
let processEast (cs : char list list) : char list list =
    cs
    |> turnCounterClockwise
    |> turnCounterClockwise
    |> processWest
    |> turnClockwise
    |> turnClockwise

let processSouth (cs : char list list) : char list list =
    cs
    |> turnClockwise
    |> processWest
    |> turnCounterClockwise

let cycle (cs : char list list) : char list list =
    cs
    |> processNorth 
    |> processWest 
    |> processSouth 
    |> processEast 
    
let score  (cs: char list list) : int =
    cs
    |> turnCounterClockwise
    |> List.map (fun column -> 
        column
        |> List.rev
        |> List.mapi (fun i c -> (i + 1,c))
        |> List.filter (fun (i,c) -> c = 'O')
        |> List.map fst
        |> List.sum
    )
    |> List.sum

let startPosition =
    input.Split(Environment.NewLine)
    |> Array.map (fun s -> s.ToCharArray() |> List.ofArray)
    |> List.ofArray    
    
let q14a () =
    let resultMap =
        startPosition
        |>  processNorth
    
    let result =
        resultMap
        |> score
        
    Console.WriteLine $"14a: {result}"
    
let q14b () =
    Console.WriteLine "printed a lot of stuff, saw the pattern, calculated some stuff in excel, and got the answer"
    let (i, _, score) =
        [1..150]
        |> List.fold (fun (_, state, previousScore) i ->
                let next = cycle state
                let score = score next
                (i, next, score))
            (0, startPosition, 0)

    
    Console.WriteLine $"14b: {score}"

   