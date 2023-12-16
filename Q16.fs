module q16

open System
open System.IO
open System.Linq
open FSharp.Collections.ParallelSeq
open Microsoft.FSharp.Core
open utils

type Direction = | Up | Down | Left | Right
type Coordinate = int * int

let move (grid : char[,]) ((row,column): Coordinate) (direction : Direction): (Coordinate * Direction) option =
    match direction with
    | Left ->
        if column = 0
        then None
        else Some (row, column - 1)
    | Right ->
        if column = Array2D.length2 grid - 1 
        then None
        else Some (row, column + 1)
    | Up ->            
        if row = 0 
        then None
        else Some (row - 1, column) 
    | Down -> 
        if row = Array2D.length1 grid - 1
        then None
        else Some  (row + 1, column) 
    |> Option.map (fun nextPos -> nextPos, direction)

let getNextPossitions (grid: char[,]) (direction : Direction) (currentPos : Coordinate) (currentChar: char) : (Coordinate * Direction) list=
    match direction, currentChar with
    | Up   , '-' 
    | Down , '-'  ->  [ move grid currentPos Left
                        move grid currentPos Right
                      ] |> List.collect Option.toList
    | Left  , '|' 
    | Right , '|' -> [ move grid currentPos Up
                       move grid currentPos Down
                     ] |> List.collect Option.toList
    | Left,  '/'  -> move grid currentPos Down |> Option.toList
    | Right, '/'  -> move grid currentPos Up |> Option.toList
    | Up,    '/'  -> move grid currentPos Right |> Option.toList
    | Down,  '/'  -> move grid currentPos Left |> Option.toList
    | Left,  '\\' -> move grid currentPos Up |> Option.toList
    | Right, '\\' -> move grid currentPos Down |> Option.toList
    | Up,    '\\' -> move grid currentPos Left |> Option.toList
    | Down,  '\\' -> move grid currentPos Right |> Option.toList
    | Left, _     -> move grid currentPos Left |> Option.toList
    | Right, _    -> move grid currentPos Right |> Option.toList
    | Up, _       -> move grid currentPos Up |> Option.toList
    | Down, _     -> move grid currentPos Down |> Option.toList

type TracePoint = Coordinate * char * Direction
type Trace = TracePoint list

[<TailCall>]
let rec followPath (visited : Set<TracePoint>) (grid : char [,]) (runningTraces: (Coordinate * char * Direction) list list) (finishedTraces: (Coordinate * char * Direction) list list) =
    match runningTraces with
    | [] -> finishedTraces
    | currentTrace :: restRunningTraces ->                
        let currentPos, currentChar, currentDirection = List.head currentTrace
        let nextPositions = getNextPossitions grid currentDirection currentPos currentChar
       
        let newPlacesToCheck =
            nextPositions
            |> List.collect (fun (nextPos, nextDirection) ->
                    let nextChar = grid[fst nextPos, snd nextPos]
                    if List.contains (nextPos, nextChar, nextDirection) currentTrace || (Set.contains (nextPos, nextChar, nextDirection) visited)
                    then []
                    else [(nextPos, nextChar, nextDirection)]
                )
    
        let updatedVisited = visited.Add(currentPos, currentChar, currentDirection)     
        match newPlacesToCheck with
        | [] -> followPath updatedVisited grid restRunningTraces (currentTrace :: finishedTraces)
        | newPlaces ->
            let newRunningTraces: (Coordinate * char * Direction) list list =
                newPlaces
                |> List.map (fun p -> p :: currentTrace)
            followPath updatedVisited grid (newRunningTraces @ restRunningTraces) finishedTraces
 
let q16a () =
    let input = File.ReadAllText "16.txt"   
    let grid = Array2D.createGrid input
    let trace = followPath Set.empty grid [[((0,0), grid[0,0], Right)]] []
    let result =
        trace
        |> List.collect (fun xs -> xs |> List.map(fun (coord, _,_) -> coord))
        |> set
        |> Set.toList
        
    Console.WriteLine $"16a: {result.Length}"
    
let q16b () =
    let input = File.ReadAllText "16.txt"   
    let grid = Array2D.createGrid input 
    
    let topStartPoints = 
        [0..Array2D.length2 grid - 1]
        |> List.map (fun column ->
            let row = 0
            ((row,column), grid[row,column], Down))
    
    let bottomStartPoints =
        [0..Array2D.length2 grid - 1]
        |> List.map (fun column ->
            let row = Array2D.length1 grid - 1
            ((row,column), grid[row,column], Up))
    
    let leftStartPoints =
        [0..Array2D.length1 grid - 1]
        |> List.map (fun row ->
            let column = 0
            ((row,column), grid[row,column], Right))
        
    let rightStartpoints =
        [0..Array2D.length1 grid - 1]
        |> List.map (fun row ->
            let column = Array2D.length2 grid - 1
            ((row,column), grid[row,column], Left))
        
    let result =
        leftStartPoints @ rightStartpoints @ bottomStartPoints @ topStartPoints
        |> PSeq.withMergeOptions ParallelMergeOptions.NotBuffered
        |> PSeq.withExecutionMode ParallelExecutionMode.ForceParallelism
        |> PSeq.map (fun (startPos, startChar, direction) ->
            let tracesForAStartingPoint = followPath Set.empty grid [[(startPos, startChar, direction)]] []
            let x = tracesForAStartingPoint
                    |> List.collect (fun xs ->
                        xs
                        |> List.map(fun (coord, _,_) -> coord))
                    |> set
                    |> Set.count
            x)
        |> PSeq.max
    Console.WriteLine $"16b: {result}"
    
    
   