module q10

open System
open System.IO

type Coordinate = int * int
type Pipe = {
    Coordinate : Coordinate
    PipeType : Char
}
type Direction =
| Up | Right | Down | Left

let createGrid (input: String) =
    let lines = input.Split Environment.NewLine
    let linesLength = lines.Length
    let columnsLength = lines[0].Length
    let grid: char[,] = Array2D.create (linesLength + 2) (columnsLength + 2) '.'
    for line in [0..linesLength - 1] do
        for column in [0..columnsLength - 1] do
            Array2D.set grid (line + 1) (column + 1) (lines[line][column])
    grid

let findAll (grid: Char[,]) (c : Char) : Coordinate seq =
    seq {
        for i in [0..(Array2D.length1 grid - 1)] do
            for j in [0..(Array2D.length2 grid - 1)] do
                if grid[i,j] = c then
                    yield (i,j)
    } 

let find (grid: Char[,]) (c : Char) =
    findAll grid c
    |> Seq.head 


let inverseDirection = function
    | Left -> Right
    | Right -> Left
    | Up -> Down
    | Down -> Up
        
let possibleDirections (pipeType: Char) =
    match pipeType with
    | 'S' -> [Up;Down;Left;Right]
    | '|' -> [Up;Down]
    | '-' -> [Left;Right]
    | 'L' -> [Up;Right]
    | 'J' -> [Up;Left]
    | '7' -> [Left;Down]
    | 'F' -> [Right;Down]
    | _ -> []
let mapChar (pipeType: Char) =
    match pipeType with
    | 'L' -> '└'
    | 'J' -> '┘'
    | '7' -> '┐'
    | 'F' -> '┌'
    | _ -> pipeType

let canConnect (direction: Direction) (toPipe: Pipe) =
    possibleDirections toPipe.PipeType
    |> List.contains (inverseDirection direction)
    
let getAdjacentPipe (grid: Char[,]) ((currentX, currentY): Coordinate) (direction:Direction) : Pipe option =
    match direction with
    | Up when currentX = 0 -> None
    | Down when currentX = (Array2D.length1 grid - 1) -> None
    | Left when currentY = 0 -> None
    | Right when currentY = (Array2D.length2 grid - 1) -> None
    | Up -> Some (currentX - 1, currentY)
    | Down -> Some(currentX + 1, currentY) 
    | Left -> Some (currentX, currentY - 1)
    | Right -> Some(currentX, currentY + 1)
    |> Option.map (fun (x,y) -> {PipeType = grid[x,y]; Coordinate = (x,y)})
    
let getConnectingPipes (grid: Char[,]) (currentPipe: Pipe) =
    let possibleDirections = possibleDirections currentPipe.PipeType
    possibleDirections
    |> List.map (fun d -> d, getAdjacentPipe grid currentPipe.Coordinate d)
    |> List.collect (fun (d, pipeOption: Pipe option) ->
        match pipeOption with
        | Some c -> [(d, c)]
        | None -> [])
    |> List.filter (fun (direction, nextPipe) -> canConnect direction nextPipe )
    |> List.map snd
    
let rec createLoop (grid: Char[,]) (tail : Pipe list) : Pipe list = 
    let nextPipe =
        getConnectingPipes grid tail.Head
        |> List.filter (fun c -> c.PipeType = 'S' || not <| List.contains c tail)
        |> List.head
    if grid[fst nextPipe.Coordinate, snd nextPipe.Coordinate] = 'S' then
        tail
    else
        createLoop grid (nextPipe :: tail)
    
let expand (grid: Char[,]) =
    let (biggerdoubleGrid : Char[,]) = Array2D.create (Array2D.length1 grid * 2) (Array2D.length2 grid * 2) '.' 
    for x in [0..(Array2D.length1 grid - 1)] do
        for y in [0..(Array2D.length2 grid - 1)] do
            biggerdoubleGrid[x * 2, y * 2] <- grid[x,y]
            biggerdoubleGrid[x * 2, y * 2 + 1] <-
                match grid[x,y] with
                | 'S' -> 'S'
                | '|' -> '.'
                | '-' -> '-'
                | 'L' -> '-'
                | 'J' -> '.'
                | '7' -> '.'
                | 'F' -> '-'
                | '.' -> '.'
                | x -> failwith $"illegal char '{x.ToString()}'"
    
    for y in [0..(Array2D.length2 grid - 1)] do
        for x in [0..(Array2D.length1 grid - 1)] do
            biggerdoubleGrid[x * 2, y * 2] <- grid[x,y]
            biggerdoubleGrid[x * 2 + 1, y * 2] <-
                match grid[x,y] with
                | 'S' -> 'S'
                | '|' -> '|'
                | '-' -> '.'
                | 'L' -> '.'
                | 'J' -> '.'
                | '7' -> '|'
                | 'F' -> '|'
                | '.' -> '.'
                | x -> failwith $"illegal char {x.ToString()}"
    biggerdoubleGrid
let contractGrid (grid: Char[,]) =
    let contractedGrid = Array2D.create (Array2D.length1 grid / 2) (Array2D.length2 grid / 2) ' ' 
    for x in [0.. +2 .. (Array2D.length1 grid - 1)] do
        for y in [0.. +2 .. (Array2D.length2 grid - 1)] do
            contractedGrid[x/2,y/2] <- grid[x,y]
    contractedGrid        
let gridWithOnlyLoop (grid: Char[,]) (loop: Pipe list) : Char[,]=
    let cleanedGrid = Array2D.create (Array2D.length1 grid) (Array2D.length2 grid) '.'
    for x in [0..(Array2D.length1 grid) - 1] do
        for y in [0..(Array2D.length2 grid) - 1] do
            match loop |> List.tryFind (fun pipe -> pipe.Coordinate = (x,y)) with
            | Some pipe -> Array2D.set cleanedGrid x y pipe.PipeType
            | None -> ()
    cleanedGrid

let floodClean (grid: Char[,]) : Char[,]=
    let cleanedGrid = Array2D.copy grid
    let maxX = Array2D.length1 grid - 1
    let maxY = Array2D.length2 grid - 1
    
    let getAdjacent (x, y) = [
        Math.Max(0,x - 1), y
        Math.Min(maxX, x + 1), y
        x, Math.Max(0, y - 1)
        x, Math.Min(maxY, y + 1)] 
    
    let rec flood (visited : Set<Coordinate>) (toVisit : Set<Coordinate>) : Char[,]=
        if Set.isEmpty toVisit then
            cleanedGrid
        else 
            let x,y = toVisit |> Set.toSeq |> Seq.head
            cleanedGrid[x,y] <- ' '
            let adjacents = getAdjacent (x,y)
            let moreToVisit =
                adjacents
                |> List.filter (fun c -> not <| Set.contains c visited && not (fst c = x && snd c = y))
                |> List.filter (fun (x,y) -> cleanedGrid[x,y] = '.')
                |> set
            let newToVisit =
                toVisit
                |> Set.union moreToVisit
                |> Set.remove (x,y)
                
            flood (Set.add (x,y) visited) newToVisit 
    flood Set.empty (Set.singleton (0,0))
    
let printGrid (grid: Char[,]) =
    for x in [0..(Array2D.length1 grid) - 1] do
        Console.WriteLine ""
        for y in [0..(Array2D.length2 grid) - 1] do
            Console.Write ((mapChar grid[x,y]).ToString())

let q10a () =
    
    let fileInput = File.ReadAllText "10.txt"
    let grid = createGrid fileInput
    let sPosition = find grid 'S'
    let loop = createLoop grid [{ PipeType = 'S';Coordinate = sPosition }]
    let result = loop.Length / 2
    Console.WriteLine $"10a: {result}"
    

let q10b () =
    
    let fileInput = File.ReadAllText "10.txt"
    let grid = createGrid fileInput
    let sPosition = find grid 'S'
    let loop = createLoop grid [{ PipeType = 'S';Coordinate = sPosition }]
    let fixedGrid = gridWithOnlyLoop grid loop
    let endGrid = fixedGrid |> expand 
                |> floodClean
                |> contractGrid
    printGrid endGrid
    let result = findAll endGrid '.' |> Seq.length
    Console.WriteLine $"10a: {result}"
    
