module utils

open System
open FParsec

module Array2D =
    let printGrid (grid: 'a[,]) =
        for x in [0..(Array2D.length1 grid) - 1] do
            Console.WriteLine ""
            for y in [0..(Array2D.length2 grid) - 1] do
                Console.Write ((grid[x,y]).ToString())

    let createGrid (input: String) =
        let lines = input.Split Environment.NewLine
        let linesLength = lines.Length
        let columnsLength = lines[0].Length
        let grid: char[,] = Array2D.create linesLength columnsLength ' '
        for line in [0..linesLength - 1] do
            for column in [0..columnsLength - 1] do
                Array2D.set grid line column (lines[line][column])
        grid
    
    
    
    let turnCounterClockwise (grid : 'a[,]) : 'a[,] =
        let transposed = Array2D.create (Array2D.length2 grid) (Array2D.length1 grid) Unchecked.defaultof<'a>
        for x in [0..Array2D.length1 grid - 1] do
            for y in [0..Array2D.length2 grid - 1] do
                transposed[Array2D.length1 grid - 1 - y,x] <- grid[x,y]
        transposed
    
    let transpose (grid : 'a[,]) : 'a[,] =
        let transposed = Array2D.create (Array2D.length2 grid) (Array2D.length1 grid) Unchecked.defaultof<'a>
        for x in [0..Array2D.length1 grid - 1] do
            for y in [0..Array2D.length2 grid - 1] do
                transposed[y,x] <- grid[x,y]
        transposed
    
    let rowsAsList (grid : 'a[,]) : 'a list list =
        seq {
            for rowNr in [0..(Array2D.length1 grid) - 1] do
                yield
                    [0..(Array2D.length2 grid) - 1]
                    |> Seq.map(fun columnNr -> grid[rowNr,columnNr])
                    |> List.ofSeq
        } |> List.ofSeq
        
let splitOnNewline (s: String) = s.Split(Environment.NewLine) |> Seq.ofArray
let allPairs (items: 'a seq) =
    let list = List.ofSeq items
    let listLength = Seq.length list

    seq {
        for i = 0 to listLength - 1 do
            for j = 0 to listLength - 1 do
                if i <> j then
                    yield (list[i], list[j])
    }

let inline findWithRest (f: 'a -> bool) (input: 'a list) : ('a * 'a list) =
    let folder ((restItems, foundItem): 'a list * 'a option) (item: 'a) =
        match foundItem, f item with
        | None, true -> (restItems, Some item)
        | Some(x), true -> failwith $"already found {x} and {item} also matches"
        | _, false -> (item :: restItems, foundItem)

    let restList, foundItemOption = input |> List.fold folder ([], None)

    match foundItemOption, restList with
    | None, _ -> failwith $"didn't find item"
    | Some(x), rest -> (x, rest)

let unwrapParserResult (r: ParserResult<'a, 'b>) =
    match r with
    | Success(result, state, pos) -> result
    | Failure(s, parserError, userState) -> failwith (s)

let inline charToInt c = int c - int '0'

let rec greatestCommonDivisor a b =
    let rec gcd' a b =
        if b = 0L then a else gcd' b (a % b)
    gcd' (abs a) (abs b)

let lowestCommonMultiple a b = a*b/(greatestCommonDivisor a b)

let rec distribute e = function
  | [] -> [[e]]
  | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

let rec permute = function
  | [] -> [[]]
  | e::xs -> List.collect (distribute e) (permute xs)

module Seq =
    let takeUntil predicate (source: seq<_>) =
        seq {
            use e = source.GetEnumerator()
            let mutable latest = Unchecked.defaultof<_>
            let mutable emitLatest = false

            while e.MoveNext()
                  && (latest <- e.Current
                      predicate latest |> not) do
                yield latest
                emitLatest <- true

            if emitLatest then
                yield latest
        }
