module q18

open System
open System.Globalization
open System.IO
open FParsec
open Microsoft.FSharp.Core
open utils

type Coordinate = int64*int64
type Direction =
| Right of int64
| Left of int64
| Up of int64
| Down of int64

let move (direction: Direction) ((x :int64, y : int64) : Coordinate) : Coordinate =
    match direction with
    | Up d -> (x + d, y)
    | Down d -> (x - d, y)
    | Left d -> (x, y - d)
    | Right d -> (x, y + d)
    
module Parsing =
    let direction = choice [
        skipString "U " >>. pint64 |>>  Up
        skipString "D " >>. pint64 |>>  Down
        skipString "L " >>. pint64 |>>  Left
        skipString "R " >>. pint64 |>>  Right
    ]
    let distance =  anyString 5 |>> fun s -> Int64.Parse(s, NumberStyles.HexNumber)
    let direction2 = distance .>>. anyChar |>> fun (dist, c) -> 
                    match c with
                    | '0' -> Right dist
                    | '1' -> Down dist
                    | '2' -> Left dist
                    | '3' -> Up dist
                    | _ -> failwith $"can't parse {c}"

    let rowParser = sepBy (direction .>> skipString " (#" .>>. direction2 .>> skipString ")" ) newline

let distance (p1:int64 * int64) (p2:int64 * int64) = Math.Abs(fst p1 - fst p2) +  Math.Abs(snd p1 - snd p2) 

let processDirections (directions : Direction list) = 
    let startingPoint = (0L,0L)
    let allPoints =
        directions
        |> List.scan
               (fun (currentCoord: Coordinate) dir -> move dir currentCoord)
                startingPoint
    let area =
        List.foldBack
            (fun p (area, previousPoint) ->
                         let newArea = (fst previousPoint * snd p) - (fst p * snd previousPoint)
                         (area + newArea, p))
            (allPoints |> List.rev)
            (0L, allPoints[allPoints.Length - 1])
        |> fst
        |> double
        |> fun x -> x / 2.0

    let circumference =
        allPoints
        |> List.fold (fun (o, p) p' ->
                    let added = distance p p'
                    (o + added, p'))
                    (0L, List.head allPoints)
        |> fst
        |> double
        |> fun x -> x / 2.0
    area + circumference + 1.0
    
let q18a () =
    let input = run Parsing.rowParser (File.ReadAllText "18.txt") |> unwrapParserResult
    let result = 
        input
        |> List.map fst
        |> processDirections

    Console.WriteLine $"18a: {result}"
    
let q18b () =
    let input = run Parsing.rowParser (File.ReadAllText "18.txt") |> unwrapParserResult
    let result = 
        input
        |> List.map snd
        |> processDirections
        
    Console.WriteLine $"18a: {result}"
    
    
   