module q2

open System
open System.IO
open utils
open FParsec

let testinput = """Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green""" |> splitOnNewline


type Color = Red | Green | Blue

type HandfulOfCubes = {
    R: int
    G: int
    B: int
}
let emptyHandfulOfCubes = {R = 0; G = 0; B = 0 }

type Game = {
   nr: int
   handfulOfCubesList: HandfulOfCubes list
}

let addToHandfullOfCubes (cubes: int * Color) (handfulOfCubes: HandfulOfCubes) =
    match cubes with
    | c, Red ->  { handfulOfCubes with R = handfulOfCubes.R + c }
    | c, Green -> { handfulOfCubes with G = handfulOfCubes.G + c }
    | c, Blue -> { handfulOfCubes with B = handfulOfCubes.B + c }

let maxOfEachColor (handfulOfCubesList: HandfulOfCubes seq) =
    handfulOfCubesList
    |> Seq.fold
        (fun max h -> { R = Int32.Max(h.R, max.R)
                        G = Int32.Max(h.G, max.G)
                        B = Int32.Max(h.B, max.B) }
        )
        emptyHandfulOfCubes
let isSubsetOf (handfulOfCubes: HandfulOfCubes) (possibleSubset: HandfulOfCubes) =
    possibleSubset.R <= handfulOfCubes.R &&
    possibleSubset.G <= handfulOfCubes.G &&
    possibleSubset.B <= handfulOfCubes.B
    
module Parsing =
    
    let gameNumberParser = pstring "Game " >>. pint32
    let colorParser = choice [
        pstring "red" |>> fun _ -> Red
        pstring "green" |>> fun _ -> Green
        pstring "blue" |>> fun _ -> Blue
    ]
    let ballsOfAColorParser = spaces >>. pint32 .>> spaces .>>. colorParser 
    let handfulOfCubesParser = 
        sepBy ballsOfAColorParser (pstring ", ")
        |>> fun xs ->
                xs
                |> List.fold
                     (fun hand x -> addToHandfullOfCubes x hand)
                     emptyHandfulOfCubes
    let multipleHandfulOfCubesParser =
        sepBy handfulOfCubesParser (pchar ';')
        
    let gameParser =
        gameNumberParser .>> pchar ':' .>>. multipleHandfulOfCubesParser
        |>> fun (nr, handfulOfCubes) -> { nr = nr; handfulOfCubesList = handfulOfCubes  }
        
    let parseGame s = run gameParser s |> unwrapParserResult

let q2a () = 
    // let games = testinput |> Seq.map Parsing.parseGame
    let games = File.ReadLines "02.txt" |> Seq.map Parsing.parseGame
    
    let referenceHand = {
        R = 12
        G = 13
        B = 14
    }
    
    let result =
        games
        |> Seq.filter (fun g ->
                g.handfulOfCubesList
                |> Seq.forall (fun hand -> isSubsetOf referenceHand hand)
            )
        |> Seq.sumBy _.nr
    Console.WriteLine $"q2a: {result}"
    
let q2b () = 
    // let games = testinput |> Seq.map Parsing.parseGame
    let games = File.ReadLines "02.txt" |> Seq.map Parsing.parseGame
    
    let result =
        games
        |> Seq.map (fun g -> maxOfEachColor g.handfulOfCubesList)
        |> Seq.map (fun h -> h.R * h.G * h.B)
        |> Seq.sum
    Console.WriteLine $"q2a: {result}"