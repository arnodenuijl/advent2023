module q15

open System
open System.IO
open System.Text
open utils
open FParsec

type Label = string
type Lens = { label: Label
              FocalLength: int }

type Box = { lenses: Lens list }

type Operation =
| RemoveLens of {|boxNr: int;label: Label|}
| AddLens of {|boxNr: int;lens: Lens|}

let hash (s:string) =
    Encoding.ASCII.GetBytes(s)
    |> Array.fold
        ( fun (acc : int) (b: Byte) ->
            b
            |> int
            |> ((+) acc)
            |> ((*) 17)
            |> (fun x -> x % 256) )
        0
        
module Parsing =
    let label = many asciiLetter |>> (fun cs -> String(cs |> Array.ofList))
    let operation = label .>>.
                    choice [
                        pchar '-' |>> fun _ -> Int32.MinValue
                        pchar '=' >>. pint32
                    ] |>> fun (l, i) ->
                            if i = Int32.MinValue
                            then RemoveLens {| boxNr = hash l; label= l |}
                            else AddLens {| boxNr = hash l; lens ={label=l;FocalLength = i} |} 
    let operations = sepBy operation (pchar ',')
    
    let parseOperations s =
        run operations s
        |> unwrapParserResult

let performStep (operation : Operation) (boxes : Box list) :  Box list =
    match operation with
    | AddLens x ->
        let box = boxes[x.boxNr]     
        let updatedBox =
            match List.tryFindIndex (fun ls -> ls.label = x.lens.label ) box.lenses with
            | Some i -> { lenses = List.updateAt i x.lens box.lenses }
            | None -> {lenses = box.lenses @ [x.lens] }
        List.updateAt x.boxNr updatedBox boxes

    | RemoveLens x ->
        let box = boxes[x.boxNr]
        let updatedLenses =
            match box.lenses |> List.tryFindIndex (fun l -> l.label = x.label) with
            | Some i -> box.lenses |> List.removeAt i
            | None -> box.lenses            
        let updatedBox = 
            { lenses = updatedLenses }
        List.updateAt x.boxNr updatedBox boxes

let calculatePower (boxes : Box list) =
    boxes
    |> List.mapi (fun boxNr box ->
                    box.lenses
                    |> List.mapi (fun lensNr l -> (1 + boxNr) * (lensNr + 1) * l.FocalLength )
                    |> List.sum
                )
    |> List.sum
    
let q15a () =
    let input = File.ReadAllText "15.txt"
    
    let result =
        input.Split(",")
        |> Array.map hash
        |> Array.sum
    Console.WriteLine $"15a: {result}"
 
let q15b () =
    let input = File.ReadAllText "15.txt"

    let boxes = List.replicate 256 { Box.lenses = [] }
    let operations = Parsing.parseOperations input

    let endState =
        operations
        |> List.fold
               (fun boxes op -> performStep op boxes)
               boxes 
    Console.WriteLine $"15b: {calculatePower endState}"

   