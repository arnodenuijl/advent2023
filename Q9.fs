module q9

open System
open System.IO
open FParsec
open utils
    
module Parsing =
    let histories = sepEndBy (sepBy pint32 (pchar ' ')) newline 
    
let rec calculateSerie (serie: int list) =
    seq {
        yield serie
        if not <| List.forall (fun i -> i = 0) serie then
            let nextSerie =
                serie
                |> List.windowed 2
                |> List.map (fun xs -> xs[1] - xs[0])
            yield! calculateSerie nextSerie
    }
    |> List.ofSeq
    
let reduceSeries (series : int list list) =
    List.scanBack
        (fun (serie: int list) (previousAppendedSerie: int list)->
            let last = List.last serie
            let previousLast = List.last previousAppendedSerie
            let newLast =  last + previousLast
            let appendedSerie = serie @ [newLast]
            appendedSerie)
        series
        [0]
    |> List.map List.last
    |> List.head
        
let reduceSeriesFront (series : int list list) =
    List.scanBack
        (fun (serie: int list) (previousAppendedSerie: int list)->
            let first = List.head serie
            let previousFirst = List.head previousAppendedSerie
            let newFirst =  first - previousFirst 
            let appendedSerie = newFirst :: serie
            appendedSerie)
        series
        [0]
    |> List.map List.head
    |> List.head
    
let q9a () =
    let histories = run Parsing.histories (File.ReadAllText "09.txt") |> unwrapParserResult
    let result =
        histories
        |> List.map (calculateSerie >> reduceSeries)
        |> List.sum
        
    Console.WriteLine $"9a: {result}" 
let q9b () =
    let histories = run Parsing.histories (File.ReadAllText "09.txt") |> unwrapParserResult
    let result =
        histories
        |> List.map (calculateSerie >> reduceSeriesFront)
        |> List.sum
        
    Console.WriteLine $"9b: {result}" 
    distribute 2 [0;1]
    |> List.iter (fun x -> Console.WriteLine x)