module q5

open System
open System.IO
open FParsec
open utils

let testInput = """seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4"""

type Range = {
    from : int64
    uptoIncluding: int64
}

type Mapping = {
    range : Range
    delta : int64
} 

type Transformation = Mapping list

module Parser =
    let seedsParser = pstring "seeds: " >>. sepBy pint64 (pchar ' ')
    let mappingParser = pint64 .>> (pchar ' ') .>>. pint64 .>> (pchar ' ') .>>. pint64 .>> opt newline
                        |>> fun ((a, b), c) ->
                            let range = { Range.from = b; Range.uptoIncluding = b + c - int64 1 }
                            { range = range; Mapping.delta = a - b }
    let mappingsParser = many mappingParser 
  
    let allMappings = many (charsTillString  "map:" true 100 >>. skipNewline >>. mappingsParser)
    let allParser = seedsParser .>> newline .>> newline .>>. allMappings
    

type OverlapResult = {
    overlapping : Range option
    notOverlapping : Range list
}

let calculateOverlap (original: Range) (rangeToFindOverlap: Range) : OverlapResult =
    if rangeToFindOverlap.from > original.uptoIncluding || rangeToFindOverlap.uptoIncluding < original.from then
       { overlapping = None
         notOverlapping = [rangeToFindOverlap] }
    else // overlap
        let overlapStart = Math.Max(rangeToFindOverlap.from, original.from)
        let overlapEnd = Math.Min(rangeToFindOverlap.uptoIncluding, original.uptoIncluding)
        let overlap = { from = overlapStart; uptoIncluding = overlapEnd }
        let notOverlapping = seq {
                if rangeToFindOverlap.from < overlap.from then yield { from = rangeToFindOverlap.from; uptoIncluding = overlap.from - int64 1 }
                if rangeToFindOverlap.uptoIncluding > overlap.uptoIncluding then yield { from = overlap.uptoIncluding + int64 1; uptoIncluding = rangeToFindOverlap.uptoIncluding }
            }
        {
            overlapping = Some overlap
            notOverlapping = notOverlapping |> List.ofSeq
        }    

let applyDelta (x: Range) (delta: int64) : Range =
            { from = x.from + delta
              uptoIncluding = x.uptoIncluding + delta }

let processMapping (mapping : Mapping) (ranges: Range list): {| mappedRanges: Range list; unmappedRanges: Range list |} =
    ranges
    |> List.fold
            (fun acc range ->
                let overlapResult = calculateOverlap mapping.range range
                match overlapResult.overlapping with
                | Some overlapRange ->
                                   {| mappedRanges = (applyDelta overlapRange mapping.delta) :: acc.mappedRanges
                                      unmappedRanges = acc.unmappedRanges @ overlapResult.notOverlapping |}
                | None -> {| mappedRanges = acc.mappedRanges
                             unmappedRanges = acc.unmappedRanges @ overlapResult.notOverlapping |})
            {| mappedRanges = [] 
               unmappedRanges = [] |}
    

let applyTransformationToInputRanges (transformation : Transformation) (ranges: Range list): Range list  =
    let something =
        transformation
        |> List.fold
            (fun (acc:  {| mappedRanges: Range list; unmappedRanges: Range list |})
                 (mapping: Mapping) ->
                let x  = processMapping mapping acc.unmappedRanges
                {| mappedRanges = acc.mappedRanges @ x.mappedRanges
                   unmappedRanges = x.unmappedRanges |}
                   )
            {| mappedRanges  = [] 
               unmappedRanges = ranges |}
    something.mappedRanges @ something.unmappedRanges

let calculateMinimum (seedRanges: Range list) (transformations : Transformation list) =
    transformations
    |> List.fold
        (fun (ranges: Range list) (transformation: Transformation) ->
            transformation |> List.iter (fun t -> Console.WriteLine $"Mapping : {t}")
            let result = applyTransformationToInputRanges transformation ranges
            result)
        seedRanges
    |> List.map _.from
    |> List.min

let (seedsInput : int64 list, transformations : Transformation list) =
    let input = File.ReadAllText "05.txt"
    run Parser.allParser input  
        |> unwrapParserResult
        
let q5a () =
    let seedRanges =
        seedsInput
        |> List.map (fun s -> {Range.from = s;Range.uptoIncluding = s })
    let result = calculateMinimum seedRanges transformations
    Console.WriteLine $"5a2: {result}"

let q5b () =
    let seedRanges =
        seedsInput
        |> List.chunkBySize 2
        |> List.map (fun seedCombo -> { Range.from = seedCombo[0] ; Range.uptoIncluding = seedCombo[0] + seedCombo[1] - int64 1})
        
    let result = calculateMinimum seedRanges transformations
    Console.WriteLine $"5b: {result}"
    
