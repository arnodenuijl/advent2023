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

let input = File.ReadAllText "05.txt"
type Mapping = {
    destinationRangeStart : int64
    sourceRangeStart : int64
    length : int64
} with
    member x.isInRange (n : Int64) =
        n >= x.sourceRangeStart && n < x.sourceRangeStart + x.length
    member x.map (n:int64) = n - (x.sourceRangeStart - x.destinationRangeStart)

let map (mappings : Mapping list) (n: int64) =
    let mapper =
        mappings
        |> List.tryFind(fun m -> m.isInRange n)
        |> Option.map (_.map)
        |> Option.defaultValue id
    mapper n
    
        

module Parser =
    let seedsParser = pstring "seeds: " >>. sepBy pint64 (pchar ' ')
    let mappingParser = pint64 .>> (pchar ' ') .>>. pint64 .>> (pchar ' ') .>>. pint64 .>> opt newline |>> fun ((a, b), c) -> { destinationRangeStart = a; sourceRangeStart = b; length = c}
    let mappingsParser = many mappingParser 
    // let seedToSoil = pstring "seed-to-soil map:" >>. newline >>. mappingsParser .>> newline 
    // let soilToFertilizer = pstring "soil-to-fertilizer map:" >>. newline >>. mappingsParser .>> newline 
    // let fertilizerToWater = pstring "fertilizer-to-water map:" >>. newline >>. mappingsParser .>> newline 
    // let waterToLight = pstring "water-to-light map:" >>. newline >>. mappingsParser .>> newline 
    // let lightToTemp = pstring "light-to-temperature map:" >>. newline >>. mappingsParser .>> newline 
    // let tempToHumidity = pstring "temperature-to-humidity map:" >>. newline >>. mappingsParser .>> newline 
    // let humidityToLocation = pstring "humidity-to-location map:" >>. newline >>. mappingsParser .>> newline
  
    let allMappings = many (charsTillString  "map:" true 100 >>. skipNewline >>. mappingsParser)
    let allParser = seedsParser .>> newline .>> newline .>>. allMappings

let seeds, mappings  =
    run Parser.allParser input 
        |> unwrapParserResult
       
let q5a () =
    let result =
        seeds
        |> List.map (fun seed ->
                mappings
                |> List.fold
                    (fun (n : int64) (mapping : Mapping list) -> map mapping n)
                    seed
            )
        |> List.min
    Console.WriteLine $"5b: {result}"
