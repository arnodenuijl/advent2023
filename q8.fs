module q8

open System
open System.IO
open System.Linq
open FParsec
open utils

let test = "LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)"
let test2 = """LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)"""

type NodeMap = Map<string, string*string>

module Parsing =
    let movement = choice [
                        skipChar 'L' |>> (fun _ -> fst)
                        skipChar 'R' |>> (fun _ -> snd)
                   ]
    let node =
        anyString 3 .>> skipString " = (" .>>. anyString 3 .>> skipString ", " .>>. anyString 3 .>> skipString ")"
        |>> fun ((node, nextLeft), nextRight) -> (node, (nextLeft, nextRight))
    let nodes = sepBy node newline |>> Map.ofSeq
    
    let movementsAndNodesParser = many movement .>> newline .>> newline .>>. nodes
    



let q8a () =
    let movements, nodes = run Parsing.movementsAndNodesParser test |> unwrapParserResult
    // let movements, nodes = run Parsing.movementsAndNodesParser (File.ReadAllText "08.txt") |> unwrapParserResult
    
    let movementsRepeated = seq{ while true do yield! movements }
    let result =
        movementsRepeated
        |> Seq.scan
            (fun ( stepsTaken, currentNode) movement -> (stepsTaken + 1), movement nodes[currentNode])
            (0, "AAA")
        |> Seq.takeUntil (fun (_, currentNode) -> currentNode = "ZZZ")
        |> Seq.last
        |> fst
    Console.WriteLine $"q8a: {result}"
    
let q8b () =
    // let movements, nodes = run Parsing.movementsAndNodesParser test2 |> unwrapParserResult
    let movements, nodes = run Parsing.movementsAndNodesParser (File.ReadAllText "08.txt") |> unwrapParserResult
    let movementsRepeated = seq{ while true do yield! movements }

    let allStartNodes =
        Map.keys nodes
        |> Seq.filter (fun n -> n.EndsWith "A")
        |> List.ofSeq
    
    let stepSizes =
        allStartNodes
        |> List.map (fun startNode ->
            let stepSize =
                movementsRepeated
                |> Seq.scan
                    (fun ( stepsTaken, currentNode) movement -> (stepsTaken + 1), movement nodes[currentNode])
                    (0, startNode)
                |> Seq.filter (fun (_, currentNode) -> currentNode.EndsWith("Z"))
                |> Seq.map fst
                |> Seq.head
                |> int64
            stepSize)
    for s in stepSizes do
        Console.WriteLine $"{s}"
    let result =
        stepSizes
        |> Seq.reduce lowestCommonMultiple
        
    Console.WriteLine $"q8b: {result}" 
