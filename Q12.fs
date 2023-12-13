module q12

open System
open System.Collections.Generic
open System.IO
open utils
open FParsec

module Parsing =
    let charParser = choice [pchar '?';pchar '.'; pchar '#']
    let springs = many charParser
    let conditions = sepBy pint32 (pchar ',')
    let lineParser = springs .>> spaces .>>. conditions
    let mainParser = sepBy lineParser newline

type SolveState =
| SearchingNextCondition
| ReadingCondition of int

type Key =   (Char list) * SolveState * (int list)
let cache = Dictionary<Key, int64>()
let rec calcValidSolutions (processed : Char list) (remaining : Char list) (solveState: SolveState) (nextConditions : int list) : int64 =
    let cacheKey = (remaining, solveState, nextConditions)
    match cache.TryGetValue(cacheKey) with
    | true, cacheValue ->
        cacheValue
    | false, _ -> 
            
        let result =
            match solveState, remaining, nextConditions with
            | ReadingCondition 0, [], [] -> 1L // no more # and no more conditions. We've solved something  
            | SearchingNextCondition , [] , []   -> 1L
            | ReadingCondition 0, [], _ -> 0L // no more # needed, but we still have them
            | ReadingCondition 0, '#' :: restChars, _ -> 0L // no more # needed, but we still have them
            | ReadingCondition 0, '.' :: restChars, _ -> calcValidSolutions ('.' :: processed) restChars SearchingNextCondition nextConditions 
            | ReadingCondition 0, '?' :: restChars, _ -> calcValidSolutions ('.' :: processed) restChars SearchingNextCondition nextConditions
            | ReadingCondition n, [], _ -> 0L // more # needed, but we don't have them  
            | ReadingCondition n, '#' :: restChars, _ -> calcValidSolutions ('#' :: processed)  restChars (ReadingCondition (n - 1)) nextConditions
            | ReadingCondition n, '.' :: restChars, _ -> 0L 
            | ReadingCondition n, '?' :: restChars, _ -> calcValidSolutions ('#' :: processed)  restChars (ReadingCondition (n - 1))  nextConditions   
            
            | SearchingNextCondition, [], x :: _ -> 0L // no more # and no more conditions. We've solved something  
            | SearchingNextCondition , nextChar :: restChars, [] ->
                if nextChar = '#'
                then 0
                else calcValidSolutions ('.' :: processed) restChars SearchingNextCondition []
            | SearchingNextCondition , nextChar :: restChars, nextCondition :: restConditions -> 
                if nextChar = '#'
                then calcValidSolutions (nextChar :: processed) restChars (ReadingCondition (nextCondition - 1 )) restConditions
                elif nextChar = '.' then calcValidSolutions ('.' :: processed)  restChars SearchingNextCondition (nextCondition :: restConditions)
                else 
                    calcValidSolutions ('.' :: processed) restChars SearchingNextCondition (nextCondition :: restConditions) +
                    calcValidSolutions ('#' :: processed) restChars (ReadingCondition (nextCondition - 1)) restConditions
            | _ -> failwith $"can't handle {solveState} with {remaining} and {nextConditions}"
        cache[cacheKey] <- result
        result

let inputString = File.ReadAllText "12.txt"

let q12a () =
    cache.Clear()
    let all = run Parsing.mainParser inputString |> unwrapParserResult
    
    let result =
        all
        |> List.map (fun (springs, conditions) -> calcValidSolutions [] springs SearchingNextCondition conditions)
        |> List.sum
    Console.WriteLine $"12a: {result}"

let q12b () =
    cache.Clear()

    let all = run Parsing.mainParser inputString |> unwrapParserResult
    
    let expandSprings (springs : Char list) =
        springs @ ['?'] @ springs @ ['?'] @ springs @ ['?'] @ springs @ ['?'] @ springs
    
    let expandCondition (conditions : int list) = conditions @ conditions @ conditions @ conditions @ conditions
    
    let result =
        all
        |> List.map (fun (springs, conditions) -> (expandSprings springs), (expandCondition conditions)) 
        |> List.map (fun (springs, conditions) -> calcValidSolutions [] springs SearchingNextCondition conditions)
        |> List.sum
    Console.WriteLine $"12a: {result}"

   