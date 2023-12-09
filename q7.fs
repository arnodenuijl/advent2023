module q7

open System
open System.IO
open FParsec
open utils

module Parsing =
    let cardBidParser = anyString 5 .>> spaces .>>. pint32 |>>  fun (s, i) -> (s, i)
    let cardBidsParser = sepBy cardBidParser newline

let compareTwoCardsBy_CardsFromHighToLow (cardsFormHighToLow : Char list) (c1:Char) (c2: Char) =
    let index1 = List.findIndex (fun c -> c = c1) cardsFormHighToLow
    let index2 = List.findIndex (fun c -> c = c2) cardsFormHighToLow
    index1.CompareTo(index2)

let  compareHandOfCardsBy_CardsFromHighToLow (cardsFormHighToLow: Char list) (c1: String)  (c2: String) =
    Array.compareWith (compareTwoCardsBy_CardsFromHighToLow cardsFormHighToLow) (c1.ToCharArray())  (c2.ToCharArray()) 
    
let highestCard  (cardsFormHighToLow : Char list) (cards: String) =
    cards.ToCharArray()
    |> Array.sortWith (compareTwoCardsBy_CardsFromHighToLow cardsFormHighToLow)
    |> Array.head
    
type HandType =
    | FiveOfAKind of Char
    | FourOfAKind of Char
    | FullHouse of Char * Char
    | ThreeOfAKind of Char
    | TwoPair of Char * Char
    | OnePair of Char
    | HighCard of Char
    
let handTypesValue (h : HandType) =
    match h with
    | FiveOfAKind _ -> 7
    | FourOfAKind _ -> 6
    | FullHouse _ -> 5
    | ThreeOfAKind _ -> 4
    | TwoPair _ -> 3
    | OnePair _ -> 2
    | HighCard _-> 1
    
let determineType (cardsFormHighToLow: Char list) (cards: String) =
    let groups =
        cards.ToCharArray()
        |> List.ofArray
        |> List.countBy id
        |> List.sortByDescending snd
        
    match groups with
    | [(c1,5)] -> FiveOfAKind c1 
    | [(c1,4);(_,1)] -> FourOfAKind c1 
    | [(c1,3);(c2,2)] -> FullHouse (c1, c2) 
    | [(c1,3);(_,1);(_,1)] -> ThreeOfAKind c1 
    | [(c1,2);(c2,2);(_,1)] -> TwoPair (c1, c2) 
    | [(c1,2);(_,1);(_,1);(_,1)] -> OnePair c1
    | [(_,1);(_,1);(_,1);(_,1);(_,1)] -> HighCard (highestCard cardsFormHighToLow cards)
    | _ -> failwith $"Cant determine card type for {String(cards)}"
    |> handTypesValue
    
let q7a () =
    let cardsFormHighToLow = ['A'; 'K'; 'Q'; 'J'; 'T'; '9'; '8'; '7'; '6'; '5'; '4'; '3'; '2']
    
    let cardsWithBids = run Parsing.cardBidsParser (File.ReadAllText "07.txt") |> unwrapParserResult

    let cardsWithBitGroupedByHandTypeOrdered =
        cardsWithBids
        |> List.groupBy (fun (cards, _) ->  determineType cardsFormHighToLow cards )
        |> List.sort
        |> List.collect (fun (handsType, cardsWithBit) ->
                cardsWithBit
                |> List.sortWith (fun (cards1, _) (cards2, _) -> compareHandOfCardsBy_CardsFromHighToLow cardsFormHighToLow cards2 cards1 )
                |> List.map (fun (cards, bid) -> (handsType, cards, bid)))
        |> List.mapi  (fun i (handstypeNumber, cards, bid)-> i + 1, handstypeNumber, cards, bid )
   
    let result =
        cardsWithBitGroupedByHandTypeOrdered
        |> List.map (fun (rank, _, _, bid) -> rank * bid)
        |> List.sum
    Console.WriteLine $"7a: {result}"

let rec replaceAllJs (s:string) : string list =
   seq {
        let indexOfJ = s.IndexOf 'J'
        for replacementChar in ['A'; 'K'; 'Q'; 'T'; '9'; '8'; '7'; '6'; '5'; '4'; '3'; '2'] do
            let replaced = String.mapi(fun i c -> if indexOfJ=i then replacementChar else c) s
            if replaced.Contains('J') then
                yield! replaceAllJs replaced
            else yield replaced                   
   } |> List.ofSeq

let getBestHandstypeWithJsReplaced cardsFormHighToLow (cards : String) : int =
    replaceAllJs cards
    |> List.map (determineType cardsFormHighToLow)
    |> List.max
    
let q7b () =
    let cardsFormHighToLow = ['A'; 'K'; 'Q'; 'T'; '9'; '8'; '7'; '6'; '5'; '4'; '3'; '2';'J']

    let cardsWithBids = run Parsing.cardBidsParser (File.ReadAllText "07.txt") |> unwrapParserResult

    let cardsWithBitGroupedByHandTypeOrdered =
        cardsWithBids
        |> List.groupBy (fun (cards, _) -> getBestHandstypeWithJsReplaced cardsFormHighToLow cards)
        |> List.sort
        |> List.collect (fun (handsType, cardsWithBit) ->
                cardsWithBit
                |> List.sortWith (fun (cards1, _) (cards2, _) -> compareHandOfCardsBy_CardsFromHighToLow cardsFormHighToLow cards2 cards1 )
                |> List.map (fun (cards, bid) -> (handsType, cards, bid)))
        |> List.mapi (fun i (handstypeNumber, cards, bid)-> i + 1, handstypeNumber, cards, bid )
   
    let result =
        cardsWithBitGroupedByHandTypeOrdered
        |> List.map (fun (rank, _, _, bid) -> rank * bid)
        |> List.sum
    
    Console.WriteLine $"7b: {result}"

