module q4

open System
open System.IO
open FParsec
open utils

type ScratchCard = {
    cardNr : int
    winningNumbers : int Set
    myNumbers : int Set
}

module Parser =
    let cardParser = pstring "Card" >>. spaces >>. pint32 .>> pstring ":"
    let numbersParser = sepEndBy pint32 spaces |>> set
    let lineParser = cardParser .>> spaces .>>. numbersParser .>> pstring "|" .>> spaces .>>. numbersParser  
    let parseGame s =
        run lineParser s
        |> unwrapParserResult
        |> fun ((cardNr, winningNumbers), myNumbers) -> { cardNr = cardNr
                                                          winningNumbers = winningNumbers
                                                          myNumbers = myNumbers }
       
let allCards =
    File.ReadAllLines "04.txt"
    |> List.ofArray|> List.map Parser.parseGame

let q4a () =
    let calculatePoints (c: ScratchCard) =
        Set.intersect c.winningNumbers c.myNumbers
        |> Set.count
        |> function
           | 0 -> 0
           | n -> pown 2 (n - 1)

    let result =
        allCards
        |> List.map calculatePoints
        |> List.sum
        
    Console.WriteLine $"4a: {result}"

let q4b () =
    let maxIndex = allCards.Length - 1

    let rec calculateCardsWon (card : ScratchCard) : int =      
        let correctNumbers = Set.intersect card.winningNumbers card.myNumbers
                
        let extraCards =
            match correctNumbers.Count with
            | 0 -> []
            | n ->
                let startIndex = card.cardNr
                let endIndex = Math.Min(maxIndex, card.cardNr + n - 1)
                allCards[startIndex..endIndex]
                
        let totalCardsWonFromExtraCards = extraCards |> List.sumBy calculateCardsWon
        1 + totalCardsWonFromExtraCards
        
    let result =
        allCards
        |> List.map calculateCardsWon
        |> List.sum

        
    Console.WriteLine $"4b: {result}"
