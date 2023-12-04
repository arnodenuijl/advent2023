module q3

open System
open System.IO

let input = File.ReadAllLines "03.txt" |> Array.map (_.ToCharArray())

type Point = int * int

type FoundNumber = {
    number: int
    startPoint: Point
    endPoint: Point
} with
    override x.ToString() = $"{x.number}: ({x.startPoint}, {x.endPoint})"

type FoundSymbol = {
    point: Point
    symbol: Char
}

type CharDotOrSymbol =
    | Dot
    | Number of Char
    | Symbol of Char
    
let readCharDotOrSymbol (c: Char) =
    if Char.IsDigit c then Number c
    elif c = '.' then Dot
    else Symbol c
    
type GetItemsFromLineFoldType = {
    currentNumber : Option<Point * int * Char[]>
    foundSymbols : Set<FoundSymbol>
    foundNumbers: Set<FoundNumber>
} with static member Zero = {currentNumber = None; foundSymbols = Set.empty; foundNumbers = Set.empty }  
    
let getItemsFromLine (lineNumber: int) (line: Char[]): Set<FoundSymbol> * Set<FoundNumber> =
    line
    |> Array.mapi (fun i c -> i,readCharDotOrSymbol c) // attach the linenumber to the line
    |> Array.fold 
        (fun acc (columnNumber, item) ->
            match item, acc.currentNumber with
            // we encounter a . and weren't reading a number. Just continue
            | Dot , None -> acc 
            // we encounter a dot and were reading a number. The number is finished, add it to the foundNumbers
            | Dot , Some (startPoint, currentEndColumn, numberChars) -> 
                { acc with currentNumber = None
                           foundNumbers = Set.add {
                                number = String(numberChars) |> int
                                startPoint  = startPoint
                                endPoint = lineNumber, currentEndColumn
                            } acc.foundNumbers
                }
            // we encounter a number but weren't currently reading a number. So set this is the start of a new number
            | Number c, None -> { acc with currentNumber = Some((lineNumber, columnNumber), columnNumber , [|c|]) }
            // we encounter a number and were reading a number. So add this number to the current number we are reading
            | Number c, Some (startPoint, _, numberChars) ->
                {acc with currentNumber = Some(startPoint, columnNumber, Array.append numberChars [|c|]) }
            // we encounter a symbol and weren't reading a number. Add it to the found symbols
            | Symbol c, None -> { acc with foundSymbols =
                                            Set.add {
                                                point = (lineNumber, columnNumber)
                                                symbol = c } acc.foundSymbols
                                }
            // we encounter a symbol and were reading the number. Add the symbol to the found symbols and add the foundNumber
            | Symbol c, Some (startPoint, currentEndColumn, numberChars) ->
                {  currentNumber = None
                   foundNumbers = Set.add {
                                number = String(numberChars) |> int
                                startPoint  = startPoint
                                endPoint = lineNumber, currentEndColumn } acc.foundNumbers
                   foundSymbols =   Set.add {
                                        point = (lineNumber, columnNumber)
                                        symbol = c } acc.foundSymbols
                                    }
        )
        GetItemsFromLineFoldType.Zero
    // if the fold ended we might have an unfinished number in the accumulator. We don't want to forget to add that and
    // spend 3 ours of the sunday to think about this
    |> fun x -> match x.currentNumber with
                | None -> x.foundSymbols, x.foundNumbers
                | Some (startPoint, endColumn, numberChars) ->
                    let lastFoundNumber = { number = String(numberChars) |> int
                                            startPoint = startPoint
                                            endPoint = lineNumber, endColumn }
                    x.foundSymbols, Set.add lastFoundNumber x.foundNumbers

let symbols, numbers =
    input
    |> Array.mapi (fun i line -> i, line)
    |> Array.fold ( fun (allFoundSymbols, allFoundNumbers) (lineNumber, line) ->
        let foundSymbols, foundNumbers = getItemsFromLine lineNumber line
        (Set.union allFoundSymbols foundSymbols, Set.union allFoundNumbers foundNumbers))
        (Set.empty, Set.empty)


let isNumberTouchingSymbol (number: FoundNumber) (symbol: FoundSymbol) =
    ((fst symbol.point) + 1  >= fst number.startPoint
        && (fst symbol.point) - 1 <= fst number.endPoint) && 
    ((snd symbol.point) + 1  >= snd number.startPoint
        && (snd symbol.point) - 1 <= snd number.endPoint)
    
let q3a () =
    let touchingNumbers =
        numbers
        |> Set.filter (fun number ->
                symbols
                |> Set.exists (fun symbol -> isNumberTouchingSymbol number symbol))
    
    let result =
        touchingNumbers
        |> Seq.sumBy _.number
    Console.WriteLine $"3a: {result}"
    
    
let q3b () =
    let gears = symbols |> Set.filter(fun s -> s.symbol = '*')
    let gearsWithTwoNumberTouching =
        gears
        |> Seq.collect (fun g ->
            let allTouchingNumbers =
                Set.filter (fun n -> isNumberTouchingSymbol n g) numbers
                |> List.ofSeq
            if List.length allTouchingNumbers = 2 then
                [(allTouchingNumbers[0], allTouchingNumbers[1])]
            else List.empty
        )

    let result =
        gearsWithTwoNumberTouching
        |> Seq.sumBy (fun (n1, n2) -> n1.number * n2.number)
    Console.WriteLine $"3a: {result}"
