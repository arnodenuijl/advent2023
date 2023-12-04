module utils

open System
open FParsec

let splitOnNewline (s: String) = s.Split(Environment.NewLine) |> Seq.ofArray
let allPairs (items: 'a seq) =
    let list = List.ofSeq items
    let listLength = Seq.length list

    seq {
        for i = 0 to listLength - 1 do
            for j = 0 to listLength - 1 do
                if i <> j then
                    yield (list[i], list[j])
    }

let inline findWithRest (f: 'a -> bool) (input: 'a list) : ('a * 'a list) =
    let folder ((restItems, foundItem): 'a list * 'a option) (item: 'a) =
        match foundItem, f item with
        | None, true -> (restItems, Some item)
        | Some(x), true -> failwith $"already found {x} and {item} also matches"
        | _, false -> (item :: restItems, foundItem)

    let restList, foundItemOption = input |> List.fold folder ([], None)

    match foundItemOption, restList with
    | None, _ -> failwith $"didn't find item"
    | Some(x), rest -> (x, rest)

let unwrapParserResult (r: ParserResult<'a, 'b>) =
    match r with
    | Success(result, state, pos) -> result
    | Failure(s, parserError, userState) -> failwith (s)

let inline charToInt c = int c - int '0'

module Seq =
    let takeUntil predicate (source: seq<_>) =
        seq {
            use e = source.GetEnumerator()
            let mutable latest = Unchecked.defaultof<_>
            let mutable emitLatest = false

            while e.MoveNext()
                  && (latest <- e.Current
                      predicate latest |> not) do
                yield latest
                emitLatest <- true

            if emitLatest then
                yield latest
        }
