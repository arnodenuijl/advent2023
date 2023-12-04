module q1

open System
open System.IO

let findNumberAtStart (s: String) : String =
    if Char.IsDigit s[0] then s[0].ToString()
    elif s.StartsWith "one" then "1"
    elif s.StartsWith "two" then "2"
    elif s.StartsWith "three" then "3"
    elif s.StartsWith "four" then "4"
    elif s.StartsWith "five" then "5"
    elif s.StartsWith "six" then "6"
    elif s.StartsWith "seven" then "7"
    elif s.StartsWith "eight" then "8"
    elif s.StartsWith "nine" then "9"
    else ""

let replaceNumbers (s: String) =
    [ 0 .. s.Length - 1 ]
    |> Seq.map (fun start -> findNumberAtStart (s.Substring(start)))
    |> fun ss -> String.Join("", ss)


// go through string char by char.
// if the char is a digit and we don't have a first digit yet, this is the first
// if the char is a digit set it as the last digit (if there is one after that it will overwrite)
let rec numberFromString (s: String) : int =
    s
    |> Seq.fold
        (fun (first, last) c ->
            match Int32.TryParse(c.ToString()) with
            | true, d -> (Option.orElse (Some d) first, Some d)
            | false, _ -> (first, last))
        (None, None)
    |> fun (first, last) -> Int32.Parse($"{first.Value}{last.Value}")

let q1a () =
    let result =
        File.ReadLines "01.txt"
        |> Seq.sumBy numberFromString

    Console.WriteLine $"1a: {result}"

let q1b () =
    let result =
        File.ReadLines "01.txt"
        |> Seq.map replaceNumbers
        |> Seq.sumBy numberFromString

    Console.WriteLine $"1b: {result}"
