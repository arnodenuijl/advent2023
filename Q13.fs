module q13

open System
open System.IO
open utils
    
let getHorizontalLine (grid : char[,])=
    [0..(Array2D.length1 grid) - 1 ]
    |> List.map(fun i -> String(grid[i,*]))
    
let getVerticalLines (grid : char[,])=
    [0..(Array2D.length2 grid) - 1 ]
    |> List.map(fun i -> String(grid[*, i]))

let findPossibleSymmetryHalves (input : string list) =
    [0..input.Length - 2]
    |> List.mapi (fun i x -> 
        let side1 = input[..x] |> List.rev
        let side2 = input[x + 1..] 
        (i, (side1,side2 )) )
    |> List.map (fun (i, (side1, side2)) ->
        let min = Math.Min(side1.Length, side2.Length)
        (i + 1, side1[..min - 1] , side2[..min - 1]) )


let totalDifferences (ss1 : string list) (ss2 : string list) =
    let charactersDifference (s1 : String) (s2: String) =
        Array.map2 (=) (s1.ToCharArray()) (s2.ToCharArray())
        |> Array.filter (not >> id)
        |> Array.length
        
    List.map2 charactersDifference ss1 ss2
    |> List.sum
    
let findSymmetryLineWithXDefects (defects: int) (input : string list) =
    findPossibleSymmetryHalves input
    |> List.filter (fun (_, side1, side2) -> (totalDifferences side1 side2) = defects )
    |> List.map (fun (i, _, _) -> i)
    |> List.tryExactlyOne

let grids =
    File.ReadAllText("13.txt").Split($"{Environment.NewLine}{Environment.NewLine}")
    |> Array.map _.Trim()
    |> Array.map Array2D.createGrid
    |> List.ofArray     
    
let q13a () =     
    let result =
        grids
        |> List.map (fun g ->
            let horizontalSymmetry = (findSymmetryLineWithXDefects 0) <| getHorizontalLine g
            let verticalSymmetry = (findSymmetryLineWithXDefects 0) <| getVerticalLines g
            match horizontalSymmetry, verticalSymmetry with
            | Some h, None -> h * 100
            | None, Some v -> v
            | _ -> failwith "oeps")
        |> List.sum
        
    Console.WriteLine $"13a: {result}"

let q13b () =
    let result =
        grids
        |> List.map (fun g ->
            let horizontalSymmetry = (findSymmetryLineWithXDefects 1) <| getHorizontalLine g
            let verticalSymmetry = (findSymmetryLineWithXDefects 1) <| getVerticalLines g
            match horizontalSymmetry, verticalSymmetry with
            | Some h, None -> h * 100
            | None, Some v -> v
            | _ -> failwith "oeps")
        |> List.sum
        
    Console.WriteLine $"13b: {result}"

   