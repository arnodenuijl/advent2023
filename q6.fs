module q6

open System

let q6a () =
    let races = [
        (47,207)
        (84,1394)
        (74,1209)
        (67,1014)
    ]
    
    let calculateRace (maxTime : int, minDistance : int) =
        [0..maxTime]
        |> List.map (fun holdTime ->
            holdTime * (maxTime - holdTime))
        |> List.filter (fun d -> d > minDistance)
        |> List.length

    let result =
        races
        |> List.map calculateRace
        |> List.fold (*) 1
        
    Console.WriteLine $"6a: {result}"
    
let q6b () =
    let race = (47847467L, 207139412091014L)
     
    let calculate  (maxTime : int64, minDistance : int64) =
        let a = double -1
        let b = double maxTime
        let c = double  -minDistance

        let x1 = (-b + sqrt(  (pown b 2) - 4.0 * a * c )) / (2.0 * a)
        let x2 = (-b - sqrt((pown b 2) - 4.0 * a * c )) / (2.0 * a)
        Console.WriteLine $"{x1}, {x2}"
        Math.Floor(x2) - Math.Ceiling(x1) + 1.0

    Console.WriteLine $"6a: {calculate race}"

