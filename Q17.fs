module q17

open System
open System.IO
open Microsoft.FSharp.Core
open utils
let testinput = """2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533"""

type Coordinate = int*int
let q17a () =
    let input = File.ReadAllText "17.txt"   
    let grid = Array2D.createGrid testinput
    let startPoint = (0,0)
    let endPoint = (Array2D.length1 grid, Array2D.length2 grid)
    Console.WriteLine $"17a: {0}"
    
let q17b () =
    let input = File.ReadAllText "17.txt"   
    Console.WriteLine $"17b: {0}"
    
    
   