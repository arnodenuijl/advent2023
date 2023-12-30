module q20

open System
open System.IO
open System.Text
open FParsec
open Microsoft.FSharp.Core
open utils

type ModuleId = string

type ModuleState =
| Broadcast
| FlipFlop of bool
| Conjunction of Map<ModuleId, bool>

type Module = {
    moduleId : ModuleId
    moduleState : ModuleState
    connectedModules : List<ModuleId>
}

type Pulse = bool // high is true
type Signal = {
    fromModule: ModuleId
    toModule: ModuleId
    pulse:  Pulse
}

module Parsing =
    let moduleIdParser: Parser<ModuleId, unit> = many asciiLower |>> (Array.ofList >> String)
    let connectedModulesParser = skipString " -> " >>. (sepBy moduleIdParser (skipString ", "))
    let moduleParser : Parser<Module, unit> =
        choice [
            pstring "broadcaster" >>. connectedModulesParser
                |>> fun xs -> {
                    moduleId = "broadcaster"
                    moduleState = Broadcast
                    connectedModules = xs
                } 
            pchar '%' >>. moduleIdParser .>>. connectedModulesParser
                |>> fun (modId, xs) -> {
                    moduleId = modId
                    moduleState = FlipFlop false
                    connectedModules = xs
                }
            pchar '&' >>. moduleIdParser .>>. connectedModulesParser
                |>> fun (modId, xs) -> {
                    moduleId = modId
                    moduleState = Conjunction Map.empty
                    connectedModules = xs
                }
        ]
    let modulesParser = sepBy moduleParser newline

let setConjuctionStates (modules : Module list) : Module list =
    modules
    |> List.map (fun m ->
        match m.moduleState with
        | Broadcast
        | FlipFlop _ -> m
        | Conjunction _ ->
            let inputs =
                modules
                |> List.filter ( fun mm ->  List.contains m.moduleId mm.connectedModules)
                |> List.map (fun mm -> mm.moduleId, false)
                |> Map.ofList
            { m with moduleState = Conjunction inputs}
         )
    
let handleSignal (modu : Module) (signal: Signal) : Module * Signal list=
    let updatedModule, outgoingPulse =
        match modu.moduleState with
        | Broadcast -> Broadcast,  Some signal.pulse 
        | FlipFlop isOn ->
            match signal.pulse, isOn with
            | true , _ -> FlipFlop isOn, None
            | false, true ->  FlipFlop false, Some false
            | false, false ->  FlipFlop true, Some true
        | Conjunction state ->
            let updatedModuleState = state |> Map.add signal.fromModule signal.pulse
            let outgoingPulse = 
                if updatedModuleState |> Map.forall (fun _ v -> v = true)
                then false
                else true
            (Conjunction updatedModuleState), Some outgoingPulse
    let outgoingSignals =
        match     outgoingPulse with
        | Some p -> modu.connectedModules
                    |> List.map (fun m -> {fromModule = modu.moduleId; toModule = m; pulse = p })
        | None -> []
                    
    ({ modu with moduleState = updatedModule }, outgoingSignals)

let rec processWorklist (modules: Map<ModuleId, Module>) (worklist : Signal list) (signalHistory: Signal list): Map<ModuleId, Module> * Signal list =
    match worklist with
    | [] -> modules, List.rev signalHistory
    | first :: rest ->
        if not <| Map.containsKey first.toModule modules then
            processWorklist modules rest (first :: signalHistory)
        else 
            let modu = modules[first.toModule]
            let updatedModule, newSignals = handleSignal modu first
            let updatedModules = modules |> Map.add modu.moduleId updatedModule
            processWorklist updatedModules (rest @ newSignals) (first :: signalHistory)

let pushButton (modules: Map<ModuleId, Module>) =
    let firstSignal = { Signal.fromModule = "broadcaster"
                        Signal.toModule = "broadcaster"
                        Signal.pulse = false }
    
    processWorklist modules [firstSignal] []    

let calculateScore (signals: Signal list) =
    let low, high =
        signals
        |> List.fold
            (fun (lows, highs) s ->
                if s.pulse
                then (lows, highs + 1)
                else (lows + 1, highs))
            (0,0)
    low * high
    
let q20a () =
    let input = File.ReadAllText "20.txt"
    let modules = 
        run Parsing.modulesParser input |> unwrapParserResult
        |> setConjuctionStates
        |> List.map (fun m -> m.moduleId, m)
        |> Map.ofList
    
    let _, signals = 
        [1..1000]
        |> List.fold
            (fun (mods, signals) _ ->
                let updatesModules, newSignals = pushButton mods
                (updatesModules, signals @ newSignals))
            (modules, [])    
        
    let score = calculateScore signals
    Console.WriteLine $"20a: {score} "

let q20b () =
    let input = File.ReadAllText "20.txt"
    let modules = 
        run Parsing.modulesParser input |> unwrapParserResult
        |> setConjuctionStates
        |> List.map (fun m -> m.moduleId, m)
        |> Map.ofList

    let findSignal (s:Signal) =
        Seq.initInfinite (fun i -> i + 1)
        |> Seq.scan
            (fun (_, mods, _) i ->
                let updatesModules, newSignals = pushButton mods
                (i ,updatesModules, newSignals))
            (1, modules, [])
        |> Seq.find (fun (i, _, signals) -> List.contains s signals )
        |> fun (i, _, _) -> i
        
    let qs = int64 <| findSignal {Signal.fromModule = "ph";Signal.toModule = "qs"; pulse = false }
    let xj = int64 <| findSignal {Signal.fromModule = "mf";Signal.toModule = "xj"; pulse = false }
    let kz = int64 <| findSignal {Signal.fromModule = "zp";Signal.toModule = "kz"; pulse = false }
    let km = int64 <| findSignal {Signal.fromModule = "jn";Signal.toModule = "km"; pulse = false }
    Console.WriteLine $"qs {qs}"
    Console.WriteLine $"xj {xj}"
    Console.WriteLine $"kz {kz}"
    Console.WriteLine $"km {km}"
    
    let result =
        qs
        |> lowestCommonMultiple xj
        |> lowestCommonMultiple kz
        |> lowestCommonMultiple km

    Console.WriteLine $"20b: {result} "   