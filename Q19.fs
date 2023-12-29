module q19

open System
open System.IO
open FParsec
open Microsoft.FSharp.Core
open utils

type Category = X|M|A|S

type Part = {
    X : int64
    M : int64
    A : int64
    S : int64
}
type WorkflowId = string

type Action =
    | Reject
    | Accept
    | Goto of WorkflowId
with override x.ToString() =
        match x with
        | Reject -> "Reject"
        | Accept -> "Accept"
        | Goto x -> $"Goto {x}"

type ConditionExp =
    | GreaterThen
    | LesserThen
    
type Condition = {
    Category : Category
    ConditionExp : ConditionExp
    ConditionValue : int64
}
with override x.ToString() =
        $"{x.Category} {x.ConditionExp} {x.ConditionValue}"

type WorkflowRule =
    | Action of Action
    | ConditionalAction of Condition * Action
with override x.ToString() =
        match x with
        | Action a -> $"{a}"
        | ConditionalAction (c,a) -> $"[{c} {a}]"
        
module Parsing =
    let workflowIdParser: Parser<WorkflowId,Unit> = many asciiLower |>> (Array.ofList >> String)  
    let actionParser : Parser<Action, Unit> = choice [
        skipChar 'R' |>> fun _ -> Reject
        skipChar 'A' |>> fun _ -> Accept
        workflowIdParser |>> Goto 
    ]
    let categoryParser = choice [
        pchar 'x' |>> fun _ -> X
        pchar 'm' |>> fun _ -> M
        pchar 'a' |>> fun _ -> A
        pchar 's' |>> fun _ -> S
    ]
    let conditionExpParser : Parser<ConditionExp, Unit> =
        choice [
            pchar '<' |>> fun _ -> ConditionExp.LesserThen
            pchar '>' |>> fun _ -> ConditionExp.GreaterThen
        ]
    let conditionParser = categoryParser .>>.? conditionExpParser .>>.? pint64
                          |>> fun ((c, exp), v) -> { Condition.Category = c
                                                     Condition.ConditionExp = exp
                                                     Condition.ConditionValue = v }
    let conditionAndActionParser = conditionParser .>> skipChar ':' .>>. actionParser
    let ruleParser = choice [
        conditionAndActionParser |>> WorkflowRule.ConditionalAction
        actionParser |>> WorkflowRule.Action
    ]
    let rulesParser = sepBy ruleParser (skipChar ',')
    let workflowParser = workflowIdParser .>> pchar '{' .>>. rulesParser .>> pchar '}'
    let workflowsParser = sepEndBy workflowParser skipNewline |>> Map.ofList
    let partParser: Parser<Part,unit> =
                     skipString "{x=" >>. pint64 .>>
                     skipString ",m=" .>>. pint64 .>>
                     skipString ",a=" .>>. pint64 .>>
                     skipString ",s=" .>>. pint64 .>>
                     skipString "}" |>> fun (((x, m), a), s) -> { X=x;M=m;A=a;S=s }
    let partsParser = sepBy partParser newline
    let inputParser = workflowsParser .>> newline .>>. partsParser

let evaluate (condition : Condition) (part: Part) =
    let xmas = match condition.Category with
               | X -> part.X
               | M -> part.M
               | A -> part.A
               | S -> part.S
    match condition.ConditionExp with
    | LesserThen -> xmas < condition.ConditionValue
    | GreaterThen -> xmas > condition.ConditionValue

let processWorkflow (workflows : Map<String, WorkflowRule list>) (part: Part) =
    let rec performAction =
        function
        | Accept -> true
        | Reject -> false
        | Goto x -> processWorkflow x
    and processRules (rules : WorkflowRule list) =
        match rules with
        | [] -> failwith "can't have empty rules"
        | Action a :: _ -> performAction a
        | ConditionalAction (cond, act) :: rest ->
            if evaluate cond part then performAction act
            else processRules rest    
    and processWorkflow (wfId: WorkflowId) : bool =
        processRules workflows[wfId]
        
    processWorkflow "in"
    
let allWaysToA (workflows : Map<String, WorkflowRule list>): (WorkflowRule * bool) list list=
    let rec performAction (visited: (WorkflowRule * bool) list) (a: Action) : (WorkflowRule * bool) list list =
        match a with
        | Accept -> [visited]
        | Reject -> []
        | Goto x -> processWorkflow x visited
    and processRules (rules : WorkflowRule list) (visited: (WorkflowRule * bool) list): (WorkflowRule * bool) list list=
        match rules with
        | [] -> failwith "can't have empty rules"
        | Action a :: _ -> performAction ((Action a, true) :: visited) a
        | ConditionalAction (cond, act) :: rest -> 
                performAction ((ConditionalAction (cond, act), true) :: visited) act @
                processRules rest ((ConditionalAction (cond, act), false) :: visited) 
            
    and processWorkflow (wfId: WorkflowId) (visited: (WorkflowRule * bool) list) : (WorkflowRule * bool) list list=
        processRules workflows[wfId] visited
            
    processWorkflow "in" []
    |> List.map List.rev
    
let q19a () =
    let input = File.ReadAllText "19.txt"
    let workflows, parts = run Parsing.inputParser input |> unwrapParserResult
    let result =
        parts
        |> List.filter (processWorkflow workflows)
        |> List.sumBy (fun part -> part.X + part.M + part.A + part.S )
        
    Console.WriteLine $"19a: {result} "

type PartCategoryRange = Map<Category, int64*int64>
let asString (p : PartCategoryRange) =
    $"{p[X]} {p[M]} {p[A]} {p[S]}"
let startPartCategoryRange =
    [ (X, (1L,4000L))
      (M, (1L,4000L))
      (A, (1L,4000L))
      (S, (1L,4000L))
    ]|> Map.ofList

let restrict (partCategoryRange: PartCategoryRange) ((workflowRule, shouldBeTrue) : WorkflowRule * bool) =
    match workflowRule with
    | Action _ -> partCategoryRange
    | ConditionalAction (c, a)  ->
        match c.ConditionExp, shouldBeTrue with
        | GreaterThen, true ->
            let min, max = partCategoryRange[c.Category]
            let newMin = Math.Max(min, c.ConditionValue + 1L)
            partCategoryRange |> Map.add c.Category (newMin, max)
        | GreaterThen, false ->
            let min, max = partCategoryRange[c.Category]
            let newMax = Math.Min(max, c.ConditionValue)
            partCategoryRange |> Map.add c.Category (min, newMax)
        | LesserThen, true ->
            let min, max = partCategoryRange[c.Category]
            let newMax = Math.Min(max, c.ConditionValue - 1L)
            partCategoryRange |> Map.add c.Category (min, newMax)            
        | LesserThen, false ->  
            let min, max = partCategoryRange[c.Category]
            let newMin = Math.Max(min, c.ConditionValue)
            partCategoryRange |> Map.add c.Category (newMin, max)

let caclulateCombinations (p: PartCategoryRange) =
    let combs (fromVal,toVal) = Math.Max(toVal - fromVal + 1L, 0L)
    combs p[X] * combs p[M] * combs p[A] * combs p[S]
    
let q19b () =
    let input = File.ReadAllText "19.txt"
    let workflows, _ = run Parsing.inputParser input |> unwrapParserResult
    
    let result =
        allWaysToA workflows
        |> List.map (List.fold restrict startPartCategoryRange)
        |> List.sumBy caclulateCombinations
        
    Console.WriteLine $"19b: {result}"
    
    
   