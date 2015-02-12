module test
open MicrosoftResearch.Infer.Fun.FSharp.Syntax
open MicrosoftResearch.Infer.Fun.FSharp.Inference
open MathNet.Numerics
open System
open Utils

let D0=[|true;true;false;true;true;true;false;true;false;false;true;true;true;true;true;false;false;false;false;false|]
let D1=[|true;true;false;true;true;true;false;true;false;false;true;true;true;true;true;false;false;false;false;true|]
let n=D0.Length
let trials = 1000        
let aPrior = 1.0
let bPrior = 1.0

//THE MODEL        
[<ReflectedDefinition>]
let prior a b=
    random (Beta(a,b))
[<ReflectedDefinition>]
let model obs=
    let priorTheta=prior aPrior bPrior
    observe(obs=[|for v in obs -> random(Bernoulli(priorTheta))|])
    priorTheta

open MicrosoftResearch.Infer.Distributions

//Inference
let modelofUrna = makeModel <@ model @>
let PostTheta0: Beta = inferModel modelofUrna D0
let PostTheta1: Beta = inferModel modelofUrna D1

//Noise added to output
let array0BetaOutput:array<float*float>= 
            [|for i in 0.. trials-1 -> expMech PostTheta0 epsilon sensitivity n aPrior bPrior |]
let array1BetaOutput:array<float*float>= 
            [|for i in 0.. trials-1 -> expMech PostTheta1 epsilon sensitivity n aPrior bPrior |]

//Graphical part
printGraphs array0BetaOutput array1BetaOutput

open System.Windows.Forms
[<EntryPoint>]
let main argv = 
    Application.Run()
    0