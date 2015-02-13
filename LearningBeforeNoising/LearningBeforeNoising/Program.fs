module test
open MicrosoftResearch.Infer.Fun.FSharp.Syntax
open MicrosoftResearch.Infer.Fun.FSharp.Inference
open MathNet.Numerics
open System
open Utils
open Data

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

let distrOut0:DistrBeta= 
            [|for i in 0.. trials-1 -> (expMech PostTheta0 epsilon score aux) |]
let distrOut1:DistrBeta= 
            [|for i in 0.. trials-1 -> (expMech PostTheta1 epsilon score aux) |]

//Graphical part
printGraphs distrOut0 distrOut1

open System.Windows.Forms
[<EntryPoint>]
let main argv = 
    Application.Run()
    0