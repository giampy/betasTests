module test
open MicrosoftResearch.Infer.Fun.FSharp.Syntax
open MicrosoftResearch.Infer.Fun.FSharp.Inference
    
open MathNet.Numerics
open System
open Utils

let D0=[|true;true;false;true;true;true;false;true;false;false;true;true;true;true;true;false;false;false;false;false|]
let D1=[|true;true;false;true;true;true;false;true;false;false;true;true;true;true;true;false;false;false;false;true|]
        
let aPrior = 1.0
let bPrior = 1.0
let sensitivity=Math.Sqrt(1.0-(Math.PI/4.0))
let eps= 3.0
let epsilon = eps / (2.0*sensitivity)
let trials = 1000
//let sensitivity=Math.Sqrt 2.0 //if we use euclidean distance
        
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

printfn "Prior: Beta(%d,%d)[mean=%2.1f]" (int aPrior) (int bPrior) (aPrior/(aPrior+bPrior))
printfn "Real Posterior DB0\n %A\n" PostTheta0
printfn "***********************************\n"
printfn "Real Posterior DB1\n %A\n" PostTheta1
printfn "***********************************\n"
//Compute the distributions of the expmech on the points PostTheta0 PostTheta1
//from these two distributions we are next going to sample

let distr0 = computeDistribution PostTheta0.TrueCount PostTheta0.FalseCount epsilon sensitivity (float D0.Length) aPrior bPrior

let distr1 = computeDistribution PostTheta1.TrueCount PostTheta1.FalseCount epsilon sensitivity (float D1.Length) aPrior bPrior
                                        
printfn "Exponential distribution Induced by real Posterior DB0:\n %A\n" distr0
printfn "***********************************\n"
printfn "Exponential distribution Induced by real Posterior DB1:\n %A\n" distr1

let samplingArray0 = [|for index in 0 .. distr0.Length  -> 
                            Array.fold (fun acc ((a,b), v) -> acc+v) 0.0 (Array.sub distr0 0 index)
                        |]
let samplingArray1 = [|for index in 0 .. distr1.Length  -> 
                            Array.fold (fun acc ((a,b), v) -> acc+v) 0.0 (Array.sub distr1 0 index)
                        |]
//printfn "%A\n" samplingArray0
//printfn "%A\n" samplingArray1

//Sampling from ExpMech using uniform distribution

let rnd0 = System.Random()
let array0BetaOutput:array<float*float>=
    [|for tr in 0..trials-1 do
        let value0=(rnd0.NextDouble ()) in
            let index=(findValueInIntervals samplingArray0 value0 0) in
                let (a,b)= (range (float D0.Length) aPrior bPrior).[index] in yield ((float a),(float b))                          
        |]

let rnd1 = System.Random()

let array1BetaOutput:array<float*float>=
    [|for tr in 0..trials-1 do
        let value1=(rnd1.NextDouble ()) in
            let index=(findValueInIntervals samplingArray1 value1 0) in
                let (a,b)= (range (float D1.Length) aPrior bPrior).[index] in yield ((float a),(float b))                          
        |]

//printfn "%A\n" array0BetaOutput
//printfn "%A\n" array1BetaOutput











//GRAPHICAL PART
//this pretty print will make the job

let histo0 = [|for j in (Seq.distinct array0BetaOutput) -> (j, count j array0BetaOutput)|]
let histo1 = [|for j in (Seq.distinct array1BetaOutput) -> (j, count j array1BetaOutput)|]

prettyPrintHisto histo0
printfn "******************************"
prettyPrintHisto histo1

open FSharp.Charting
open FnuPlot
open System.Drawing

let path="C:\\Program Files (x86)\\gnuplot\\bin\\gnuplot.exe"
let gp0 = new GnuPlot(path)
let tit0 = [for  ((b, c)) in histo0 -> ( b.ToString() )] 
let tit1 = [for  ((b, c)) in histo1 -> ( b.ToString() )] 
gp0.Set ( style = Style(Solid), titles = Titles(x = tit0, xrotate = -90) )
let gp1 = new GnuPlot(path)
gp1.Set ( style = Style(Solid), titles = Titles(x = tit1, xrotate = -90) )
gp0.SendCommand("set term wxt title 'DB0 NoisyOutput'")
gp1.SendCommand("set term wxt title 'DB1 NoisyOutput'")

Series.Histogram ( data = [|for  ((b, c)) in histo0 -> (float c)|])  |> gp0.Plot
Series.Histogram ( data = [|for  ((b, c)) in histo1 -> (float c)|])  |> gp1.Plot

(*
let draw0 = [| for b in array0BetaOutput -> 
                            (createGraph b 0.0 1.0 0.01 "Betas from DB0 with Noise" Color.Black);
                    |]
let draw1 = [| for b in array1BetaOutput -> 
                            (createGraph b 0.0 1.0 0.01 "Betas from DB1 with Noise" Color.Red);
                    |]  
printfn "NoisyInput 0: %A\n" array0BetaOutput
printfn "NoisyInput 1: %A\n" array1BetaOutput
let A2=Chart.Combine draw0
let A3=Chart.Combine draw1
let a2=A2.ShowChart()
let a3=A3.ShowChart() 
*)
open System.Windows.Forms
[<EntryPoint>]
let main argv = 
    Application.Run()
    0
