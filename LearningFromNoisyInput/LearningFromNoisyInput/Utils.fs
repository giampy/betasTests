module Utils
open MicrosoftResearch.Infer.Distributions
open MathNet.Numerics
open FSharp.Charting
open System
open FnuPlot
open System.Drawing

let gamma n= SpecialFunctions.Gamma n

let betaf a b=
    (gamma(a)*gamma(b))/gamma(a+b)

let betaPdf x a b=
    ((Math.Pow(x, a-1.0)*Math.Pow(1.0-x, b-1.0) )/(betaf a b))

let createGraph (theta:Beta) (init:float) (end_:float) (step:float) (title: String) (c: System.Drawing.Color) = 
                                    Chart.Line([
                                                    for j in init .. (step) .. end_ -> 
                                                                (j, 
                                                                  (betaPdf    j
                                                                    ((theta.TrueCount))  
                                                                    ((theta.FalseCount)) 
                                            )
                                            )
                                    ],Title=title,Color=c).WithYAxis(Max=8.0 )
open MicrosoftResearch.Infer.Fun.FSharp.Syntax

let expMech D epsilon constant = [|for i in D -> if i=true then (random(Bernoulli(Math.Exp(epsilon/2.0)/(constant)))) 
                                                    else (random(Bernoulli(1.0/(constant))))|]

let count a v=  (Array.filter (fun x -> x = a) v).Length


//prettyprint histograms (arrays of pairs of pairs x count)
let prettyPrintHisto (h: (Beta*int)[]) =
    for (beta, count) in h do
        printf "%f %f: " beta.TrueCount beta.FalseCount
        for elem in 1 .. count do
            printf "+"
        printfn ""


let showInfo arrayBeta0 arrayBeta1 =
        let histo0 = [|for j in (Seq.distinct arrayBeta0) -> (j, count j arrayBeta0)|]
        let histo1 = [|for j in (Seq.distinct arrayBeta1) -> (j, count j arrayBeta1)|]

        prettyPrintHisto histo0
        printfn "******************************"
        prettyPrintHisto histo1

        

        let path="C:\\Program Files (x86)\\gnuplot\\bin\\gnuplot.exe"
        let gp0 = new GnuPlot(path)

        let tit0 = [for  ((b, c)) in histo0 -> ( (b.TrueCount,b.FalseCount).ToString() )] 
        let tit1 = [for  ((b, c)) in histo1 -> ( (b.TrueCount,b.FalseCount).ToString() )] 
        gp0.Set ( style = Style(Solid), titles = Titles(x = tit0, xrotate = -90) )
        gp0.SendCommand("set term wxt title 'DB0 NoisyInput'")
        let gp1 = new GnuPlot(path)
        gp1.Set ( style = Style(Solid), titles = Titles(x = tit1, xrotate = -90) )
        gp1.SendCommand("set term wxt title 'DB1 NoisyInput'")
// Create a chart combining several histograms
        Series.Histogram ( data = [|for  ((b, c)) in histo0 -> (float c)|])  |> gp0.Plot
        Series.Histogram ( data = [|for  ((b, c)) in histo1 -> (float c)|])  |> gp1.Plot

