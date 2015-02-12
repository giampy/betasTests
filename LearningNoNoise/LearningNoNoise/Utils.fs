module Utils
open MicrosoftResearch.Infer.Distributions
open MathNet.Numerics
open FSharp.Charting
open System

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
