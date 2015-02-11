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
open MicrosoftResearch.Infer.Fun.FSharp.Syntax

let addNoise D epsilon constant = [|for i in D -> if i=true then (random(Bernoulli(Math.Exp(epsilon/2.0)/(constant)))) 
                                                    else (random(Bernoulli(1.0/(constant))))|]
let createNoisyCopy trials DB epsilon constant =
    [|for j in 0..trials-1 -> addNoise DB epsilon constant|]


let HellingerDistance a0 b0 a1 b1=
    let first=(a0+a1)/2.0 
    let second=((b0+b1)/2.0)
    let num=betaf first second
    let denum=(betaf a0 b0)*(betaf a1 b1)
    let denums=Math.Sqrt(denum)
    let retsq=1.0-(num/denums)
    Math.Sqrt retsq
    //retsq
    //Math.Sqrt((a0-a1)*(a0-a1)+(b0-b1)*(b0-b1))

let delta=Math.Sqrt(1.0-(Math.PI/4.0))

let score iA iB oA oB=(-(HellingerDistance iA iB oA oB))
let computeExp eps d a b i j=Math.Exp(((eps)/(2.0*d))*(score a b i j))

let rec computeConstFixedI iA iB oA iJ N cum eps=
    if (oA+iJ>N) then cum
    else 
       computeConstFixedI iA iB oA (iJ+1.0) N (cum+(computeExp eps delta iA iB oA iJ)) eps

let rec computeConst iA iB N iI cum eps=
    if iI>N then cum
    else 
        computeConst iA iB N (iI+1.0) (cum+(computeConstFixedI iA iB iI 1.0 N 0.0 eps)) eps


let constantAB a b N eps=computeConst a b (float N) (1.0) (0.0) (eps)

