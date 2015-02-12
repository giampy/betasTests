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

    (*Used to add noise to the DB*)
    let addNoise D epsilon constant = 
                        [|for i in D -> if i=true then (random(Bernoulli(Math.Exp(epsilon/2.0)/(constant)))) 
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
        //Math.Sqrt((a0-a1)*(a0-a1)+(b0-b1)*(b0-b1)) //if we use euclidean distance



    let score iA iB oA oB=(-(HellingerDistance iA iB oA oB))

    let range N aPrior bPrior= [|for a in aPrior .. aPrior + N -> (a, N + aPrior + bPrior - a ) |]

    let computeK (a0, b0) epsilon sensitivity N aPrior bPrior=   
        Array.fold (fun acc (a1, b1) -> acc + Math.Exp((score a0 b0 a1 b1) * (2.0/(sensitivity*epsilon)) )) 0.0 (range N aPrior bPrior)

    let computeDistribution trueCount falseCount epsilon sensitivity N aPrior bPrior = 
        [|for (x,y) in (range (float N) aPrior bPrior) -> ((x,y), 
                                           (Math.Exp((score trueCount falseCount x y) * (2.0/                        (sensitivity*epsilon)) ) ) / computeK (trueCount, falseCount) epsilon sensitivity N aPrior bPrior)|]
    
    
    let rec findValueInIntervals (intervals:float[]) v index:int=
     if index<=intervals.Length-2 then
            if (intervals.[index]<=v && v<intervals.[index+1]) then
                 index
            else 
                 findValueInIntervals intervals v (index+1)  
     else  
        printfn "Should never happen: %f %A %d\n" v intervals index
        0

    //used to count element in arrays to make histograms
    let count a v=  (Array.filter (fun x -> x = a) v).Length


    //prettyprint histograms (arrays of pairs of pairs x count)
    let prettyPrintHisto h =
        for ((a,b), count) in h do
        printf "%f %f: " a b
        for elem in 1 .. count do
            printf "+"
        printfn ""
