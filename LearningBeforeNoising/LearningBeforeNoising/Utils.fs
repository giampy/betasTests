module Utils
    open MicrosoftResearch.Infer.Distributions
    open MathNet.Numerics
    open FSharp.Charting
    open System
    let sensitivity=Math.Sqrt(1.0-(Math.PI/4.0))
    //let sensitivity=Math.Sqrt 2.0 //if we use euclidean distance
    let eps= 2.0
    let epsilon = eps / (2.0*sensitivity)
    let gamma n= SpecialFunctions.Gamma n

    let betaf a b=
        (gamma(a)*gamma(b))/gamma(a+b)

    let betaPdf x a b=
        ((Math.Pow(x, a-1.0)*Math.Pow(1.0-x, b-1.0) )/(betaf a b))

    let createGraph ((a, b):(float*float)) (init:float) (end_:float) (step:float) (title: String) (c: System.Drawing.Color) = 
                                    Chart.Line([
                                                    for j in init .. (step) .. end_ -> 
                                                                (j, 
                                                                  (betaPdf    j
                                                                    ((a))  
                                                                    ((b)) 
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
        Array.fold (fun acc (a1, b1) -> acc + Math.Exp((score a0 b0 a1 b1) * epsilon )) 0.0 (range N aPrior bPrior)

    let computeDistribution trueCount falseCount epsilon sensitivity N aPrior bPrior = 
        [|for (x,y) in (range (float N) aPrior bPrior) -> ((x,y), 
                                           (Math.Exp((score trueCount falseCount x y) * epsilon ) ) / computeK (trueCount, falseCount) epsilon sensitivity N aPrior bPrior)|]
    
    
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
    
    let rnd = System.Random() 

    let expMech (b: Beta) (eps:float) (sens:float) (N:int) (aPrior:float) (bPrior:float) =
        let distr=computeDistribution b.TrueCount b.FalseCount eps sens (float N) aPrior bPrior in
        let samplingArray = [|for index in 0 .. distr.Length  -> 
                                Array.fold (fun acc ((a,b), v) -> acc+v) 0.0 (Array.sub distr 0 index)
                            |] in
        let value0=(rnd.NextDouble ()) in
            let index=(findValueInIntervals samplingArray value0 0) in
                let (a,b)= (range (float N) aPrior bPrior).[index] in 
                        ((float a),(float b))
    open FSharp.Charting
    open FnuPlot
    open System.Drawing

    let printGraphs array0 array1 =
        let histo0 = [|for j in (Seq.distinct array0) -> (j, count j array0)|]
        let histo1 = [|for j in (Seq.distinct array1) -> (j, count j array1)|]

        prettyPrintHisto histo0
        printfn "******************************"
        prettyPrintHisto histo1

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
    let printBeta b=
        printfn "Real Posterior DB0\n %A\n" b
        printfn "***********************************\n"
    (*
    
    printBeta PostTheta0
printBeta PostTheta1
    
    *)