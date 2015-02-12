namespace learningFromNoisyInput 
    module test2 =
        open MicrosoftResearch.Infer.Fun.FSharp.Syntax
        open MicrosoftResearch.Infer.Fun.FSharp.Inference
        open System.Windows.Forms
        open FSharp.Charting
        open System.Drawing
        open System
        open Utils


        let epsilon = 0.01
        let constant= 1.0+ Math.Exp(epsilon/2.0)
        let trials=150

        let D0=[|true;true;false;true;true;true;false;true;false;false;true;true;true;true;true;false;false;false;false;false|]
        let D1=[|true;true;false;true;true;true;false;true;false;false;true;true;true;true;true;false;false;false;false;true|]

        [<ReflectedDefinition>]
        let prior a b=
            random (Beta(a,b))

        [<ReflectedDefinition>]
        let model obs=
            let priorTheta=prior 1.0 1.0
            observe(obs=[|for v in obs -> random(Bernoulli(priorTheta))|])
            priorTheta
        
        //Create trials noisy copy of the database DB0 and DB1
        let db0n=createNoisyCopy trials D0 epsilon constant
        let db1n=createNoisyCopy trials D1 epsilon constant

        let modelofUrna = makeModel <@ model @>

        open MicrosoftResearch.Infer.Distributions

        let arrayBeta0:array<Beta>=[| for i in 0..trials-1->(inferModel modelofUrna db0n.[i] )|] 
        let arrayBeta1:array<Beta>=[| for i in 0..trials-1->(inferModel modelofUrna db1n.[i] )|]


        (*let draw0 = [| for b in arrayBeta0 -> 
                            (createGraph b 0.0 1.0 0.01 "Betas from DB0 with Noise" Color.Black);
                    |]
        let draw1 = [| for b in arrayBeta1 -> 
                            (createGraph b 0.0 1.0 0.01 "Betas from DB1 with Noise" Color.Red);
                    |]  

        printfn "NoisyInput 0: %A\n" arrayBeta0
        printfn "NoisyInput 1: %A\n" arrayBeta1
        let A2=Chart.Combine draw0
        let A3=Chart.Combine draw1
        let a2=A2.ShowChart()
        let a3=A3.ShowChart() *)


        let histo0 = [|for j in (Seq.distinct arrayBeta0) -> (j, count j arrayBeta0)|]
        let histo1 = [|for j in (Seq.distinct arrayBeta1) -> (j, count j arrayBeta1)|]

        prettyPrintHisto histo0
        printfn "******************************"
        prettyPrintHisto histo1

        open FnuPlot
        open System.Drawing

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
        [<EntryPoint>]
        let main argv = 
            Application.Run()
            0   