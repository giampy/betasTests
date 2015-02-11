namespace learningNoNoise

module test1 = 
    open MicrosoftResearch.Infer.Fun.FSharp.Syntax
    open MicrosoftResearch.Infer.Fun.FSharp.Inference
    open FSharp.Charting
    open System

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

    open MicrosoftResearch.Infer.Distributions
    open System.Drawing
    open Utils

    let modelofUrna = makeModel <@ model @>
    let PostTheta0: Beta = inferModel modelofUrna D0
    let PostTheta1: Beta = inferModel modelofUrna D1
    
    
    printfn "Real 0: %A\n" PostTheta0
    printfn "Real 1: %A\n" PostTheta1
    
    let raw=[| (createGraph PostTheta0 0.0 1.0 0.01 "Beta 0 No Noise" Color.Black);
               (createGraph PostTheta1 0.0 1.0 0.01 "Beta 1 No Noise" Color.Red) 
            |]

    let A1=Chart.Combine raw
    let out=A1.ShowChart()


    open System.Windows.Forms
    [<EntryPoint>]
    let main argv = 
        Application.Run()
        0