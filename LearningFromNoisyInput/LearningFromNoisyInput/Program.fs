namespace learningFromNoisyInput 
    module test2 =
        open MicrosoftResearch.Infer.Fun.FSharp.Syntax
        open MicrosoftResearch.Infer.Fun.FSharp.Inference
        open System.Windows.Forms
        open Utils
        open Data

        //MODEL
        [<ReflectedDefinition>]
        let prior a b=
            random (Beta(a,b))

        [<ReflectedDefinition>]
        let model obs=
            let priorTheta=prior 1.0 1.0
            observe(obs=[|for v in obs -> random(Bernoulli(priorTheta))|])
            priorTheta
        
        //Exponential mechanism applied to the two databases "many" times (because we want to print the distributions)
        //Here the expMech doesn't take in input the score function (because in this cause we built it inside the expMech
        let db0n=[|for i in 0.. trials-1 -> (expMech D0 epsilon)|]
        let db1n=[|for i in 0.. trials-1 -> (expMech D1 epsilon)|]

        open MicrosoftResearch.Infer.Distributions
        //Inference from noisy data
        let modelofUrna = makeModel <@ model @>
        let arrayBeta0:array<Beta>=[| for i in 0..trials-1->(inferModel modelofUrna db0n.[i] )|] 
        let arrayBeta1:array<Beta>=[| for i in 0..trials-1->(inferModel modelofUrna db1n.[i] )|]
        


        //show results
        showInfo arrayBeta0 arrayBeta1
        
        [<EntryPoint>]
        let main argv = 
            Application.Run()
            0   