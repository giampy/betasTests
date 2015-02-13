module Data
    open Utils
    let D0= [|true;true;false;true;true;true;false;true;false;false;true;true;true;true;true;false;false;false;false;false|]
    let D1=[|true;true;false;true;true;true;false;true;false;false;true;true;true;true;true;false;false;false;false;true|]
    let n=D0.Length
    let trials = 100        
    let aPrior = 1.0
    let bPrior = 1.0
    let score A B C D= 
        (-1.0) * (HellingerDistance A B C D)
    let aux = (n, aPrior, bPrior)

