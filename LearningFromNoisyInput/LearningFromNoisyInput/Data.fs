module Data
    open System
    let trials=100
    let D0=[|true;true;false;true;true;true;false;true;false;false;true;true;true;true;true;false;false;false;false;false|]
    let D1=[|true;true;false;true;true;true;false;true;false;false;true;true;true;true;true;false;false;false;false;true|]
    let n=D0.Length
    let eps = 3.0 //eps-DP
    let epsilon =  eps/(2.0*(float n))
    let constant= 1.0+ Math.Exp(epsilon/2.0)

