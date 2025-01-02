namespace Tic_tac_toe_FSharp_WebAppMVC.Models

open System

type ErrorViewModel =
    { RequestId: string }

    member this.ShowRequestId =
        not (String.IsNullOrEmpty(this.RequestId))
