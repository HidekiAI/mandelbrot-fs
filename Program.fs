module Main

open Mandelbrot

[<EntryPoint>]
let main argv =
    //let screenWidth = 250L // 100L - i.e. use $(tput cols) to get actual width
    //let screenHeight = 59L // 24L
    let screenWidth = 100L
    let screenHeight = 24L
    TextMandelbrot.calculateMandelbrot 1000L -2.0 1.0 -1.0 1.0 screenWidth screenHeight
    |> TextMandelbrot.getMandelbrotRow
    |> TextMandelbrot.dumpRows

    0
