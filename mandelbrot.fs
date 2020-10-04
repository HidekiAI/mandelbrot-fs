namespace Mandelbrot

open System.Numerics

// see https://en.wikipedia.org/wiki/Mandelbrot_set (Brooks & Matelski)
module TextMandelbrot =
    let getMandelbrotRow (escapeVals: int64 list list): string list =
        escapeVals
        |> List.fold (fun (lines: string list) row ->
            let nextLine =
                row
                |> List.fold (fun (line: char list) col ->
                    // generate char[] which will convert to string later
                    let image =
                        match col with
                        // assume any norm that is 2 or less are not rendered
                        | cPos when (cPos <= 2L) -> ' ' // TODO: in Rust, you'd do 'match | 0..=2 => ''' - I'm told there are similar ways on F#
                        | cPos when (cPos <= 5L) -> '.'
                        | cPos when (cPos <= 10L) -> '•'
                        | cPos when (cPos <= 30L) -> '*'
                        | cPos when (cPos <= 100L) -> '+'
                        | cPos when (cPos <= 200L) -> 'x'
                        | cPos when (cPos <= 400L) -> '$'
                        | cPos when (cPos <= 700L) -> '#'
                        | _ -> '%' // anything larger than 700, is just too dark

                    image :: line) []
                // convert char[] to string (first convert List<char> to char[])
                |> List.toArray
            //|> string // possible do `new string` here? (see line below, though here it doesn't result to Type.ToString() of "System.Char[]")
            let strLine = new string(nextLine) // most strangest thing, I have to use 'new string(myArray)' or else this comes out as 'System.Char[]' (it's doing Type.ToString())
            strLine :: lines) []

    let mandelbrotAtPoint (cx: double) (cy: double) (maxIters: int64): int64 =
        let mutable z = Complex(0.0, 0.0) // z = x + yi
        let maxInclusive = maxIters + 1L // in rust, we'd do '..=maxIters' rather than +1 to make it inclusive
        let constC = Complex(cx, cy)        // calc outside the loop for (very minor) optimization
        ///
        // Imperitive method: (just bail out when norm exceeds desired value)
        // for i in 0L .. maxInclusive do
        //     if z.Magnitude > 2.0 then // norm: a^2 + b^2, aka Complex.Abs(z)
        //         return i // escape
        //     // z = z^2 + c (iteration of quadratic map)
        //     z <- z * z + Complex(cx, cy) // Q: optimize by making this constant? Or perhaps pass it in pre-calculated Complex number...
        // maxIters // escape at max
        ///
        // Unsure if I call this "functional" method, but using Fold (to find iterator doing z^2+c) and Choose to weed out unwanted result
        Seq.fold (fun (z: Complex, iter) i ->

            let result =
                match iter with
                | Some _ -> iter // just return what we already have without touching it
                | None ->
                    // only set escape state if it's not set yet
                    if ((z.Magnitude > 2.0) || (i = maxIters)) then Some i else iter // reuse last
            // z = z^2 + c (iteration of quadratic map)
            let newZ = z * z + constC
            (newZ, result)) (Complex(0.0, 0.0), None) [ 0L .. maxInclusive ]
        // Hopefully, the logic above have not touched `iter` once  it's set (escaped) and the tuple represents escape value
        |> fun myTuple ->
            snd myTuple
            |> fun v ->
                match v with
                | Some _v -> _v
                | None -> maxIters

    // NOTE: Due to the nature of list :: operator, it is currently reversing the order so images will be flipped
    let calculateMandelbrot
        (maxIters: int64)
        (xMin: double)
        (xMax: double)
        (yMin: double)
        (yMax: double)
        (width: int64)
        (height: int64)
        : int64 list list
        =
        let mutable all_rows: int64 list list = [ [] ] // interest point
        for imaginaryY in 0L .. height do
            let mutable row = list.Empty
            for imaginaryX in 0L .. width do
                let cx = xMin + (xMax - xMin) * (double (imaginaryX) / double (width))

                let cy = yMin + (yMax - yMin) * (double (imaginaryY) / double (height))

                let escapedAt = mandelbrotAtPoint cx cy maxIters
                row <- escapedAt :: row // append to head of the list (image will be flipped horizontally)
            all_rows <- row :: all_rows // append to the top of the row list (image will be bottom-up, flipped vertically), but fortunately upper and lower quadrants will be symmetric so it won't be noticied
        all_rows

    let dumpRows (rows: string list) =
        for row in rows do
            printf "%s\n" row
