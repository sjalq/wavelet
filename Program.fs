open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats
open SixLabors.ImageSharp.Processing

type Img =
    { red: int [,]
      green: int [,]
      blue: int [,]
      width: int
      height: int }

let getImageData (filePath: string) =
    use image = Image.Load<Rgba32>(filePath)
    let width = image.Width
    let height = image.Height

    // Find nearest 2^x value
    let maxSize = max width height
    let newSize = pown 2 (int (ceil (log (float maxSize) / log 2.0)))

    // Resize the image
    image.Mutate(fun x -> x.Resize(newSize, newSize) |> ignore)

    let rChannel = Array2D.zeroCreate newSize newSize
    let gChannel = Array2D.zeroCreate newSize newSize
    let bChannel = Array2D.zeroCreate newSize newSize

    for x in 0 .. newSize - 1 do
        for y in 0 .. newSize - 1 do
            let pixel = image.[x, y]
            rChannel.[x, y] <- int pixel.R
            gChannel.[x, y] <- int pixel.G
            bChannel.[x, y] <- int pixel.B

    { red = rChannel
      green = gChannel
      blue = bChannel
      width = width
      height = height }


let writeImageData filepath img =
    let width = img.red.GetLength(0)
    let height = img.red.GetLength(1)

    use image = new Image<Rgba32>(width, height)

    // Set each pixel's color
    for x in 0 .. width - 1 do
        for y in 0 .. height - 1 do
            let r = byte img.red.[x, y]
            let g = byte img.green.[x, y]
            let b = byte img.blue.[x, y]
            let color = Rgba32(r, g, b)
            image.[x, y] <- color

    // Resize the image
    image.Mutate(fun ctx -> ctx.Resize(img.width, img.height) |> ignore)

    // Save the image
    image.Save(filepath)


// Usage


let log message value =
    printfn "%s:\n%A" message value
    value

let getWH (arr: 'a [,]) =
    Array2D.length2 arr, Array2D.length1 arr


let transpose (arr: 'a [,]) =
    let width = Array2D.length1 arr
    let height = Array2D.length2 arr
    Array2D.init height width (fun x y -> arr.[y, x])


let logXY msg (arr: int [,]) =
    // Find the maximum absolute value in the array
    let maxVal = arr |> Seq.cast<int> |> Seq.map abs |> Seq.max

    // Get the length of the maximum absolute value + 1 for potential negative sign
    let maxLen = maxVal.ToString().Length + 1

    // Iterate through the array and print each number
    printfn "%s:" msg

    for j in 0 .. (arr.GetLength(1) - 1) do
        for i in 0 .. (arr.GetLength(0) - 1) do
            // Format number with leading zeros and a sign
            let num = arr.[i, j]
            printf "%+0*d " maxLen num

        printfn ""

    arr



let encode (pixels: int [,]) =
    let width = Array2D.length1 pixels
    let height = Array2D.length2 pixels


    let getAvg x y (arr: int [,]) =
        let x = (x / 2) * 2
        let y = (y / 2) * 2

        let sum =
            arr.[x, y]
            + arr.[x + 1, y]
            + arr.[x, y + 1]
            + arr.[x + 1, y + 1]

        sum / 4

    let avgs =
        Array2D.init (width / 2) (height / 2) (fun x y -> getAvg (x * 2) (y * 2) pixels)

    let topRight =
        Array2D.init (width / 2) (height / 2) (fun x y -> pixels.[x * 2 + 1, y * 2] - avgs.[x, y])

    let bottomLeft =
        Array2D.init (width / 2) (height / 2) (fun x y -> pixels.[x * 2, y * 2 + 1] - avgs.[x, y])

    let bottomRight =
        Array2D.init (width / 2) (height / 2) (fun x y -> pixels.[x * 2 + 1, y * 2 + 1] - avgs.[x, y])


    Array2D.init width height (fun x y ->
        let halfWidth = width / 2
        let halfHeight = height / 2

        match x < halfWidth, y < halfHeight with
        | true, true -> avgs.[x, y]
        | false, true -> topRight.[x - halfWidth, y]
        | true, false -> bottomLeft.[x, y - halfHeight]
        | false, false -> bottomRight.[x - halfWidth, y - halfHeight])


let rec cycle fn cycles arr =
    if cycles <= 0 then
        arr
    else
        arr |> fn |> cycle fn (cycles - 1)


let encodeLvl minLvl (pixels: int [,]) =
    let width = Array2D.length1 pixels
    let height = Array2D.length2 pixels

    let cycles =
        System.Math.Log(float width)
        / System.Math.Log(2.0)
        |> int

    cycle encode (cycles - minLvl) pixels

let decode (pixels: int [,]) =
    let width = Array2D.length1 pixels
    let height = Array2D.length2 pixels

    let halfWidth = width / 2
    let halfHeight = height / 2

    let avg x y = pixels.[x / 2, y / 2]
    let topRight x y = pixels.[x / 2 + halfWidth, y / 2]
    let bottomLeft x y = pixels.[x / 2, y / 2 + halfHeight]

    let bottomRight x y =
        pixels.[x / 2 + halfWidth, y / 2 + halfHeight]

    Array2D.init width height (fun x y ->
        match x % 2 = 0, y % 2 = 0 with
        | false, true -> topRight x y + avg x y
        | true, false -> bottomLeft x y + avg x y
        | false, false -> bottomRight x y + avg x y
        | true, true ->
            avg x y
            - topRight x y
            - bottomLeft x y
            - bottomRight x y)


let decodeLvl minLvl (pixels: int [,]) =
    let width = Array2D.length1 pixels
    let height = Array2D.length2 pixels

    let cycles =
        System.Math.Log(float width)
        / System.Math.Log(2.0)
        |> int

    cycle decode (cycles - minLvl) pixels

let diff (a: int [,]) (b: int [,]) =
    let width = Array2D.length1 a
    let height = Array2D.length2 a

    Array2D.init width height (fun x y -> a.[x, y] - b.[x, y])

let sum (m: int [,]) =
    let width = Array2D.length1 m
    let height = Array2D.length2 m

    let mutable i = 0

    Array2D.init width height (fun x y -> i <- i + abs (m.[x, y]))
    |> ignore

    i


let makeRandomPixels width height maxVal =
    Array2D.init width height (fun x y -> System.Random().Next(0, maxVal))
//|> Array2D.map float

let takeTopLeft (pixels: int [,]) =
    let width = Array2D.length1 pixels
    let height = Array2D.length2 pixels
    Array2D.init (width / 2) (height / 2) (fun x y -> pixels.[x, y])



let rec encodeRec lvls pixels =
    let width = Array2D.length1 pixels
    let height = Array2D.length2 pixels


    let avgDiff =
        if lvls = 0 then
            pixels
        else
            encode pixels

    let partToReplace =
        if lvls = 0 then
            pixels
        else
            let topLeft = takeTopLeft avgDiff 
            encodeRec (lvls-1) topLeft

    let result =
        Array2D.init width height (fun x y ->
            if x < width / 2 && y < height / 2 then
                partToReplace.[x, y]
            else
                avgDiff.[x, y])

    result


let rec decodeRec lvls pixels =
    if lvls = 2 then
        decode pixels
    else
        let topLeft = takeTopLeft pixels 
        decodeRec (lvls-1) topLeft


let makeIncrementing2DArray width height =
    Array2D.init width height (fun x y -> x * 10 + y)

makeIncrementing2DArray 8 8
|> logXY "makeIncrementing2DArray"
|> ignore

let pixels =
    [ [ 1; 2; 3; 4; 5; 6; 7; 8 ]
      [ 9; 10; 11; 12; 13; 14; 15; 16 ]
      [ 17; 18; 19; 20; 21; 22; 23; 24 ]
      [ 25; 26; 27; 28; 29; 30; 31; 32 ]
      [ 33; 34; 35; 36; 37; 38; 39; 40 ]
      [ 41; 42; 43; 44; 45; 46; 47; 48 ]
      [ 49; 50; 51; 52; 53; 54; 55; 56 ]
      [ 57; 58; 59; 60; 61; 62; 63; 64 ] ]
    |> array2D
    //|> Array2D.map float
    |> transpose

pixels
|> log "pixels"
|> encodeLvl 4
|> log "encoded"
|> decodeLvl 4
|> log "decoded"
|> diff pixels
|> log "diff"
|> ignore

let d1 = 128
let d2 = 128
let maxVal = 256 //2.0**16.0 |> int |> log "maxVal"

let minLvl = 9

let errorRate errorSum =
    (float errorSum |> log "errorSum")
    / ((float d1) * (float d2) * (float maxVal))

let rnd = makeRandomPixels d1 d2 maxVal

rnd
//|> log "original"
|> encodeLvl minLvl
//|> log "encoded"
|> decodeLvl minLvl
//|> log "decoded"
|> diff rnd
//|> log "diff"
|> sum
|> log "totalError"
|> errorRate
|> log "errorRate"
|> ignore




let filePath = @"image.jpeg"
let outputPath = @"image2.jpeg"
let img = getImageData filePath

let lvls = 3

let encodedImg =
    { img with
        red = img.red |> encodeRec lvls
        green = img.green |> encodeRec lvls
        blue = img.blue |> encodeRec lvls }

writeImageData outputPath encodedImg

let decodedImg =
    { encodedImg with
        red = encodedImg.red |> decodeRec lvls
        green = encodedImg.green |> decodeRec lvls
        blue = encodedImg.blue |> decodeRec lvls }

writeImageData @"image3.bmp" decodedImg
