namespace BitReaderWriterFs

open System
open System.IO

module BitReaderWorkflow =
    exception InsufficientBytes

    type Count      = | N of int | Rest
    type Reader<'a> = Reader of Count * (byte[] -> 'a)
   
    type BitReaderBuilder (stream : Stream) =
        let mutable buffer        = Array.zeroCreate<byte> 1024
        let mutable bufferSize    = 0
        let mutable bufferIdx     = 0
        let mutable bufferBytePos = 0

        let readIntoBuffer () =
            match stream.Read(buffer, 0, 1024) with
            | -1 -> raise InsufficientBytes
            | n  -> bufferSize    <- n
                    bufferIdx     <- 0
                    bufferBytePos <- 0

        let rec readBits count outputBytePos outputIdx (output : byte[]) = 
            // bufferBytePos = 3, outputBytePos = 2, count = 2, canTake = 2
            let canTake = min count (8 - bufferBytePos - outputBytePos)            

            let outputByte = output.[outputIdx]

            //                                          _
            // outputByte                             01000000
            //                                           __
            let byte = buffer.[bufferIdx]          // 10011010
            byte >>> (8 - bufferBytePos - canTake) // 00010011
                 <<< (8 - canTake - outputBytePos) // 00110000
                 ||| outputByte                    // 01110000
            |> (fun byte ->
                if canTake < count then // the last byte needs to be filled in right-to-left
                    //                                00000111
                    output.[outputIdx] <- byte >>> (8 - outputBytePos - canTake)
                else  // others need to leave space on the right for next byte 
                    output.[outputIdx] <- byte)    // 0111000

        let readFrom (reader : Reader<'a>) =
            42

        let bind reader cont = cont (readFrom reader)

        member this.Bind(reader, cont) = bind reader cont