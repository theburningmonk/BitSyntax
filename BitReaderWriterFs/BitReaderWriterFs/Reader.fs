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

        let readBits count (output : byte[]) = 
            let rec loop count outputIdx outputBytePos = 
                if count > 0 then
                    if bufferBytePos = 8 then 
                        bufferBytePos <- 0
                        bufferIdx     <- bufferIdx + 1
                    if bufferIdx >= bufferSize then
                        readIntoBuffer()
                   
                    let outputBytePos, outputIdx = 
                        match outputBytePos with
                        | 8 -> 0, outputIdx + 1
                        | n -> n, outputIdx

                    // bufferBytePos = 3, outputBytePos = 2, count = 2, canTake = 2
                    let canTake = min count (8 - bufferBytePos - outputBytePos)            

                    let outputByte = output.[outputIdx]

                    //                                          _
                    // outputByte                             01000000
                    let byte =                             //    __
                        buffer.[bufferIdx]                 // 10011010
                        >>> (8 - bufferBytePos - canTake)  // 00010011
                        <<< (8 - canTake - outputBytePos)  // 00110000
                        ||| outputByte                     // 01110000

                    if canTake < count then // the last byte needs to be filled in right-to-left
                        //                                    00000111
                        output.[outputIdx] <- byte >>> (8 - outputBytePos - canTake)
                    else  // others need to leave space on the right for next byte 
                        output.[outputIdx] <- byte         // 01110000

                    bufferBytePos <- (bufferBytePos + canTake) % 8
                    readBits (count - canTake) outputIdx (outputBytePos + canTake) output

            loop 0 0

        let readFrom (Reader(count, convert)) =
            match count with
            | N n ->
                // TODO : use pool buffer
                let arrSize = if count % 8 <> 0 then count / 8 + 1 else count / 8
                let arr     = Array.zeroCreate<byte> arrSize
                readBits count arr
                convert arr
            | Rest -> 
                failwith "todo"

        let bind reader cont = cont (readFrom reader)

        member this.Bind(reader, cont) = bind reader cont