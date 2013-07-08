namespace BitReaderWriterFs

open System
open System.IO

[<AutoOpen>]
module internal Ext =
    type Byte with
        member this.ToBitSequence () = seq {
            for i in 7..-1..0 do 
                yield ((this >>> i) &&& 1uy) = 1uy
            }

    type Stream with
        /// Extension method for getting a sequence of bits from a stream
        member this.ToBitSequence () = 
            let readByte = ref 0
            seq {
                while (readByte := this.ReadByte(); !readByte >= 0) do
                    for i in 7..-1..0 do 
                        yield ((!readByte >>> i) &&& 1) = 1
            }