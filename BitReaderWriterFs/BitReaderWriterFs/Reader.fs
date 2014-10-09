namespace BitReaderWriterFs

open System
open System.IO

module BitReaderWorkflow =
    type Count      = | N of int | Rest
    type Reader<'a> = Reader of Count * (byte[] -> 'a)
   
    type BitReaderBuilder (stream : Stream) =
        let readFrom (reader : Reader<'a>) =
            42

        let bind reader cont = cont (readFrom reader)

        member this.Bind(reader, cont) = bind reader cont