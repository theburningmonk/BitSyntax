#load "Ext.fs"
#load "Reader.fs"
//#load "Writer.fs"

open BitReaderWriterFs

open System
open System.IO
open System.Collections
open System.Collections.Generic

let stream = new MemoryStream()
stream.WriteByte(255uy)
stream.WriteByte(0uy)
stream.WriteByte(128uy)
stream.Write(Text.Encoding.ASCII.GetBytes("Hello World!"), 0, 12)

let bytes = Text.Encoding.ASCII.GetBytes("Hello World!")

let allT, allF, t, sevenF, msg = bitReader stream {
        let! allT   = Bits.ReadInt(4)
        do! Bits.Skip(8)
        let! allF   = Bits.ReadInt(4)
        let! t      = Bits.ReadBool()
        let! sevenF = Bits.ReadInt(7)
        let! bytes' = Bits.ReadBytes(96)

        return allT, allF, t, sevenF, bytes'
    }