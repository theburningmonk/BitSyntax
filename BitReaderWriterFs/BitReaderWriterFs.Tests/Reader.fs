module Reader

open System
open System.IO
open BitReaderWriterFs
open FsUnit
open NUnit.Framework

[<TestFixture>]
type ``BitReaderBuilder tests`` () =
    [<Test>]
    member test.``bitReader workflow should be able to read Int16 from stream`` () =
        let stream = new MemoryStream()

        do bitWriter stream {
            do! BitWriter.WriteInt16(Int16.MaxValue)
            do! BitWriter.WriteInt16(256s, 9)   // 256
            do! BitWriter.WriteInt16(256s, 7)   // 0
            do! BitWriter.WriteInt16(Int16.MinValue)
        }

        stream.Position <- 0L

        let x, y, z, neg = bitReader stream {
            let! x = BitReader.ReadInt16(16)
            let! y = BitReader.ReadInt16(9)
            let! z = BitReader.ReadInt16(7)
            let! neg = BitReader.ReadInt16(16)
            return x, y, z, neg
        }

        x   |> should equal Int16.MaxValue
        y   |> should equal 256s
        z   |> should equal 0s
        neg |> should equal Int16.MinValue

    [<Test>]
    member test.``bitReader workflow should be able to read UInt16 from stream`` () =
        let stream = new MemoryStream()

        do bitWriter stream {
            do! BitWriter.WriteUInt16(UInt16.MaxValue)
            do! BitWriter.WriteUInt16(256us, 9)   // 256
            do! BitWriter.WriteUInt16(256us, 7)   // 0
        }

        stream.Position <- 0L

        let x, y, z = bitReader stream {
            let! x = BitReader.ReadUInt16()
            let! y = BitReader.ReadUInt16(9)
            let! z = BitReader.ReadUInt16(7)
            return x, y, z
        }

        x |> should equal UInt16.MaxValue
        y |> should equal 256us
        z |> should equal 0us

    [<Test>]
    member test.``bitReader workflow should be able to read Int32 from stream`` () =
        let stream = new MemoryStream()

        do bitWriter stream {            
            do! BitWriter.WriteInt32(Int32.MaxValue)
            do! BitWriter.WriteInt32(256, 9)   // 256
            do! BitWriter.WriteInt32(256, 7)   // 0
            do! BitWriter.WriteInt32(Int32.MinValue)
        }

        stream.Position <- 0L

        let x, y, z, neg = bitReader stream {
            let! x = BitReader.ReadInt32()
            let! y = BitReader.ReadInt32(9)
            let! z = BitReader.ReadInt32(7)
            let! neg = BitReader.ReadInt32()
            return x, y, z, neg
        }

        x   |> should equal Int32.MaxValue
        y   |> should equal 256
        z   |> should equal 0
        neg |> should equal Int32.MinValue

    [<Test>]
    member test.``bitReader workflow should be able to read UInt32 from stream`` () =
        let stream = new MemoryStream()

        do bitWriter stream {            
            do! BitWriter.WriteUInt32(UInt32.MaxValue)
            do! BitWriter.WriteUInt32(256u, 9)   // 256
            do! BitWriter.WriteUInt32(256u, 7)   // 0
            do! BitWriter.WriteUInt32(UInt32.MinValue)
        }

        stream.Position <- 0L

        let x, y, z, neg = bitReader stream {
            let! x = BitReader.ReadUInt32()
            let! y = BitReader.ReadUInt32(9)
            let! z = BitReader.ReadUInt32(7)
            let! neg = BitReader.ReadUInt32()
            return x, y, z, neg
        }

        x   |> should equal UInt32.MaxValue
        y   |> should equal 256u
        z   |> should equal 0
        neg |> should equal UInt32.MinValue

    [<Test>]
    member test.``bitReader workflow should be able to read Int64 from stream`` () =
        let stream = new MemoryStream()

        do bitWriter stream {            
            do! BitWriter.WriteInt64(Int64.MaxValue)
            do! BitWriter.WriteInt64(256L, 9)   // 256
            do! BitWriter.WriteInt64(256L, 7)   // 0
            do! BitWriter.WriteInt64(Int64.MinValue)
        }

        stream.Position <- 0L

        let x, y, z, neg = bitReader stream {
            let! x = BitReader.ReadInt64()
            let! y = BitReader.ReadInt64(9)
            let! z = BitReader.ReadInt64(7)
            let! neg = BitReader.ReadInt64()
            return x, y, z, neg
        }

        x   |> should equal Int64.MaxValue
        y   |> should equal 256L
        z   |> should equal 0
        neg |> should equal Int64.MinValue

    [<Test>]
    member test.``bitReader workflow should be able to read UInt64 from stream`` () =
        let stream = new MemoryStream()

        do bitWriter stream {            
            do! BitWriter.WriteUInt64(UInt64.MaxValue)
            do! BitWriter.WriteUInt64(256UL, 9)   // 256
            do! BitWriter.WriteUInt64(256UL, 7)   // 0
            do! BitWriter.WriteUInt64(UInt64.MinValue)
        }

        stream.Position <- 0L

        let x, y, z, neg = bitReader stream {
            let! x = BitReader.ReadUInt64()
            let! y = BitReader.ReadUInt64(9)
            let! z = BitReader.ReadUInt64(7)
            let! neg = BitReader.ReadUInt64()
            return x, y, z, neg
        }

        x   |> should equal UInt64.MaxValue
        y   |> should equal 256UL
        z   |> should equal 0
        neg |> should equal UInt64.MinValue

    [<Test>]
    member test.``bitReader workflow should be able to read float from stream`` () =
        let stream = new MemoryStream()

        do bitWriter stream {            
            do! BitWriter.WriteFloat(3.1415926f)
        }

        stream.Position <- 0L

        let x = bitReader stream {
            return! BitReader.ReadFloat()
        }

        x   |> should equal 3.1415926f

    [<Test>]
    member test.``bitReader workflow should be able to read double from stream`` () =
        let stream = new MemoryStream()

        do bitWriter stream {            
            do! BitWriter.WriteDouble(3.1415926)
        }

        stream.Position <- 0L

        let x = bitReader stream {
            return! BitReader.ReadDouble()
        }

        x   |> should equal 3.1415926

    [<Test>]
    member test.``bitReader workflow should be able to read bool from stream`` () =
        let stream = new MemoryStream()

        do bitWriter stream {            
            do! BitWriter.WriteBool(true)
            do! BitWriter.WriteBool(false)
            do! BitWriter.WriteBool(true)
        }

        stream.Position <- 0L

        let x, y, z = bitReader stream {
            let! x = BitReader.ReadBool()
            let! y = BitReader.ReadBool()
            let! z = BitReader.ReadBool()

            return x, y, z
        }

        x   |> should equal true
        y   |> should equal false
        z   |> should equal true

    [<Test>]
    member test.``bitReader workflow should be able to read byte from stream`` () =
        let stream = new MemoryStream()

        do bitWriter stream {            
            do! BitWriter.WriteByte(255uy)
            do! BitWriter.WriteByte(255uy, 5) // 31
            do! BitWriter.WriteByte(255uy, 1) // 1
        }

        stream.Position <- 0L

        let x, y, z = bitReader stream {
            let! x = BitReader.ReadByte()
            let! y = BitReader.ReadByte(5)
            let! z = BitReader.ReadByte(1)

            return x, y, z
        }

        x   |> should equal 255uy
        y   |> should equal 31uy
        z   |> should equal 1uy

    [<Test>]
    member test.``bitReader workflow should be able to read bytes from stream`` () =
        let stream = new MemoryStream()

        do bitWriter stream {            
            do! BitWriter.WriteBytes([| 0uy; 1uy; 0uy; 0uy |], 2)
            do! BitWriter.WriteBytes([| 0uy; 1uy; 0uy; 0uy |])
        }

        stream.Position <- 0L

        let x, y = bitReader stream {
            let! x = BitReader.ReadBytes(2)
            let! y = BitReader.ReadBytes(4)

            return x, y
        }

        x   |> should equal [| 0uy; 1uy |]
        y   |> should equal [| 0uy; 1uy; 0uy; 0uy |]

    [<Test>]
    member test.``bitReader workflow should be able to read char from stream`` () =
        let stream = new MemoryStream()

        do bitWriter stream {            
            do! BitWriter.WriteChar('X')
            do! BitWriter.WriteChar('x')
        }

        stream.Position <- 0L

        let x, y = bitReader stream {
            let! x = BitReader.ReadChar()
            let! y = BitReader.ReadChar()

            return x, y
        }

        x   |> should equal 'X'
        y   |> should equal 'x'

    [<Test>]
    member test.``bitReader workflow should be able to read string from stream`` () =
        let stream = new MemoryStream()

        do bitWriter stream {
            do! BitWriter.WriteString("Hello world!")
        }

        stream.Position <- 0L

        let x, y = bitReader stream {
            let! x = BitReader.ReadString(5)
            let! _ = BitReader.ReadChar()
            let! y = BitReader.ReadString(6)

            return x, y
        }

        x   |> should equal "Hello"
        y   |> should equal "world!"