module YouReadWhatYouWrite

open System.IO
open BitSyntax
open FsUnit
open NUnit.Framework

[<TestFixture>]
type ``Given a sequence of values`` () =
    [<Test>]
    member test.``when it's written to a stream the same values should be read back out`` () =
        let stream = new MemoryStream()

        do bitWriter stream {
            do! BitWriter.WriteInt16(10s, 4)
            do! BitWriter.WriteUInt16(10us, 4)
            do! BitWriter.WriteInt32(10, 4)
            do! BitWriter.WriteUInt32(10u, 4)
            do! BitWriter.WriteInt64(10L, 4)
            do! BitWriter.WriteUInt64(10UL, 4)
            
            do! BitWriter.WriteFloat(1024.0f)
            do! BitWriter.WriteDouble(1024.0)
            
            do! BitWriter.WriteBool(true)
            do! BitWriter.WriteBool(false)
            do! BitWriter.WriteByte(10uy)
            do! BitWriter.WriteBytes([| 3uy; 2uy; 1uy |])
            
            do! BitWriter.WriteChar('Y')
            do! BitWriter.WriteString("Hello World!")
        }

        stream.Position <- 0L
        let a, b, c, d, e, f, g, h, i, j, k, l, m, n = 
            bitReader stream {
                let! a = BitReader.ReadInt16(4)
                let! b = BitReader.ReadUInt16(4)
                let! c = BitReader.ReadInt32(4)
                let! d = BitReader.ReadUInt32(4)
                let! e = BitReader.ReadInt64(4)
                let! f = BitReader.ReadUInt64(4)

                let! g = BitReader.ReadFloat()
                let! h = BitReader.ReadDouble()

                let! i = BitReader.ReadBool()
                let! j = BitReader.ReadBool()
                let! k = BitReader.ReadByte()
                let! l = BitReader.ReadBytes(3)

                let! m = BitReader.ReadChar()
                let! n = BitReader.ReadString(12)

                return a, b, c, d, e, f, g, h, i, j, k, l, m, n
            }

        a |> should equal 10s
        b |> should equal 10us
        c |> should equal 10
        d |> should equal 10u
        e |> should equal 10L
        f |> should equal 10UL
        g |> should equal 1024.0
        h |> should equal 1024.0
        i |> should equal true
        j |> should equal false
        k |> should equal 10uy
        l |> should equal [| 3uy; 2uy; 1uy |]
        m |> should equal 'Y'
        n |> should equal "Hello World!"

    [<Test>]
    member test.``when TCP headers are written in with bitWriter, they should be read out with bitReader`` () =
        let stream = new MemoryStream()
        do bitWriter stream {
            do! BitWriter.WriteInt16(12345s) // source port
            do! BitWriter.WriteInt16(12321s) // destination port
            do! BitWriter.WriteInt32(1)      // sequence number
            do! BitWriter.WriteInt32(1)      // ack number
            do! BitWriter.WriteInt32(2, 4)   // data offset
            do! BitWriter.WriteInt32(0, 3)   // reserved
            do! BitWriter.WriteBool(false)   // NS
            do! BitWriter.WriteBool(true)    // CWR
            do! BitWriter.WriteBool(true)    // ECE
            do! BitWriter.WriteBool(true)    // URG
            do! BitWriter.WriteBool(true)    // ACK
            do! BitWriter.WriteBool(true)    // PSH
            do! BitWriter.WriteBool(false)   // RST
            do! BitWriter.WriteBool(true)    // SYN
            do! BitWriter.WriteBool(false)   // FIN
            do! BitWriter.WriteInt16(8s)     // Window Size
            do! BitWriter.WriteInt16(42s)    // Checksum
            do! BitWriter.WriteInt16(1208s)  // Urgent Pointer
            do! BitWriter.WriteString("Hello World!")  // payload
        }

        stream.Position <- 0L
        let srcPort, destPort, 
            seqNum, ackNum, dataOffset, 
            ns, cwr, ece, urg, ack, psh, rst, syn, fin,
            winSize, checkSum, pointer, payload = 
                bitReader stream {
                    let! srcPort    = BitReader.ReadInt32(16)
                    let! destPort   = BitReader.ReadInt32(16)
                    let! seqNum     = BitReader.ReadInt32(32)
                    let! ackNum     = BitReader.ReadInt32(32)
                    let! dataOffset = BitReader.ReadInt32(4)
                    let! _          = BitReader.ReadByte(3)

                    let! ns         = BitReader.ReadBool()
                    let! cwr        = BitReader.ReadBool()
                    let! ece        = BitReader.ReadBool()
                    let! urg        = BitReader.ReadBool()
                    let! ack        = BitReader.ReadBool()
                    let! psh        = BitReader.ReadBool()
                    let! rst        = BitReader.ReadBool()
                    let! syn        = BitReader.ReadBool()
                    let! fin        = BitReader.ReadBool()
                    let! winSize    = BitReader.ReadInt32(16)
                    let! checkSum   = BitReader.ReadInt32(16)
                    let! pointer    = BitReader.ReadInt32(16)
                    let! payload    = BitReader.ReadString(12)

                    return srcPort, destPort, 
                           seqNum, ackNum, dataOffset, 
                           ns, cwr, ece, urg, ack, psh, rst, syn, fin,
                           winSize, checkSum, pointer, payload
                }

        srcPort     |> should equal 12345
        destPort    |> should equal 12321
        seqNum      |> should equal 1
        ackNum      |> should equal 1
        dataOffset  |> should equal 2

        ns          |> should equal false
        cwr         |> should equal true
        ece         |> should equal true
        urg         |> should equal true
        ack         |> should equal true
        psh         |> should equal true
        rst         |> should equal false
        syn         |> should equal true
        fin         |> should equal false

        winSize     |> should equal 8
        checkSum    |> should equal 42
        pointer     |> should equal 1208
        payload     |> should equal "Hello World!"