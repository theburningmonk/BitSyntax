module Wrappers

open System
open System.Linq
open BitReaderWriterCs
open NUnit.Framework
open FsCheck

let inline littleEndianIsRevOfBigEndian (wrapper : ^a) = 
    let little = (^a : (member GetBytes : Endianness -> seq<byte>) (wrapper, Endianness.Little))
    let big    = (^a : (member GetBytes : Endianness -> seq<byte>) (wrapper, Endianness.Big))
    (little |> Array.ofSeq |> Array.rev) = (big |> Array.ofSeq)

[<TestFixture>]
type ``Given a FloatWrapper wrapper`` () =
    [<Test>] 
    member test.``FloatWrapper should return bytes in correct order for little endian`` () =
        let property (n : float32) = 
            let mutable wrapped = FloatWrapper()
            wrapped.Instance <- n
            wrapped.GetBytes(Endianness.Little).ToArray() = BitConverter.GetBytes(n)
        
        Check.Quick property

    [<Test>] 
    member test.``FloatWrapper should return bytes in reversed order for the different endianness`` () =
        let property (n : float32) = 
            let mutable wrapped = FloatWrapper()
            wrapped.Instance <- n
            littleEndianIsRevOfBigEndian wrapped
        
        Check.Quick property

[<TestFixture>]
type ``Given a DoubleWrapper wrapper`` () =
    [<Test>] 
    member test.``DoubleWrapper should return bytes in correct order for little endian`` () =
        let property (n : double) = 
            let mutable wrapped = DoubleWrapper()
            wrapped.Instance <- n
            wrapped.GetBytes(Endianness.Little).ToArray() = BitConverter.GetBytes(n)
        
        Check.Quick property

    [<Test>] 
    member test.``DoubleWrapper should return bytes in reversed order for the different endianness`` () =
        let property (n : double) = 
            let mutable wrapped = DoubleWrapper()
            wrapped.Instance <- n
            littleEndianIsRevOfBigEndian wrapped
        
        Check.Quick property

[<TestFixture>]
type ``Given an Int16Wrapper wrapper`` () =
    [<Test>] 
    member test.``Int16Wrapper should return bytes in correct order for little endian`` () =
        let property (n : int16) = 
            let mutable wrapped = Int16Wrapper()
            wrapped.Instance <- n
            wrapped.GetBytes(Endianness.Little).ToArray() = BitConverter.GetBytes(n)
        
        Check.Quick property

    [<Test>] 
    member test.``Int16Wrapper should return bytes in reversed order for the different endianness`` () =
        let property (n : int16) = 
            let mutable wrapped = Int16Wrapper()
            wrapped.Instance <- n
            littleEndianIsRevOfBigEndian wrapped
        
        Check.Quick property

[<TestFixture>]
type ``Given an Uint16Wrapper wrapper`` () =
    [<Test>] 
    member test.``Uint16Wrapper should return bytes in correct order for little endian`` () =
        let property (n : uint16) = 
            let mutable wrapped = Uint16Wrapper()
            wrapped.Instance <- n
            wrapped.GetBytes(Endianness.Little).ToArray() = BitConverter.GetBytes(n)
        
        Check.Quick property

    [<Test>] 
    member test.``Uint16Wrapper should return bytes in reversed order for the different endianness`` () =
        let property (n : uint16) = 
            let mutable wrapped = Uint16Wrapper()
            wrapped.Instance <- n
            littleEndianIsRevOfBigEndian wrapped
        
        Check.Quick property

[<TestFixture>]
type ``Given an Int32Wrapper wrapper`` () =
    [<Test>] 
    member test.``Int32Wrapper should return bytes in correct order for little endian`` () =
        let property (n : int32) = 
            let mutable wrapped = Int32Wrapper()
            wrapped.Instance <- n
            wrapped.GetBytes(Endianness.Little).ToArray() = BitConverter.GetBytes(n)
        
        Check.Quick property

    [<Test>] 
    member test.``Int32Wrapper should return bytes in reversed order for the different endianness`` () =
        let property (n : int32) = 
            let mutable wrapped = Int32Wrapper()
            wrapped.Instance <- n
            littleEndianIsRevOfBigEndian wrapped
        
        Check.Quick property
       
[<TestFixture>]
type ``Given an Uint32Wrapper wrapper`` () =
    [<Test>] 
    member test.``Uint32Wrapper should return bytes in correct order for little endian`` () =
        let property (n : uint32) = 
            let mutable wrapped = Uint32Wrapper()
            wrapped.Instance <- n
            wrapped.GetBytes(Endianness.Little).ToArray() = BitConverter.GetBytes(n)
        
        Check.Quick property

    [<Test>] 
    member test.``Uint32Wrapper should return bytes in reversed order for the different endianness`` () =
        let property (n : uint32) = 
            let mutable wrapped = Uint32Wrapper()
            wrapped.Instance <- n
            littleEndianIsRevOfBigEndian wrapped
        
        Check.Quick property
       
[<TestFixture>]
type ``Given an Int64Wrapper wrapper`` () =
    [<Test>] 
    member test.``Int64Wrapper should return bytes in correct order for little endian`` () =
        let property (n : int64) = 
            let mutable wrapped = Int64Wrapper()
            wrapped.Instance <- n
            wrapped.GetBytes(Endianness.Little).ToArray() = BitConverter.GetBytes(n)
        
        Check.Quick property

    [<Test>] 
    member test.``Int64Wrapper should return bytes in reversed order for the different endianness`` () =
        let property (n : int64) = 
            let mutable wrapped = Int64Wrapper()
            wrapped.Instance <- n
            littleEndianIsRevOfBigEndian wrapped
        
        Check.Quick property

[<TestFixture>]
type ``Given an Uint64Wrapper wrapper`` () =
    [<Test>] 
    member test.``Uint64Wrapper should return bytes in correct order for little endian`` () =
        let property (n : uint64) = 
            let mutable wrapped = Uint64Wrapper()
            wrapped.Instance <- n
            wrapped.GetBytes(Endianness.Little).ToArray() = BitConverter.GetBytes(n)
        
        Check.Quick property

    [<Test>] 
    member test.``Uint64Wrapper should return bytes in reversed order for the different endianness`` () =
        let property (n : uint64) = 
            let mutable wrapped = Uint64Wrapper()
            wrapped.Instance <- n
            littleEndianIsRevOfBigEndian wrapped
        
        Check.Quick property