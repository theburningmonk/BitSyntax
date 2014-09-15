module Tools

open System
open System.Linq
open BitReaderWriterFs.Tools
open FsUnit
open NUnit.Framework

[<TestFixture>]
type ``Tools tests`` () =
    [<TestCase(1, Result = 1uy)>]
    [<TestCase(2, Result = 3uy)>]
    [<TestCase(3, Result = 7uy)>]
    [<TestCase(4, Result = 15uy)>]
    [<TestCase(5, Result = 31uy)>]
    [<TestCase(6, Result = 63uy)>]
    [<TestCase(7, Result = 127uy)>]
    [<TestCase(8, Result = 255uy)>]
    member test.``keepLastBits of a byte`` (n) =
        keepLastBits n 255uy

    // 11100000 + 11000111 => 11101110
    //    ^           ^  ^
    [<TestCase(3, 224uy, 4, 4, 199uy, Result = 238uy)>]
    // 11100000 + 11000111 => 11101100
    //    ^           ^ ^
    [<TestCase(3, 224uy, 4, 3, 199uy, Result = 236uy)>]
    // 11100000 + 11000111 => 11101100
    //    ^           ^   ^
    [<TestCase(3, 224uy, 4, 5, 199uy, Result = 238uy)>]
    // 00000000 + 11000111 => 01110000
    // ^              ^   ^
    [<TestCase(0, 0uy, 4, 5, 199uy, Result = 112uy)>]
    // 00000000 + 11000111 => 01100000
    // ^              ^ ^  
    [<TestCase(0, 0uy, 4, 3, 199uy, Result = 96uy)>]
    // 00000000 + 11000111 => 11000100
    // ^          ^    ^  
    [<TestCase(0, 0uy, 0, 6, 199uy, Result = 96uy)>]
    member test.``combine should combine two bytes at the indices`` (leftIdx, left, rightIdx, count, right) =
        combine leftIdx left rightIdx count right