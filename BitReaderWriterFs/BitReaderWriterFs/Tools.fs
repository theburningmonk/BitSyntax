namespace BitReaderWriterFs

module Tools =
    /// Keeps the last x number of bits in a byte, e.g.
    ///     keepLastBits 1 255uy => 1uy
    ///     keepLastBits 2 255uy => 3uy
    ///     keepLastBits 3 255uy => 7uy
    let keepLastBits n (byte : byte) = 
        let n = 8 - n
        byte <<< n >>> n

    /// Combines two bytes at the specified indices, whilst taking only the specified number of
    /// bits from the byte on the right, e.g.
    /// left      :       11100000
    /// left idx  : 3        ^ 
    /// right     :       11000111
    /// right idx : 4         ^
    /// count     : 4
    /// result    :       11101110 (first 3 bits from left + 4 bits from index 4 onward from right)
    let combine leftIdx (left : byte) rightIdx count (right : byte) =
        let right    = right <<< rightIdx >>> leftIdx
        let combined = left ||| right
        let size     = leftIdx + count
        if size < 8 then
            let trailingSpace = 8 - size
            combined >>> trailingSpace <<< trailingSpace
        else combined

        
        