using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;

namespace BitReaderWriterCs
{
    [StructLayout(LayoutKind.Explicit)]
    public struct Uint32Wrapper
    {
        [FieldOffset(0)] public uint Instance;

        [FieldOffset(0)] public byte Byte1;
        [FieldOffset(1)] public byte Byte2;
        [FieldOffset(2)] public byte Byte3;
        [FieldOffset(3)] public byte Byte4;

        public IEnumerable<byte> GetBytes(Endianness endianness)
        {
            if ((endianness == Endianness.Little) == BitConverter.IsLittleEndian)
            {
                yield return Byte1;
                yield return Byte2;
                yield return Byte3;
                yield return Byte4;
            }
            else
            {
                yield return Byte4;
                yield return Byte3;
                yield return Byte2;
                yield return Byte1;
            }
        }
    }
}