using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;

namespace BitReaderWriterCs
{
    [StructLayout(LayoutKind.Explicit)]
    public struct Int16Wrapper
    {
        [FieldOffset(0)] public short Instance;

        [FieldOffset(0)] public byte Byte1;
        [FieldOffset(1)] public byte Byte2;

        public IEnumerable<byte> GetBytes(Endianness endianness)
        {
            if ((endianness == Endianness.Little) == BitConverter.IsLittleEndian)
            {
                yield return Byte1;
                yield return Byte2;
            }
            else
            {
                yield return Byte2;
                yield return Byte1;
            }
        }
    }
}