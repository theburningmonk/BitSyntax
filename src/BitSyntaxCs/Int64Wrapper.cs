using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;

namespace BitSyntaxCs
{
    [StructLayout(LayoutKind.Explicit)]
    public struct Int64Wrapper
    {
        [FieldOffset(0)] public long Instance;

        [FieldOffset(0)] public byte Byte1;
        [FieldOffset(1)] public byte Byte2;
        [FieldOffset(2)] public byte Byte3;
        [FieldOffset(3)] public byte Byte4;
        [FieldOffset(4)] public byte Byte5;
        [FieldOffset(5)] public byte Byte6;
        [FieldOffset(6)] public byte Byte7;
        [FieldOffset(7)] public byte Byte8;

        public IEnumerable<byte> GetBytes(Endianness endianness)
        {
            if ((endianness == Endianness.Little) == BitConverter.IsLittleEndian)
            {
                yield return Byte1;
                yield return Byte2;
                yield return Byte3;
                yield return Byte4;
                yield return Byte5;
                yield return Byte6;
                yield return Byte7;
                yield return Byte8;
            }
            else
            {
                yield return Byte8;
                yield return Byte7;
                yield return Byte6;
                yield return Byte5;
                yield return Byte4;
                yield return Byte3;
                yield return Byte2;
                yield return Byte1;
            }
        }
    }
}