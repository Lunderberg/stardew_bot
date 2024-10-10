#[macro_export]
macro_rules! unpack_fields{
    (
        $name:ident: {$ty:ty, $byte_range:expr}
    ) => {
        ::paste::paste!{
            #[allow(dead_code)]
            pub fn [< $name _unpacked >](
                &self,
            ) -> ::memory_reader::UnpackedValue<$ty> {
                self.bytes.subrange($byte_range).unpack().unwrap()
            }

            pub fn $name(&self) -> $ty {
                self.bytes.subrange($byte_range).unpack().unwrap()
            }
        }
    };

    (
        $name:ident: {$ty:ty, $byte_range:expr, $bit_range:expr}
    ) => {
        ::paste::paste!{
            fn [< $name _unpack_bits >](value: $ty) -> $ty {
                let start = ($bit_range).start;
                let end = ($bit_range).end;

                let total_bits = std::mem::size_of::<$ty>() * 8;
                let shift_bits = total_bits - end;

                let mask = ((1 << (end - start)) - 1) as $ty;

                (value >> shift_bits) & mask
            }

            pub fn [< $name _unpacked >](
                &self,
            ) -> ::memory_reader::UnpackedValue<$ty> {
                self.bytes
                    .subrange($byte_range)
                    .unpack::<::memory_reader::UnpackedValue<$ty>>()
                    .unwrap()
                    .map( Self::[< $name _unpack_bits >] )
            }

            pub fn $name(&self) -> $ty {
                let value = self.bytes
                    .subrange($byte_range)
                    .unpack::<$ty>()
                    .unwrap();
                Self::[< $name _unpack_bits >] (value)
            }
        }
    };

    (
        $(
            $name:ident: {$ty:ty, $byte_range:expr $(, $bit_range:expr )?}
        ),* $(,)?
    ) => {
        $(
            unpack_fields!{$name: {$ty, $byte_range $(, $bit_range)? } }
        )*
    };
}
