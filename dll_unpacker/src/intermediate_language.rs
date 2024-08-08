use std::ops::Range;

use memory_reader::Pointer;

use crate::Annotation as _;
use crate::{Annotator, ByteRange, Error, UnpackedValue};

pub struct CILMethod<'a> {
    bytes: ByteRange<'a>,
}

pub enum CILMethodHeader<'a> {
    Tiny(CILTinyMethodHeader<'a>),
    Fat(CILFatMethodHeader<'a>),
}

pub struct CILTinyMethodHeader<'a> {
    bytes: ByteRange<'a>,
}

pub struct CILFatMethodHeader<'a> {
    bytes: ByteRange<'a>,
}

impl<'a> CILMethod<'a> {
    pub fn new(bytes: ByteRange<'a>) -> Self {
        Self { bytes }
    }

    pub(crate) fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator.value(self.header()?).name("CIL Method Header");

        Ok(())
    }

    fn header(&self) -> Result<UnpackedValue<CILMethodHeader<'a>>, Error> {
        let two_bits = self.bytes[0] & 0x3;
        let header = match two_bits {
            0 | 1 => Err(Error::InvalidCILMethodHeader(two_bits)),
            2 => {
                let bytes = self.bytes.subrange(..1);
                Ok(CILMethodHeader::Tiny(CILTinyMethodHeader { bytes }))
            }
            3 => {
                let header_size =
                    CILFatMethodHeader { bytes: self.bytes }.header_size();
                let bytes = self.bytes.subrange(..header_size);
                Ok(CILMethodHeader::Fat(CILFatMethodHeader { bytes }))
            }
            _ => panic!("Unreachable, two bits only define four values"),
        }?;

        let bytes = self.bytes.address_range(..header.header_size());
        Ok(UnpackedValue::new(bytes, header))
    }

    /// The address range for the IL of method's definition.
    pub fn body_range(&self) -> Result<Range<Pointer>, Error> {
        Ok(self.header()?.value().body_range())
    }

    fn iter_data_sections(
        &self,
    ) -> Result<impl Iterator<Item = ByteRange> + '_, Error> {
        let header = self.header()?.value();

        let iter = header
            .has_additional_section()
            .then(|| -> Result<_, Error> {
                let body_end = header.body_range().end;
                let padded_end = body_end.next_multiple_of(4);

                let mut remaining = self.bytes.subrange(padded_end..);

                let iter = std::iter::from_fn(move || {
                    if remaining.len() == 0 {
                        return None;
                    }

                    let flags = remaining[0];
                    let is_fat_format = flags & 0x40 > 0;
                    let has_additional_section = flags & 0x80 > 0;

                    let size = if is_fat_format {
                        let leading: u32 =
                            remaining.subrange(..4).unpack().unwrap();
                        (leading >> 8) as usize
                    } else {
                        remaining[1] as usize
                    };

                    let data_section = remaining.subrange(..size);

                    remaining = if has_additional_section {
                        let padded_end = data_section.end().next_multiple_of(4);
                        remaining.subrange(padded_end..)
                    } else {
                        remaining.subrange(..0)
                    };

                    Some(data_section)
                });

                Ok(iter)
            })
            .transpose()?
            .into_iter()
            .flatten();

        Ok(iter)
    }

    pub fn method_range(&self) -> Result<Range<Pointer>, Error> {
        let header = self.header()?.value();

        let end = std::iter::once(header.body_range().end)
            .chain(self.iter_data_sections()?.map(|range| range.end()))
            .last()
            .unwrap();

        Ok(self.bytes.address_range(..end))
    }
}

impl<'a> CILMethodHeader<'a> {
    fn header_size(&self) -> usize {
        match self {
            CILMethodHeader::Tiny(_) => 1,
            CILMethodHeader::Fat(header) => header.header_size(),
        }
    }

    fn body_size(&self) -> usize {
        match self {
            CILMethodHeader::Tiny(header) => header.body_size(),
            CILMethodHeader::Fat(header) => header.body_size(),
        }
    }

    fn has_additional_section(&self) -> bool {
        match self {
            CILMethodHeader::Tiny(_) => false,
            CILMethodHeader::Fat(header) => header.has_additional_section(),
        }
    }

    fn body_range(&self) -> Range<Pointer> {
        let header_start = match self {
            CILMethodHeader::Tiny(header) => header.bytes,
            CILMethodHeader::Fat(header) => header.bytes,
        }
        .start;
        let start = header_start + self.header_size();
        let size = self.body_size();
        start..start + size
    }
}

impl<'a> CILTinyMethodHeader<'a> {
    fn body_size(&self) -> usize {
        (self.bytes[0] >> 2) as usize
    }
}

impl<'a> CILFatMethodHeader<'a> {
    fn header_size(&self) -> usize {
        let header_size_num_int32 = ((self.bytes[1] & 0xf0) >> 4) as usize;
        header_size_num_int32 * 4
    }

    fn body_size(&self) -> usize {
        self.bytes.subrange(4..8).unpack::<u32>().unwrap() as usize
    }

    fn has_additional_section(&self) -> bool {
        self.bytes[0] & 0x8 > 0
    }
}

impl<'a> std::fmt::Display for CILMethodHeader<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CILMethodHeader::Tiny(header) => write!(f, "{}", header),
            CILMethodHeader::Fat(header) => write!(f, "{}", header),
        }
    }
}

impl<'a> std::fmt::Display for CILTinyMethodHeader<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "TinyHeader({}-byte method)", self.body_size())
    }
}

impl<'a> std::fmt::Display for CILFatMethodHeader<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "FatHeader({} byte method)", self.body_size())
    }
}
