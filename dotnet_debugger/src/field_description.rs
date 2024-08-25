use std::ops::Range;

use dll_unpacker::{
    dll_unpacker::{Field, MetadataTableIndex},
    Annotation as _, Annotator, ByteRange,
};
use memory_reader::Pointer;

use crate::{unpack_fields, Error, OwnedBytes, RuntimeModule, RuntimeType};

pub struct FieldDescriptions {
    pub bytes: OwnedBytes,
}

pub struct FieldDescription<'a> {
    pub bytes: ByteRange<'a>,
}

impl FieldDescriptions {
    pub fn ptr_range(&self) -> Range<Pointer> {
        (&self.bytes).into()
    }

    pub fn iter<'a>(&'a self) -> impl Iterator<Item = FieldDescription<'a>> {
        (&self).into_iter()
    }
}

pub struct FieldDescriptionIterator<'a> {
    bytes: ByteRange<'a>,
}

impl<'a> Iterator for FieldDescriptionIterator<'a> {
    type Item = FieldDescription<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        (self.bytes.len() >= FieldDescription::SIZE).then(|| {
            let bytes = self.bytes.subrange(..FieldDescription::SIZE);
            self.bytes = self.bytes.subrange(FieldDescription::SIZE..);
            FieldDescription { bytes }
        })
    }
}

impl<'a> IntoIterator for &'a FieldDescriptions {
    type Item = FieldDescription<'a>;

    type IntoIter = FieldDescriptionIterator<'a>;

    fn into_iter(self) -> Self::IntoIter {
        FieldDescriptionIterator {
            bytes: (&self.bytes).into(),
        }
    }
}

impl<'a> FieldDescription<'a> {
    pub const SIZE: usize = 16;

    unpack_fields! {
        method_table: {Pointer, 0..8},
        raw_token: {u32, 8..12, 8..32},
        raw_is_static: {u32,  8..12, 7..8},
        is_thread_local: {u32,  8..12, 6..7},
        raw_is_rva: {u32,  8..12, 5..6},
        protection: {u32,  8..12, 2..5},
        requires_all_token_bits : {u32,  8..12, 1..2},
        raw_offset: {u32,  12..16, 5..32},
        raw_runtime_type: {u32,  12..16, 0..5},
    }

    pub fn ptr_range(&self) -> Range<Pointer> {
        self.bytes.into()
    }

    pub fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator
            .value(self.method_table_unpacked())
            .name("method_table");
        annotator
            .range(self.raw_token_unpacked().loc())
            .value(self.token())
            .name("token");

        annotator
            .range(self.raw_is_static_unpacked().loc())
            .value(self.is_static())
            .name("is_static");
        annotator
            .value(self.is_thread_local_unpacked())
            .name("is_thread_local");
        annotator
            .range(self.raw_is_rva_unpacked().loc())
            .value(self.is_rva())
            .name("is_rva");
        annotator
            .value(self.protection_unpacked())
            .name("protection");
        annotator
            .value(self.requires_all_token_bits_unpacked())
            .name("requires_all_token_bits");
        annotator.value(self.raw_offset_unpacked()).name("offset");
        annotator
            .range(self.raw_runtime_type_unpacked().loc())
            .value(self.runtime_type()?)
            .name("runtime_type");

        Ok(())
    }

    pub fn is_static(&self) -> bool {
        self.raw_is_static() > 0
    }

    pub fn is_rva(&self) -> bool {
        self.raw_is_rva() > 0
    }

    pub fn offset(&self) -> usize {
        self.raw_offset() as usize
    }

    pub fn token(&self) -> MetadataTableIndex<Field> {
        let index = self.raw_token() as usize;
        MetadataTableIndex::new(index - 1)
    }

    pub fn runtime_type(&self) -> Result<RuntimeType, Error> {
        (self.raw_runtime_type() as u8).try_into()
    }

    pub fn location(
        &self,
        module: &RuntimeModule,
        instance: Option<Pointer>,
    ) -> Result<Range<Pointer>, Error> {
        let runtime_type = self.runtime_type()?;
        let is_instance_field = !self.is_static();

        let base = if is_instance_field {
            let instance = instance
                .ok_or(Error::LocationOfInstanceFieldRequiresInstance)?;
            instance + Pointer::SIZE
        } else if matches!(
            runtime_type,
            RuntimeType::Class | RuntimeType::ValueType
        ) {
            module.base_ptr_of_gc_statics
        } else {
            module.base_ptr_of_non_gc_statics
        };
        let ptr = base + self.offset();

        let size = runtime_type.size_bytes();

        Ok(ptr..ptr + size)
    }
}
