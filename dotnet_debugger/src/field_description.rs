use std::{borrow::Borrow, ops::Range};

use dll_unpacker::{
    dll_unpacker::{Field, MetadataTableIndex},
    Annotation as _, Annotator, RelativeVirtualAddress,
};
use memory_reader::{ByteRange, MemoryReader, OwnedBytes, Pointer};

use crate::{
    unpack_fields, CorElementType, Error, MethodTable, RuntimeModule,
    TypedPointer,
};

pub struct FieldDescriptions {
    pub bytes: OwnedBytes,
}

#[derive(Clone, Copy)]
pub struct FieldDescription<'a> {
    pub bytes: ByteRange<'a>,
}

#[derive(Clone, Copy)]
pub enum FieldContainer {
    /// Field is contained in a Class.  The Pointer is the value
    /// stored in the containing Class, and points to the MethodTable
    /// pointer.  Offsets to fields within the Class are relative to
    /// the location just after the MethodTable pointer.
    Class(Pointer),

    /// Field is contained in a ValueType.  There is no Pointer to the
    /// ValueType itself, but instead is located relative to the
    /// containing object.  Offsets to fields within the ValueType are
    /// relative to the Pointer itself.
    ValueType(Pointer),

    /// Field is static.
    Static,
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
        method_table: {TypedPointer<MethodTable>, 0..8},
        raw_token: {u32, 8..12, 8..32},
        raw_is_static: {u32,  8..12, 7..8},
        is_thread_local: {u32,  8..12, 6..7},
        raw_is_rva: {u32,  8..12, 5..6},
        protection: {u32,  8..12, 2..5},
        requires_all_token_bits : {u32,  8..12, 1..2},
        raw_offset: {u32,  12..16, 5..32},
        raw_cor_element_type: {u32,  12..16, 0..5},
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
            .range(self.raw_cor_element_type_unpacked().loc())
            .value(self.cor_element_type()?)
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

    pub fn cor_element_type(&self) -> Result<CorElementType, Error> {
        (self.raw_cor_element_type() as u8).try_into()
    }

    pub fn is_pointer(&self) -> Result<bool, Error> {
        Ok(self.cor_element_type()?.is_ptr())
    }

    pub fn is_value_type(&self) -> Result<bool, Error> {
        Ok(matches!(
            self.cor_element_type()?,
            CorElementType::ValueType
        ))
    }

    pub fn location(
        &self,
        module: &RuntimeModule,
        container: FieldContainer,
        reader: impl Borrow<MemoryReader>,
    ) -> Result<Pointer, Error> {
        let reader = reader.borrow();
        let is_instance_field = !self.is_static();
        let is_rva_field = self.is_rva();

        let ptr = if is_instance_field {
            let base = match container {
                FieldContainer::Class(ptr) => Ok(ptr + Pointer::SIZE),
                FieldContainer::ValueType(ptr) => Ok(ptr),
                FieldContainer::Static => {
                    Err(Error::LocationOfInstanceFieldRequiresInstance)
                }
            }?;
            base + self.offset()
        } else if is_rva_field {
            let layout = module.metadata_layout(reader)?;
            let rva = RelativeVirtualAddress::new(self.offset());
            layout.virtual_address_to_raw(rva)?
        } else if matches!(self.cor_element_type()?, CorElementType::Class) {
            // The Module contains two pointers for static values,
            // depending on whether the value must be inspected by the
            // garbage collector.  Objects that may be managed by the
            // garbage collected are stored separately from unmanaged
            // objects.
            let base = module.base_ptr_of_gc_statics(reader)?;
            base + self.offset()
        } else if matches!(self.cor_element_type()?, CorElementType::ValueType)
        {
            // Static value types are not stored inline, but are
            // instead stored as if they were classes.  Need to read
            // the pointer, dereference, then advance past a method
            // table pointer.
            let base = module.base_ptr_of_gc_statics(reader)?;
            let ptr_loc = base + self.offset();
            let ptr: Pointer = reader
                .read_bytes(ptr_loc..ptr_loc + Pointer::SIZE)?
                .subrange(..)
                .unpack()?;
            ptr + Pointer::SIZE
        } else {
            let base = module.base_ptr_of_non_gc_statics(reader)?;
            base + self.offset()
        };

        Ok(ptr)
    }
}
