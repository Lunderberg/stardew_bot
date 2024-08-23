use std::ops::Range;

use dll_unpacker::{
    dll_unpacker::{MetadataTableIndex, TypeDef},
    Annotation, Annotator, ByteRange,
};
use memory_reader::{MemoryReader, Pointer};

use crate::{
    unpack_fields, Error, FieldDescription, FieldDescriptions, OwnedBytes,
};

pub struct MethodTableLookup {
    pub location: Range<Pointer>,
    pub method_tables: Vec<Range<Pointer>>,
}

pub struct MethodTable {
    pub bytes: OwnedBytes,
}

pub struct EEClass {
    pub bytes: OwnedBytes,
}

pub struct EEClassPackedFields<'a> {
    pub bytes: ByteRange<'a>,
}

impl MethodTableLookup {
    pub fn location_of_method_table_pointer(
        &self,
        index: MetadataTableIndex<TypeDef>,
    ) -> Range<Pointer> {
        let index: usize = index.into();
        let index = index + 1;
        let ptr_start = self.location.start + index * Pointer::SIZE;
        ptr_start..ptr_start + Pointer::SIZE
    }

    pub fn iter_tables<'a>(
        &'a self,
        reader: &'a MemoryReader,
    ) -> impl Iterator<Item = Result<MethodTable, Error>> + 'a {
        self.method_tables
            .iter()
            .cloned()
            .filter(|ptr| !ptr.start.is_null())
            .map(move |location| {
                let nbytes = location.end - location.start;
                let bytes = reader.read_bytes(location.start, nbytes)?;
                Ok(MethodTable {
                    bytes: OwnedBytes::new(location.start, bytes),
                })
            })
    }

    pub fn get(
        &self,
        index: MetadataTableIndex<TypeDef>,
        reader: &MemoryReader,
    ) -> Result<MethodTable, Error> {
        let byte_range = &self[index];
        let bytes = reader
            .read_bytes(byte_range.start, byte_range.end - byte_range.start)?;
        let bytes = OwnedBytes::new(byte_range.start, bytes);
        Ok(MethodTable { bytes })
    }
}

impl std::ops::Index<MetadataTableIndex<TypeDef>> for MethodTableLookup {
    type Output = Range<Pointer>;

    fn index(&self, index: MetadataTableIndex<TypeDef>) -> &Self::Output {
        let index: usize = index.into();
        &self.method_tables[index + 1]
    }
}

impl MethodTable {
    pub const SIZE: usize = 64;

    unpack_fields! {
        flags: {u32, 0..4},
        raw_base_size: {u32, 4..8},
        flags_2: {u16, 8..10},
        raw_token: {u16, 10..12},
        num_virtuals: {u16, 12..14},
        num_interfaces: {u16, 14..16},
        parent_method_table: {Pointer, 16..24},
        module: {Pointer, 24..32},
        writable_data: {Pointer, 32..40},
        ee_class_or_canonical_table: {Pointer, 40..48},

    }

    pub fn read(ptr: Pointer, reader: &MemoryReader) -> Result<Self, Error> {
        let bytes = reader.read_bytes(ptr, Self::SIZE)?;
        Ok(Self {
            bytes: OwnedBytes::new(ptr, bytes),
        })
    }

    pub fn ptr_range(&self) -> Range<Pointer> {
        (&self.bytes).into()
    }

    pub fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator.value(self.flags_unpacked()).name("flags");
        annotator
            .value(self.raw_base_size_unpacked())
            .name("base_size");
        annotator.value(self.flags_2_unpacked()).name("flags2");
        annotator
            .range(self.raw_token_unpacked().loc())
            .value(self.token())
            .name("token");
        annotator
            .value(self.num_virtuals_unpacked())
            .name("num_virtuals");
        annotator
            .value(self.num_interfaces_unpacked())
            .name("num_interfaces");
        annotator
            .value(self.parent_method_table_unpacked())
            .name("parent_method_table");
        annotator.value(self.module_unpacked()).name("module");
        annotator
            .value(self.ee_class_or_canonical_table_unpacked())
            .name("ee_class_or_canonical_table");

        Ok(())
    }

    pub fn base_size(&self) -> usize {
        self.raw_base_size() as usize
    }

    pub fn has_finalizer(&self) -> bool {
        self.flags() & 0x00100000 > 0
    }

    pub fn has_dynamic_statics(&self) -> bool {
        self.flags_2() & 0x0002 > 0
    }

    pub fn token(&self) -> MetadataTableIndex<TypeDef> {
        let index = self.raw_token() as usize;
        MetadataTableIndex::new(index - 1)
    }

    pub fn get_ee_class(
        &self,
        reader: &MemoryReader,
    ) -> Result<EEClass, Error> {
        let ptr = self.ee_class_or_canonical_table();
        let is_ee_class = ptr.as_usize() & 3usize == 0;

        let ptr = if is_ee_class {
            ptr
        } else {
            let canonical_method_table: Pointer = (ptr.as_usize() & !3).into();
            reader.read_byte_array(canonical_method_table + 40)?.into()
        };

        let base_size = 56;
        // EEClass stores extra data after the end of the class
        // members.  The length of these is about 18 bytes
        let extra_size = 256;
        let bytes = reader.read_bytes(ptr, base_size + extra_size)?;
        Ok(EEClass {
            bytes: OwnedBytes::new(ptr, bytes),
        })
    }

    pub fn get_parent(
        &self,
        reader: &MemoryReader,
    ) -> Result<Option<Self>, Error> {
        let ptr = self.parent_method_table();
        Ok(if ptr.is_null() {
            None
        } else {
            Some(Self::read(ptr, reader)?)
        })
    }

    pub fn iter_parents<'a>(
        &self,
        reader: &'a MemoryReader,
    ) -> impl Iterator<Item = Result<Self, Error>> + 'a {
        std::iter::successors(
            self.get_parent(reader).transpose(),
            |res_method_table: &Result<Self, Error>| -> Option<Result<Self, Error>>{
                let method_table = res_method_table.as_ref().ok()?;

                let res_parent = method_table.get_parent(reader).transpose()?;

                Some(res_parent)
            },
        )
    }

    pub fn get_field_descriptions(
        &self,
        reader: &MemoryReader,
    ) -> Result<Option<FieldDescriptions>, Error> {
        let ee_class = self.get_ee_class(reader)?;

        let ptr = ee_class.fields();
        if ptr.is_null() {
            return Ok(None);
        }

        let num_instance_fields =
            ee_class.packed_fields().num_instance_fields();
        let num_static_fields = ee_class.packed_fields().num_static_fields();

        let num_parent_instance_fields = self
            .get_parent(reader)?
            .map(|parent| {
                parent.get_ee_class(reader).map(|parent_ee_class| {
                    parent_ee_class.packed_fields().num_instance_fields()
                })
            })
            .transpose()?
            .unwrap_or(0);

        assert!(
            num_instance_fields >= num_parent_instance_fields,
            "MethodTable has {num_instance_fields}, \
             but parent has {num_parent_instance_fields}"
        );

        let num_fields = (num_static_fields + num_instance_fields
            - num_parent_instance_fields) as usize;
        let nbytes = num_fields * FieldDescription::SIZE;

        let bytes = reader.read_bytes(ptr, nbytes)?;
        Ok(Some(FieldDescriptions {
            bytes: OwnedBytes::new(ptr, bytes),
        }))
    }
}

impl EEClass {
    pub fn ptr_range(&self) -> Range<Pointer> {
        let start = self.bytes.start();
        let size = self.fixed_class_fields() as usize + 44;
        start..start + size
    }

    unpack_fields! {
        guid_info: {Pointer, 0..8},
        optional_fields: {Pointer, 8..16},
        method_table: {Pointer, 16..24},
        fields: {Pointer, 24..32},
        method_chunks: {Pointer, 32..40},
        attr_class: {u32, 40..44},
        vm_flags: {u32, 44..48},
        norm_type: {u8, 48..49},
        // TODO: Handle newer versions of coreclr, which removed the
        // packed fields.  Maybe just grabbing the number of fields
        // from the metadata instead?
        fields_are_packed: {bool, 49..50},
        fixed_class_fields: {u8, 50..51},
        base_size_padding: {u8, 51..52},
    }

    pub fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator.value(self.guid_info_unpacked()).name("guid_info");
        annotator
            .value(self.optional_fields_unpacked())
            .name("optional_fields");
        annotator
            .value(self.method_table_unpacked())
            .name("method_table");
        annotator.value(self.fields_unpacked()).name("fields");
        annotator
            .value(self.method_chunks_unpacked())
            .name("method_chunks");
        annotator
            .value(self.attr_class_unpacked())
            .name("attr_class");
        annotator.value(self.vm_flags_unpacked()).name("vm_flags");
        annotator.value(self.norm_type_unpacked()).name("norm_type");
        annotator
            .value(self.fields_are_packed_unpacked())
            .name("fields_are_packed");
        annotator
            .value(self.fixed_class_fields_unpacked())
            .name("fixed_class_fields");
        annotator
            .value(self.base_size_padding_unpacked())
            .name("base_size_padding");

        self.packed_fields().collect_annotations(annotator)?;

        Ok(())
    }

    pub fn packed_fields<'a>(&'a self) -> EEClassPackedFields<'a> {
        let offset = self.fixed_class_fields() as usize;
        let bytes = self.bytes.subrange(offset..offset + 44);
        EEClassPackedFields { bytes }
    }
}

impl<'a> EEClassPackedFields<'a> {
    unpack_fields! {
        num_instance_fields: {u32, 0..4},
        num_methods: {u32, 4..8},
        num_static_fields: {u32, 8..12},
        num_handle_statics: {u32, 12..16},
        num_boxed_statics: {u32, 16..20},
        num_gc_static_field_bytes: {u32, 20..24},
        num_thread_static_fields: {u32, 24..28},
        num_handle_thread_statics: {u32, 28..32},
        num_boxed_thread_statics: {u32, 32..36},
        num_gc_thread_static_field_bytes: {u32, 36..40},
        num_non_virtual_slots: {u32, 40..44},
    }

    pub fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator
            .value(self.num_instance_fields_unpacked())
            .name("num_instance_fields");
        annotator
            .value(self.num_methods_unpacked())
            .name("num_methods");
        annotator
            .value(self.num_static_fields_unpacked())
            .name("num_static_fields");
        annotator
            .value(self.num_handle_statics_unpacked())
            .name("num_handle_statics");
        annotator
            .value(self.num_boxed_statics_unpacked())
            .name("num_boxed_statics");
        annotator
            .value(self.num_gc_static_field_bytes_unpacked())
            .name("num_gc_static_field_bytes");
        annotator
            .value(self.num_thread_static_fields_unpacked())
            .name("num_thread_static_fields");
        annotator
            .value(self.num_handle_thread_statics_unpacked())
            .name("num_handle_thread_statics");
        annotator
            .value(self.num_boxed_thread_statics_unpacked())
            .name("num_boxed_thread_statics");
        annotator
            .value(self.num_gc_thread_static_field_bytes_unpacked())
            .name("num_gc_thread_static_field_bytes");
        annotator
            .value(self.num_non_virtual_slots_unpacked())
            .name("num_non_virtual_slots");
        Ok(())
    }

    pub fn num_fields_total(&self) -> usize {
        (self.num_instance_fields() + self.num_static_fields()) as usize
    }
}