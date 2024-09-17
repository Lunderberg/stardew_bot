use std::ops::Range;

use dll_unpacker::{
    dll_unpacker::{MetadataTableIndex, TypeDef},
    Annotation, Annotator,
};
use memory_reader::{ByteRange, MemoryReader, OwnedBytes, Pointer};

use crate::{
    runtime_type::RuntimePrimType, unpack_fields, CorElementType, Error,
    FieldDescription, FieldDescriptions, ReadTypedPointer, RuntimeModule,
    RuntimeType, TypedPointer,
};

pub struct MethodTable {
    pub bytes: OwnedBytes,
}

pub struct EEClass {
    pub bytes: OwnedBytes,
}

pub struct EEClassPackedFields<'a> {
    pub bytes: ByteRange<'a>,
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
        parent_method_table: {TypedPointer<MethodTable>, 16..24},
        module: {TypedPointer<RuntimeModule>, 24..32},
        writable_data: {Pointer, 32..40},
        ee_class_or_canonical_table: {Pointer, 40..48},
        element_type_handle: {TypedPointer<MethodTable>, 48..56},
    }

    pub fn read(ptr: Pointer, reader: &MemoryReader) -> Result<Self, Error> {
        let bytes = reader.read_bytes(ptr..ptr + Self::SIZE)?;
        Ok(Self { bytes })
    }

    pub fn ptr(&self) -> TypedPointer<Self> {
        self.bytes.start().into()
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
            .opt_value(self.token())
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

    pub fn component_size(&self) -> Option<usize> {
        let flags = self.flags();
        let has_component_size = (flags & 0x80000000) > 0;
        if has_component_size {
            let component_size = self.flags() & 0xFFFF;
            Some(component_size as usize)
        } else {
            None
        }
    }

    fn local_runtime_type(&self) -> Option<RuntimeType> {
        let flags = self.flags();
        let type_flag = flags & 0x000F0000;

        if type_flag == 0 && self.component_size() == Some(2) {
            Some(RuntimeType::String)
        } else if type_flag == 0 {
            Some(RuntimeType::Class)
        } else if type_flag == 0x000A0000 {
            Some(RuntimeType::Array)
        } else if type_flag == 0x00080000 {
            todo!("Unpacking of ELEMENT_TYPE_ARRAY")
        } else if type_flag == 0x00040000 {
            Some(RuntimeType::ValueType {
                method_table: self.ptr_range().start.into(),
                size: self.base_size(),
            })
        } else if type_flag == 0x00060000 {
            // This is a RuntimePrimType, but which primitive type
            // cannot be determined solely from the MethodTable.
            // Instead, this case should be handled with
            // `EEClass::prim_type`.
            None
        } else {
            None
        }
    }

    pub fn is_array(&self) -> bool {
        matches!(self.local_runtime_type(), Some(RuntimeType::Array))
    }

    pub fn is_string(&self) -> bool {
        matches!(self.local_runtime_type(), Some(RuntimeType::String))
    }

    pub fn runtime_type(
        &self,
        reader: &MemoryReader,
    ) -> Result<RuntimeType, Error> {
        if let Some(ty) = self.local_runtime_type() {
            Ok(ty)
        } else {
            let ee_class = self.get_ee_class(reader)?;
            if let Some(prim) = ee_class.prim_type() {
                Ok(RuntimeType::Prim(prim))
            } else {
                let type_flag = self.flags() & 0x000F0000;
                Err(Error::InvalidTypeFlag(type_flag))
            }
        }
    }

    pub fn array_element_type(&self) -> Option<TypedPointer<MethodTable>> {
        if matches!(self.local_runtime_type()?, RuntimeType::Array) {
            Some(self.element_type_handle())
        } else {
            None
        }
    }

    pub fn token(&self) -> Option<MetadataTableIndex<TypeDef>> {
        let index = self.raw_token() as usize;
        (index > 0).then(|| MetadataTableIndex::new(index - 1))
    }

    pub fn ee_class_ptr(
        &self,
        reader: &MemoryReader,
    ) -> Result<TypedPointer<EEClass>, Error> {
        let ptr = self.ee_class_or_canonical_table();
        let is_ee_class = ptr.as_usize() & 3usize == 0;

        let ptr = if is_ee_class {
            ptr
        } else {
            let canonical_method_table: Pointer = (ptr.as_usize() & !3).into();
            reader.read_byte_array(canonical_method_table + 40)?.into()
        };

        Ok(ptr.into())
    }

    pub fn get_ee_class(
        &self,
        reader: &MemoryReader,
    ) -> Result<EEClass, Error> {
        let ptr = self.ee_class_ptr(reader)?;
        let ee_class = ptr.read(reader)?;
        Ok(ee_class)
    }

    pub fn get_parent(
        &self,
        reader: &MemoryReader,
    ) -> Result<Option<Self>, Error> {
        let ptr = self.parent_method_table();
        let parent = if ptr.is_null() {
            None
        } else {
            Some(ptr.read(reader)?)
        };

        Ok(parent)
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

        let bytes = reader.read_bytes(ptr..ptr + nbytes)?;
        Ok(Some(FieldDescriptions { bytes }))
    }
}

impl ReadTypedPointer for MethodTable {
    fn read_typed_ptr(
        ptr: Pointer,
        reader: &MemoryReader,
    ) -> Result<Self, Error> {
        MethodTable::read(ptr, reader)
    }
}

impl EEClass {
    pub fn read(
        location: Pointer,
        reader: &MemoryReader,
    ) -> Result<Self, Error> {
        let base_size = 56;
        // EEClass stores extra data after the end of the class
        // members.  The length of these is about 44 bytes (11 fields
        // * 4 bytes/field), but they occur after the subclass's
        // fields.
        //
        // This will need to change for .NET 9.0 onward, which moves
        // these sizes into the class definition.
        // https://github.com/dotnet/runtime/commit/8b581cad
        // (2024-01-24)
        let extra_size = 256;
        let bytes =
            reader.read_bytes(location..location + base_size + extra_size)?;
        Ok(Self { bytes })
    }

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

    pub fn prim_type(&self) -> Option<RuntimePrimType> {
        let norm_type: u8 = self.norm_type();
        let element_type: CorElementType = norm_type.try_into().ok()?;
        match element_type {
            CorElementType::Prim(prim_type) => Some(prim_type),
            _ => None,
        }
    }
}

impl ReadTypedPointer for EEClass {
    fn read_typed_ptr(
        ptr: Pointer,
        reader: &MemoryReader,
    ) -> Result<Self, Error> {
        EEClass::read(ptr, reader)
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
