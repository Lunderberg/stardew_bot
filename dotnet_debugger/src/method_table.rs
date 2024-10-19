use std::{borrow::Borrow, ops::Range};

use dll_unpacker::{
    dll_unpacker::{MetadataTableIndex, TypeDef},
    Annotation, Annotator,
};
use memory_reader::{ByteRange, MemoryReader, OwnedBytes, Pointer};

use crate::{
    extensions::all_ok::AllOk as _, unpack_fields, CorElementType, Error,
    FieldDescription, FieldDescriptions, ReadTypedPointer, RuntimeModule,
    RuntimeType, TypeHandle, TypedPointer,
};

#[derive(Clone)]
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
        parent_method_table: {Option<TypedPointer<MethodTable>>, 16..24},
        module: {TypedPointer<RuntimeModule>, 24..32},
        writable_data: {Pointer, 32..40},
        ee_class_or_canonical_table: {Pointer, 40..48},
        element_type_handle_or_per_instance_info: {Pointer, 48..56},
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
            .opt_value(self.parent_method_table_unpacked())
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

    pub fn has_indirect_parent_method_table(&self) -> bool {
        // In .NET 6 and earlier, parent may be an "indirect" pointer.
        self.flags() & 0x00800000 > 0
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
            Some(RuntimeType::Class {
                method_table: Some(self.ptr()),
            })
        } else if type_flag == 0x000A0000 {
            // This is a dyanamically-sized 1-d array.
            None
        } else if type_flag == 0x00080000 {
            // This is a multi-dimensional array array, but
            // determining the element type and size require the field
            // metadata to unpack.
            None
        } else if type_flag == 0x00040000 || type_flag == 0x00050000 {
            Some(RuntimeType::ValueType {
                method_table: self.ptr(),
                size: self.base_size(),
            })
        } else if type_flag == 0x00070000 {
            // This is a RuntimePrimType, but which primitive type
            // cannot be determined solely from the MethodTable.
            // Instead, this case should be handled with
            // `EEClass::prim_type`.
            None
        } else if type_flag == 0x00060000 {
            // This is a primitive type, but not a "true
            // primitive" type.  I think this just means that it's
            // a user-defined enum.
            Some(RuntimeType::ValueType {
                method_table: self.ptr(),
                size: self.base_size(),
            })
        } else if type_flag == 0x000C0000 {
            // This is the method table for an interface.  Since I'm
            // only interested in the fields, this doesn't given any
            // useful information, but is valid.  Treating it as if it
            // is a Class, for now.
            Some(RuntimeType::Class {
                method_table: Some(self.ptr()),
            })
        } else {
            None
        }
    }

    pub fn is_nullable(&self) -> bool {
        (self.flags() & 0x000F0000) == 0x00050000
    }

    pub fn is_class(&self) -> bool {
        let flags = self.flags();
        let type_flag = flags & 0x000F0000;
        type_flag == 0
    }

    pub fn is_array(&self) -> bool {
        let flags = self.flags();
        let type_flag = flags & 0x000F0000;
        type_flag == 0x000A0000
    }

    pub fn is_string(&self) -> bool {
        matches!(self.local_runtime_type(), Some(RuntimeType::String))
    }

    pub fn is_prim_type(&self) -> bool {
        // Can't be expressed in terms of `self.local_runtime_type()`,
        // because we don't actually know the exact PrimType based on
        // the fields in the MethodTable.  That information is only
        // stored in the EEClass.  But often it is sufficient to check
        // if something is a prim_type.
        let flags = self.flags();
        let type_flag = flags & 0x000F0000;
        type_flag == 0x00070000
    }

    pub fn is_multi_dim_array(&self) -> bool {
        // Can't be expressed in terms of `self.local_runtime_type()`,
        // because we can't actually determine the array's rank,
        // element type, or shape based solely on the fields in the
        // MethodTable.  That information is only stored in the
        // EEClass.  But often it is sufficient to check if something
        // is a multi-dimensional array.
        let flags = self.flags();
        let type_flag = flags & 0x000F0000;
        type_flag == 0x00080000
    }

    pub fn runtime_type(
        &self,
        reader: impl Borrow<MemoryReader>,
    ) -> Result<RuntimeType, Error> {
        if let Some(ty) = self.local_runtime_type() {
            Ok(ty)
        } else if self.is_array() {
            let reader = reader.borrow();
            let element_type = self
                .array_element_type()
                .expect("Returns Some(_) for multi-dim array")
                .read(reader)?
                .runtime_type(reader)?;
            Ok(RuntimeType::Array {
                element_type: Some(Box::new(element_type)),
            })
        } else if self.is_multi_dim_array() {
            let reader = reader.borrow();
            let rank = self.get_ee_class(reader)?.multi_dim_rank() as usize;
            let element_type = self
                .array_element_type()
                .expect("Returns Some(_) for multi-dim array")
                .read(reader)?
                .runtime_type(reader)?;
            Ok(RuntimeType::MultiDimArray {
                element_type: Some(Box::new(element_type)),
                rank,
            })
        } else if self.is_prim_type() {
            let reader = reader.borrow();
            let ee_class = self.get_ee_class(reader)?;
            let element_type = ee_class.element_type()?;
            if let CorElementType::Prim(prim) = element_type {
                Ok(RuntimeType::Prim(prim))
            } else {
                Err(Error::ExpectedPrimType(element_type))
            }
        } else {
            let type_flag = self.flags() & 0x000F0000;
            Err(Error::InvalidTypeFlag(type_flag))
        }
    }

    pub fn array_element_type(&self) -> Option<TypedPointer<MethodTable>> {
        (self.is_array() || self.is_multi_dim_array()).then(|| {
            let ptr = self.element_type_handle_or_per_instance_info();
            ptr.into()
        })
    }

    pub fn has_generics(&self) -> bool {
        let flags = self.flags();

        // These two bits encode whether the type contains any generics.
        //    0: No generics
        //    1: Instantiation of a generic type
        //    2: Shared instantiation between compatible types
        //    3: Generic type in terms of formal parameters
        (flags & 0x00000030) > 0
    }

    pub fn generic_types_excluding_base_class(
        &self,
        reader: &MemoryReader,
    ) -> Result<impl Iterator<Item = TypedPointer<TypeHandle>>, Error> {
        let iter = self.has_generics().then(|| -> Result<_,Error> {
            // Step 1: If the flag-check passed, then bytes 48-56 hold
            // the location of per-instance information of a generic
            // type.  However, in order to find the generics of this
            // MethodTable, and not those of the parents, we need to
            // apply an offset to the pointer.  The size of the offset
            // is at negative offsets relative to the pointer that is
            // actually being stored.  (-4 byte offset to -2 byte
            // offset)
            let ptr = self.element_type_handle_or_per_instance_info();
            let per_inst_info_bytes = reader.read_bytes(ptr - 4..ptr)?;
            let num_dicts =
                per_inst_info_bytes.subrange(0..2).unpack::<u16>()? as usize;

            let location_of_ptr_to_array =
                ptr + (num_dicts - 1) * Pointer::SIZE;
            let ptr_to_array: Pointer =
                reader.read_byte_array(location_of_ptr_to_array)?.into();

            // Step 2: Now, we need to read some number of elements
            // from this array.  The number of elements in this array
            // is also stored at negative offsets relative to the
            // original pointer.  (-2 byte offset to 0 byte offset)
            let num_type_params =
                per_inst_info_bytes.subrange(2..4).unpack::<u16>()? as usize;

            // Step 3: Read the actual array.  This is now the third
            // value being read based on a single pointer, none of
            // which are actually stored at the pointed-to location.
            // (Well, unless `num_dicts` is one, but that just means
            // it's sometimes possible to get the right answer while
            // skipping a step.)
            let generic_arg_bytes = reader.read_bytes(
                ptr_to_array..ptr_to_array + num_type_params * Pointer::SIZE,
            )?;

            let iter = (0..num_type_params)
                .map(move |i| -> TypedPointer<TypeHandle> {
                    let ptr = generic_arg_bytes
                        .subrange(i * Pointer::SIZE..(i + 1) * Pointer::SIZE)
                        .unpack()
                        .expect("Unpacking of pointer only depends on size of slice");
                    ptr
                });
            Ok(iter)
        }).transpose()?.into_iter().flatten();

        Ok(iter)
    }

    pub fn generic_types(
        &self,
        reader: impl Borrow<MemoryReader>,
    ) -> Result<Vec<TypedPointer<TypeHandle>>, Error> {
        let reader = reader.borrow();

        let element_type =
            std::iter::successors(Some(Ok(self.clone())), |res_prev| {
                let prev = res_prev.as_ref().ok()?;
                let element_type = prev.array_element_type()?;
                Some(element_type.read(reader))
            })
            .last()
            .expect("Iterator will at least produce self")?;

        let from_self =
            element_type.generic_types_excluding_base_class(reader)?;
        let from_parents = element_type
            .iter_parents(reader)
            .map(|res_parent| {
                res_parent.and_then(|parent| {
                    parent.generic_types_excluding_base_class(reader)
                })
            })
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .flatten();

        Ok(from_self.chain(from_parents).collect())
    }

    pub fn has_non_instantiated_generic_types(
        &self,
        reader: impl Borrow<MemoryReader>,
    ) -> Result<bool, Error> {
        if !self.has_generics() {
            return Ok(false);
        }

        let reader = reader.borrow();
        self.generic_types(reader)?
            .into_iter()
            .all_ok(|type_handle_ptr| {
                let type_handle = type_handle_ptr.read(reader)?;
                let is_non_instantiated_generic = match type_handle {
                    TypeHandle::MethodTable(_) => false,
                    TypeHandle::TypeDescription(type_desc) => {
                        match type_desc.element_type() {
                            CorElementType::Var => true,
                            _ => false,
                        }
                    }
                };

                Ok(is_non_instantiated_generic)
            })
    }

    pub fn token(&self) -> Option<MetadataTableIndex<TypeDef>> {
        let index = self.raw_token() as usize;
        (index > 0).then(|| MetadataTableIndex::new(index - 1))
    }

    pub fn canonical_method_table_ptr(
        &self,
    ) -> Option<TypedPointer<MethodTable>> {
        let ptr = self.ee_class_or_canonical_table();
        let is_ee_class = ptr.as_usize() & 3usize == 0;

        if is_ee_class {
            None
        } else {
            let ptr = ptr & !3;
            Some(ptr.into())
        }
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
        self.parent_method_table()
            .map(|ptr| ptr.read(reader))
            .transpose()
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
        reader: impl Borrow<MemoryReader>,
    ) -> Result<Option<FieldDescriptions>, Error> {
        let reader = reader.borrow();
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
        if ptr.as_usize() % Pointer::SIZE == 0 {
            MethodTable::read(ptr, reader)
        } else {
            Err(Error::MisalignedMethodTable(ptr))
        }
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

        // This field is only available for multi-dimensional array.
        multi_dim_rank: {u8, 56..57},
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

    pub fn element_type(&self) -> Result<CorElementType, Error> {
        let norm_type: u8 = self.norm_type();
        norm_type.try_into()
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
