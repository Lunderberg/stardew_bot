use derive_more::From;
use paste::paste;
use std::{borrow::Borrow, marker::PhantomData, ops::Range};

use memory_reader::{
    ByteRange, Pointer, UnpackBytes, UnpackOptBytes, UnpackedValue,
};

use crate::enum_map::{EnumKey, EnumMap};
use crate::intermediate_language::CILMethod;
use crate::portable_executable::DataDirectoryKind;
use crate::relative_virtual_address::{
    RelativeVirtualAddress, VirtualAddressRelocation, VirtualRange,
};
use crate::{Error, Signature, UnpackedBlob};

/// Convenience function for unpacking DLL metadata.
///
/// Returns a `MetadataLayout` object, containing the cache-able
/// information required to interpret a CLR DLL.
pub fn unpack_metadata_layout<'a>(
    dll_bytes: impl Into<ByteRange<'a>>,
) -> Result<MetadataLayout, Error> {
    DLLUnpacker::new(dll_bytes).metadata_layout()
}

#[derive(Copy, Clone)]
pub struct DLLUnpacker<'a> {
    pub(crate) bytes: ByteRange<'a>,
}

pub struct ClrRuntimeHeader<'a> {
    pub(crate) bytes: ByteRange<'a>,
}

#[derive(Copy, Clone)]
pub struct RawCLRMetadata<'a> {
    /// The bytes owned by the Metadata stream, as determined from the
    /// CLR runtime header.
    pub(crate) bytes: ByteRange<'a>,
}

pub struct StreamHeader<'a> {
    /// The offset of the stream, in bytes, relative to the start of
    /// the metadata.
    pub offset: UnpackedValue<u32>,

    /// The size of the stream, in bytes.
    pub size: UnpackedValue<u32>,

    /// The name of the metadata section.  Max of 32 ASCII characters.
    pub name: UnpackedValue<&'a str>,

    /// The location of the stream
    ptr_range: Range<Pointer>,

    /// The contents of the stream
    pub bytes: ByteRange<'a>,
}

pub struct MetadataTableHeader<'a> {
    /// The entire contents of the #~ stream of the metadata.  While
    /// most of the unpacker types only have access to bytes that they
    /// directly contain, the size of the header for metadata tables
    /// depends on the number of tables present.
    bytes: ByteRange<'a>,
}

#[derive(Clone, Copy)]
pub struct Metadata<'a> {
    layout: &'a MetadataLayout,
    dll_bytes: ByteRange<'a>,
}

#[derive(Clone, Copy)]
pub struct HeapSizes {
    string_stream_uses_u32_addr: bool,
    guid_stream_uses_u32_addr: bool,
    blob_stream_uses_u32_addr: bool,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MetadataTableKind {
    Module,
    TypeRef,
    TypeDef,
    Field,
    MethodDef,
    Param,
    InterfaceImpl,
    MemberRef,
    Constant,
    CustomAttribute,
    FieldMarshal,
    DeclSecurity,
    ClassLayout,
    FieldLayout,
    StandAloneSig,
    EventMap,
    Event,
    PropertyMap,
    Property,
    MethodSemantics,
    MethodImpl,
    ModuleRef,
    TypeSpec,
    ImplMap,
    FieldRVA,
    Assembly,
    AssemblyProcessor,
    AssemblyOS,
    AssemblyRef,
    AssemblyRefProcessor,
    AssemblyRefOS,
    File,
    ExportedType,
    ManifestResource,
    NestedClass,
    GenericParam,
    MethodSpec,
    GenericParamConstraint,
}

#[derive(Clone, Copy, Debug)]
pub enum MetadataHeapKind {
    String,
    GUID,
    Blob,
}

#[derive(Clone, Copy, Debug)]
pub enum MetadataCodedIndexKind {
    TypeDefOrRef,
    HasConstant,
    HasCustomAttribute,
    HasFieldMarshal,
    HasDeclSecurity,
    MemberRefParent,
    HasSemantics,
    MethodDefOrRef,
    MemberForwarded,
    Implementation,
    CustomAttributeType,
    ResolutionScope,
    TypeOrMethodDef,
}

#[derive(Clone, Copy, Debug, From)]
pub enum MetadataIndexKind {
    Heap(MetadataHeapKind),
    Table(MetadataTableKind),
    CodedIndex(MetadataCodedIndexKind),
}

#[derive(Clone, Copy, Debug, From)]
pub enum MetadataColumnType {
    Index(MetadataIndexKind),
    FixedSize(usize),
}

pub struct MetadataLayout {
    /// The relocations to be used for any RVA fields (e.g. the RVA
    /// field of the MethodDef table).  These fields point to
    /// locations within the DLL, but not necessarily within the
    /// metadata section.  These relocations are determined from the
    /// PE section headers of the DLL.
    rva_relocations: Vec<VirtualAddressRelocation>,

    /// The location of each metadata heap.  Variable-sized fields,
    /// such as strings and type signatures, are stored as references
    /// into these heaps.
    heap_locations: EnumMap<MetadataHeapKind, Range<Pointer>>,

    /// Number of rows in each MetadataTable.  This is determined from
    /// the variable-sized header at the start of the #~ metadata
    /// stream.
    num_rows: EnumMap<MetadataTableKind, usize>,

    /// The location of all metadata tables.  This is the #~ stream in
    /// the metadata, excluding the variable-sized header.
    tables_location: Range<Pointer>,

    /// Location of each metadata table, relative to the start of the
    table_offsets: EnumMap<MetadataTableKind, usize>,

    /// Number of bytes (either 2 or 4) used to specify indices in the
    /// metadata.
    ///
    /// * #GUID heap: Offset relative to the start of the #GUID heap,
    ///   index specifies the number of 128-bit GUIDs to advancePoints
    ///   to a 128-bit GUID.  Size of the index depends on the number
    ///   of GUIDs present.
    ///
    /// * All other heaps: Offset relative to the start of the
    ///   corresponding heap, in bytes.  Size of the index depends on
    ///   the size of the pointed-to heap, in bytes.
    ///
    /// * Simple table indices: Point to a row within a specific
    ///   table.  Size depends on the number of rows in the pointed-to
    ///   table.
    ///
    ///   Index is one-indexed, with a value of zero used to indicate
    ///   an absent value.
    ///
    /// * Coded table index: Point to a row within a set of allowed
    ///   tables.  Size depends on the number of allowed tables and
    ///   the maximum number of rows across tables in that set.
    index_size: EnumMap<MetadataIndexKind, usize>,

    /// Number of bytes for each row in a metadata table.  Depends on
    /// the columns present in that metadata, including both
    /// fixed-width fields of those columns (e.g. flags), and the size
    /// of variable-width fields (as stored in `index_size`).
    bytes_per_row: EnumMap<MetadataTableKind, usize>,
}

impl MetadataLayout {
    pub(crate) fn virtual_address_to_raw(
        &self,
        addr: RelativeVirtualAddress,
    ) -> Result<Pointer, Error> {
        self.rva_relocations
            .iter()
            .find_map(|relocation| relocation.apply(addr))
            .ok_or(Error::InvalidVirtualAddress(addr))
    }
}

pub struct MetadataTable<'a, TableTag> {
    /// The bytes that represent this table.
    bytes: ByteRange<'a>,

    /// The CLR metadata that contains this table.
    metadata: Metadata<'a>,

    /// A compile-time tag indicating which table this is.
    _phantom: PhantomData<TableTag>,
}

pub trait MetadataColumnValue {
    const SIZE: usize;
}

pub trait TypedMetadataIndex {
    type Output<'a: 'b, 'b>;

    fn access<'a: 'b, 'b>(
        self,
        metadata: &'b Metadata<'a>,
    ) -> Result<Self::Output<'a, 'b>, Error>;

    fn location(self, metadata: &Metadata) -> Result<Range<Pointer>, Error>;
}

/// Typed index into a specific metadata type
#[derive(Clone, Copy, Debug)]
pub struct MetadataTableIndex<TableTag> {
    index: usize,
    _phantom: PhantomData<TableTag>,
}

#[derive(Clone, Copy)]
pub struct MetadataHeapIndex<HeapTag> {
    index: usize,
    _phantom: PhantomData<HeapTag>,
}

/// Typed index into a specific metadata type
///
/// When #![feature(step_trait)] is stabilized
/// (https://github.com/rust-lang/rust/issues/42168), this will be
/// replaced with `Range<MetadataTableIndex<Unpacker>>`.
#[derive(Clone, Copy)]
pub struct MetadataTableIndexRange<TableTag> {
    pub(crate) start: usize,
    pub(crate) end: usize,
    _phantom: PhantomData<TableTag>,
}

#[derive(Clone, Copy, Debug)]
pub struct MetadataCodedIndex<CodedIndexType> {
    index: usize,
    kind: MetadataTableKind,
    _phantom: PhantomData<CodedIndexType>,
}

pub struct MetadataRow<'a, TableTag> {
    /// The bytes that are directly owned by this row of the table.
    bytes: ByteRange<'a>,

    /// The bytes that are directly owned by the next row of the
    /// table.  This is needed to interpret columns that provide a
    /// start index, with the end index provided by the same column in
    /// the next row (e.g. the Field and Method columns of the TypeDef
    /// table).
    next_row_bytes: Option<ByteRange<'a>>,

    /// The typed index that refers to this row.
    index: MetadataTableIndex<TableTag>,

    /// The metadata object that contains this row.
    pub(crate) metadata: Metadata<'a>,
}

pub trait MetadataTableTag: Copy {
    const KIND: MetadataTableKind;

    const COLUMNS: &'static [MetadataColumnType];
}

mod column_type {
    #![allow(non_upper_case_globals, dead_code)]

    use super::{
        MetadataCodedIndexKind, MetadataColumnType, MetadataHeapKind,
        MetadataIndexKind, MetadataTableKind,
    };

    pub(super) use super::MetadataColumnType::FixedSize;

    macro_rules! heap_kinds {
        ( $( $name:ident ),* $(,)? ) => {
            $(
                pub(super) const $name: MetadataColumnType =
                    MetadataColumnType::Index(
                        MetadataIndexKind::Heap(
                            MetadataHeapKind::$name
                        ),
                    );
            )*
        };
    }
    heap_kinds! {String, GUID, Blob}

    macro_rules! simple_index_kinds {
        ( $( $name:ident ),* $(,)? ) => {
            $(
                pub(super) const $name: MetadataColumnType =
                    MetadataColumnType::Index(
                        MetadataIndexKind::Table(
                            MetadataTableKind::$name,
                        )
                    );
            )*
        }
    }
    simple_index_kinds! {
        Module, TypeRef, TypeDef, Field, MethodDef, Param,
        InterfaceImpl, MemberRef, Constant, CustomAttribute,
        FieldMarshal, DeclSecurity, ClassLayout, FieldLayout,
        StandAloneSig, EventMap, Event, PropertyMap, Property,
        MethodSemantics, MethodImpl, ModuleRef, TypeSpec,
        ImplMap, FieldRVA, Assembly, AssemblyProcessor,
        AssemblyOS, AssemblyRef, AssemblyRefProcessor,
        AssemblyRefOS, File, ExportedType, ManifestResource,
        NestedClass, GenericParam, MethodSpec,
        GenericParamConstraint,
    }

    macro_rules! coded_index_kinds {
        ( $( $name:ident ),* $(,)? ) => {
            $(
                pub(super) const $name: MetadataColumnType =
                    MetadataColumnType::Index(
                        MetadataIndexKind::CodedIndex(
                            MetadataCodedIndexKind::$name,
                        )
                    );
            )*
        }
    }
    coded_index_kinds! {
        TypeDefOrRef, HasConstant, HasCustomAttribute,
        HasFieldMarshal, HasDeclSecurity, MemberRefParent,
        HasSemantics, MethodDefOrRef, MemberForwarded,
        Implementation, CustomAttributeType, ResolutionScope,
        TypeOrMethodDef,
    }
}

impl MetadataColumnValue for u8 {
    const SIZE: usize = 1;
}
impl MetadataColumnValue for u16 {
    const SIZE: usize = 2;
}
impl MetadataColumnValue for u32 {
    const SIZE: usize = 4;
}
impl MetadataColumnValue for FieldFlags {
    const SIZE: usize = 2;
}
impl MetadataColumnValue for RelativeVirtualAddress {
    const SIZE: usize = 4;
}
impl MetadataColumnValue for MethodDefFlags {
    const SIZE: usize = 2;
}

macro_rules! decl_metadata_table {
    (
        $(
            $table_name:ident: { $(

                $field_name:ident : $field_type:tt

            ),* $(,)? }
        ),* $(,)?
    ) => {
        $(
            #[derive(Clone, Copy, Debug)]
            pub struct $table_name;

            impl MetadataTableTag for $table_name {
                const KIND: MetadataTableKind =
                    MetadataTableKind::$table_name;

                const COLUMNS: &'static [MetadataColumnType] = &[
                    $( decl_metadata_table!(column $field_type) ),*
                ];
            }


            decl_metadata_table!{
                member_accessors
                    0usize,
                    $table_name: {
                        $( $field_name: $field_type, )*
                    }
            }

        )*
    };


    (column {value $name:ident}) => {
        column_type::FixedSize(
            <$name as MetadataColumnValue>::SIZE
        )
    };
    (column {value Option<$name:ident>}) => {
        column_type::FixedSize(
            <$name as MetadataColumnValue>::SIZE
        )
    };
    (column {Option<$name:ident>}) => { column_type::$name };
    (column {heap $name:ident}) => { column_type::$name };
    (column {range $name:ident $plural:ident}) => { column_type::$name };
    (column {index $name:ident}) => { column_type::$name };
    (column {coded_index $name:ident}) => { column_type::$name };
    (column {coded_index Option<$name:ident>}) => { column_type::$name };
    (column $name:ident) => { column_type::$name };


    (
        member_accessors
            $field_index:expr,
            $table_name:ident: { }
    ) => { };

    (
        member_accessors
            $field_index:expr,
            $table_name:ident: {
                $first_field_name:ident : $first_field_type:tt,
                $(
                    $rest_field_name:ident : $rest_field_type:tt,
                )*
            }
    ) => {
        decl_metadata_table!{
            member_accessor
                $field_index,
                $table_name: { $first_field_name: $first_field_type }
        }

        decl_metadata_table!{
            member_accessors
                ($field_index + 1usize),
                $table_name: {
                    $(
                        $rest_field_name: $rest_field_type,
                    )*
                }
        }

    };

    (
        member_accessor
            $field_index: expr,
            $table_name:ident: {$field_name:ident: {value $field_type:ident}}
    ) => {
        paste!{
            impl<'a> MetadataRow<'a, $table_name> {
                pub(crate) fn [< $field_name _unpacked >](
                    &self,
                ) -> Result<UnpackedValue<$field_type>, Error> {
                    Ok(self.get_field_bytes($field_index).unpack()?)
                }

                pub fn $field_name(&self) -> Result<$field_type, Error> {
                    let unpacked = self. [< $field_name _unpacked >] ()?;
                    Ok(unpacked.value())
                }

            }
        }
    };

    (
        member_accessor
            $field_index: expr,
            $table_name:ident: {
                $field_name:ident: {
                    value Option<$field_type:ident>
                }
            }
    ) => {
        paste!{
            impl<'a> MetadataRow<'a, $table_name> {
                pub(crate) fn [< $field_name _unpacked >](
                    &self,
                ) -> Result<UnpackedValue<Option<$field_type>>, Error> {
                    self.get_field_bytes($field_index).unpack()
                }

                pub fn $field_name(&self) -> Result<Option<$field_type>, Error> {
                    let unpacked = self. [< $field_name _unpacked >] ()?;
                    Ok(unpacked.value())
                }

            }
        }
    };

    (
        member_accessor
            $field_index: expr,
            $table_name:ident: {$field_name:ident: {heap $heap_type:ident}}
    ) => {
        paste!{
            impl<'a> MetadataRow<'a, $table_name> {
                pub(crate) fn [< $field_name _index >](
                    &self,
                ) -> Result<UnpackedValue<MetadataHeapIndex<$heap_type>>, Error> {
                    self.get_field_bytes($field_index).unpack()
                }

                pub fn [< $field_name _location >](
                    &self,
                ) -> Result<Pointer, Error> {
                    let index = self. [< $field_name _index >] ()?;
                    let index = index.value();
                    let location = self.metadata.location_of(index)?;
                    Ok(location.start)
                }

                pub fn $field_name<'b>(
                    &'b self,
                ) -> Result<
                    <MetadataHeapIndex<$heap_type> as TypedMetadataIndex>::Output<'a,'b>,
                    Error,
                > {
                    let index = self. [< $field_name _index >] ()?;
                    let index = index.value();
                    self.metadata.get(index)
                }

            }
        }
    };

    (
        member_accessor
            $field_index: expr,
            $table_name:ident: {$field_name:ident: GUID}
    ) => {
        paste!{
            impl<'a> MetadataRow<'a, $table_name> {
                pub(crate) fn [< $field_name _index >](
                    &self,
                ) -> Result<UnpackedValue<MetadataHeapIndex<GUID>>, Error> {
                    self.get_field_bytes($field_index).unpack()
                }

                pub(crate) fn [< $field_name _unpacked >](
                    &self,
                ) -> Result<UnpackedValue<u128>, Error> {
                    let index = self. [< $field_name _index >] ()?;
                    let index = index.value();
                    self.metadata.get(index)
                }

                pub fn $field_name(&self) -> Result<u128, Error> {
                    let unpacked = self. [< $field_name _unpacked >] ()?;
                    Ok(unpacked.value())
                }

            }
        }
    };

    (
        member_accessor
            $field_index: expr,
            $table_name:ident: {$field_name:ident: {Option<GUID>}}
    ) => {
        paste!{
            impl<'a> MetadataRow<'a, $table_name> {
                pub(crate) fn [< $field_name _index >](
                    &self,
                ) -> Result<UnpackedValue<Option<MetadataHeapIndex<GUID>>>, Error> {
                    self.get_field_bytes($field_index).unpack()
                }

                pub(crate) fn [< $field_name _unpacked >](
                    &self,
                ) -> Result<Option<UnpackedValue<u128>>, Error> {
                    let opt_index = self. [< $field_name _index >] ()?.value();

                    let opt_guid = opt_index.map(|index| self.metadata.get(index)).transpose()?;
                    Ok(opt_guid)
                }

                pub fn $field_name(&self) -> Result<Option<u128>, Error> {
                    let opt_unpacked = self. [< $field_name _unpacked >] ()?;
                    Ok(opt_unpacked.map(|unpacked| unpacked.value()))
                }

            }
        }
    };

    (
        member_accessor
            $field_index: expr,
            $table_name:ident: {$field_name:ident: {coded_index $coded_type:ident}}
    ) => {
        paste!{
            impl<'a> MetadataRow<'a, $table_name> {
                pub(crate) fn [< $field_name _index >](
                    &self,
                ) -> Result<UnpackedValue<MetadataCodedIndex<$coded_type>>, Error> {
                    self.get_field_bytes($field_index).unpack()
                }

                pub fn $field_name<'b>(&'b self) -> Result<
                    <
                        MetadataCodedIndex<$coded_type> as TypedMetadataIndex
                    >::Output<'a,'b>,
                    Error,
                > {
                    let index = self. [< $field_name _index >] ()?;
                    self.metadata.get(index)
                }

            }
        }
    };

    (
        member_accessor
            $field_index: expr,
            $table_name:ident: {
                $field_name:ident: {
                    coded_index Option<$coded_type:ident>
                }
            }
    ) => {
        paste!{
            impl<'a> MetadataRow<'a, $table_name> {
                pub(crate) fn [< $field_name _index >](
                    &self,
                ) -> Result<
                        UnpackedValue<Option<MetadataCodedIndex<$coded_type>>>,
                        Error,
                > {
                    self.get_field_bytes($field_index).unpack()
                }

                pub fn $field_name<'b>(
                    &'b self
                ) -> Result<
                    Option<
                        <
                            MetadataCodedIndex<$coded_type>
                            as TypedMetadataIndex
                        >::Output<'a,'b>
                    >,
                    Error,
                > {
                    let opt_index = self. [< $field_name _index >] ()?.value();
                    opt_index
                        .map(|index| self.metadata.get(index))
                        .transpose()
                }

            }
        }
    };

    (
        member_accessor
            $field_index: expr,
            $table_name:ident: {
                $field_name:ident: {
                    index $field_type:ident
                }
            }
    ) => {
        paste!{
            impl<'a> MetadataRow<'a, $table_name> {
                pub(crate) fn [< $field_name _index >](
                    &self,
                ) -> Result<
                        UnpackedValue<MetadataTableIndex<$field_type>>,
                        Error,
                > {
                    self.get_field_bytes($field_index).unpack()
                }

                pub fn $field_name<'b>(
                    &'b self
                ) -> Result<
                    <
                        MetadataTableIndex<$field_type> as TypedMetadataIndex
                    >::Output<'a,'b>,
                    Error,
                > {
                    let index = self. [< $field_name _index >] ()?.value();
                    self.metadata.get(index)
                }

            }
        }
    };

    (
        member_accessor
            $field_index: expr,
            $table_name:ident: {
                $field_name:ident: {
                    range
                    $field_type:ident
                    $plural_field_name:ident
                }
            }
    ) => {
        paste!{
            impl<'a> MetadataRow<'a, $table_name> {
                pub(crate) fn [< $field_name _indices >](
                    &self,
                ) -> UnpackedValue<MetadataTableIndexRange<$field_type>> {
                    self.get_index_range($field_index)
                }

                pub fn [< num_ $plural_field_name >](
                    &self,
                ) -> usize {
                    let index_range = self
                        .get_untyped_index_range($field_index)
                        .value();

                    index_range.end - index_range.start
                }

                pub fn [< iter_ $plural_field_name >]<'b>(&'b self) -> Result<
                    impl DoubleEndedIterator<Item=MetadataRow<$field_type>>,
                    Error,
                > {
                    let indices = self. [< $field_name _indices >] ();
                    self.metadata.iter_range(indices)
                }

            }
        }
    };
}

decl_metadata_table! {
    Module: {
        generation: {value u16},
        name: {heap String},
        module_id: GUID,
        enc_id: {Option<GUID>},
        enc_base_id: {Option<GUID>},
    },

    TypeRef: {
        resolution_scope: {coded_index ResolutionScope},
        name: {heap String},
        namespace: {heap String},
    },

    TypeDef: {
        flags: {value u32},
        name: {heap String},
        namespace: {heap String},
        extends: {coded_index Option<TypeDefOrRef>},
        field: {range Field fields},
        method: {range MethodDef methods},
    },

    Field: {
        flags: {value FieldFlags},
        name: {heap String},
        raw_signature: {heap Blob},
    },

    MethodDef: {
        rva: {value Option<RelativeVirtualAddress>},
        impl_flags: {value u16},
        flags: {value MethodDefFlags},
        name: {heap String},
        signature: {heap Blob},
        param: {range Param params},
    },

    Param: {
        flags: {value u16},
        sequence: {value u16},
        name: {heap String},
    },

    InterfaceImpl: {
        class: {index TypeDef},
        interface: {coded_index TypeDefOrRef},
    },

    MemberRef: {
        class: {coded_index MemberRefParent},
        name: {heap String},
        signature: {heap Blob},
    },

    Constant: {
        type_value: {value u8},
        _unused: {value u8},
        parent: {coded_index HasConstant},
        value: {heap Blob},
    },

    CustomAttribute: {
        parent: {coded_index HasCustomAttribute},
        attribute_type: {coded_index CustomAttributeType},
        value: {heap Blob},
    },

    FieldMarshal: {
        parent: {coded_index HasFieldMarshal},
        native_type: {heap Blob},
    },

    DeclSecurity: {
        action: {value u16},
        parent: {coded_index HasDeclSecurity},
        permission_set: {heap Blob},
    },

    ClassLayout: {
        alignment: {value u16},
        size: {value u32},
        class: {index TypeDef},
    },

    FieldLayout: {
        offset: {value u32},
        field: {index Field},
    },

    StandAloneSig: {
        signature: {heap Blob},
    },

    EventMap: {
        class: {index TypeDef},
        event: {range Event events},
    },

    Event: {
        flags: {value u16},
        name: {heap String},
        event_type: {coded_index Option<TypeDefOrRef>},
    },

    PropertyMap: {
        class: {index TypeDef},
        property: {range Property properties},
    },

    Property: {
        flags: {value u16},
        name: {heap String},
        signature: {heap Blob},
    },

    MethodSemantics: {
        semantics: {value u16},
        method: {index MethodDef},
        association: {coded_index HasSemantics},
    },

    MethodImpl: {
        class: {index TypeDef},
        method_body: {coded_index MethodDefOrRef},
        method_declaration: {coded_index MethodDefOrRef},
    },

    ModuleRef: {
        name: {heap String},
    },

    TypeSpec: {
        signature: {heap Blob},
    },

    ImplMap: {
        flags: {value u16},
        member_forwarded: {coded_index MemberForwarded},
        import_name: {heap String},
        import_scope: {index ModuleRef},
    },

    FieldRVA: {
        rva: {value RelativeVirtualAddress},
        field: {index Field},
    },

    Assembly: {
        hash_algorithm_id: {value u32},
        major_version: {value u16},
        minor_version: {value u16},
        build_number: {value u16},
        revision_number: {value u16},
        flags: {value u32},
        public_key: {heap Blob},
        name: {heap String},
        culture: {heap String},
    },

    AssemblyProcessor: {
        processor: {value u32},
    },

    AssemblyOS: {
        platform_id: {value u32},
        major_version: {value u32},
        minor_version: {value u32},
    },

    AssemblyRef: {
        major_version: {value u16},
        minor_version: {value u16},
        build_number: {value u16},
        revision_number: {value u16},
        flags: {value u32},
        public_key_or_token: {heap Blob},
        name: {heap String},
        culture: {heap String},
        hash_value: {heap Blob},
    },

    AssemblyRefProcessor: {
        processor: {value u32},
        assembly_ref: {index AssemblyRef},
    },

    AssemblyRefOS: {
        platform_id: {value u32},
        major_version: {value u32},
        minor_version: {value u32},
        assembly_ref: {index AssemblyRef},
    },

    File: {
        flags: {value u32},
        name: {heap String},
        hash_value: {heap Blob},
    },

    ExportedType: {
        flags: {value u32},
        type_def_id: {index TypeDef},
        name: {heap String},
        namespace: {heap String},
        implementation: {coded_index Implementation},
    },

    ManifestResource: {
        offset: {value u32},
        flags: {value u32},
        name: {heap String},
        implementation: {coded_index Option<Implementation>},
    },

    NestedClass: {
        nested_class: {index TypeDef},
        enclosing_class: {index TypeDef},
    },

    GenericParam: {
        number: {value u16},
        flags: {value u16},
        owner: {coded_index TypeOrMethodDef},
        name: {heap String},
    },

    MethodSpec: {
        method: {coded_index MethodDefOrRef},
        instantiation: {heap Blob},
    },

    GenericParamConstraint: {
        generic_param: {index GenericParam},
        constraint: {coded_index TypeDefOrRef},
    },
}

pub trait CodedIndex {
    const OPTIONS: &'static [Option<MetadataTableKind>];

    fn num_table_bits() -> usize {
        Self::OPTIONS.len().next_power_of_two().ilog2() as usize
    }

    fn table_index(raw_index: usize) -> usize {
        let table_mask = (1 << Self::num_table_bits()) - 1;
        raw_index & table_mask
    }

    fn table_kind(raw_index: usize) -> Result<MetadataTableKind, Error> {
        let table_index = Self::table_index(raw_index);
        let table_kind = Self::OPTIONS
            .get(table_index)
            .ok_or(Error::InvalidCodedIndex {
                table_index,
                num_tables: Self::OPTIONS.len(),
            })?
            .ok_or(Error::CodedIndexRefersToReservedTableIndex)?;

        Ok(table_kind)
    }

    fn row_index(raw_index: usize) -> Option<usize> {
        let row_index = raw_index >> Self::num_table_bits();
        if row_index == 0 {
            None
        } else {
            // Since `row_index==0` represents a NULL value, all
            // non-zero rows have an offset of one, so `row_index==1`
            // is the first row of a table.  Currently, this is
            // applied as part of the `get_row()` method, but it would
            // be cleaner to apply it here.  That way, all `usize`
            // instances represent valid zero-indexed Rust indices.
            Some(row_index - 1)
        }
    }
}

macro_rules! decl_core_coded_index_type {
    ($tag:ident,
     [ $( $table_type:ident ),+ $(,)? ] $(,)?
    ) => {
        paste!{
            #[derive(Clone, Copy, Debug)]
            pub struct $tag;

            pub enum [< Metadata $tag >] <'a> {
                $(
                    $table_type(MetadataRow<'a, $table_type>),
                )*
            }

            impl TypedMetadataIndex for MetadataCodedIndex<$tag> {
                type Output<'a: 'b, 'b> = [< Metadata $tag >]<'a>;

                fn access<'a: 'b, 'b>(
                    self,
                    tables: &'b Metadata<'a>,
                ) -> Result<Self::Output<'a, 'b>, Error> {
                    Ok(match self.kind {
                        $(
                            MetadataTableKind::$table_type => [< Metadata $tag >]::$table_type(
                                tables.get(MetadataTableIndex::new(self.index))?,
                            ),
                        )*

                            _ => panic!(
                                "Shouldn't be possible for {} to contain {}",
                                stringify!{$tag},
                                self.kind,
                            ),
                    })
                }

                fn location(
                    self,
                    tables: &Metadata,
                ) -> Result<Range<Pointer>, Error> {
                    Ok(match self.kind {
                        $(
                            MetadataTableKind::$table_type => {
                                let index = MetadataTableIndex::<$table_type>::new(self.index);
                                let row = tables.get(index)?;
                                row.ptr_range()
                            }
                        )*

                            _ => panic!(
                                "Shouldn't be possible for {} to contain {}",
                                stringify!{$tag},
                                self.kind,
                            ),
                    })
                }
            }



            $(
                impl std::cmp::PartialEq<MetadataTableIndex<$table_type>>
                    for MetadataCodedIndex<$tag> {
                        fn eq(&self, other: &MetadataTableIndex<$table_type>) -> bool {
                            self.kind == <$table_type as MetadataTableTag>::KIND
                                && self.index == other.index
                        }
                    }

                impl std::cmp::PartialEq<MetadataCodedIndex<$tag>>
                    for MetadataTableIndex<$table_type> {
                        fn eq(&self, other: &MetadataCodedIndex<$tag>) -> bool {
                            other.kind == <$table_type as MetadataTableTag>::KIND
                                && self.index == other.index
                        }
                    }
            )+

        }
    };
}

macro_rules! impl_coded_index {
    ($tag:ident,
     [ $( $table_type:ident ),+ $(,)? ] $(,)?
    ) => {
        impl CodedIndex for $tag {
            const OPTIONS: &'static [Option<MetadataTableKind>] = &[
                $(
                    Some(MetadataTableKind::$table_type),
                )*
            ];
        }
    };
}

macro_rules! decl_coded_index_type {
    ($tag:ident,
     [ $( $table_type:ident ),+ $(,)? ] $(,)?
    ) => {
        decl_core_coded_index_type!{ $tag, [ $($table_type),+ ] }
        impl_coded_index!{ $tag, [ $($table_type),+ ] }
    };
}

decl_coded_index_type! {
    TypeDefOrRef,
    [TypeDef, TypeRef, TypeSpec],
}
decl_coded_index_type! {
    HasConstant,
    [Field, Param, Property],
}
decl_coded_index_type! {
    HasCustomAttribute,
    [
        MethodDef, Field, TypeRef, TypeDef,
        Param, InterfaceImpl, MemberRef, Module,
        // This option is listed as "Permission" in II.24.2.6 of
        // ECMA-335, but no such metadata table exists.  It looks like
        // it might be a holdover from an earlier draft.  For now,
        // assuming that it refers to the "DeclSecurity" table, which
        // handles object permissions.
        DeclSecurity,
        Property, Event, StandAloneSig, ModuleRef,
        TypeSpec, Assembly, AssemblyRef, File,
        ExportedType, ManifestResource, GenericParam,
        GenericParamConstraint, MethodSpec
    ],
}
decl_coded_index_type! {
    HasFieldMarshal,
    [Field, Param],
}
decl_coded_index_type! {
    HasDeclSecurity,
    [TypeDef, MethodDef, Assembly],
}
decl_coded_index_type! {
    MemberRefParent,
    [TypeDef, TypeRef, ModuleRef,
     MethodDef, TypeSpec],
}
decl_coded_index_type! {
    HasSemantics,
    [Event, Property],
}
decl_coded_index_type! {
    MethodDefOrRef,
    [MethodDef, MemberRef],
}
decl_coded_index_type! {
    MemberForwarded,
    [Field, MethodDef],
}
decl_coded_index_type! {
    Implementation,
    [File, AssemblyRef, ExportedType],
}
decl_coded_index_type! {
    ResolutionScope,
    [Module, ModuleRef, AssemblyRef, TypeRef]
}
decl_coded_index_type! {
    TypeOrMethodDef,
    [TypeDef, MethodDef],
}

// The CustomAttributeType implements the `CustomAttributeType`
// without the `decl_coded_index_type`, because it contains extra
// values in the table-type encoding, represented by `None` values in
// `OPTIONS`, and these `None` values would not be generated by the
// `decl_coded_index_type` macro.
decl_core_coded_index_type! { CustomAttributeType, [MethodDef, MemberRef] }

impl CodedIndex for CustomAttributeType {
    const OPTIONS: &'static [Option<MetadataTableKind>] = &[
        None,
        None,
        Some(MetadataTableKind::MethodDef),
        Some(MetadataTableKind::MemberRef),
        None,
    ];
}

#[derive(Clone, Copy)]
pub struct FieldFlags(u16);

#[derive(Clone, Copy)]
pub struct MethodDefFlags(u16);

impl<CodedIndexType> std::fmt::Display for MetadataCodedIndex<CodedIndexType> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}[{}]", self.kind, self.index)
    }
}

impl<'a> MetadataTypeDefOrRef<'a> {
    pub fn name(&self) -> Result<&'a str, Error> {
        match self {
            MetadataTypeDefOrRef::TypeDef(row) => row.name(),
            MetadataTypeDefOrRef::TypeRef(row) => row.name(),
            MetadataTypeDefOrRef::TypeSpec(_) => Ok("TypeSpec (anon)"),
        }
    }
}

impl<'a> MetadataHasConstant<'a> {
    pub fn name(&self) -> Result<&'a str, Error> {
        match self {
            MetadataHasConstant::Field(row) => row.name(),
            MetadataHasConstant::Param(row) => row.name(),
            MetadataHasConstant::Property(row) => row.name(),
        }
    }
}

impl<'a> MetadataHasFieldMarshal<'a> {
    pub fn name(&self) -> Result<&'a str, Error> {
        match self {
            MetadataHasFieldMarshal::Field(row) => row.name(),
            MetadataHasFieldMarshal::Param(row) => row.name(),
        }
    }
}

impl<'a> MetadataHasDeclSecurity<'a> {
    pub fn name(&self) -> Result<&'a str, Error> {
        match self {
            MetadataHasDeclSecurity::TypeDef(row) => row.name(),
            MetadataHasDeclSecurity::MethodDef(row) => row.name(),
            MetadataHasDeclSecurity::Assembly(row) => row.name(),
        }
    }
}

impl<'a> MetadataMemberRefParent<'a> {
    pub fn name(&self) -> Result<&'a str, Error> {
        match self {
            MetadataMemberRefParent::TypeRef(row) => row.name(),
            MetadataMemberRefParent::TypeDef(row) => row.name(),
            MetadataMemberRefParent::ModuleRef(row) => row.name(),
            MetadataMemberRefParent::MethodDef(row) => row.name(),
            MetadataMemberRefParent::TypeSpec(_) => Ok("TypeSpec (anon)"),
        }
    }
}

impl<'a> MetadataHasSemantics<'a> {
    pub fn name(&self) -> Result<&'a str, Error> {
        match self {
            MetadataHasSemantics::Event(row) => row.name(),
            MetadataHasSemantics::Property(row) => row.name(),
        }
    }
}

impl<'a> MetadataMethodDefOrRef<'a> {
    pub fn name(&self) -> Result<&'a str, Error> {
        match self {
            MetadataMethodDefOrRef::MethodDef(row) => row.name(),
            MetadataMethodDefOrRef::MemberRef(row) => row.name(),
        }
    }
}

impl<'a> MetadataMemberForwarded<'a> {
    pub fn name(&self) -> Result<&'a str, Error> {
        match self {
            MetadataMemberForwarded::Field(row) => row.name(),
            MetadataMemberForwarded::MethodDef(row) => row.name(),
        }
    }
}

impl<'a> MetadataCustomAttributeType<'a> {
    pub fn name(&self) -> Result<&'a str, Error> {
        match self {
            MetadataCustomAttributeType::MethodDef(row) => row.name(),
            MetadataCustomAttributeType::MemberRef(row) => row.name(),
        }
    }
}

impl<'a> MetadataResolutionScope<'a> {
    pub fn name(&self) -> Result<&'a str, Error> {
        match self {
            MetadataResolutionScope::Module(row) => row.name(),
            MetadataResolutionScope::ModuleRef(row) => row.name(),
            MetadataResolutionScope::AssemblyRef(row) => row.name(),
            MetadataResolutionScope::TypeRef(row) => row.name(),
        }
    }
}

impl<'a> MetadataTypeOrMethodDef<'a> {
    pub fn name(&self) -> Result<&'a str, Error> {
        match self {
            MetadataTypeOrMethodDef::TypeDef(row) => row.name(),
            MetadataTypeOrMethodDef::MethodDef(row) => row.name(),
        }
    }
}

pub struct Blob;
pub struct GUID;

macro_rules! impl_display_heap_indices{
    ( $( $name:ident ),* $(,)? ) => {
        $(
            impl std::fmt::Display for MetadataHeapIndex<$name> {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    write!(f, "{}[{}]", stringify!($name), self.index)
                }
            }
        )*
    };
}

macro_rules! impl_unpack_heap_indices {
    ( $( $name:ident ),* $(,)? ) => {
        $(
            impl<'a> UnpackBytes<'a> for MetadataHeapIndex<$name> {
                type Error = Error;
                fn unpack(bytes: ByteRange<'a>) -> Result<Self, Self::Error> {
                    let index = bytes.unpack()?;
                    Ok(MetadataHeapIndex {
                        index,
                        _phantom: PhantomData,
                    })
                }
            }
        )*
    };
}

impl_display_heap_indices! {String, Blob, GUID}
impl_unpack_heap_indices! {String, Blob}

impl<'a> UnpackOptBytes<'a> for MetadataHeapIndex<GUID> {
    type Error = Error;
    fn unpack_opt(bytes: ByteRange<'a>) -> Result<Option<Self>, Self::Error> {
        let index: usize = bytes.unpack()?;

        // Unlike indices into the #Strings or #Blob heaps, which
        // are zero-indexed, indices into the #GUID heap are
        // one-indexed.  An index of zero is used to represent a
        // null GUID.
        //
        // Converting the indices when unpacking avoids having a
        // Rust `usize` that cannot be used as an index.

        let typed_index = if index == 0 {
            None
        } else {
            Some(MetadataHeapIndex {
                index: index - 1,
                _phantom: PhantomData,
            })
        };

        Ok(typed_index)
    }
}

impl<'a> UnpackBytes<'a> for MetadataHeapIndex<GUID> {
    type Error = Error;
    fn unpack(bytes: ByteRange<'a>) -> Result<Self, Self::Error> {
        bytes
            .unpack()
            .and_then(|index: Option<MetadataHeapIndex<GUID>>| {
                index.ok_or(Error::InvalidGuidIndexZero)
            })
    }
}

impl<'a, TableTag: MetadataTableTag> UnpackOptBytes<'a>
    for MetadataTableIndex<TableTag>
{
    type Error = Error;
    fn unpack_opt(bytes: ByteRange<'a>) -> Result<Option<Self>, Self::Error> {
        let index: usize = bytes.unpack()?;
        let typed_index = if index == 0 {
            None
        } else {
            Some(MetadataTableIndex::new(index - 1))
        };
        Ok(typed_index)
    }
}

impl<'a, TableTag: MetadataTableTag> UnpackBytes<'a>
    for MetadataTableIndex<TableTag>
{
    type Error = Error;
    fn unpack(bytes: ByteRange<'a>) -> Result<Self, Self::Error> {
        bytes.unpack().and_then(|opt_index: Option<Self>| {
            opt_index.ok_or(Error::InvalidMetadataTableIndexZero {
                kind: TableTag::KIND,
            })
        })
    }
}

impl<TableTag: MetadataTableTag> std::fmt::Display
    for MetadataTableIndex<TableTag>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}[{}]", TableTag::KIND, self.index)
    }
}

impl<TableTag> MetadataTableIndex<TableTag> {
    pub fn new(index: usize) -> Self {
        Self {
            index,
            _phantom: PhantomData,
        }
    }
}

impl<TableTag> Into<usize> for MetadataTableIndex<TableTag> {
    fn into(self) -> usize {
        self.index
    }
}

impl<CodedIndexType> MetadataCodedIndex<CodedIndexType> {
    pub(crate) fn new(kind: MetadataTableKind, index: usize) -> Self
    where
        CodedIndexType: CodedIndex,
    {
        debug_assert!(
            CodedIndexType::OPTIONS.contains(&Some(kind)),
            "Valid options for this CodedIndexType are {:?}, \
                       and do not include {kind}",
            CodedIndexType::OPTIONS.iter().flatten().collect::<Vec<_>>()
        );
        Self {
            index,
            kind,
            _phantom: PhantomData,
        }
    }
}

impl<'a, CodedIndexType> UnpackOptBytes<'a>
    for MetadataCodedIndex<CodedIndexType>
where
    CodedIndexType: CodedIndex,
{
    type Error = Error;
    fn unpack_opt(bytes: ByteRange<'a>) -> Result<Option<Self>, Self::Error> {
        let raw_index = bytes.unpack()?;
        let table_kind = CodedIndexType::table_kind(raw_index)?;
        let Some(row_index) = CodedIndexType::row_index(raw_index) else {
            return Ok(None);
        };

        Ok(Some(MetadataCodedIndex::new(table_kind, row_index)))
    }
}

impl<'a, CodedIndexType> UnpackBytes<'a> for MetadataCodedIndex<CodedIndexType>
where
    CodedIndexType: CodedIndex,
{
    type Error = Error;
    fn unpack(bytes: ByteRange<'a>) -> Result<Self, Self::Error> {
        let raw_index = bytes.unpack()?;
        let table_kind = CodedIndexType::table_kind(raw_index)?;
        let row_index = CodedIndexType::row_index(raw_index)
            .ok_or(Error::InvalidMetadataTableIndexZero { kind: table_kind })?;
        Ok(MetadataCodedIndex::new(table_kind, row_index))
    }
}

impl<TableType> std::cmp::PartialEq for MetadataTableIndex<TableType> {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}
impl<TableType> std::cmp::Eq for MetadataTableIndex<TableType> {}
impl<TableType> std::hash::Hash for MetadataTableIndex<TableType> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.index.hash(state);
    }
}

impl<'a> DLLUnpacker<'a> {
    pub fn new(bytes: impl Into<ByteRange<'a>>) -> Self {
        Self {
            bytes: bytes.into(),
        }
    }

    pub fn clr_runtime_header(&self) -> Result<ClrRuntimeHeader, Error> {
        let optional_header = self.optional_header()?;
        let data_dir = optional_header
            .data_directory(DataDirectoryKind::ClrRuntimeHeader)?
            .value()
            .ok_or(Error::MissingClrRuntimeHeader)?;

        let addr = self.virtual_address_to_raw(data_dir.rva)?;

        let size = data_dir.size as usize;
        let bytes = self.bytes.subrange(addr..addr + size);
        Ok(ClrRuntimeHeader { bytes })
    }

    pub fn raw_metadata(self) -> Result<RawCLRMetadata<'a>, Error> {
        let clr_runtime_header = self.clr_runtime_header()?;
        let metadata_range = clr_runtime_header.metadata_range()?.value();
        let raw_start = self.virtual_address_to_raw(metadata_range.rva)?;
        let num_bytes = metadata_range.size as usize;
        let bytes = self.bytes.subrange(raw_start..raw_start + num_bytes);
        Ok(RawCLRMetadata { bytes })
    }

    pub fn metadata_layout(&self) -> Result<MetadataLayout, Error> {
        let rva_relocations = self.virtual_address_relocations()?;

        let raw_metadata = self.raw_metadata()?;
        let heap_locations = raw_metadata.metadata_heap_locations()?;

        let header = raw_metadata.metadata_tables_header()?;
        let layout = header.metadata_layout(heap_locations, rva_relocations)?;

        Ok(layout)
    }
}

impl<'a> ClrRuntimeHeader<'a> {
    pub fn header_size(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(0).map_err(Into::into)
    }

    pub fn major_runtime_version(&self) -> Result<UnpackedValue<u16>, Error> {
        self.bytes.get_u16(4).map_err(Into::into)
    }

    pub fn minor_runtime_version(&self) -> Result<UnpackedValue<u16>, Error> {
        self.bytes.get_u16(6).map_err(Into::into)
    }

    pub fn metadata_range(&self) -> Result<UnpackedValue<VirtualRange>, Error> {
        self.bytes.subrange(8..16).unpack()
    }

    pub fn flags(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(16).map_err(Into::into)
    }

    pub fn entry_point_token(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(20).map_err(Into::into)
    }

    pub fn resources(&self) -> Result<UnpackedValue<VirtualRange>, Error> {
        self.bytes.subrange(24..32).unpack()
    }

    pub fn strong_name_signature(
        &self,
    ) -> Result<UnpackedValue<Option<VirtualRange>>, Error> {
        self.bytes.subrange(32..40).unpack()
    }

    pub fn code_manager_table(
        &self,
    ) -> Result<UnpackedValue<Option<VirtualRange>>, Error> {
        self.bytes.subrange(40..48).unpack()
    }

    pub fn vtable_fixups(
        &self,
    ) -> Result<UnpackedValue<Option<VirtualRange>>, Error> {
        self.bytes.subrange(48..56).unpack()
    }

    pub fn export_address_table_jumps(
        &self,
    ) -> Result<UnpackedValue<Option<VirtualRange>>, Error> {
        self.bytes.subrange(56..64).unpack()
    }

    pub fn managed_native_header(
        &self,
    ) -> Result<UnpackedValue<Option<VirtualRange>>, Error> {
        self.bytes.subrange(64..72).unpack()
    }
}

impl<'a> RawCLRMetadata<'a> {
    pub fn metadata_signature(&self) -> Result<UnpackedValue<()>, Error> {
        let metadata_signature = [0x42, 0x53, 0x4A, 0x42];

        let byte_range = 0..4;
        if metadata_signature == self.bytes[byte_range.clone()] {
            Ok(UnpackedValue::new(self.bytes.address_range(byte_range), ()))
        } else {
            Err(Error::IncorrectMetadataSignature)
        }
    }

    pub fn major_version(&self) -> Result<UnpackedValue<u16>, Error> {
        self.bytes.get_u16(4).map_err(Into::into)
    }

    pub fn minor_version(&self) -> Result<UnpackedValue<u16>, Error> {
        self.bytes.get_u16(6).map_err(Into::into)
    }

    pub fn reserved(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(8).map_err(Into::into)
    }

    pub fn version_str_len(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(12).map_err(Into::into)
    }

    fn padded_version_str_len(&self) -> Result<usize, Error> {
        let len = self.version_str_len()?.value() as usize;
        Ok(len.div_ceil(4) * 4)
    }

    pub fn version_str(&self) -> Result<UnpackedValue<&'a str>, Error> {
        let len = self.version_str_len()?.value() as usize;
        let start = 16;

        let padded_len = self.padded_version_str_len()?;
        let value = std::str::from_utf8(
            self.bytes.subrange(start..start + len).into(),
        )?
        .trim_end_matches('\0');

        Ok(UnpackedValue::new(
            self.bytes.address_range(start..start + padded_len),
            value,
        ))
    }

    pub fn flags(&self) -> Result<UnpackedValue<u16>, Error> {
        let start = 16 + self.padded_version_str_len()?;
        self.bytes.get_u16(start).map_err(Into::into)
    }

    pub fn num_streams(&self) -> Result<UnpackedValue<u16>, Error> {
        let start = 18 + self.padded_version_str_len()?;
        self.bytes.get_u16(start).map_err(Into::into)
    }

    pub fn iter_stream_header(
        &self,
    ) -> Result<impl Iterator<Item = Result<StreamHeader<'a>, Error>> + '_, Error>
    {
        let num_streams = self.num_streams()?.value();

        let mut curr_offset = 20 + self.padded_version_str_len()?;

        let iter = (0..num_streams).map(move |_| {
            let stream_offset = self.bytes.get_u32(curr_offset)?;
            let stream_size = self.bytes.get_u32(curr_offset + 4)?;

            let name_start = curr_offset + 8;
            let name_len = self.bytes[name_start..name_start + 32]
                .iter()
                .take_while(|byte| **byte > 0)
                .count();
            assert!(name_len > 0);

            let padded_name_len = (name_len + 1).div_ceil(4) * 4;

            let stream_name = {
                let loc = self
                    .bytes
                    .address_range(name_start..name_start + padded_name_len);
                let value = std::str::from_utf8(
                    self.bytes
                        .subrange(name_start..name_start + name_len)
                        .into(),
                )?;
                UnpackedValue::new(loc, value)
            };

            let ptr_range = {
                let start = stream_offset.value() as usize;
                let size = stream_size.value() as usize;
                self.bytes.address_range(start..start + size)
            };

            let stream = self.bytes.subrange(ptr_range.clone());

            curr_offset += 8 + padded_name_len;

            Ok(StreamHeader {
                offset: stream_offset,
                size: stream_size,
                name: stream_name,
                bytes: stream,
                ptr_range,
            })
        });

        Ok(iter)
    }

    fn find_stream(&self, name: &'static str) -> Result<ByteRange<'a>, Error> {
        for res_header in self.iter_stream_header()? {
            let stream = res_header?;
            if stream.name.value() == name {
                return Ok(stream.bytes);
            }
        }
        Err(Error::MissingStream(name))
    }

    pub fn metadata_tables_header(
        &self,
    ) -> Result<MetadataTableHeader<'a>, Error> {
        let bytes = self.find_stream("#~")?;
        Ok(MetadataTableHeader { bytes })
    }

    fn get_stream_location(
        &self,
        name: &'static str,
    ) -> Result<Range<Pointer>, Error> {
        for res_header in self.iter_stream_header()? {
            let stream = res_header?;
            if stream.name.value() == name {
                return Ok(stream.ptr_range);
            }
        }
        Err(Error::MissingStream(name))
    }

    fn metadata_heap_locations(
        &self,
    ) -> Result<EnumMap<MetadataHeapKind, Range<Pointer>>, Error> {
        let mut heaps = EnumMap::init(|| Pointer::null()..Pointer::null());
        heaps[MetadataHeapKind::String] =
            self.get_stream_location("#Strings")?;
        heaps[MetadataHeapKind::Blob] = self.get_stream_location("#Blob")?;
        heaps[MetadataHeapKind::GUID] = self.get_stream_location("#GUID")?;

        Ok(heaps)
    }
}

impl<'a> MetadataTableHeader<'a> {
    pub fn reserved_0(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(0).map_err(Into::into)
    }

    pub fn major_version(&self) -> Result<UnpackedValue<u8>, Error> {
        self.bytes.get_u8(4).map_err(Into::into)
    }

    pub fn minor_version(&self) -> Result<UnpackedValue<u8>, Error> {
        self.bytes.get_u8(5).map_err(Into::into)
    }

    pub fn heap_sizes(&self) -> Result<UnpackedValue<HeapSizes>, Error> {
        let (loc, value) = self.bytes.get_u8(6)?.into();
        let heap_sizes = HeapSizes {
            string_stream_uses_u32_addr: value & 0x1 > 0,
            guid_stream_uses_u32_addr: value & 0x2 > 0,
            blob_stream_uses_u32_addr: value & 0x4 > 0,
        };

        Ok(UnpackedValue::new(loc, heap_sizes))
    }

    pub fn reserved_1(&self) -> Result<UnpackedValue<u8>, Error> {
        self.bytes.get_u8(7).map_err(Into::into)
    }

    /// Bitfield indicating which tables are present.
    pub fn valid_table_bitfield(&self) -> Result<UnpackedValue<u64>, Error> {
        self.bytes.get_u64(8).map_err(Into::into)
    }

    /// Bitfield indicating which tables are sorted.  Should have
    /// exactly 14 entries, matching the tables listed in section
    /// II.22 of ECMA-335.
    pub fn sorted_table_bitfield(&self) -> Result<UnpackedValue<u64>, Error> {
        self.bytes.get_u64(16).map_err(Into::into)
    }

    pub fn iter_num_rows(
        &self,
    ) -> Result<
        impl Iterator<
                Item = Result<(UnpackedValue<u32>, MetadataTableKind), Error>,
            > + '_,
        Error,
    > {
        let bitfield = self.valid_table_bitfield()?.value();

        let iter = (0..64)
            .filter(move |i_bit| bitfield & (1 << i_bit) > 0)
            .scan(24, |offset, i_bit| -> Option<Result<_, Error>> {
                let row_offset = *offset;
                *offset += 4;

                let num_rows = match self.bytes.get_u32(row_offset) {
                    Ok(val) => val,
                    Err(err) => return Some(Err(err.into())),
                };
                let kind = match MetadataTableKind::from_bit_index(i_bit) {
                    Ok(val) => val,
                    Err(err) => return Some(Err(err.into())),
                };
                Some(Ok((num_rows, kind)))
            });

        Ok(iter)
    }

    pub fn metadata_layout(
        &self,
        heap_locations: EnumMap<MetadataHeapKind, Range<Pointer>>,
        rva_relocations: Vec<VirtualAddressRelocation>,
    ) -> Result<MetadataLayout, Error> {
        let heap_sizes = {
            let mut heap_sizes = EnumMap::<MetadataHeapKind, bool>::default();
            let raw_heap_sizes = self.heap_sizes()?.value();
            heap_sizes[MetadataHeapKind::String] =
                raw_heap_sizes.string_stream_uses_u32_addr;
            heap_sizes[MetadataHeapKind::GUID] =
                raw_heap_sizes.guid_stream_uses_u32_addr;
            heap_sizes[MetadataHeapKind::Blob] =
                raw_heap_sizes.blob_stream_uses_u32_addr;
            heap_sizes
        };

        let num_rows = {
            let mut num_rows = EnumMap::<MetadataTableKind, usize>::default();
            for res in self.iter_num_rows()? {
                let (num_table_rows, kind) = res?;
                num_rows[kind] = num_table_rows.value() as usize;
            }
            num_rows
        };

        let index_size = {
            let mut index_size = EnumMap::<MetadataIndexKind, usize>::default();
            for heap_kind in MetadataHeapKind::iter_keys() {
                index_size[heap_kind] =
                    if heap_sizes[heap_kind] { 4 } else { 2 };
            }

            for table_kind in MetadataTableKind::iter_keys() {
                index_size[table_kind] =
                    if num_rows[table_kind] >= 65536 { 4 } else { 2 };
            }

            for coded_index_kind in MetadataCodedIndexKind::iter_keys() {
                let max_rows = coded_index_kind
                    .table_options()
                    .iter()
                    .cloned()
                    .filter_map(|opt_index| opt_index)
                    .map(|index| num_rows[index])
                    .max()
                    .expect("Coded index must contain at least one index.");
                let row_bits =
                    (max_rows + 1).next_power_of_two().ilog2() as usize;
                let table_bits = coded_index_kind.table_bits();

                index_size[coded_index_kind] =
                    if row_bits + table_bits <= 16 { 2 } else { 4 };
            }

            index_size
        };

        let bytes_per_row = {
            let mut row_size = EnumMap::<MetadataTableKind, usize>::default();
            for table_kind in MetadataTableKind::iter_keys() {
                row_size[table_kind] = table_kind
                    .column_types()
                    .iter()
                    .cloned()
                    .map(|column_type| match column_type {
                        MetadataColumnType::Index(index) => index_size[index],
                        MetadataColumnType::FixedSize(nbytes) => nbytes,
                    })
                    .sum();
            }

            row_size
        };

        let table_offsets = {
            let mut table_offsets =
                EnumMap::<MetadataTableKind, usize>::default();

            let mut curr_offset = 0;
            for res in self.iter_num_rows()? {
                let (num_rows, kind) = res?;
                let num_rows = num_rows.value() as usize;
                table_offsets[kind] = curr_offset;
                curr_offset += num_rows * bytes_per_row[kind];
            }

            table_offsets
        };

        let tables_location = self.tables_location()?;

        Ok(MetadataLayout {
            num_rows,
            index_size,
            bytes_per_row,
            table_offsets,
            heap_locations,
            tables_location,
            rva_relocations,
        })
    }

    fn tables_location(&self) -> Result<Range<Pointer>, Error> {
        let num_tables =
            self.valid_table_bitfield()?.value().count_ones() as usize;
        let header_size = 24 + 4 * num_tables;
        let location = self.bytes.address_range(header_size..);

        Ok(location)
    }
}

impl<'a> Metadata<'a> {
    pub fn ptr_range(&self) -> Range<Pointer> {
        self.layout.tables_location.clone()
    }

    pub fn iter_table_locations(
        &self,
    ) -> impl Iterator<Item = (MetadataTableKind, Range<Pointer>)> + '_ {
        MetadataTableKind::iter_keys().map(|kind| {
            let bytes = self.get_table_bytes(kind);
            let location: Range<Pointer> = bytes.into();
            (kind, location)
        })
    }

    pub fn iter_heap_locations(
        &self,
    ) -> impl Iterator<Item = (MetadataHeapKind, Range<Pointer>)> + '_ {
        MetadataHeapKind::iter_keys().map(|kind| {
            let location = self.layout.heap_locations[kind].clone();
            (kind, location)
        })
    }

    fn get_table<TableTag>(self) -> MetadataTable<'a, TableTag>
    where
        TableTag: MetadataTableTag,
    {
        let bytes = self.get_table_bytes(TableTag::KIND);

        MetadataTable {
            bytes,
            metadata: self,
            _phantom: PhantomData,
        }
    }

    fn get_table_bytes(&self, table_kind: MetadataTableKind) -> ByteRange<'a> {
        let offset = self.layout.table_offsets[table_kind];
        let bytes_per_row = self.layout.bytes_per_row[table_kind];
        let num_rows = self.layout.num_rows[table_kind];
        let size = bytes_per_row * num_rows;

        let bytes_all_tables =
            self.dll_bytes.subrange(self.layout.tables_location.clone());
        let bytes_this_table = bytes_all_tables.subrange(offset..offset + size);

        bytes_this_table
    }

    pub fn get<'b, Index>(
        &'b self,
        index: Index,
    ) -> Result<Index::Output<'a, 'b>, Error>
    where
        Index: TypedMetadataIndex,
    {
        index.access(self)
    }

    pub fn location_of<'b, Index>(
        &'b self,
        index: Index,
    ) -> Result<Range<Pointer>, Error>
    where
        Index: TypedMetadataIndex,
    {
        index.location(self)
    }

    fn iter_range<TableTag>(
        &self,
        indices: impl Borrow<MetadataTableIndexRange<TableTag>>,
    ) -> Result<impl DoubleEndedIterator<Item = MetadataRow<TableTag>>, Error>
    where
        TableTag: MetadataTableTag,
    {
        let indices = indices.borrow();
        let table = self.get_table();
        table.validate_index_range(*indices)?;

        let iter = (indices.start..indices.end).map(move |index| {
            table.get_row(index).expect(
                "Out-of-bounds access \
                 should already be caught by \
                 `table.validate_index_range(indices)`",
            )
        });
        Ok(iter)
    }

    pub(crate) fn iter_untyped_rows(
        &self,
    ) -> impl Iterator<Item = (MetadataTableKind, usize, Range<Pointer>)> + '_
    {
        self.iter_table_locations()
            .flat_map(|(table_kind, table_range)| {
                let ptr_to_table = table_range.start;
                let bytes_per_row = self.layout.bytes_per_row[table_kind];
                let num_rows = self.layout.num_rows[table_kind];
                (0..num_rows).map(move |i_row| {
                    let ptr_to_row = ptr_to_table + i_row * bytes_per_row;
                    (table_kind, i_row, ptr_to_row..ptr_to_row + bytes_per_row)
                })
            })
    }
}

macro_rules! define_table_method {
    ($method_name:ident,$tag:ident) => {
        impl<'a> Metadata<'a> {
            pub fn $method_name(&self) -> MetadataTable<'a, $tag> {
                self.get_table()
            }
        }
    };
}
define_table_method! { module_table, Module }
define_table_method! { type_ref_table, TypeRef }
define_table_method! { type_def_table, TypeDef }
define_table_method! { field_table, Field }
define_table_method! { method_def_table, MethodDef }
define_table_method! { param_table, Param }
define_table_method! { interface_impl_table, InterfaceImpl }
define_table_method! { member_ref_table, MemberRef }
define_table_method! { constant_table, Constant }
define_table_method! { custom_attribute_table, CustomAttribute }
define_table_method! { field_marshal_table, FieldMarshal }
define_table_method! { decl_security_table, DeclSecurity }
define_table_method! { class_layout_table, ClassLayout }
define_table_method! { field_layout_table, FieldLayout }
define_table_method! { stand_alone_sig_table, StandAloneSig }
define_table_method! { event_map_table, EventMap }
define_table_method! { event_table, Event }
define_table_method! { property_map_table, PropertyMap }
define_table_method! { property_table, Property }
define_table_method! { method_semantics_table, MethodSemantics }
define_table_method! { method_impl_table, MethodImpl }
define_table_method! { module_ref_table, ModuleRef }
define_table_method! { type_spec_table, TypeSpec }
define_table_method! { impl_map_table, ImplMap }
define_table_method! { field_rva_table, FieldRVA }
define_table_method! { assembly_table, Assembly }
define_table_method! { assembly_processor_table, AssemblyProcessor }
define_table_method! { assembly_os_table, AssemblyOS }
define_table_method! { assembly_ref_table, AssemblyRef }
define_table_method! { assembly_ref_processor_table, AssemblyRefProcessor }
define_table_method! { assembly_ref_os_table, AssemblyRefOS }
define_table_method! { file_table, File }
define_table_method! { exported_type_table, ExportedType }
define_table_method! { manifest_resource_table, ManifestResource }
define_table_method! { nested_class_table, NestedClass }
define_table_method! { generic_param_table, GenericParam }
define_table_method! { method_spec_table, MethodSpec }
define_table_method! { generic_param_constraint_table, GenericParamConstraint }

impl<T> TypedMetadataIndex for UnpackedValue<T>
where
    T: TypedMetadataIndex,
{
    type Output<'a: 'b, 'b> = T::Output<'a, 'b>;

    fn access<'a: 'b, 'b>(
        self,
        metadata: &'b Metadata<'a>,
    ) -> Result<Self::Output<'a, 'b>, Error> {
        self.value().access(metadata)
    }

    fn location(self, metadata: &Metadata) -> Result<Range<Pointer>, Error> {
        self.value().location(metadata)
    }
}

impl TypedMetadataIndex for MetadataHeapIndex<String> {
    type Output<'a: 'b, 'b> = &'a str;

    fn access<'a: 'b, 'b>(
        self,
        metadata: &'b Metadata<'a>,
    ) -> Result<&'a str, Error> {
        let heap_location =
            metadata.layout.heap_locations[MetadataHeapKind::String].clone();
        let bytes = metadata.dll_bytes.subrange(heap_location);
        Ok(bytes.get_null_terminated(self.index)?.value())
    }

    fn location(self, metadata: &Metadata) -> Result<Range<Pointer>, Error> {
        let heap_location =
            metadata.layout.heap_locations[MetadataHeapKind::String].clone();
        let bytes = metadata.dll_bytes.subrange(heap_location);
        Ok(bytes.get_null_terminated(self.index)?.loc())
    }
}

impl TypedMetadataIndex for MetadataHeapIndex<Blob> {
    type Output<'a: 'b, 'b> = UnpackedBlob<'a>;

    fn access<'a: 'b, 'b>(
        self,
        metadata: &'b Metadata<'a>,
    ) -> Result<UnpackedBlob<'a>, Error> {
        let heap_location =
            metadata.layout.heap_locations[MetadataHeapKind::Blob].clone();
        let heap_bytes = metadata.dll_bytes.subrange(heap_location);
        UnpackedBlob::new(heap_bytes.subrange(self.index..))
    }

    fn location(self, metadata: &Metadata) -> Result<Range<Pointer>, Error> {
        Ok(self.access(metadata)?.ptr_range())
    }
}

impl TypedMetadataIndex for MetadataHeapIndex<GUID> {
    type Output<'a: 'b, 'b> = UnpackedValue<u128>;

    fn access<'a: 'b, 'b>(
        self,
        metadata: &'b Metadata<'a>,
    ) -> Result<Self::Output<'a, 'b>, Error> {
        let location = self.location(metadata)?;
        let value = metadata.dll_bytes.subrange(location).unpack()?;
        Ok(value)
    }

    fn location(self, metadata: &Metadata) -> Result<Range<Pointer>, Error> {
        let index = self.index;
        let guid_size = 16;

        let heap_location =
            metadata.layout.heap_locations[MetadataHeapKind::GUID].clone();
        let heap_bytes = metadata.dll_bytes.subrange(heap_location);
        let location = heap_bytes
            .address_range(index * guid_size..(index + 1) * guid_size);
        Ok(location)
    }
}

impl<TableTag: MetadataTableTag> TypedMetadataIndex
    for MetadataTableIndex<TableTag>
{
    type Output<'a: 'b, 'b> = MetadataRow<'a, TableTag>;

    fn access<'a: 'b, 'b>(
        self,
        metadata: &'b Metadata<'a>,
    ) -> Result<Self::Output<'a, 'b>, Error> {
        metadata.get_table().get_row(self.index)
    }

    fn location(self, metadata: &Metadata) -> Result<Range<Pointer>, Error> {
        Ok(self.access(metadata)?.ptr_range())
    }
}

impl<T> TypedMetadataIndex for Option<T>
where
    T: TypedMetadataIndex,
{
    type Output<'a: 'b, 'b> = Option<<T as TypedMetadataIndex>::Output<'a, 'b>>;

    fn access<'a: 'b, 'b>(
        self,
        metadata: &'b Metadata<'a>,
    ) -> Result<Self::Output<'a, 'b>, Error> {
        self.map(|index| index.access(metadata)).transpose()
    }

    fn location(self, metadata: &Metadata) -> Result<Range<Pointer>, Error> {
        Ok(self
            .map(|index| index.location(metadata))
            .transpose()?
            .unwrap_or_else(|| Pointer::null()..Pointer::null()))
    }
}

impl<TableTag: MetadataTableTag> std::fmt::Display
    for MetadataTableIndexRange<TableTag>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}[{}..{}]", TableTag::KIND, self.start, self.end)
    }
}

mod _impl_typed_index_range_iter {
    use super::*;
    pub struct MetadataTableIndexIterRange<TableTag> {
        index: usize,
        end: usize,
        _phantom: PhantomData<TableTag>,
    }
    impl<TableTag> IntoIterator for MetadataTableIndexRange<TableTag> {
        type Item = MetadataTableIndex<TableTag>;

        type IntoIter = MetadataTableIndexIterRange<TableTag>;

        fn into_iter(self) -> Self::IntoIter {
            MetadataTableIndexIterRange {
                index: self.start,
                end: self.end,
                _phantom: PhantomData,
            }
        }
    }

    impl<TableTag> Iterator for MetadataTableIndexIterRange<TableTag> {
        type Item = MetadataTableIndex<TableTag>;

        fn next(&mut self) -> Option<Self::Item> {
            if self.index < self.end {
                let index = MetadataTableIndex {
                    index: self.index,
                    _phantom: PhantomData,
                };
                self.index += 1;
                Some(index)
            } else {
                None
            }
        }
    }
}

impl std::fmt::Display for HeapSizes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "HeapSizes{{")?;

        let mut need_comma = false;
        if self.string_stream_uses_u32_addr {
            write!(f, "#Strings")?;
            need_comma = true;
        }

        if self.guid_stream_uses_u32_addr {
            if need_comma {
                write!(f, ", ")?;
            }
            write!(f, "#GUID")?;
            need_comma = true;
        }

        if self.blob_stream_uses_u32_addr {
            if need_comma {
                write!(f, ", ")?;
            }
            write!(f, "#Blob")?;
        }

        write!(f, "}}")
    }
}

impl std::fmt::Display for MetadataTableKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

impl std::fmt::Display for MetadataHeapKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

impl MetadataTableKind {
    fn from_bit_index(bit: u8) -> Result<Self, Error> {
        match bit {
            0x00 => Ok(Self::Module),
            0x01 => Ok(Self::TypeRef),
            0x02 => Ok(Self::TypeDef),
            0x04 => Ok(Self::Field),
            0x06 => Ok(Self::MethodDef),
            0x08 => Ok(Self::Param),
            0x09 => Ok(Self::InterfaceImpl),
            0x0A => Ok(Self::MemberRef),
            0x0B => Ok(Self::Constant),
            0x0C => Ok(Self::CustomAttribute),
            0x0D => Ok(Self::FieldMarshal),
            0x0E => Ok(Self::DeclSecurity),
            0x0F => Ok(Self::ClassLayout),
            0x10 => Ok(Self::FieldLayout),
            0x11 => Ok(Self::StandAloneSig),
            0x12 => Ok(Self::EventMap),
            0x14 => Ok(Self::Event),
            0x15 => Ok(Self::PropertyMap),
            0x17 => Ok(Self::Property),
            0x18 => Ok(Self::MethodSemantics),
            0x19 => Ok(Self::MethodImpl),
            0x1A => Ok(Self::ModuleRef),
            0x1B => Ok(Self::TypeSpec),
            0x1C => Ok(Self::ImplMap),
            0x1D => Ok(Self::FieldRVA),
            0x20 => Ok(Self::Assembly),
            0x21 => Ok(Self::AssemblyProcessor),
            0x22 => Ok(Self::AssemblyOS),
            0x23 => Ok(Self::AssemblyRef),
            0x24 => Ok(Self::AssemblyRefProcessor),
            0x25 => Ok(Self::AssemblyRefOS),
            0x26 => Ok(Self::File),
            0x27 => Ok(Self::ExportedType),
            0x28 => Ok(Self::ManifestResource),
            0x29 => Ok(Self::NestedClass),
            0x2A => Ok(Self::GenericParam),
            0x2B => Ok(Self::MethodSpec),
            0x2C => Ok(Self::GenericParamConstraint),
            _ => Err(Error::InvalidMetadataTable(bit)),
        }
    }
}

impl MetadataLayout {
    pub fn metadata<'a>(
        &'a self,
        dll_bytes: impl Into<ByteRange<'a>>,
    ) -> Metadata<'a> {
        Metadata {
            layout: self,
            dll_bytes: dll_bytes.into(),
        }
    }

    fn column_size(&self, column_type: MetadataColumnType) -> usize {
        match column_type {
            MetadataColumnType::Index(index) => self.index_size[index],
            MetadataColumnType::FixedSize(nbytes) => nbytes,
        }
    }
}

impl EnumKey for MetadataHeapKind {
    const N: usize = 3;

    fn as_index(self) -> usize {
        match self {
            Self::String => 0,
            Self::GUID => 1,
            Self::Blob => 2,
        }
    }

    fn iter_keys() -> impl Iterator<Item = Self> {
        [Self::String, Self::GUID, Self::Blob].into_iter()
    }
}

impl EnumKey for MetadataTableKind {
    const N: usize = 38;

    fn as_index(self) -> usize {
        match self {
            Self::Module => 0,
            Self::TypeRef => 1,
            Self::TypeDef => 2,
            Self::Field => 3,
            Self::MethodDef => 4,
            Self::Param => 5,
            Self::InterfaceImpl => 6,
            Self::MemberRef => 7,
            Self::Constant => 8,
            Self::CustomAttribute => 9,
            Self::FieldMarshal => 10,
            Self::DeclSecurity => 11,
            Self::ClassLayout => 12,
            Self::FieldLayout => 13,
            Self::StandAloneSig => 14,
            Self::EventMap => 15,
            Self::Event => 16,
            Self::PropertyMap => 17,
            Self::Property => 18,
            Self::MethodSemantics => 19,
            Self::MethodImpl => 20,
            Self::ModuleRef => 21,
            Self::TypeSpec => 22,
            Self::ImplMap => 23,
            Self::FieldRVA => 24,
            Self::Assembly => 25,
            Self::AssemblyProcessor => 26,
            Self::AssemblyOS => 27,
            Self::AssemblyRef => 28,
            Self::AssemblyRefProcessor => 29,
            Self::AssemblyRefOS => 30,
            Self::File => 31,
            Self::ExportedType => 32,
            Self::ManifestResource => 33,
            Self::NestedClass => 34,
            Self::GenericParam => 35,
            Self::MethodSpec => 36,
            Self::GenericParamConstraint => 37,
        }
    }

    fn iter_keys() -> impl Iterator<Item = Self> {
        [
            Self::Module,
            Self::TypeRef,
            Self::TypeDef,
            Self::Field,
            Self::MethodDef,
            Self::Param,
            Self::InterfaceImpl,
            Self::MemberRef,
            Self::Constant,
            Self::CustomAttribute,
            Self::FieldMarshal,
            Self::DeclSecurity,
            Self::ClassLayout,
            Self::FieldLayout,
            Self::StandAloneSig,
            Self::EventMap,
            Self::Event,
            Self::PropertyMap,
            Self::Property,
            Self::MethodSemantics,
            Self::MethodImpl,
            Self::ModuleRef,
            Self::TypeSpec,
            Self::ImplMap,
            Self::FieldRVA,
            Self::Assembly,
            Self::AssemblyProcessor,
            Self::AssemblyOS,
            Self::AssemblyRef,
            Self::AssemblyRefProcessor,
            Self::AssemblyRefOS,
            Self::File,
            Self::ExportedType,
            Self::ManifestResource,
            Self::NestedClass,
            Self::GenericParam,
            Self::MethodSpec,
            Self::GenericParamConstraint,
        ]
        .into_iter()
    }
}

impl EnumKey for MetadataCodedIndexKind {
    const N: usize = 13;

    fn as_index(self) -> usize {
        match self {
            Self::TypeDefOrRef => 0,
            Self::HasConstant => 1,
            Self::HasCustomAttribute => 2,
            Self::HasFieldMarshal => 3,
            Self::HasDeclSecurity => 4,
            Self::MemberRefParent => 5,
            Self::HasSemantics => 6,
            Self::MethodDefOrRef => 7,
            Self::MemberForwarded => 8,
            Self::Implementation => 9,
            Self::CustomAttributeType => 10,
            Self::ResolutionScope => 11,
            Self::TypeOrMethodDef => 12,
        }
    }

    fn iter_keys() -> impl Iterator<Item = Self> {
        [
            Self::TypeDefOrRef,
            Self::HasConstant,
            Self::HasCustomAttribute,
            Self::HasFieldMarshal,
            Self::HasDeclSecurity,
            Self::MemberRefParent,
            Self::HasSemantics,
            Self::MethodDefOrRef,
            Self::MemberForwarded,
            Self::Implementation,
            Self::CustomAttributeType,
            Self::ResolutionScope,
            Self::TypeOrMethodDef,
        ]
        .into_iter()
    }
}

impl EnumKey for MetadataIndexKind {
    const N: usize = 3 + 38 + 13;

    fn as_index(self) -> usize {
        match self {
            Self::Heap(heap_kind) => heap_kind.as_index(),
            Self::Table(table_kind) => {
                MetadataHeapKind::N + table_kind.as_index()
            }
            Self::CodedIndex(coded_index_kind) => {
                MetadataTableKind::N
                    + MetadataHeapKind::N
                    + coded_index_kind.as_index()
            }
        }
    }

    fn iter_keys() -> impl Iterator<Item = Self> {
        let heaps = MetadataHeapKind::iter_keys().map(Self::Heap);
        let tables = MetadataTableKind::iter_keys().map(Self::Table);
        let coded_indices =
            MetadataCodedIndexKind::iter_keys().map(Self::CodedIndex);

        std::iter::empty()
            .chain(heaps)
            .chain(tables)
            .chain(coded_indices)
    }
}

impl MetadataCodedIndexKind {
    fn table_options(self) -> &'static [Option<MetadataTableKind>] {
        match self {
            Self::TypeDefOrRef => TypeDefOrRef::OPTIONS,
            Self::HasConstant => HasConstant::OPTIONS,
            Self::HasCustomAttribute => HasCustomAttribute::OPTIONS,
            Self::HasFieldMarshal => HasFieldMarshal::OPTIONS,
            Self::HasDeclSecurity => HasDeclSecurity::OPTIONS,
            Self::MemberRefParent => MemberRefParent::OPTIONS,
            Self::HasSemantics => HasSemantics::OPTIONS,
            Self::MethodDefOrRef => MethodDefOrRef::OPTIONS,
            Self::MemberForwarded => MemberForwarded::OPTIONS,
            Self::Implementation => Implementation::OPTIONS,
            Self::CustomAttributeType => CustomAttributeType::OPTIONS,
            Self::ResolutionScope => ResolutionScope::OPTIONS,
            Self::TypeOrMethodDef => TypeOrMethodDef::OPTIONS,
        }
    }

    fn table_bits(self) -> usize {
        self.table_options().len().next_power_of_two().ilog2() as usize
    }
}

impl MetadataTableKind {
    fn column_types(self) -> &'static [MetadataColumnType] {
        match self {
            Self::Module => Module::COLUMNS,
            Self::TypeRef => TypeRef::COLUMNS,
            Self::TypeDef => TypeDef::COLUMNS,
            Self::Field => Field::COLUMNS,
            Self::MethodDef => MethodDef::COLUMNS,
            Self::Param => Param::COLUMNS,
            Self::InterfaceImpl => InterfaceImpl::COLUMNS,
            Self::MemberRef => MemberRef::COLUMNS,
            Self::Constant => Constant::COLUMNS,
            Self::CustomAttribute => CustomAttribute::COLUMNS,
            Self::FieldMarshal => FieldMarshal::COLUMNS,
            Self::DeclSecurity => DeclSecurity::COLUMNS,
            Self::ClassLayout => ClassLayout::COLUMNS,
            Self::FieldLayout => FieldLayout::COLUMNS,
            Self::StandAloneSig => StandAloneSig::COLUMNS,
            Self::EventMap => EventMap::COLUMNS,
            Self::Event => Event::COLUMNS,
            Self::PropertyMap => PropertyMap::COLUMNS,
            Self::Property => Property::COLUMNS,
            Self::MethodSemantics => MethodSemantics::COLUMNS,
            Self::MethodImpl => MethodImpl::COLUMNS,
            Self::ModuleRef => ModuleRef::COLUMNS,
            Self::TypeSpec => TypeSpec::COLUMNS,
            Self::ImplMap => ImplMap::COLUMNS,
            Self::FieldRVA => FieldRVA::COLUMNS,
            Self::Assembly => Assembly::COLUMNS,
            Self::AssemblyProcessor => AssemblyProcessor::COLUMNS,
            Self::AssemblyOS => AssemblyOS::COLUMNS,
            Self::AssemblyRef => AssemblyRef::COLUMNS,
            Self::AssemblyRefProcessor => AssemblyRefProcessor::COLUMNS,
            Self::AssemblyRefOS => AssemblyRefOS::COLUMNS,
            Self::File => File::COLUMNS,
            Self::ExportedType => ExportedType::COLUMNS,
            Self::ManifestResource => ManifestResource::COLUMNS,
            Self::NestedClass => NestedClass::COLUMNS,
            Self::GenericParam => GenericParam::COLUMNS,
            Self::MethodSpec => MethodSpec::COLUMNS,
            Self::GenericParamConstraint => GenericParamConstraint::COLUMNS,
        }
    }
}

impl<'a, TableTag> MetadataTable<'a, TableTag> {
    fn validate_index_range(
        &self,
        indices: MetadataTableIndexRange<TableTag>,
    ) -> Result<(), Error>
    where
        TableTag: MetadataTableTag,
    {
        let num_rows = self.metadata.layout.num_rows[TableTag::KIND];
        let MetadataTableIndexRange { start, end, .. } = indices;

        if end <= num_rows {
            Ok(())
        } else {
            Err(Error::InvalidMetadataTableIndexRange {
                kind: TableTag::KIND,
                indices: start..end,
                num_rows,
            })
        }
    }

    pub fn iter_rows(
        self,
    ) -> impl Iterator<Item = MetadataRow<'a, TableTag>>
           + DoubleEndedIterator
           + ExactSizeIterator
    where
        TableTag: MetadataTableTag,
    {
        let num_rows = self.metadata.layout.num_rows[TableTag::KIND];
        let bytes_per_row = self.metadata.layout.bytes_per_row[TableTag::KIND];
        let tables = self.metadata;
        let table_bytes = self.bytes;
        (0..num_rows).map(move |i_row| {
            let bytes = table_bytes
                .subrange(i_row * bytes_per_row..(i_row + 1) * bytes_per_row);
            let next_row_bytes = (i_row + 1 < num_rows).then(|| {
                table_bytes.subrange(
                    (i_row + 1) * bytes_per_row..(i_row + 2) * bytes_per_row,
                )
            });
            MetadataRow {
                bytes,
                next_row_bytes,
                metadata: tables,
                index: MetadataTableIndex::new(i_row),
            }
        })
    }

    pub fn num_rows(&self) -> usize
    where
        TableTag: MetadataTableTag,
    {
        self.metadata.layout.num_rows[TableTag::KIND]
    }

    pub fn address_range(
        &self,
        indices: impl Borrow<MetadataTableIndexRange<TableTag>>,
    ) -> Result<Range<Pointer>, Error>
    where
        TableTag: MetadataTableTag,
    {
        let indices = indices.borrow();
        self.address_range_from_untyped(indices.start..indices.end)
    }

    pub(crate) fn address_range_from_untyped(
        &self,
        indices: Range<usize>,
    ) -> Result<Range<Pointer>, Error>
    where
        TableTag: MetadataTableTag,
    {
        let kind = TableTag::KIND;
        let num_rows = self.metadata.layout.num_rows[kind];
        let bytes_per_row = self.metadata.layout.bytes_per_row[kind];

        if indices.end <= num_rows {
            Ok(self.bytes.address_range(
                indices.start * bytes_per_row..indices.end * bytes_per_row,
            ))
        } else {
            Err(Error::InvalidMetadataTableIndex {
                kind,
                index: indices.end,
                num_rows,
            })
        }
    }

    pub fn get(
        &self,
        index: MetadataTableIndex<TableTag>,
    ) -> Result<MetadataRow<'a, TableTag>, Error>
    where
        TableTag: MetadataTableTag,
    {
        self.get_row(index.index)
    }

    pub fn get_row(
        &self,
        index: usize,
    ) -> Result<MetadataRow<'a, TableTag>, Error>
    where
        TableTag: MetadataTableTag,
    {
        let kind = TableTag::KIND;
        let num_rows = self.metadata.layout.num_rows[kind];

        // There's an offset between the one-indexed CLI indices
        // (where zero represents a null value) and the usual
        // zero-indexed `usize` used by Rust (with `Option<usize>` to
        // represent a nullable index).
        //
        // This offset is already applied prior to this point, during
        // the unpacking of CLI indices.

        if index < num_rows {
            let bytes_per_row = self.metadata.layout.bytes_per_row[kind];
            let bytes = self
                .bytes
                .subrange(index * bytes_per_row..(index + 1) * bytes_per_row);
            let next_row_bytes = (index + 1 < num_rows).then(|| {
                self.bytes.subrange(
                    (index + 1) * bytes_per_row..(index + 2) * bytes_per_row,
                )
            });
            Ok(MetadataRow {
                bytes,
                next_row_bytes,
                index: MetadataTableIndex::new(index),
                metadata: self.metadata,
            })
        } else {
            Err(Error::InvalidMetadataTableIndex {
                kind,
                index,
                num_rows,
            })
        }
    }
}

impl<'a, TableTag> MetadataRow<'a, TableTag>
where
    TableTag: MetadataTableTag,
{
    #[inline]
    pub fn index(&self) -> MetadataTableIndex<TableTag> {
        self.index
    }

    #[inline]
    pub fn ptr_range(&self) -> Range<Pointer> {
        self.bytes.into()
    }

    fn get_field_bytes(&self, column: usize) -> ByteRange<'a> {
        let fields = TableTag::COLUMNS;

        let offset: usize = fields
            .iter()
            .take(column)
            .cloned()
            .map(|field| self.metadata.layout.column_size(field))
            .sum();

        let size = self.metadata.layout.column_size(fields[column]);

        self.bytes.subrange(offset..offset + size)
    }

    fn get_untyped_index_range(
        &self,
        column: usize,
    ) -> UnpackedValue<Range<usize>> {
        let begin_bytes = self.get_field_bytes(column);
        let end_index = self
            .next_row_bytes
            .as_ref()
            .map(|next_row| {
                let field_range: Range<Pointer> = begin_bytes.into();
                let row_range: Range<Pointer> = self.bytes.into();
                let offset_range = field_range.start - row_range.start
                    ..field_range.end - row_range.start;
                let end_bytes = next_row.subrange(offset_range);
                end_bytes.unpack::<UnpackedValue<_>>().unwrap().value()
            })
            .unwrap_or_else(|| {
                let MetadataColumnType::Index(MetadataIndexKind::Table(
                    table_kind,
                )) = TableTag::COLUMNS[column]
                else {
                    panic!("Only simple indices are used as ranges")
                };
                self.metadata.layout.num_rows[table_kind]
            });

        let (loc, begin_index): (_, usize) =
            begin_bytes.unpack::<UnpackedValue<_>>().unwrap().into();

        let begin_index = begin_index - 1;
        let end_index = end_index - 1;

        UnpackedValue::new(loc, begin_index..end_index)
    }

    fn get_index_range<Column>(
        &self,
        column: usize,
    ) -> UnpackedValue<MetadataTableIndexRange<Column>> {
        self.get_untyped_index_range(column)
            .map(|Range { start, end }| MetadataTableIndexRange {
                start,
                end,
                _phantom: PhantomData,
            })
    }
}

impl FieldFlags {
    pub fn is_static(self) -> bool {
        self.0 & 0x0010 > 0
    }

    pub fn is_compile_time_constant(self) -> bool {
        self.0 & 0x0040 > 0
    }
}

impl<'a> UnpackBytes<'a> for FieldFlags {
    type Error = Error;
    fn unpack(bytes: ByteRange<'a>) -> Result<Self, Self::Error> {
        Ok(Self(bytes.unpack()?))
    }
}

impl std::fmt::Display for FieldFlags {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<'a> MetadataRow<'a, Field> {
    pub fn is_static(&self) -> Result<bool, Error> {
        Ok(self.flags()?.is_static())
    }

    pub fn find_owning_class(&self) -> Result<MetadataRow<'a, TypeDef>, Error> {
        // TODO: Generalize this to apply to Method/Param tables as well.
        let field_index = self.index().index;
        let type_def_table = self.metadata.type_def_table();

        let mut index_range = 0..type_def_table.num_rows();

        while index_range.len() > 1 {
            let midpoint: usize =
                index_range.start + (index_range.end - index_range.start) / 2;
            debug_assert!(midpoint != index_range.start);
            debug_assert!(midpoint != index_range.end);

            let midpoint_field_index: usize = type_def_table
                .get_row(midpoint)?
                .field_indices()
                .value()
                .start;
            if midpoint_field_index <= field_index {
                index_range = index_range.start..midpoint_field_index;
            } else {
                index_range = midpoint_field_index..index_range.end;
            }
        }

        type_def_table.get_row(index_range.start)
    }

    pub fn signature(&self) -> Result<Signature<'a>, Error> {
        let bytes = self.raw_signature()?.content();
        Ok(Signature::new(bytes, self.metadata))
    }
}

impl<'a> MetadataRow<'a, MethodDef> {
    pub fn address(&self) -> Result<Option<Pointer>, Error> {
        self.rva()?
            .map(|rva| self.metadata.layout.virtual_address_to_raw(rva))
            .transpose()
    }

    pub fn cil_method(&self) -> Result<Option<CILMethod<'a>>, Error> {
        Ok(self.address()?.map(|addr| {
            CILMethod::new(self.metadata.dll_bytes.subrange(addr..))
        }))
    }

    pub fn is_static(&self) -> Result<bool, Error> {
        Ok(self.flags()?.is_static())
    }
}

impl MethodDefFlags {
    pub fn is_static(self) -> bool {
        self.0 & 0x0010 > 0
    }

    pub fn is_virtual(self) -> bool {
        self.0 & 0x0040 > 0
    }
}

impl<'a> UnpackBytes<'a> for MethodDefFlags {
    type Error = Error;
    fn unpack(bytes: ByteRange<'a>) -> Result<Self, Self::Error> {
        Ok(Self(bytes.unpack()?))
    }
}

impl std::fmt::Display for MethodDefFlags {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<'a> MetadataRow<'a, FieldRVA> {
    pub fn address(&self) -> Result<Pointer, Error> {
        self.metadata.layout.virtual_address_to_raw(self.rva()?)
    }
}
