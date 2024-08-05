use std::{borrow::Borrow, marker::PhantomData, ops::Range};

use memory_reader::{MemoryRegion, Pointer};

use crate::dos_header::DosHeaderUnpacker;
use crate::portable_executable::{
    DataDirectoryKind, OptionalHeaderUnpacker, PEHeaderUnpacker,
    SectionHeaderUnpacker,
};
use crate::relative_virtual_address::{RelativeVirtualAddress, VirtualRange};
use crate::UnpackedValue;
use crate::{Annotation as _, Annotator, Error};
use crate::{ByteRange, UnpackBytes};

#[derive(Copy, Clone)]
pub struct Unpacker<'a> {
    bytes: ByteRange<'a>,
}

pub struct ClrRuntimeHeaderUnpacker<'a> {
    bytes: ByteRange<'a>,
}

#[derive(Copy, Clone)]
pub struct MetadataUnpacker<'a> {
    bytes: ByteRange<'a>,
    dll_unpacker: Unpacker<'a>,
}

pub struct StreamHeader<'a> {
    /// The offset of the stream, in bytes, relative to the start of
    /// the metadata.
    pub offset: UnpackedValue<u32>,

    /// The size of the stream, in bytes.
    pub size: UnpackedValue<u32>,

    /// The name of the metadata section.  Max of 32 ASCII characters.
    pub name: UnpackedValue<&'a str>,

    /// The contents of the stream
    pub bytes: ByteRange<'a>,
}

pub trait EnumKey: Sized {
    const N: usize;

    fn as_index(self) -> usize;

    fn iter_keys() -> impl Iterator<Item = Self>;
}

pub struct MetadataTablesHeaderUnpacker<'a> {
    /// The entire contents of the #~ stream of the metadata.  While
    /// most of the unpacker types only have access to bytes that they
    /// directly contain, the size of the header for metadata tables
    /// depends on the number of tables present.
    bytes: ByteRange<'a>,
}

pub struct MetadataTables<'a> {
    bytes: ByteRange<'a>,
    dll_unpacker: Unpacker<'a>,
    table_sizes: MetadataTableSizes,
    heaps: MetadataMap<MetadataHeapKind, ByteRange<'a>>,
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

#[derive(Clone, Copy, Debug)]
pub enum MetadataIndexKind {
    Heap(MetadataHeapKind),
    Table(MetadataTableKind),
    CodedIndex(MetadataCodedIndexKind),
}

#[derive(Clone, Copy, Debug)]
pub enum MetadataColumnType {
    Index(MetadataIndexKind),
    FixedSize(usize),
}

/// Helper macro to define a static slice of `MetadataColumnType`
/// entries.
macro_rules! fields{
    (field String) => {
        MetadataColumnType::Index(MetadataIndexKind::Heap(
            MetadataHeapKind::String,
        ))
    };

    (field GUID) => {
        MetadataColumnType::Index(MetadataIndexKind::Heap(
            MetadataHeapKind::GUID,
        ))
    };
    (field Blob) => {
        MetadataColumnType::Index(MetadataIndexKind::Heap(
            MetadataHeapKind::Blob,
        ))
    };

    (field Module             ) => {
        MetadataColumnType::Index(MetadataIndexKind::Table(
            MetadataTableKind::Module,
        ))
    };
    (field TypeRef            ) => {
        MetadataColumnType::Index(MetadataIndexKind::Table(
            MetadataTableKind::TypeRef,
        ))
    };
    (field TypeDef            ) => {
        MetadataColumnType::Index(MetadataIndexKind::Table(
            MetadataTableKind::TypeDef,
        ))
    };
    (field Field              ) => {
        MetadataColumnType::Index(MetadataIndexKind::Table(
            MetadataTableKind::Field,
        ))
    };
    (field MethodDef          ) => {
        MetadataColumnType::Index(MetadataIndexKind::Table(
            MetadataTableKind::MethodDef,
        ))
    };
    (field Param              ) => {
        MetadataColumnType::Index(MetadataIndexKind::Table(
            MetadataTableKind::Param,
        ))
    };
    (field InterfaceImpl      ) => {
        MetadataColumnType::Index(MetadataIndexKind::Table(
            MetadataTableKind::InterfaceImpl,
        ))
    };
    (field MemberRef          ) => {
        MetadataColumnType::Index(MetadataIndexKind::Table(
            MetadataTableKind::MemberRef,
        ))
    };
    (field Constant           ) => {
        MetadataColumnType::Index(MetadataIndexKind::Table(
            MetadataTableKind::Constant,
        ))
    };
    (field CustomAttribute    ) => {
        MetadataColumnType::Index(MetadataIndexKind::Table(
            MetadataTableKind::CustomAttribute,
        ))
    };
    (field FieldMarshal       ) => {
        MetadataColumnType::Index(MetadataIndexKind::Table(
            MetadataTableKind::FieldMarshal,
        ))
    };
    (field DeclSecurity       ) => {
        MetadataColumnType::Index(MetadataIndexKind::Table(
            MetadataTableKind::DeclSecurity,
        ))
    };
    (field ClassLayout        ) => {
        MetadataColumnType::Index(MetadataIndexKind::Table(
            MetadataTableKind::ClassLayout,
        ))
    };
    (field FieldLayout        ) => {
        MetadataColumnType::Index(MetadataIndexKind::Table(
            MetadataTableKind::FieldLayout,
        ))
    };
    (field StandAloneSig      ) => {
        MetadataColumnType::Index(MetadataIndexKind::Table(
            MetadataTableKind::StandAloneSig,
        ))
    };
    (field EventMap           ) => {
        MetadataColumnType::Index(MetadataIndexKind::Table(
            MetadataTableKind::EventMap,
        ))
    };
    (field Event              ) => {
        MetadataColumnType::Index(MetadataIndexKind::Table(
            MetadataTableKind::Event,
        ))
    };
    (field PropertyMap        ) => {
        MetadataColumnType::Index(MetadataIndexKind::Table(
            MetadataTableKind::PropertyMap,
        ))
    };
    (field Property           ) => {
        MetadataColumnType::Index(MetadataIndexKind::Table(
            MetadataTableKind::Property,
        ))
    };
    (field MethodSemantics    ) => {
        MetadataColumnType::Index(MetadataIndexKind::Table(
            MetadataTableKind::MethodSemantics,
        ))
    };
    (field MethodImpl         ) => {
        MetadataColumnType::Index(MetadataIndexKind::Table(
            MetadataTableKind::MethodImpl,
        ))
    };
    (field ModuleRef          ) => {
        MetadataColumnType::Index(MetadataIndexKind::Table(
            MetadataTableKind::ModuleRef,
        ))
    };
    (field TypeSpec           ) => {
        MetadataColumnType::Index(MetadataIndexKind::Table(
            MetadataTableKind::TypeSpec,
        ))
    };
    (field ImplMap            ) => {
        MetadataColumnType::Index(MetadataIndexKind::Table(
            MetadataTableKind::ImplMap,
        ))
    };
    (field FieldRVA           ) => {
        MetadataColumnType::Index(MetadataIndexKind::Table(
            MetadataTableKind::FieldRVA,
        ))
    };
    (field Assembly           ) => {
        MetadataColumnType::Index(MetadataIndexKind::Table(
            MetadataTableKind::Assembly,
        ))
    };
    (field AssemblyProcessor  ) => {
        MetadataColumnType::Index(MetadataIndexKind::Table(
            MetadataTableKind::AssemblyProcessor,
        ))
    };
    (field AssemblyOS         ) => {
        MetadataColumnType::Index(MetadataIndexKind::Table(
            MetadataTableKind::AssemblyOS,
        ))
    };
    (field AssemblyRef        ) => {
        MetadataColumnType::Index(MetadataIndexKind::Table(
            MetadataTableKind::AssemblyRef,
        ))
    };
    (field AssemblyRefProcessor) => {
        MetadataColumnType::Index(MetadataIndexKind::Table(
            MetadataTableKind::AssemblyRefProcessor,
        ))
    };
    (field AssemblyRefOS      ) => {
        MetadataColumnType::Index(MetadataIndexKind::Table(
            MetadataTableKind::AssemblyRefOS,
        ))
    };
    (field File               ) => {
        MetadataColumnType::Index(MetadataIndexKind::Table(
            MetadataTableKind::File,
        ))
    };
    (field ExportedType       ) => {
        MetadataColumnType::Index(MetadataIndexKind::Table(
            MetadataTableKind::ExportedType,
        ))
    };
    (field ManifestResource   ) => {
        MetadataColumnType::Index(MetadataIndexKind::Table(
            MetadataTableKind::ManifestResource,
        ))
    };
    (field NestedClass        ) => {
        MetadataColumnType::Index(MetadataIndexKind::Table(
            MetadataTableKind::NestedClass,
        ))
    };
    (field GenericParam       ) => {
        MetadataColumnType::Index(MetadataIndexKind::Table(
            MetadataTableKind::GenericParam,
        ))
    };
    (field MethodSpec         ) => {
        MetadataColumnType::Index(MetadataIndexKind::Table(
            MetadataTableKind::MethodSpec,
        ))
    };
    (field GenericParamConstraint) => {
        MetadataColumnType::Index(MetadataIndexKind::Table(
            MetadataTableKind::GenericParamConstraint,
        ))
    };

    (field TypeDefOrRef        ) => {
        MetadataColumnType::Index(MetadataIndexKind::CodedIndex(
            MetadataCodedIndexKind::TypeDefOrRef,
        ))
    };
    (field HasConstant         ) => {
        MetadataColumnType::Index(MetadataIndexKind::CodedIndex(
            MetadataCodedIndexKind::HasConstant,
        ))
    };
    (field HasCustomAttribute  ) => {
        MetadataColumnType::Index(MetadataIndexKind::CodedIndex(
            MetadataCodedIndexKind::HasCustomAttribute,
        ))
    };
    (field HasFieldMarshal     ) => {
        MetadataColumnType::Index(MetadataIndexKind::CodedIndex(
            MetadataCodedIndexKind::HasFieldMarshal,
        ))
    };
    (field HasDeclSecurity     ) => {
        MetadataColumnType::Index(MetadataIndexKind::CodedIndex(
            MetadataCodedIndexKind::HasDeclSecurity,
        ))
    };
    (field MemberRefParent     ) => {
        MetadataColumnType::Index(MetadataIndexKind::CodedIndex(
            MetadataCodedIndexKind::MemberRefParent,
        ))
    };
    (field HasSemantics        ) => {
        MetadataColumnType::Index(MetadataIndexKind::CodedIndex(
            MetadataCodedIndexKind::HasSemantics,
        ))
    };
    (field MethodDefOrRef      ) => {
        MetadataColumnType::Index(MetadataIndexKind::CodedIndex(
            MetadataCodedIndexKind::MethodDefOrRef,
        ))
    };
    (field MemberForwarded     ) => {
        MetadataColumnType::Index(MetadataIndexKind::CodedIndex(
            MetadataCodedIndexKind::MemberForwarded,
        ))
    };
    (field Implementation      ) => {
        MetadataColumnType::Index(MetadataIndexKind::CodedIndex(
            MetadataCodedIndexKind::Implementation,
        ))
    };
    (field CustomAttributeType ) => {
        MetadataColumnType::Index(MetadataIndexKind::CodedIndex(
            MetadataCodedIndexKind::CustomAttributeType,
        ))
    };
    (field ResolutionScope     ) => {
        MetadataColumnType::Index(MetadataIndexKind::CodedIndex(
            MetadataCodedIndexKind::ResolutionScope,
        ))
    };
    (field TypeOrMethodDef     ) => {
        MetadataColumnType::Index(MetadataIndexKind::CodedIndex(
            MetadataCodedIndexKind::TypeOrMethodDef,
        ))
    };

    (field $bytes:literal) => {
        MetadataColumnType::FixedSize($bytes)
    };

    ( $( $field:tt ),+ ) => {
        &[
            $( fields!{field $field} ),+
        ]
    };
}

#[derive(Clone)]
pub struct MetadataMap<Key, Value> {
    // Once `generic_const_exprs` is available, the size can be
    // determined from `<Key as EnumKey>::N` For now, setting it to
    // the size required for `MetadataIndexKind`..
    values: [Value; 3 + 38 + 13],
    _phantom: PhantomData<Key>,
}

pub struct MetadataTableSizes {
    num_rows: MetadataMap<MetadataTableKind, usize>,
    index_size: MetadataMap<MetadataIndexKind, usize>,
    bytes_per_row: MetadataMap<MetadataTableKind, usize>,
    table_offsets: MetadataMap<MetadataTableKind, usize>,
}

pub struct MetadataTableUnpacker<'a, RowUnpacker> {
    /// The bytes that represent this table
    bytes: ByteRange<'a>,
    tables: &'a MetadataTables<'a>,
    row_unpacker: PhantomData<RowUnpacker>,
}

pub trait TypedMetadataIndex {
    type Output<'a: 'b, 'b>;

    fn access<'a: 'b, 'b>(
        self,
        tables: &'b MetadataTables<'a>,
    ) -> Result<Self::Output<'a, 'b>, Error>;
}

/// Typed index into a specific metadata type
#[derive(Clone, Copy)]
pub struct MetadataTableIndex<Unpacker> {
    index: usize,
    _phantom: PhantomData<Unpacker>,
}

/// Typed index into a specific metadata type
///
/// When #![feature(step_trait)] is stabilized
/// (https://github.com/rust-lang/rust/issues/42168), this will be
/// replaced with `Range<MetadataTableIndex<Unpacker>>`.
#[derive(Clone, Copy)]
pub struct MetadataTableIndexRange<Unpacker> {
    start: usize,
    end: usize,
    _phantom: PhantomData<Unpacker>,
}

#[derive(Clone, Copy)]
pub struct MetadataCodedIndex<CodedIndexType> {
    index: usize,
    kind: MetadataTableKind,
    _phantom: PhantomData<CodedIndexType>,
}

pub struct MetadataRowUnpacker<'a, RowUnpacker> {
    /// The bytes that are directly owned by this row of the table.
    bytes: ByteRange<'a>,

    /// The bytes that are directly owned by the next row of the
    /// table.  This is needed to interpret columns that provide a
    /// start index, with the end index provided by the same column in
    /// the next row (e.g. the Field and Method columns of the TypeDef
    /// table).
    next_row_bytes: Option<ByteRange<'a>>,

    tables: &'a MetadataTables<'a>,
    row_unpacker: PhantomData<RowUnpacker>,
}

pub trait MetadataTableTag: Copy {
    const KIND: MetadataTableKind;

    const COLUMNS: &'static [MetadataColumnType];
}

macro_rules! decl_metadata_table_tag{
    ($table_name:ident, $fields:tt) => {
        #[derive(Clone, Copy)]
        pub struct $table_name;

        impl MetadataTableTag for $table_name {
            const KIND: MetadataTableKind = MetadataTableKind::$table_name;
            const COLUMNS: &'static [MetadataColumnType] = fields! $fields;
        }
    };
}

decl_metadata_table_tag! { Module, [2, String, GUID, GUID, GUID] }
decl_metadata_table_tag! { TypeRef, [ResolutionScope, String, String] }
decl_metadata_table_tag! { TypeDef, [4, String, String, TypeDefOrRef, Field, MethodDef] }
decl_metadata_table_tag! { Field, [2, String, Blob] }
decl_metadata_table_tag! { MethodDef, [4, 2, 2, String, Blob, Param] }
decl_metadata_table_tag! { Param, [2, 2, String] }
decl_metadata_table_tag! { InterfaceImpl, [TypeDef, TypeDefOrRef] }
decl_metadata_table_tag! { MemberRef, [MemberRefParent, String, Blob] }
decl_metadata_table_tag! { Constant, [1, 1, HasConstant, Blob] }
decl_metadata_table_tag! { CustomAttribute, [HasCustomAttribute, CustomAttributeType, Blob] }
decl_metadata_table_tag! { FieldMarshal, [HasFieldMarshal, Blob] }
decl_metadata_table_tag! { DeclSecurity, [2, HasDeclSecurity, Blob] }
decl_metadata_table_tag! { ClassLayout, [2, 4, TypeDef] }
decl_metadata_table_tag! { FieldLayout, [4, Field] }
decl_metadata_table_tag! { StandAloneSig, [Blob] }
decl_metadata_table_tag! { EventMap, [TypeDef, Event] }
decl_metadata_table_tag! { Event, [2, String, TypeDefOrRef] }
decl_metadata_table_tag! { PropertyMap, [TypeDef, Property] }
decl_metadata_table_tag! { Property, [2, String, Blob] }
decl_metadata_table_tag! { MethodSemantics, [2, MethodDef, HasSemantics] }
decl_metadata_table_tag! { MethodImpl, [TypeDef, MethodDefOrRef, MethodDefOrRef] }
decl_metadata_table_tag! { ModuleRef, [String] }
decl_metadata_table_tag! { TypeSpec, [Blob] }
decl_metadata_table_tag! { ImplMap, [2, MemberForwarded, String, ModuleRef] }
decl_metadata_table_tag! { FieldRVA, [4, Field] }
decl_metadata_table_tag! { Assembly, [4, 2, 2, 2, 2, 4, Blob, String, String] }
decl_metadata_table_tag! { AssemblyProcessor, [4] }
decl_metadata_table_tag! { AssemblyOS, [4, 4, 4] }
decl_metadata_table_tag! { AssemblyRef, [2, 2, 2, 2, 4, Blob, String, String, Blob] }
decl_metadata_table_tag! { AssemblyRefProcessor, [4, AssemblyRef] }
decl_metadata_table_tag! { AssemblyRefOS, [4, 4, 4, AssemblyRef] }
decl_metadata_table_tag! { File, [4, String, Blob] }
decl_metadata_table_tag! { ExportedType, [4, TypeDef, String, String, Implementation] }
decl_metadata_table_tag! { ManifestResource, [4, 4, String, Implementation] }
decl_metadata_table_tag! { NestedClass, [TypeDef, TypeDef] }
decl_metadata_table_tag! { GenericParam, [2, 2, TypeOrMethodDef, String] }
decl_metadata_table_tag! { MethodSpec, [MethodDefOrRef, Blob] }
decl_metadata_table_tag! { GenericParamConstraint, [GenericParam, TypeDefOrRef] }

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
     $row_unpacker:ident,
     [ $( $table_type:ident ),+ $(,)? ] $(,)?
    ) => {
        pub struct $tag;

        pub enum $row_unpacker<'a> {
            $(
                $table_type(MetadataRowUnpacker<'a, $table_type>),
            )*
        }

        impl TypedMetadataIndex for MetadataCodedIndex<$tag> {
            type Output<'a: 'b, 'b> = $row_unpacker<'b>;

            fn access<'a: 'b, 'b>(
                self,
                tables: &'b MetadataTables<'a>,
            ) -> Result<Self::Output<'a, 'b>, Error> {
                Ok(match self.kind {
                    $(
                        MetadataTableKind::$table_type => $row_unpacker::$table_type(
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
        }

        impl TypedMetadataIndex for Option<MetadataCodedIndex<$tag>> {
            type Output<'a:'b, 'b> = Option<$row_unpacker<'b>>;

            fn access<'a: 'b, 'b>(
                self,
                tables: &'b MetadataTables<'a>,
            ) -> Result<Self::Output<'a, 'b>, Error> {
                self.map(|index| tables.get(index)).transpose()
            }
        }
    };
}

macro_rules! impl_coded_index {
    ($tag:ident,
     $row_unpacker:ident,
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
     $row_unpacker:ident,
     [ $( $table_type:ident ),+ $(,)? ] $(,)?
    ) => {
        decl_core_coded_index_type!{ $tag, $row_unpacker, [ $($table_type),+ ] }
        impl_coded_index!{ $tag, $row_unpacker, [ $($table_type),+ ] }
    };
}

decl_coded_index_type! {
    TypeDefOrRef, MetadataTypeDefOrRef,
    [TypeDef, TypeRef, TypeSpec],
}
decl_coded_index_type! {
    HasConstant, MetadataHasConstant,
    [Field, Param, Property],
}
decl_coded_index_type! {
    HasCustomAttribute, MetadataHasCustomAttribute,
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
    HasFieldMarshal, MetadataHasFieldMarshal,
    [Field, Param],
}
decl_coded_index_type! {
    HasDeclSecurity, MetadataHasDeclSecurity,
    [TypeDef, MethodDef, Assembly],
}
decl_coded_index_type! {
    MemberRefParent, MetadataMemberRefParent,
    [TypeDef, TypeRef, ModuleRef,
     MethodDef, TypeSpec],
}
decl_coded_index_type! {
    HasSemantics, MetadataHasSemantics,
    [Event, Property],
}
decl_coded_index_type! {
    MethodDefOrRef, MetadataMethodDefOrRef,
    [MethodDef, MemberRef],
}
decl_coded_index_type! {
    MemberForwarded, MetadataMemberForwarded,
    [Field, MethodDef],
}
decl_coded_index_type! {
    Implementation, MetadataImplementation,
    [File, AssemblyRef, ExportedType],
}
decl_coded_index_type! {
    ResolutionScope, MetadataResolutionScope,
    [Module, ModuleRef, AssemblyRef, TypeRef]
}
decl_coded_index_type! {
    TypeOrMethodDef, MetadataTypeOrMethodDef,
    [TypeDef, MethodDef],
}

// The CustomAttributeType implements the `CustomAttributeType`
// without the `decl_coded_index_type`, because it contains extra
// values in the table-type encoding, represented by `None` values in
// `OPTIONS`, and these `None` values would not be generated by the
// `decl_coded_index_type` macro.
decl_core_coded_index_type! { CustomAttributeType, MetadataCustomAttributeType, [MethodDef, MemberRef] }

impl CodedIndex for CustomAttributeType {
    const OPTIONS: &'static [Option<MetadataTableKind>] = &[
        None,
        None,
        Some(MetadataTableKind::MethodDef),
        Some(MetadataTableKind::MemberRef),
        None,
    ];
}

impl<CodedIndexType> std::fmt::Display for MetadataCodedIndex<CodedIndexType> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}[{}]", self.kind, self.index)
    }
}

impl<'a> MetadataTypeDefOrRef<'a> {
    pub fn name(&self) -> Result<UnpackedValue<&'a str>, Error> {
        match self {
            MetadataTypeDefOrRef::TypeDef(row) => row.name(),
            MetadataTypeDefOrRef::TypeRef(row) => row.name(),
            MetadataTypeDefOrRef::TypeSpec(row) => Ok(UnpackedValue::new(
                row.bytes.subrange(0..0).into(),
                "TypeSpec (anon)",
            )),
        }
    }
}

impl<'a> MetadataHasConstant<'a> {
    pub fn name(&self) -> Result<UnpackedValue<&'a str>, Error> {
        match self {
            MetadataHasConstant::Field(row) => row.name(),
            MetadataHasConstant::Param(row) => row.name(),
            MetadataHasConstant::Property(row) => row.name(),
        }
    }
}

impl<'a> MetadataHasFieldMarshal<'a> {
    pub fn name(&self) -> Result<UnpackedValue<&'a str>, Error> {
        match self {
            MetadataHasFieldMarshal::Field(row) => row.name(),
            MetadataHasFieldMarshal::Param(row) => row.name(),
        }
    }
}

impl<'a> MetadataHasDeclSecurity<'a> {
    pub fn name(&self) -> Result<UnpackedValue<&'a str>, Error> {
        match self {
            MetadataHasDeclSecurity::TypeDef(row) => row.name(),
            MetadataHasDeclSecurity::MethodDef(row) => row.name(),
            MetadataHasDeclSecurity::Assembly(row) => row.name(),
        }
    }
}

impl<'a> MetadataMemberRefParent<'a> {
    pub fn name(&self) -> Result<UnpackedValue<&'a str>, Error> {
        match self {
            MetadataMemberRefParent::TypeDef(row) => row.name(),
            MetadataMemberRefParent::TypeRef(row) => row.name(),
            MetadataMemberRefParent::ModuleRef(row) => row.name(),
            MetadataMemberRefParent::MethodDef(row) => row.name(),
            MetadataMemberRefParent::TypeSpec(row) => Ok(UnpackedValue::new(
                row.bytes.subrange(0..0).into(),
                "TypeSpec (anon)",
            )),
        }
    }
}

impl<'a> MetadataHasSemantics<'a> {
    pub fn name(&self) -> Result<UnpackedValue<&'a str>, Error> {
        match self {
            MetadataHasSemantics::Event(row) => row.name(),
            MetadataHasSemantics::Property(row) => row.name(),
        }
    }
}

impl<'a> MetadataMethodDefOrRef<'a> {
    pub fn name(&self) -> Result<UnpackedValue<&'a str>, Error> {
        match self {
            MetadataMethodDefOrRef::MethodDef(row) => row.name(),
            MetadataMethodDefOrRef::MemberRef(row) => row.name(),
        }
    }
}

impl<'a> MetadataMemberForwarded<'a> {
    pub fn name(&self) -> Result<UnpackedValue<&'a str>, Error> {
        match self {
            MetadataMemberForwarded::Field(row) => row.name(),
            MetadataMemberForwarded::MethodDef(row) => row.name(),
        }
    }
}

impl<'a> MetadataCustomAttributeType<'a> {
    pub fn name(&self) -> Result<UnpackedValue<&'a str>, Error> {
        match self {
            MetadataCustomAttributeType::MethodDef(row) => row.name(),
            MetadataCustomAttributeType::MemberRef(row) => row.name(),
        }
    }
}

impl<'a> MetadataResolutionScope<'a> {
    pub fn name(&self) -> Result<UnpackedValue<&'a str>, Error> {
        match self {
            MetadataResolutionScope::Module(row) => row.name(),
            MetadataResolutionScope::ModuleRef(row) => row.name(),
            MetadataResolutionScope::AssemblyRef(row) => row.name(),
            MetadataResolutionScope::TypeRef(row) => row.name(),
        }
    }
}

impl<'a> MetadataTypeOrMethodDef<'a> {
    pub fn name(&self) -> Result<UnpackedValue<&'a str>, Error> {
        match self {
            MetadataTypeOrMethodDef::TypeDef(row) => row.name(),
            MetadataTypeOrMethodDef::MethodDef(row) => row.name(),
        }
    }
}

impl<'a> UnpackBytes<'a> for usize {
    fn unpack(bytes: ByteRange<'a>) -> Result<Self, Error> {
        if bytes.len() == 2 {
            Ok(bytes.unpack::<u16>()? as usize)
        } else if bytes.len() == 4 {
            Ok(bytes.unpack::<u32>()? as usize)
        } else {
            panic!("Heap index is either u16 or u32");
        }
    }
}

macro_rules! define_core_heap_index {
    ($index_type:ident, $name:ident) => {
        #[derive(Clone, Copy)]
        pub struct $index_type(usize);

        impl std::fmt::Display for $index_type {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}[{}]", stringify!($name), self.0)
            }
        }
    };
}

macro_rules! impl_unpack_heap_index {
    ($index_type:ident, $name:ident) => {
        impl<'a> UnpackBytes<'a> for $index_type {
            fn unpack(bytes: ByteRange<'a>) -> Result<Self, Error> {
                bytes.unpack().map($index_type)
            }
        }
    };
}

macro_rules! decl_heap_index {
    ($index_type:ident, $name:ident) => {
        define_core_heap_index! {$index_type, $name}
        impl_unpack_heap_index! {$index_type, $name}
    };
}

decl_heap_index! { MetadataStringIndex, String }
decl_heap_index! { MetadataBlobIndex, Blob }
define_core_heap_index! { MetadataGuidIndex, GUID }

impl<'a> UnpackBytes<'a> for MetadataGuidIndex {
    fn unpack(bytes: ByteRange<'a>) -> Result<Self, Error> {
        bytes.unpack().and_then(|index: usize| {
            // Unlike indices into the #Strings or #Blob heaps, which
            // are zero-indexed, indices into the #GUID heap are
            // one-indexed.  An index of zero is used to represent a
            // null GUID.
            //
            // Converting the indices when unpacking avoids having a
            // Rust `usize` that cannot be used as an index.
            if index == 0 {
                Err(Error::InvalidGuidIndexZero)
            } else {
                Ok(MetadataGuidIndex(index - 1))
            }
        })
    }
}

impl<'a> UnpackBytes<'a> for Option<MetadataGuidIndex> {
    fn unpack(bytes: ByteRange<'a>) -> Result<Self, Error> {
        bytes.unpack().map(|index: usize| {
            if index == 0 {
                None
            } else {
                Some(MetadataGuidIndex(index - 1))
            }
        })
    }
}

impl<'a, Unpacker: MetadataTableTag> UnpackBytes<'a>
    for Option<MetadataTableIndex<Unpacker>>
{
    fn unpack(bytes: ByteRange<'a>) -> Result<Self, Error> {
        bytes.unpack().map(|index: usize| {
            if index == 0 {
                None
            } else {
                Some(MetadataTableIndex::new(index - 1))
            }
        })
    }
}

impl<'a, Unpacker: MetadataTableTag> UnpackBytes<'a>
    for MetadataTableIndex<Unpacker>
{
    fn unpack(bytes: ByteRange<'a>) -> Result<Self, Error> {
        bytes.unpack().and_then(|opt_index: Option<Self>| {
            opt_index.ok_or(Error::InvalidMetadataTableIndexZero {
                kind: Unpacker::KIND,
            })
        })
    }
}

impl<Unpacker: MetadataTableTag> std::fmt::Display
    for MetadataTableIndex<Unpacker>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}[{}]", Unpacker::KIND, self.index)
    }
}

impl<Unpacker> MetadataTableIndex<Unpacker> {
    fn new(index: usize) -> Self {
        Self {
            index,
            _phantom: PhantomData,
        }
    }
}

impl<CodedIndexType> MetadataCodedIndex<CodedIndexType> {
    fn new(kind: MetadataTableKind, index: usize) -> Self
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

impl<'a, CodedIndexType> UnpackBytes<'a>
    for Option<MetadataCodedIndex<CodedIndexType>>
where
    CodedIndexType: CodedIndex,
{
    fn unpack(bytes: ByteRange<'a>) -> Result<Self, Error> {
        bytes.unpack().and_then(|raw_index| {
            let table_kind = CodedIndexType::table_kind(raw_index)?;
            let Some(row_index) = CodedIndexType::row_index(raw_index) else {
                return Ok(None);
            };

            Ok(Some(MetadataCodedIndex::new(table_kind, row_index)))
        })
    }
}

impl<'a, CodedIndexType> UnpackBytes<'a> for MetadataCodedIndex<CodedIndexType>
where
    CodedIndexType: CodedIndex,
{
    fn unpack(bytes: ByteRange<'a>) -> Result<Self, Error> {
        bytes.unpack().and_then(|raw_index| {
            let table_kind = CodedIndexType::table_kind(raw_index)?;
            let row_index = CodedIndexType::row_index(raw_index).ok_or(
                Error::InvalidMetadataTableIndexZero { kind: table_kind },
            )?;
            Ok(MetadataCodedIndex::new(table_kind, row_index))
        })
    }
}

impl<Key, Value> MetadataMap<Key, Value> {
    fn init(func: impl Fn() -> Value) -> Self {
        Self {
            values: std::array::from_fn(|_| func()),
            _phantom: PhantomData,
        }
    }
}

impl<Key, Value: Default> Default for MetadataMap<Key, Value> {
    fn default() -> Self {
        Self {
            values: std::array::from_fn(|_| Default::default()),
            _phantom: PhantomData,
        }
    }
}

impl<'a> Unpacker<'a> {
    pub fn new(region: &'a MemoryRegion) -> Unpacker {
        Self {
            bytes: ByteRange {
                start: region.start(),
                bytes: region.data(),
            },
        }
    }

    pub fn unpacked_so_far(&self) -> Result<Pointer, Error> {
        let metadata = self.metadata()?;
        let metadata_tables = metadata.metadata_tables()?;
        Ok(metadata_tables.method_def_table()?.bytes.start)
    }

    pub fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        self.dos_header()?.collect_annotations(annotator)?;
        self.pe_header()?.collect_annotations(annotator)?;
        self.optional_header()?.collect_annotations(annotator)?;
        self.clr_runtime_header()?.collect_annotations(annotator)?;

        for section in self.iter_section_header()? {
            annotator
                .group(section.section_range()?)
                .name(format!("{} PE section", section.name()?.value));
            section.collect_annotations(annotator)?;
        }

        self.metadata()?.collect_annotations(annotator)?;

        Ok(())
    }

    pub fn dos_header(&self) -> Result<DosHeaderUnpacker, Error> {
        let bytes = self.bytes.subrange(0..DosHeaderUnpacker::SIZE);
        Ok(DosHeaderUnpacker::new(bytes))
    }

    pub fn pe_header(&self) -> Result<PEHeaderUnpacker, Error> {
        let dos_header = self.dos_header()?;
        let bytes = self.bytes.subrange(dos_header.pe_header_range()?);
        Ok(PEHeaderUnpacker::new(bytes))
    }

    pub fn optional_header(&self) -> Result<OptionalHeaderUnpacker, Error> {
        let pe_header = self.pe_header()?;
        let bytes = self.bytes.subrange(pe_header.optional_header_range()?);
        OptionalHeaderUnpacker::new(bytes)
    }

    pub fn iter_section_header(
        &self,
    ) -> Result<impl Iterator<Item = SectionHeaderUnpacker>, Error> {
        let pe_header = self.pe_header()?;
        let num_sections = pe_header.num_sections()?.value as usize;

        let section_header_base = pe_header.optional_header_range()?.end;

        let section_header_size = 40;

        let file_start = self.bytes.start;

        let iter = (0..num_sections)
            .map(move |i_section| {
                section_header_base + i_section * section_header_size
            })
            .map(move |start| {
                let bytes =
                    self.bytes.subrange(start..start + section_header_size);
                SectionHeaderUnpacker::new(bytes, file_start)
            });

        Ok(iter)
    }

    fn virtual_address_to_raw(
        &self,
        addr: RelativeVirtualAddress,
    ) -> Result<Pointer, Error> {
        self.iter_section_header()?
            .find_map(|section| section.map_address(addr).ok()?)
            .ok_or(Error::InvalidVirtualAddress(addr))
    }

    fn virtual_range_to_raw(
        &self,
        range: VirtualRange,
    ) -> Result<Range<Pointer>, Error> {
        let start: Pointer = self.virtual_address_to_raw(range.rva)?;
        let size = range.size as usize;
        Ok(start..start + size)
    }

    pub fn iter_data_directories(
        &self,
    ) -> Result<impl Iterator<Item = (DataDirectoryKind, ByteRange)>, Error>
    {
        let optional_header = self.optional_header()?;
        let iter = DataDirectoryKind::iter()
            .filter_map(move |data_dir_kind| {
                let virtual_range = optional_header
                    .data_directory(data_dir_kind)
                    .unwrap()
                    .value()?;

                let raw_range =
                    self.virtual_range_to_raw(virtual_range).unwrap();

                Some((data_dir_kind, self.bytes.subrange(raw_range)))
            })
            .filter(|(_, bytes)| bytes.len() > 0);

        Ok(iter)
    }

    pub fn clr_runtime_header(
        &self,
    ) -> Result<ClrRuntimeHeaderUnpacker, Error> {
        let optional_header = self.optional_header()?;
        let data_dir = optional_header
            .data_directory(DataDirectoryKind::ClrRuntimeHeader)?
            .value
            .ok_or(Error::MissingClrRuntimeHeader)?;

        let addr = self.virtual_address_to_raw(data_dir.rva)?;

        let size = data_dir.size as usize;
        let bytes = self.bytes.subrange(addr..addr + size);
        Ok(ClrRuntimeHeaderUnpacker { bytes })
    }

    pub fn metadata(self) -> Result<MetadataUnpacker<'a>, Error> {
        let clr_runtime_header = self.clr_runtime_header()?;
        let metadata_range = clr_runtime_header.metadata_range()?.value;
        let raw_start = self.virtual_address_to_raw(metadata_range.rva)?;
        let num_bytes = metadata_range.size as usize;
        let bytes = self.bytes.subrange(raw_start..raw_start + num_bytes);
        Ok(MetadataUnpacker {
            bytes,
            dll_unpacker: self,
        })
    }

    pub fn metadata_tables(self) -> Result<MetadataTables<'a>, Error> {
        self.metadata()?.metadata_tables()
    }
}

impl DataDirectoryKind {
    pub(crate) fn index(self) -> usize {
        match self {
            Self::ExportTable => 0,
            Self::ImportTable => 1,
            Self::ResourceTable => 2,
            Self::ExceptionTable => 3,
            Self::CertificateTable => 4,
            Self::BaseRelocationTable => 5,
            Self::Debug => 6,
            Self::Architecture => 7,
            Self::GlobalPtr => 8,
            Self::TLSTable => 9,
            Self::LoadConfigTable => 10,
            Self::BoundImportTable => 11,
            Self::ImportAddressTable => 12,
            Self::DelayImportDescriptor => 13,
            Self::ClrRuntimeHeader => 14,
            Self::Reserved15 => 15,
        }
    }

    fn iter() -> impl Iterator<Item = Self> {
        [
            Self::ExportTable,
            Self::ImportTable,
            Self::ResourceTable,
            Self::ExceptionTable,
            Self::CertificateTable,
            Self::BaseRelocationTable,
            Self::Debug,
            Self::Architecture,
            Self::GlobalPtr,
            Self::TLSTable,
            Self::LoadConfigTable,
            Self::BoundImportTable,
            Self::ImportAddressTable,
            Self::DelayImportDescriptor,
            Self::ClrRuntimeHeader,
            Self::Reserved15,
        ]
        .into_iter()
    }
}

impl TryFrom<u32> for DataDirectoryKind {
    type Error = Error;

    fn try_from(value: u32) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Self::ExportTable),
            1 => Ok(Self::ImportTable),
            2 => Ok(Self::ResourceTable),
            3 => Ok(Self::ExceptionTable),
            4 => Ok(Self::CertificateTable),
            5 => Ok(Self::BaseRelocationTable),
            6 => Ok(Self::Debug),
            7 => Ok(Self::Architecture),
            8 => Ok(Self::GlobalPtr),
            9 => Ok(Self::TLSTable),
            10 => Ok(Self::LoadConfigTable),
            11 => Ok(Self::BoundImportTable),
            12 => Ok(Self::ImportAddressTable),
            13 => Ok(Self::DelayImportDescriptor),
            14 => Ok(Self::ClrRuntimeHeader),
            15 => Ok(Self::Reserved15),
            other => Err(Error::InvalidDataDirectoryIndex(other)),
        }
    }
}

impl<'a> ClrRuntimeHeaderUnpacker<'a> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator.group(self.bytes).name("CLR Runtime Header");

        annotator.value(self.header_size()?).name("header_size");
        annotator
            .value(self.major_runtime_version()?)
            .name("major_runtime_version");
        annotator
            .value(self.minor_runtime_version()?)
            .name("minor_runtime_version");
        annotator
            .value(self.metadata_range()?)
            .name("metadata_range");
        annotator.value(self.flags()?).name("flags");
        annotator
            .value(self.entry_point_token()?)
            .name("entry_point_token");
        annotator.value(self.resources()?).name("resources");
        annotator
            .opt_value(self.strong_name_signature()?)
            .name("strong_name_signature");
        annotator
            .opt_value(self.code_manager_table()?)
            .name("code_manager_table");
        annotator
            .opt_value(self.vtable_fixups()?)
            .name("vtable_fixups");
        annotator
            .opt_value(self.export_address_table_jumps()?)
            .name("export_address_table_jumps");
        annotator
            .opt_value(self.managed_native_header()?)
            .name("managed_native_header");

        Ok(())
    }

    fn header_size(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(0)
    }

    fn major_runtime_version(&self) -> Result<UnpackedValue<u16>, Error> {
        self.bytes.get_u16(4)
    }

    fn minor_runtime_version(&self) -> Result<UnpackedValue<u16>, Error> {
        self.bytes.get_u16(6)
    }

    fn metadata_range(&self) -> Result<UnpackedValue<VirtualRange>, Error> {
        self.bytes.subrange(8..16).unpack()
    }

    fn flags(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(16)
    }

    fn entry_point_token(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(20)
    }

    fn resources(&self) -> Result<UnpackedValue<VirtualRange>, Error> {
        self.bytes.subrange(24..32).unpack()
    }

    fn strong_name_signature(
        &self,
    ) -> Result<UnpackedValue<Option<VirtualRange>>, Error> {
        self.bytes.subrange(32..40).unpack()
    }

    fn code_manager_table(
        &self,
    ) -> Result<UnpackedValue<Option<VirtualRange>>, Error> {
        self.bytes.subrange(40..48).unpack()
    }

    fn vtable_fixups(
        &self,
    ) -> Result<UnpackedValue<Option<VirtualRange>>, Error> {
        self.bytes.subrange(48..56).unpack()
    }

    fn export_address_table_jumps(
        &self,
    ) -> Result<UnpackedValue<Option<VirtualRange>>, Error> {
        self.bytes.subrange(56..64).unpack()
    }

    fn managed_native_header(
        &self,
    ) -> Result<UnpackedValue<Option<VirtualRange>>, Error> {
        self.bytes.subrange(64..72).unpack()
    }
}

impl<'a> MetadataUnpacker<'a> {
    pub fn ptr_range(&self) -> Range<Pointer> {
        self.bytes.into()
    }

    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator.group(self.bytes).name("CLR Metadata");

        annotator
            .range(self.metadata_signature()?.loc())
            .name("Metadata signature");
        annotator.value(self.major_version()?).name("major_version");
        annotator.value(self.minor_version()?).name("minor_version");
        annotator.value(self.reserved()?).name("reserved");
        annotator
            .value(self.version_str_len()?)
            .name("version_str_len");
        annotator.value(self.version_str()?).name("version_str");
        annotator.value(self.flags()?).name("flags");
        annotator.value(self.num_streams()?).name("num_streams");

        self.iter_stream_header()?.try_for_each(|stream_header| {
            stream_header?.collect_annotations(annotator)
        })?;

        self.metadata_tables_header()?
            .collect_annotations(annotator)?;

        self.metadata_tables()?.collect_annotations(annotator)?;

        Ok(())
    }

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
        self.bytes.get_u16(4)
    }

    pub fn minor_version(&self) -> Result<UnpackedValue<u16>, Error> {
        self.bytes.get_u16(6)
    }

    pub fn reserved(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(8)
    }

    pub fn version_str_len(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(12)
    }

    fn padded_version_str_len(&self) -> Result<usize, Error> {
        let len = self.version_str_len()?.value as usize;
        Ok(len.div_ceil(4) * 4)
    }

    pub fn version_str(&self) -> Result<UnpackedValue<&'a str>, Error> {
        let len = self.version_str_len()?.value as usize;
        let start = 16;

        let padded_len = self.padded_version_str_len()?;
        let value =
            std::str::from_utf8(self.bytes.subrange(start..start + len).bytes)?
                .trim_end_matches('\0');
        Ok(UnpackedValue::new(
            self.bytes.address_range(start..start + padded_len),
            value,
        ))
    }

    pub fn flags(&self) -> Result<UnpackedValue<u16>, Error> {
        let start = 16 + self.padded_version_str_len()?;
        self.bytes.get_u16(start)
    }

    pub fn num_streams(&self) -> Result<UnpackedValue<u16>, Error> {
        let start = 18 + self.padded_version_str_len()?;
        self.bytes.get_u16(start)
    }

    pub fn iter_stream_header(
        &self,
    ) -> Result<impl Iterator<Item = Result<StreamHeader, Error>> + '_, Error>
    {
        let num_streams = self.num_streams()?.value;

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
                    &self.bytes[name_start..name_start + name_len],
                )?;
                UnpackedValue::new(loc, value)
            };

            let stream = {
                let start = stream_offset.value as usize;
                let size = stream_size.value as usize;
                self.bytes.subrange(start..start + size)
            };

            curr_offset += 8 + padded_name_len;

            Ok(StreamHeader {
                offset: stream_offset,
                size: stream_size,
                name: stream_name,
                bytes: stream,
            })
        });

        Ok(iter)
    }

    pub fn metadata_tables_header(
        &self,
    ) -> Result<MetadataTablesHeaderUnpacker<'a>, Error> {
        let header = self
            .iter_stream_header()?
            .filter_map(|res_header| res_header.ok())
            .find(|header| header.name.value == "#~")
            .ok_or(Error::MissingStream("#~"))?;

        let bytes = {
            let offset = header.offset.value as usize;
            let size = header.size.value as usize;
            self.bytes.subrange(offset..offset + size)
        };

        Ok(MetadataTablesHeaderUnpacker { bytes })
    }

    pub fn metadata_heaps(
        &self,
    ) -> Result<MetadataMap<MetadataHeapKind, ByteRange<'a>>, Error> {
        let mut string_stream = None;
        let mut blob_stream = None;
        let mut guid_stream = None;

        for res in self.iter_stream_header()? {
            let header = res?;
            let offset = header.offset.value as usize;
            let size = header.size.value as usize;
            let bytes = self.bytes.subrange(offset..offset + size);

            if header.name.value == "#Strings" {
                string_stream = Some(bytes);
            } else if header.name.value == "#Blob" {
                blob_stream = Some(bytes);
            } else if header.name.value == "#GUID" {
                guid_stream = Some(bytes);
            }
        }

        let mut heaps = MetadataMap::init(|| ByteRange {
            start: Pointer::null(),
            bytes: &[],
        });
        heaps[MetadataHeapKind::String] =
            string_stream.ok_or(Error::MissingStream("#Strings"))?;
        heaps[MetadataHeapKind::Blob] =
            blob_stream.ok_or(Error::MissingStream("#Blob"))?;
        heaps[MetadataHeapKind::GUID] =
            guid_stream.ok_or(Error::MissingStream("#GUID"))?;

        Ok(heaps)
    }

    pub fn metadata_tables(self) -> Result<MetadataTables<'a>, Error> {
        let heaps = self.metadata_heaps()?;
        let header = self.metadata_tables_header()?;

        let table_sizes = header.metadata_table_sizes()?;
        let bytes = header.tables_after_header()?;

        Ok(MetadataTables {
            bytes,
            dll_unpacker: self.dll_unpacker,
            table_sizes,
            heaps,
        })
    }
}

impl<'a> StreamHeader<'a> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator
            .group(self.offset.loc().start..self.name.loc().end)
            .name("CLR Stream Header");

        annotator.range(self.offset.loc()).name("Stream Offset");
        annotator.range(self.size.loc()).name("Stream Size");
        annotator.range(self.name.loc()).name("Stream Name");

        annotator
            .group(self.bytes)
            .name(format!("{} Stream", self.name.value()));

        Ok(())
    }
}

impl<'a> MetadataTablesHeaderUnpacker<'a> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator.value(self.reserved_0()?).name("reserved_0");
        annotator.value(self.major_version()?).name("major_version");
        annotator.value(self.minor_version()?).name("minor_version");
        annotator.value(self.heap_sizes()?).name("heap_sizes");
        annotator.value(self.reserved_1()?).name("reserved_1");
        annotator
            .value(self.valid_table_bitfield()?)
            .name("valid_table_bitfield");
        annotator
            .value(self.sorted_table_bitfield()?)
            .name("sorted_table_bitfield");

        for res in self.iter_num_rows()? {
            let (value, kind) = res?;
            annotator.value(value).name(format!("Num {kind:?} rows"));
        }

        Ok(())
    }

    pub fn reserved_0(&self) -> Result<UnpackedValue<u32>, Error> {
        self.bytes.get_u32(0)
    }

    pub fn major_version(&self) -> Result<UnpackedValue<u8>, Error> {
        self.bytes.get_u8(4)
    }

    pub fn minor_version(&self) -> Result<UnpackedValue<u8>, Error> {
        self.bytes.get_u8(5)
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
        self.bytes.get_u8(7)
    }

    /// Bitfield indicating which tables are present.
    pub fn valid_table_bitfield(&self) -> Result<UnpackedValue<u64>, Error> {
        self.bytes.get_u64(8)
    }

    /// Bitfield indicating which tables are sorted.  Should have
    /// exactly 14 entries, matching the tables listed in section
    /// II.22 of ECMA-335.
    pub fn sorted_table_bitfield(&self) -> Result<UnpackedValue<u64>, Error> {
        self.bytes.get_u64(16)
    }

    pub fn iter_num_rows(
        &self,
    ) -> Result<
        impl Iterator<
                Item = Result<(UnpackedValue<u32>, MetadataTableKind), Error>,
            > + '_,
        Error,
    > {
        let bitfield = self.valid_table_bitfield()?.value;

        let iter = (0..64)
            .filter(move |i_bit| bitfield & (1 << i_bit) > 0)
            .scan(24, |offset, i_bit| {
                let row_offset = *offset;
                *offset += 4;

                let num_rows = match self.bytes.get_u32(row_offset) {
                    Ok(val) => val,
                    Err(err) => return Some(Err(err)),
                };
                let kind = match MetadataTableKind::from_bit_index(i_bit) {
                    Ok(val) => val,
                    Err(err) => return Some(Err(err)),
                };
                Some(Ok((num_rows, kind)))
            });

        Ok(iter)
    }

    pub fn metadata_table_sizes(&self) -> Result<MetadataTableSizes, Error> {
        let heap_sizes = {
            let mut heap_sizes =
                MetadataMap::<MetadataHeapKind, bool>::default();
            let raw_heap_sizes = self.heap_sizes()?.value;
            heap_sizes[MetadataHeapKind::String] =
                raw_heap_sizes.string_stream_uses_u32_addr;
            heap_sizes[MetadataHeapKind::GUID] =
                raw_heap_sizes.guid_stream_uses_u32_addr;
            heap_sizes[MetadataHeapKind::Blob] =
                raw_heap_sizes.blob_stream_uses_u32_addr;
            heap_sizes
        };

        let num_rows = {
            let mut num_rows =
                MetadataMap::<MetadataTableKind, usize>::default();
            for res in self.iter_num_rows()? {
                let (num_table_rows, kind) = res?;
                num_rows[kind] = num_table_rows.value as usize;
            }
            num_rows
        };

        let index_size = {
            let mut index_size =
                MetadataMap::<MetadataIndexKind, usize>::default();
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

        let row_size = {
            let mut row_size =
                MetadataMap::<MetadataTableKind, usize>::default();
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
                MetadataMap::<MetadataTableKind, usize>::default();

            let mut curr_offset = 0;
            for res in self.iter_num_rows()? {
                let (num_rows, kind) = res?;
                let num_rows = num_rows.value as usize;
                table_offsets[kind] = curr_offset;
                curr_offset += num_rows * row_size[kind];
            }

            table_offsets
        };

        Ok(MetadataTableSizes {
            num_rows,
            index_size,
            bytes_per_row: row_size,
            table_offsets,
        })
    }

    fn tables_after_header(&self) -> Result<ByteRange<'a>, Error> {
        let num_tables =
            self.valid_table_bitfield()?.value.count_ones() as usize;
        let header_size = 24 + 4 * num_tables;
        let bytes = self.bytes.subrange(header_size..self.bytes.len());
        Ok(bytes)
    }
}

impl<'a> MetadataTables<'a> {
    pub fn ptr_range(&self) -> Range<Pointer> {
        self.bytes.into()
    }

    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        for table_kind in MetadataTableKind::iter_keys() {
            let table_start = self.table_sizes.table_offsets[table_kind];
            let bytes_per_row = self.table_sizes.bytes_per_row[table_kind];
            let num_rows = self.table_sizes.num_rows[table_kind];
            for i_row in 0..num_rows {
                let row_start = table_start + i_row * bytes_per_row;
                let bytes =
                    self.bytes.subrange(row_start..row_start + bytes_per_row);
                annotator
                    .range(bytes.into())
                    .name(format!("{}[{}]", table_kind, i_row));
            }
        }

        self.module_table()?
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        self.type_ref_table()?
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        let field_table = self.field_table()?;
        let method_table = self.method_def_table()?;
        let param_table = self.param_table()?;
        self.type_def_table()?.iter_rows().try_for_each(|row| {
            row.collect_annotations(
                annotator,
                &field_table,
                &method_table,
                &param_table,
            )
        })?;

        self.interface_impl_table()?
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        self.member_ref_table()?
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        self.constant_table()?
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        self.custom_attribute_table()?
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        self.field_marshal_table()?
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        self.decl_security_table()?
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        self.class_layout_table()?
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        self.field_layout_table()?
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        self.stand_alone_sig_table()?
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        self.event_map_table()?
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        self.event_table()?
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        self.property_map_table()?
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        self.property_table()?
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        self.method_semantics_table()?
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        self.method_impl_table()?
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        self.module_ref_table()?
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        self.type_spec_table()?
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        self.impl_map_table()?
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        self.field_rva_table()?
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        self.assembly_table()?
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        self.assembly_ref_table()?
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        self.manifest_resource_table()?
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        self.nested_class_table()?
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        self.generic_param_table()?
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        self.method_spec_table()?
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        self.generic_param_constraint_table()?
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        Ok(())
    }

    fn get_table<'b, Unpacker>(
        &'b self,
    ) -> Result<MetadataTableUnpacker<'b, Unpacker>, Error>
    where
        Unpacker: MetadataTableTag,
    {
        let table_kind = Unpacker::KIND;

        let offset = self.table_sizes.table_offsets[table_kind];
        let bytes_per_row = self.table_sizes.bytes_per_row[table_kind];
        let num_rows = self.table_sizes.num_rows[table_kind];
        let size = bytes_per_row * num_rows;
        let bytes = self.bytes.subrange(offset..offset + size);

        Ok(MetadataTableUnpacker {
            bytes,
            tables: &self,
            row_unpacker: PhantomData,
        })
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

    fn iter_range<Unpacker>(
        &self,
        indices: impl Borrow<MetadataTableIndexRange<Unpacker>>,
    ) -> Result<impl Iterator<Item = MetadataRowUnpacker<Unpacker>>, Error>
    where
        Unpacker: MetadataTableTag,
    {
        let indices = indices.borrow();
        let table = self.get_table()?;
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
}

macro_rules! define_table_method {
    ($method_name:ident,$unpacker_name:ident) => {
        impl<'a> MetadataTables<'a> {
            pub fn $method_name<'b>(
                &'b self,
            ) -> Result<MetadataTableUnpacker<'b, $unpacker_name>, Error> {
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
        tables: &'b MetadataTables<'a>,
    ) -> Result<Self::Output<'a, 'b>, Error> {
        self.value().access(tables)
    }
}

impl TypedMetadataIndex for MetadataStringIndex {
    type Output<'a: 'b, 'b> = UnpackedValue<&'a str>;

    fn access<'a: 'b, 'b>(
        self,
        tables: &'b MetadataTables<'a>,
    ) -> Result<UnpackedValue<&'a str>, Error> {
        let bytes = tables.heaps[MetadataHeapKind::String];
        bytes.get_null_terminated(self.0)
    }
}

impl TypedMetadataIndex for MetadataBlobIndex {
    type Output<'a: 'b, 'b> = ByteRange<'a>;

    fn access<'a: 'b, 'b>(
        self,
        tables: &'b MetadataTables<'a>,
    ) -> Result<ByteRange<'a>, Error> {
        let blob_heap = tables.heaps[MetadataHeapKind::Blob];
        let index = self.0;

        let byte: u8 = blob_heap[index];

        let leading_ones = byte.leading_ones();
        let (size_size, size) = match leading_ones {
            0 => {
                let size: usize = (byte & 0x7f).into();
                (1, size)
            }
            1 => {
                let high: usize = (byte & 0x3f).into();
                let low: usize = blob_heap[index + 1].into();
                (2, (high << 8) + low)
            }
            2 => {
                let high: usize = (byte & 0x1f).into();
                let mid1: usize = blob_heap[index + 1].into();
                let mid2: usize = blob_heap[index + 2].into();
                let low: usize = blob_heap[index + 3].into();
                (4, (high << 24) + (mid1 << 16) + (mid2 << 8) + low)
            }
            _ => {
                return Err(Error::InvalidBlobHeader { leading_ones });
            }
        };

        let bytes = blob_heap.subrange(index..index + size_size + size);
        Ok(bytes)
    }
}

impl TypedMetadataIndex for MetadataGuidIndex {
    type Output<'a: 'b, 'b> = UnpackedValue<u128>;

    fn access<'a: 'b, 'b>(
        self,
        tables: &'b MetadataTables<'a>,
    ) -> Result<Self::Output<'a, 'b>, Error> {
        let index = self.0;
        let guid_size = 16;
        tables.heaps[MetadataHeapKind::GUID]
            .subrange(index * guid_size..(index + 1) * guid_size)
            .unpack()
    }
}

impl<Unpacker: MetadataTableTag> TypedMetadataIndex
    for MetadataTableIndex<Unpacker>
{
    type Output<'a: 'b, 'b> = MetadataRowUnpacker<'b, Unpacker>;

    fn access<'a: 'b, 'b>(
        self,
        tables: &'b MetadataTables<'a>,
    ) -> Result<Self::Output<'a, 'b>, Error> {
        tables.get_table()?.get_row(self.index)
    }
}

impl<Unpacker: MetadataTableTag> std::fmt::Display
    for MetadataTableIndexRange<Unpacker>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}[{}..{}]", Unpacker::KIND, self.start, self.end)
    }
}

mod _impl_typed_index_range_iter {
    use super::*;
    pub struct MetadataTableIndexIterRange<Unpacker> {
        index: usize,
        end: usize,
        _phantom: PhantomData<Unpacker>,
    }
    impl<Unpacker> IntoIterator for MetadataTableIndexRange<Unpacker> {
        type Item = MetadataTableIndex<Unpacker>;

        type IntoIter = MetadataTableIndexIterRange<Unpacker>;

        fn into_iter(self) -> Self::IntoIter {
            MetadataTableIndexIterRange {
                index: self.start,
                end: self.end,
                _phantom: PhantomData,
            }
        }
    }

    impl<Unpacker> Iterator for MetadataTableIndexIterRange<Unpacker> {
        type Item = MetadataTableIndex<Unpacker>;

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

impl MetadataTableSizes {
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

impl<Key, Value> std::ops::Index<Key> for MetadataMap<Key, Value>
where
    Key: EnumKey,
{
    type Output = Value;

    fn index(&self, key: Key) -> &Self::Output {
        &self.values[key.as_index()]
    }
}

impl<Key, Value> std::ops::IndexMut<Key> for MetadataMap<Key, Value>
where
    Key: EnumKey,
{
    fn index_mut(&mut self, key: Key) -> &mut Self::Output {
        &mut self.values[key.as_index()]
    }
}

impl<Value> std::ops::Index<MetadataHeapKind>
    for MetadataMap<MetadataIndexKind, Value>
{
    type Output = Value;

    fn index(&self, index: MetadataHeapKind) -> &Self::Output {
        &self[MetadataIndexKind::Heap(index)]
    }
}

impl<Value> std::ops::IndexMut<MetadataHeapKind>
    for MetadataMap<MetadataIndexKind, Value>
{
    fn index_mut(&mut self, index: MetadataHeapKind) -> &mut Self::Output {
        &mut self[MetadataIndexKind::Heap(index)]
    }
}

impl<Value> std::ops::Index<MetadataTableKind>
    for MetadataMap<MetadataIndexKind, Value>
{
    type Output = Value;

    fn index(&self, index: MetadataTableKind) -> &Self::Output {
        &self[MetadataIndexKind::Table(index)]
    }
}

impl<Value> std::ops::IndexMut<MetadataTableKind>
    for MetadataMap<MetadataIndexKind, Value>
{
    fn index_mut(&mut self, index: MetadataTableKind) -> &mut Self::Output {
        &mut self[MetadataIndexKind::Table(index)]
    }
}

impl<Value> std::ops::Index<MetadataCodedIndexKind>
    for MetadataMap<MetadataIndexKind, Value>
{
    type Output = Value;

    fn index(&self, index: MetadataCodedIndexKind) -> &Self::Output {
        &self[MetadataIndexKind::CodedIndex(index)]
    }
}

impl<Value> std::ops::IndexMut<MetadataCodedIndexKind>
    for MetadataMap<MetadataIndexKind, Value>
{
    fn index_mut(
        &mut self,
        index: MetadataCodedIndexKind,
    ) -> &mut Self::Output {
        &mut self[MetadataIndexKind::CodedIndex(index)]
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

impl From<usize> for MetadataColumnType {
    fn from(value: usize) -> Self {
        Self::FixedSize(value)
    }
}
impl From<MetadataHeapKind> for MetadataColumnType {
    fn from(value: MetadataHeapKind) -> Self {
        Self::Index(MetadataIndexKind::Heap(value))
    }
}
impl From<MetadataTableKind> for MetadataColumnType {
    fn from(value: MetadataTableKind) -> Self {
        Self::Index(MetadataIndexKind::Table(value))
    }
}
impl From<MetadataCodedIndexKind> for MetadataColumnType {
    fn from(value: MetadataCodedIndexKind) -> Self {
        Self::Index(MetadataIndexKind::CodedIndex(value))
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

impl<'a, Unpacker> MetadataTableUnpacker<'a, Unpacker> {
    fn validate_index_range(
        &self,
        indices: MetadataTableIndexRange<Unpacker>,
    ) -> Result<(), Error>
    where
        Unpacker: MetadataTableTag,
    {
        let num_rows = self.tables.table_sizes.num_rows[Unpacker::KIND];
        let MetadataTableIndexRange { start, end, .. } = indices;

        if end <= num_rows {
            Ok(())
        } else {
            Err(Error::InvalidMetadataTableIndexRange {
                kind: Unpacker::KIND,
                indices: start..end,
                num_rows,
            })
        }
    }

    pub fn iter_rows<'b>(
        &'b self,
    ) -> impl Iterator<Item = MetadataRowUnpacker<'a, Unpacker>> + 'a
    where
        'a: 'b,
        Unpacker: MetadataTableTag,
    {
        let num_rows = self.tables.table_sizes.num_rows[Unpacker::KIND];
        let bytes_per_row =
            self.tables.table_sizes.bytes_per_row[Unpacker::KIND];
        let tables = self.tables;
        let table_bytes = self.bytes;
        (0..num_rows).map(move |i_row| {
            let bytes = table_bytes
                .subrange(i_row * bytes_per_row..(i_row + 1) * bytes_per_row);
            let next_row_bytes = (i_row + 1 < num_rows).then(|| {
                table_bytes.subrange(
                    (i_row + 1) * bytes_per_row..(i_row + 2) * bytes_per_row,
                )
            });
            MetadataRowUnpacker {
                bytes,
                next_row_bytes,
                tables,
                row_unpacker: PhantomData,
            }
        })
    }

    pub fn address_range(
        &self,
        indices: impl Borrow<MetadataTableIndexRange<Unpacker>>,
    ) -> Result<Range<Pointer>, Error>
    where
        Unpacker: MetadataTableTag,
    {
        let indices = indices.borrow();
        self.address_range_from_untyped(indices.start..indices.end)
    }

    fn address_range_from_untyped(
        &self,
        indices: Range<usize>,
    ) -> Result<Range<Pointer>, Error>
    where
        Unpacker: MetadataTableTag,
    {
        let kind = Unpacker::KIND;
        let num_rows = self.tables.table_sizes.num_rows[kind];
        let bytes_per_row = self.tables.table_sizes.bytes_per_row[kind];

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

    pub fn get<'b>(
        &'b self,
        index: MetadataTableIndex<Unpacker>,
    ) -> Result<MetadataRowUnpacker<'a, Unpacker>, Error>
    where
        Unpacker: MetadataTableTag,
    {
        self.get_row(index.index)
    }

    pub fn get_row<'b>(
        &'b self,
        index: usize,
    ) -> Result<MetadataRowUnpacker<'a, Unpacker>, Error>
    where
        Unpacker: MetadataTableTag,
    {
        let kind = Unpacker::KIND;
        let num_rows = self.tables.table_sizes.num_rows[kind];

        // There's an offset between the one-indexed CLI indices
        // (where zero represents a null value) and the usual
        // zero-indexed `usize` used by Rust (with `Option<usize>` to
        // represent a nullable index).
        //
        // This offset is already applied prior to this point, during
        // the unpacking of CLI indices.

        if index < num_rows {
            let bytes_per_row = self.tables.table_sizes.bytes_per_row[kind];
            let bytes = self
                .bytes
                .subrange(index * bytes_per_row..(index + 1) * bytes_per_row);
            let next_row_bytes = (index + 1 < num_rows).then(|| {
                self.bytes.subrange(
                    (index + 1) * bytes_per_row..(index + 2) * bytes_per_row,
                )
            });
            Ok(MetadataRowUnpacker {
                bytes,
                next_row_bytes,
                tables: self.tables,
                row_unpacker: self.row_unpacker,
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

impl<'a, Unpacker> MetadataRowUnpacker<'a, Unpacker> {
    pub fn ptr_range(&self) -> Range<Pointer> {
        self.bytes.into()
    }

    fn get_field_bytes(&self, column: usize) -> ByteRange<'a>
    where
        Unpacker: MetadataTableTag,
    {
        let fields = Unpacker::COLUMNS;

        let offset: usize = fields
            .iter()
            .take(column)
            .cloned()
            .map(|field| self.tables.table_sizes.column_size(field))
            .sum();

        let size = self.tables.table_sizes.column_size(fields[column]);

        self.bytes.subrange(offset..offset + size)
    }

    fn get_untyped_index_range(
        &self,
        column: usize,
    ) -> UnpackedValue<Range<usize>>
    where
        Unpacker: MetadataTableTag,
    {
        let begin_bytes = self.get_field_bytes(column);
        let end_index = self
            .next_row_bytes
            .as_ref()
            .map(|next_row| {
                let range = begin_bytes.start - self.bytes.start
                    ..begin_bytes.end() - self.bytes.start;
                let end_bytes = next_row.subrange(range);
                end_bytes.unpack::<UnpackedValue<_>>().unwrap().value()
            })
            .unwrap_or_else(|| {
                let MetadataColumnType::Index(MetadataIndexKind::Table(
                    table_kind,
                )) = Unpacker::COLUMNS[column]
                else {
                    panic!("Only simple indices are used as ranges")
                };
                self.tables.table_sizes.num_rows[table_kind]
            });

        let (loc, begin_index) =
            begin_bytes.unpack::<UnpackedValue<_>>().unwrap().into();

        UnpackedValue::new(loc, begin_index..end_index)
    }

    fn get_index_range<Column>(
        &self,
        column: usize,
    ) -> UnpackedValue<MetadataTableIndexRange<Column>>
    where
        Unpacker: MetadataTableTag,
    {
        self.get_untyped_index_range(column)
            .map(|Range { start, end }| MetadataTableIndexRange {
                start,
                end,
                _phantom: PhantomData,
            })
    }
}

impl<'a> MetadataRowUnpacker<'a, Module> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator.value(self.generation()?).name("generation");

        let name = self.name()?.value;
        annotator
            .value(self.name_index()?)
            .name("Name")
            .append_value(name);

        annotator.value(self.module_id_index()?).name("Module id");
        annotator
            .value(self.module_id()?)
            .name(format!("GUID, '{name}' Module"));
        annotator.opt_value(self.enc_id()?).name("enc_id");
        annotator.opt_value(self.enc_base_id()?).name("enc_base_id");

        Ok(())
    }

    pub fn generation(&self) -> Result<UnpackedValue<u16>, Error> {
        self.get_field_bytes(0).unpack()
    }

    pub fn name_index(
        &self,
    ) -> Result<UnpackedValue<MetadataStringIndex>, Error> {
        self.get_field_bytes(1).unpack()
    }

    pub fn name(&self) -> Result<UnpackedValue<&'a str>, Error> {
        self.tables.get(self.name_index()?)
    }

    pub fn module_id_index(
        &self,
    ) -> Result<UnpackedValue<MetadataGuidIndex>, Error> {
        self.get_field_bytes(2).unpack()
    }

    pub fn module_id(&self) -> Result<UnpackedValue<u128>, Error> {
        self.tables.get(self.module_id_index()?)
    }

    pub fn enc_id(
        &self,
    ) -> Result<UnpackedValue<Option<MetadataGuidIndex>>, Error> {
        self.get_field_bytes(3).unpack()
    }

    pub fn enc_base_id(
        &self,
    ) -> Result<UnpackedValue<Option<MetadataGuidIndex>>, Error> {
        self.get_field_bytes(4).unpack()
    }
}

impl<'a> MetadataRowUnpacker<'a, TypeRef> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator
            .value(self.resolution_scope_index()?)
            .name("resolution_scope")
            .append_value(self.resolution_scope()?.name()?.value);

        annotator
            .value(self.name_index()?)
            .name("Type name")
            .append_value(self.name()?.value);
        annotator
            .value(self.namespace_index()?)
            .name("Type namespace")
            .append_value(self.namespace()?.value);

        Ok(())
    }

    fn resolution_scope_index(
        &self,
    ) -> Result<UnpackedValue<MetadataCodedIndex<ResolutionScope>>, Error> {
        self.get_field_bytes(0).unpack()
    }

    fn resolution_scope(&self) -> Result<MetadataResolutionScope, Error> {
        self.tables.get(self.resolution_scope_index()?)
    }

    fn name_index(&self) -> Result<UnpackedValue<MetadataStringIndex>, Error> {
        self.get_field_bytes(1).unpack()
    }

    pub fn name(&self) -> Result<UnpackedValue<&'a str>, Error> {
        self.tables.get(self.name_index()?)
    }

    fn namespace_index(
        &self,
    ) -> Result<UnpackedValue<MetadataStringIndex>, Error> {
        self.get_field_bytes(2).unpack()
    }

    fn namespace(&self) -> Result<UnpackedValue<&'a str>, Error> {
        self.tables.get(self.namespace_index()?)
    }
}

impl<'a> MetadataRowUnpacker<'a, TypeDef> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
        field_table: &MetadataTableUnpacker<Field>,
        method_def_table: &MetadataTableUnpacker<MethodDef>,
        param_table: &MetadataTableUnpacker<Param>,
    ) -> Result<(), Error> {
        annotator.value(self.flags()?).name("flags");
        let type_name = self.name()?.value;
        annotator
            .value(self.name_index()?)
            .name("Type name")
            .append_value(type_name);

        let type_namespace = self.namespace()?.value;
        annotator
            .value(self.namespace_index()?)
            .name("Type namespace")
            .append_value(type_namespace);

        annotator
            .opt_value(self.extends_index()?)
            .name("extends")
            .append_value(if let Some(base_class) = self.extends()? {
                base_class.name()?.value
            } else {
                "(none)"
            });
        let field_indices = self.field_indices()?;
        annotator.value(field_indices).name("Fields");

        annotator
            .group(field_table.address_range(field_indices)?)
            .name(format!("Class '{type_name}'"));

        self.iter_fields()?.try_for_each(|field| {
            field.collect_annotations(annotator, type_name, type_namespace)
        })?;

        let method_indices = self.method_indices()?;
        annotator.value(method_indices).name("method_indices");

        annotator
            .group(method_def_table.address_range(method_indices)?)
            .name(format!("Class '{type_name}'"));

        self.iter_methods()?.try_for_each(|method| {
            method.collect_annotations(
                annotator,
                type_name,
                type_namespace,
                param_table,
            )
        })?;

        {
            let method_range = method_indices.value;
            let param_start = method_def_table
                .get_row(method_range.start)?
                .param_indices()?
                .value
                .start;
            let param_end = if method_range.end
                == self.tables.table_sizes.num_rows
                    [MetadataTableKind::MethodDef]
            {
                self.tables.table_sizes.num_rows[MetadataTableKind::Param]
            } else {
                method_def_table
                    .get_row(method_range.end)?
                    .param_indices()?
                    .value
                    .end
            };
            annotator
                .group(
                    param_table
                        .address_range_from_untyped(param_start..param_end)?,
                )
                .name(format!("Class '{type_name}'"));
        }

        Ok(())
    }

    fn flags(&self) -> Result<UnpackedValue<u32>, Error> {
        self.get_field_bytes(0).unpack()
    }

    fn name_index(&self) -> Result<UnpackedValue<MetadataStringIndex>, Error> {
        self.get_field_bytes(1).unpack()
    }

    pub fn name(&self) -> Result<UnpackedValue<&'a str>, Error> {
        self.tables.get(self.name_index()?)
    }

    fn namespace_index(
        &self,
    ) -> Result<UnpackedValue<MetadataStringIndex>, Error> {
        self.get_field_bytes(2).unpack()
    }

    fn namespace(&self) -> Result<UnpackedValue<&'a str>, Error> {
        self.tables.get(self.namespace_index()?)
    }

    fn extends_index(
        &self,
    ) -> Result<UnpackedValue<Option<MetadataCodedIndex<TypeDefOrRef>>>, Error>
    {
        self.get_field_bytes(3).unpack()
    }

    fn extends(&self) -> Result<Option<MetadataTypeDefOrRef>, Error> {
        self.tables.get(self.extends_index()?)
    }

    fn field_indices(
        &self,
    ) -> Result<UnpackedValue<MetadataTableIndexRange<Field>>, Error> {
        Ok(self.get_index_range(4))
    }

    fn iter_fields(
        &self,
    ) -> Result<impl Iterator<Item = MetadataRowUnpacker<Field>>, Error> {
        self.tables.iter_range(self.field_indices()?)
    }

    fn method_indices(
        &self,
    ) -> Result<UnpackedValue<MetadataTableIndexRange<MethodDef>>, Error> {
        Ok(self.get_index_range(5))
    }

    fn iter_methods(
        &self,
    ) -> Result<impl Iterator<Item = MetadataRowUnpacker<MethodDef>>, Error>
    {
        self.tables.iter_range(self.method_indices()?)
    }
}

impl<'a> MetadataRowUnpacker<'a, Field> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
        type_name: &str,
        _type_namespace: &str,
    ) -> Result<(), Error> {
        annotator.value(self.flags()?).name("flags");

        let name = self.name()?.value;
        annotator
            .value(self.name_index()?)
            .name("name")
            .append_value(name);
        annotator
            .value(self.signature_index()?)
            .name("signature_index");

        annotator
            .range(self.signature()?.into())
            .name(format!("'{type_name}.{name}' signature"));

        Ok(())
    }

    fn flags(&self) -> Result<UnpackedValue<u16>, Error> {
        self.get_field_bytes(0).unpack()
    }

    fn name_index(&self) -> Result<UnpackedValue<MetadataStringIndex>, Error> {
        self.get_field_bytes(1).unpack()
    }

    pub fn name(&self) -> Result<UnpackedValue<&'a str>, Error> {
        self.tables.get(self.name_index()?)
    }

    fn signature_index(
        &self,
    ) -> Result<UnpackedValue<MetadataBlobIndex>, Error> {
        self.get_field_bytes(2).unpack()
    }

    fn signature(&self) -> Result<ByteRange, Error> {
        self.tables.get(self.signature_index()?)
    }
}

impl<'a> MetadataRowUnpacker<'a, MethodDef> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
        type_name: &str,
        type_namespace: &str,
        param_table: &MetadataTableUnpacker<Param>,
    ) -> Result<(), Error> {
        let name = self.name()?.value;

        let rva_ann = annotator.opt_value(self.rva()?).name("rva");

        if let Some(address) = self.address()? {
            rva_ann.append_value(address);
            annotator
                .range(address..address + 1)
                .name(format!("Method '{name}' Def"));
        }

        annotator.value(self.impl_flags()?).name("impl_flags");
        annotator.value(self.flags()?).name("flags");

        annotator
            .value(self.name_index()?)
            .name("name")
            .append_value(name);

        annotator
            .value(self.signature_index()?)
            .name("signature_index");
        annotator
            .range(self.signature()?.into())
            .name(format!("'{type_name}.{name}' signature"));

        let param_indices = self.param_indices()?;
        annotator.value(param_indices).name("param_indices");

        annotator
            .range(param_indices.loc())
            .name("Param names")
            .value(
                self.iter_params()?
                    .map(|param| param.name().map(|unpacked| unpacked.value()))
                    .collect::<Result<String, Error>>()?,
            );

        self.iter_params()?.try_for_each(|param| {
            param.collect_annotations(
                annotator,
                type_name,
                type_namespace,
                name,
            )
        })?;

        annotator
            .group(param_table.address_range(param_indices)?)
            .name(format!("Method '{name}'"));

        Ok(())
    }

    fn rva(
        &self,
    ) -> Result<UnpackedValue<Option<RelativeVirtualAddress>>, Error> {
        self.get_field_bytes(0).unpack()
    }

    fn address(&self) -> Result<Option<Pointer>, Error> {
        self.rva()?
            .value()
            .map(|rva| self.tables.dll_unpacker.virtual_address_to_raw(rva))
            .transpose()
    }

    fn impl_flags(&self) -> Result<UnpackedValue<u16>, Error> {
        self.get_field_bytes(1).unpack()
    }

    fn flags(&self) -> Result<UnpackedValue<u16>, Error> {
        self.get_field_bytes(2).unpack()
    }

    fn name_index(&self) -> Result<UnpackedValue<MetadataStringIndex>, Error> {
        self.get_field_bytes(3).unpack()
    }

    pub fn name(&self) -> Result<UnpackedValue<&'a str>, Error> {
        self.tables.get(self.name_index()?)
    }

    fn signature_index(
        &self,
    ) -> Result<UnpackedValue<MetadataBlobIndex>, Error> {
        self.get_field_bytes(4).unpack()
    }

    fn signature(&self) -> Result<ByteRange, Error> {
        self.tables.get(self.signature_index()?)
    }

    fn param_indices(
        &self,
    ) -> Result<UnpackedValue<MetadataTableIndexRange<Param>>, Error> {
        Ok(self.get_index_range(5))
    }

    fn iter_params(
        &self,
    ) -> Result<impl Iterator<Item = MetadataRowUnpacker<Param>>, Error> {
        self.tables.iter_range(self.param_indices()?)
    }
}

impl<'a> MetadataRowUnpacker<'a, Param> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
        _type_name: &str,
        _type_namespace: &str,
        _method_name: &str,
    ) -> Result<(), Error> {
        annotator.value(self.flags()?).name("flags");
        annotator.value(self.sequence()?).name("sequence");

        let name = self.name()?.value;
        annotator
            .value(self.name_index()?)
            .name("name")
            .append_value(name);

        Ok(())
    }

    fn flags(&self) -> Result<UnpackedValue<u16>, Error> {
        self.get_field_bytes(0).unpack()
    }

    fn sequence(&self) -> Result<UnpackedValue<u16>, Error> {
        self.get_field_bytes(1).unpack()
    }

    fn name_index(&self) -> Result<UnpackedValue<MetadataStringIndex>, Error> {
        self.get_field_bytes(2).unpack()
    }

    pub fn name(&self) -> Result<UnpackedValue<&'a str>, Error> {
        self.tables.get(self.name_index()?)
    }
}

impl<'a> MetadataRowUnpacker<'a, InterfaceImpl> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator
            .value(self.class_index()?)
            .name("Class")
            .append_value(self.class()?.name()?.value);
        annotator
            .value(self.interface_index()?)
            .name("Interface")
            .append_value(self.interface()?.name()?.value);

        Ok(())
    }

    fn class_index(
        &self,
    ) -> Result<UnpackedValue<MetadataTableIndex<TypeDef>>, Error> {
        self.get_field_bytes(0).unpack()
    }

    fn class(&self) -> Result<MetadataRowUnpacker<'a, TypeDef>, Error> {
        self.tables.get(self.class_index()?)
    }

    fn interface_index(
        &self,
    ) -> Result<UnpackedValue<MetadataCodedIndex<TypeDefOrRef>>, Error> {
        self.get_field_bytes(1).unpack()
    }

    fn interface(&self) -> Result<MetadataTypeDefOrRef, Error> {
        self.tables.get(self.interface_index()?)
    }
}

impl<'a> MetadataRowUnpacker<'a, MemberRef> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator
            .value(self.class_index()?)
            .name("class_index")
            .append_value(self.class()?.name()?.value);

        let name = self.name()?.value;
        annotator
            .value(self.name_index()?)
            .name("name")
            .append_value(name);

        annotator
            .value(self.signature_index()?)
            .name("signature_index");

        annotator
            .range(self.signature()?.into())
            .name(format!("MemberRef {name}"));

        Ok(())
    }

    fn class_index(
        &self,
    ) -> Result<UnpackedValue<MetadataCodedIndex<MemberRefParent>>, Error> {
        self.get_field_bytes(0).unpack()
    }

    fn class(&self) -> Result<MetadataMemberRefParent, Error> {
        self.tables.get(self.class_index()?)
    }

    fn name_index(&self) -> Result<UnpackedValue<MetadataStringIndex>, Error> {
        self.get_field_bytes(1).unpack()
    }

    pub fn name(&self) -> Result<UnpackedValue<&'a str>, Error> {
        self.tables.get(self.name_index()?)
    }

    fn signature_index(
        &self,
    ) -> Result<UnpackedValue<MetadataBlobIndex>, Error> {
        self.get_field_bytes(2).unpack()
    }

    fn signature(&self) -> Result<ByteRange, Error> {
        self.tables.get(self.signature_index()?)
    }
}

impl<'a> MetadataRowUnpacker<'a, Constant> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator.value(self.type_value()?).name("Type value");
        annotator
            .value(self.parent_index()?)
            .name("Parent")
            .append_value(self.parent()?.name()?.value);

        annotator.value(self.value_index()?).name("Value");
        annotator.range(self.value()?.into()).name("Constant value");

        Ok(())
    }

    fn type_value(&self) -> Result<UnpackedValue<u8>, Error> {
        self.get_field_bytes(0).unpack()
    }

    fn parent_index(
        &self,
    ) -> Result<UnpackedValue<MetadataCodedIndex<HasConstant>>, Error> {
        // Skip `get_field_bytes(1)`, which is a padding byte.
        self.get_field_bytes(2).unpack()
    }

    fn parent(&self) -> Result<MetadataHasConstant, Error> {
        self.tables.get(self.parent_index()?)
    }

    fn value_index(&self) -> Result<UnpackedValue<MetadataBlobIndex>, Error> {
        self.get_field_bytes(3).unpack()
    }

    fn value(&self) -> Result<ByteRange, Error> {
        self.tables.get(self.value_index()?)
    }
}

impl<'a> MetadataRowUnpacker<'a, CustomAttribute> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator.value(self.parent_index()?).name("Parent");
        annotator
            .value(self.attribute_type_index()?)
            .name("Type")
            .append_value(self.attribute_type()?.name()?.value);

        annotator.value(self.value_index()?).name("Value");
        annotator
            .range(self.value()?.into())
            .name("CustomAttribute value");

        Ok(())
    }

    fn parent_index(
        &self,
    ) -> Result<UnpackedValue<MetadataCodedIndex<HasCustomAttribute>>, Error>
    {
        self.get_field_bytes(0).unpack()
    }

    pub fn parent(&self) -> Result<MetadataHasCustomAttribute, Error> {
        self.tables.get(self.parent_index()?)
    }

    fn attribute_type_index(
        &self,
    ) -> Result<UnpackedValue<MetadataCodedIndex<CustomAttributeType>>, Error>
    {
        self.get_field_bytes(1).unpack()
    }

    fn attribute_type(&self) -> Result<MetadataCustomAttributeType, Error> {
        self.tables.get(self.attribute_type_index()?)
    }

    fn value_index(&self) -> Result<UnpackedValue<MetadataBlobIndex>, Error> {
        self.get_field_bytes(2).unpack()
    }

    fn value(&self) -> Result<ByteRange, Error> {
        self.tables.get(self.value_index()?)
    }
}

impl<'a> MetadataRowUnpacker<'a, FieldMarshal> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator
            .value(self.parent_index()?)
            .name("Parent")
            .append_value(self.parent()?.name()?.value);
        annotator
            .value(self.native_type_index()?)
            .name("Native type");

        Ok(())
    }

    fn parent_index(
        &self,
    ) -> Result<UnpackedValue<MetadataCodedIndex<HasFieldMarshal>>, Error> {
        self.get_field_bytes(0).unpack()
    }

    fn parent(&self) -> Result<MetadataHasFieldMarshal, Error> {
        self.tables.get(self.parent_index()?)
    }

    fn native_type_index(
        &self,
    ) -> Result<UnpackedValue<MetadataBlobIndex>, Error> {
        self.get_field_bytes(1).unpack()
    }
}

impl<'a> MetadataRowUnpacker<'a, DeclSecurity> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator.value(self.action()?).name("Action");
        annotator
            .value(self.parent_index()?)
            .name("Parent")
            .append_value(self.parent()?.name()?.value);

        annotator
            .value(self.permission_set_index()?)
            .name("Permission set");
        annotator
            .range(self.permission_set()?.into())
            .name("DeclSecurity permission set");

        Ok(())
    }

    fn action(&self) -> Result<UnpackedValue<u16>, Error> {
        self.get_field_bytes(0).unpack()
    }

    fn parent_index(
        &self,
    ) -> Result<UnpackedValue<MetadataCodedIndex<HasDeclSecurity>>, Error> {
        self.get_field_bytes(1).unpack()
    }

    fn parent(&self) -> Result<MetadataHasDeclSecurity, Error> {
        self.tables.get(self.parent_index()?)
    }

    fn permission_set_index(
        &self,
    ) -> Result<UnpackedValue<MetadataBlobIndex>, Error> {
        self.get_field_bytes(2).unpack()
    }

    fn permission_set(&self) -> Result<ByteRange, Error> {
        self.tables.get(self.permission_set_index()?)
    }
}

impl<'a> MetadataRowUnpacker<'a, ClassLayout> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator.value(self.alignment()?).name("Alignment");
        annotator.value(self.size()?).name("Size (bytes)");

        annotator
            .value(self.class_index()?)
            .name("Class")
            .append_value(self.class()?.name()?.value);

        Ok(())
    }

    fn alignment(&self) -> Result<UnpackedValue<u16>, Error> {
        self.get_field_bytes(0).unpack()
    }

    fn size(&self) -> Result<UnpackedValue<u32>, Error> {
        self.get_field_bytes(1).unpack()
    }

    fn class_index(
        &self,
    ) -> Result<UnpackedValue<MetadataTableIndex<TypeDef>>, Error> {
        self.get_field_bytes(2).unpack()
    }

    fn class(&self) -> Result<MetadataRowUnpacker<'a, TypeDef>, Error> {
        self.tables.get(self.class_index()?)
    }
}

impl<'a> MetadataRowUnpacker<'a, FieldLayout> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator.value(self.offset()?).name("Offset");
        annotator
            .value(self.field_index()?)
            .name("Field")
            .append_value(self.field()?.name()?.value);

        Ok(())
    }

    fn offset(&self) -> Result<UnpackedValue<u32>, Error> {
        self.get_field_bytes(0).unpack()
    }

    fn field_index(
        &self,
    ) -> Result<UnpackedValue<MetadataTableIndex<Field>>, Error> {
        self.get_field_bytes(1).unpack()
    }

    fn field(&self) -> Result<MetadataRowUnpacker<'a, Field>, Error> {
        self.tables.get(self.field_index()?)
    }
}

impl<'a> MetadataRowUnpacker<'a, StandAloneSig> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator.value(self.signature_index()?).name("Signature");
        annotator
            .range(self.signature()?.into())
            .name("StandAloneSig signature");

        Ok(())
    }

    fn signature_index(
        &self,
    ) -> Result<UnpackedValue<MetadataBlobIndex>, Error> {
        self.get_field_bytes(0).unpack()
    }

    fn signature(&self) -> Result<ByteRange, Error> {
        self.tables.get(self.signature_index()?)
    }
}

impl<'a> MetadataRowUnpacker<'a, EventMap> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator
            .value(self.class_index()?)
            .name("Class")
            .append_value(self.class()?.name()?.value);
        annotator.value(self.event_indices()?).name("Events");

        Ok(())
    }

    fn class_index(
        &self,
    ) -> Result<UnpackedValue<MetadataTableIndex<TypeDef>>, Error> {
        self.get_field_bytes(0).unpack()
    }

    fn class(&self) -> Result<MetadataRowUnpacker<'a, TypeDef>, Error> {
        self.tables.get(self.class_index()?)
    }

    fn event_indices(
        &self,
    ) -> Result<UnpackedValue<MetadataTableIndexRange<Event>>, Error> {
        Ok(self.get_index_range(1))
    }

    pub fn iter_events(
        &self,
    ) -> Result<impl Iterator<Item = MetadataRowUnpacker<Event>>, Error> {
        self.tables.iter_range(self.event_indices()?)
    }
}

impl<'a> MetadataRowUnpacker<'a, Event> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator.value(self.flags()?).name("Flags");

        annotator
            .value(self.name_index()?)
            .name("name")
            .append_value(self.name()?.value);

        annotator
            .opt_value(self.event_type_index()?)
            .name("Event type")
            .append_value(if let Some(event) = self.event_type()? {
                event.name()?.value
            } else {
                "(none)"
            });

        Ok(())
    }

    fn flags(&self) -> Result<UnpackedValue<u16>, Error> {
        self.get_field_bytes(0).unpack()
    }

    fn name_index(&self) -> Result<UnpackedValue<MetadataStringIndex>, Error> {
        self.get_field_bytes(1).unpack()
    }

    pub fn name(&self) -> Result<UnpackedValue<&'a str>, Error> {
        self.tables.get(self.name_index()?)
    }

    fn event_type_index(
        &self,
    ) -> Result<UnpackedValue<Option<MetadataCodedIndex<TypeDefOrRef>>>, Error>
    {
        self.get_field_bytes(2).unpack()
    }

    fn event_type(&self) -> Result<Option<MetadataTypeDefOrRef>, Error> {
        self.tables.get(self.event_type_index()?)
    }
}

impl<'a> MetadataRowUnpacker<'a, PropertyMap> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator
            .value(self.class_index()?)
            .name("Class")
            .append_value(self.class()?.name()?.value);
        annotator.value(self.property_indices()?).name("Properties");

        Ok(())
    }

    fn class_index(
        &self,
    ) -> Result<UnpackedValue<MetadataTableIndex<TypeDef>>, Error> {
        self.get_field_bytes(0).unpack()
    }

    fn class(&self) -> Result<MetadataRowUnpacker<'a, TypeDef>, Error> {
        self.tables.get(self.class_index()?)
    }

    fn property_indices(
        &self,
    ) -> Result<UnpackedValue<MetadataTableIndexRange<Property>>, Error> {
        Ok(self.get_index_range(1))
    }

    pub fn iter_properties(
        &self,
    ) -> Result<impl Iterator<Item = MetadataRowUnpacker<Property>>, Error>
    {
        self.tables.iter_range(self.property_indices()?)
    }
}

impl<'a> MetadataRowUnpacker<'a, Property> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator.value(self.flags()?).name("Flags");

        annotator
            .value(self.name_index()?)
            .name("name")
            .append_value(self.name()?.value);

        annotator.value(self.signature_index()?).name("Signature");
        annotator
            .range(self.signature()?.into())
            .name("Property signature");

        Ok(())
    }

    fn flags(&self) -> Result<UnpackedValue<u16>, Error> {
        self.get_field_bytes(0).unpack()
    }

    fn name_index(&self) -> Result<UnpackedValue<MetadataStringIndex>, Error> {
        self.get_field_bytes(1).unpack()
    }

    pub fn name(&self) -> Result<UnpackedValue<&'a str>, Error> {
        self.tables.get(self.name_index()?)
    }

    fn signature_index(
        &self,
    ) -> Result<UnpackedValue<MetadataBlobIndex>, Error> {
        self.get_field_bytes(2).unpack()
    }

    fn signature(&self) -> Result<ByteRange, Error> {
        self.tables.get(self.signature_index()?)
    }
}

impl<'a> MetadataRowUnpacker<'a, MethodSemantics> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator.value(self.semantics()?).name("Semantics");
        annotator
            .value(self.method_index()?)
            .name("Method")
            .append_value(self.method()?.name()?.value);
        annotator
            .value(self.association_index()?)
            .name("Association")
            .append_value(self.association()?.name()?.value);

        Ok(())
    }

    fn semantics(&self) -> Result<UnpackedValue<u16>, Error> {
        self.get_field_bytes(0).unpack()
    }

    fn method_index(
        &self,
    ) -> Result<UnpackedValue<MetadataTableIndex<MethodDef>>, Error> {
        self.get_field_bytes(1).unpack()
    }

    fn method(&self) -> Result<MetadataRowUnpacker<'a, MethodDef>, Error> {
        self.tables.get(self.method_index()?)
    }

    fn association_index(
        &self,
    ) -> Result<UnpackedValue<MetadataCodedIndex<HasSemantics>>, Error> {
        self.get_field_bytes(2).unpack()
    }

    fn association(&self) -> Result<MetadataHasSemantics, Error> {
        self.tables.get(self.association_index()?)
    }
}

impl<'a> MetadataRowUnpacker<'a, MethodImpl> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator
            .value(self.class_index()?)
            .name("Class")
            .append_value(self.class()?.name()?.value);
        annotator
            .value(self.method_body_index()?)
            .name("Method body")
            .append_value(self.method_body()?.name()?.value);
        annotator
            .value(self.method_declaration_index()?)
            .name("Method declaration")
            .append_value(self.method_declaration()?.name()?.value);

        Ok(())
    }

    fn class_index(
        &self,
    ) -> Result<UnpackedValue<MetadataTableIndex<TypeDef>>, Error> {
        self.get_field_bytes(0).unpack()
    }

    fn class(&self) -> Result<MetadataRowUnpacker<'a, TypeDef>, Error> {
        self.tables.get(self.class_index()?)
    }

    fn method_body_index(
        &self,
    ) -> Result<UnpackedValue<MetadataCodedIndex<MethodDefOrRef>>, Error> {
        self.get_field_bytes(1).unpack()
    }

    fn method_body(&self) -> Result<MetadataMethodDefOrRef, Error> {
        self.tables.get(self.method_body_index()?)
    }

    fn method_declaration_index(
        &self,
    ) -> Result<UnpackedValue<MetadataCodedIndex<MethodDefOrRef>>, Error> {
        self.get_field_bytes(2).unpack()
    }

    fn method_declaration(&self) -> Result<MetadataMethodDefOrRef, Error> {
        self.tables.get(self.method_declaration_index()?)
    }
}

impl<'a> MetadataRowUnpacker<'a, ModuleRef> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator
            .value(self.name_index()?)
            .name("name")
            .append_value(self.name()?.value);

        Ok(())
    }

    fn name_index(&self) -> Result<UnpackedValue<MetadataStringIndex>, Error> {
        self.get_field_bytes(0).unpack()
    }

    pub fn name(&self) -> Result<UnpackedValue<&'a str>, Error> {
        self.tables.get(self.name_index()?)
    }
}

impl<'a> MetadataRowUnpacker<'a, TypeSpec> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator.value(self.signature_index()?).name("Signature");
        annotator
            .range(self.signature()?.into())
            .name("TypeSpec signature");

        Ok(())
    }

    fn signature_index(
        &self,
    ) -> Result<UnpackedValue<MetadataBlobIndex>, Error> {
        self.get_field_bytes(0).unpack()
    }

    fn signature(&self) -> Result<ByteRange, Error> {
        self.tables.get(self.signature_index()?)
    }
}

impl<'a> MetadataRowUnpacker<'a, ImplMap> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator.value(self.mapping_flags()?).name("Mapping flags");
        annotator
            .value(self.member_forwarded_index()?)
            .name("Member forwarded")
            .append_value(self.member_forwarded()?.name()?.value);

        annotator
            .value(self.import_name_index()?)
            .name("Import name")
            .append_value(self.import_name()?.value);

        annotator
            .value(self.import_scope_index()?)
            .name("Import scope")
            .append_value(self.import_scope()?.name()?.value);

        Ok(())
    }

    fn mapping_flags(&self) -> Result<UnpackedValue<u16>, Error> {
        self.get_field_bytes(0).unpack()
    }

    fn member_forwarded_index(
        &self,
    ) -> Result<UnpackedValue<MetadataCodedIndex<MemberForwarded>>, Error> {
        self.get_field_bytes(1).unpack()
    }

    fn member_forwarded(&self) -> Result<MetadataMemberForwarded, Error> {
        self.tables.get(self.member_forwarded_index()?)
    }

    fn import_name_index(
        &self,
    ) -> Result<UnpackedValue<MetadataStringIndex>, Error> {
        self.get_field_bytes(2).unpack()
    }

    fn import_name(&self) -> Result<UnpackedValue<&'a str>, Error> {
        self.tables.get(self.import_name_index()?)
    }

    fn import_scope_index(
        &self,
    ) -> Result<UnpackedValue<MetadataTableIndex<ModuleRef>>, Error> {
        self.get_field_bytes(3).unpack()
    }

    fn import_scope(&self) -> Result<MetadataRowUnpacker<ModuleRef>, Error> {
        self.tables.get(self.import_scope_index()?)
    }
}

impl<'a> MetadataRowUnpacker<'a, FieldRVA> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator
            .value(self.rva()?)
            .name("RVA")
            .append_value(self.address()?);

        let name = self.field()?.name()?.value;

        annotator
            .value(self.field_index()?)
            .name("Field")
            .append_value(name);

        let address = self.address()?;
        annotator
            .range(address..address + 1)
            .name(format!("Field '{name}' default value"));

        Ok(())
    }

    fn rva(&self) -> Result<UnpackedValue<RelativeVirtualAddress>, Error> {
        self.get_field_bytes(0).unpack()
    }

    fn address(&self) -> Result<Pointer, Error> {
        self.tables
            .dll_unpacker
            .virtual_address_to_raw(self.rva()?.value())
    }

    fn field_index(
        &self,
    ) -> Result<UnpackedValue<MetadataTableIndex<Field>>, Error> {
        self.get_field_bytes(1).unpack()
    }

    fn field(&self) -> Result<MetadataRowUnpacker<Field>, Error> {
        self.tables.get(self.field_index()?)
    }
}

impl<'a> MetadataRowUnpacker<'a, Assembly> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator
            .value(self.hash_algorithm_id()?)
            .name("Hash algorithm");
        annotator.value(self.major_version()?).name("Major version");
        annotator.value(self.minor_version()?).name("Minor version");
        annotator.value(self.build_number()?).name("Build number");
        annotator
            .value(self.revision_number()?)
            .name("Revision number");

        annotator.value(self.flags()?).name("Flags");

        let name = self.name()?.value;
        annotator
            .value(self.name_index()?)
            .name("name")
            .append_value(name);

        annotator.value(self.public_key_index()?).name("Public key");
        annotator
            .range(self.public_key()?.into())
            .name(format!("Public key, '{name}' assembly"));

        Ok(())
    }

    fn hash_algorithm_id(&self) -> Result<UnpackedValue<u32>, Error> {
        self.get_field_bytes(0).unpack()
    }

    fn major_version(&self) -> Result<UnpackedValue<u16>, Error> {
        self.get_field_bytes(1).unpack()
    }

    fn minor_version(&self) -> Result<UnpackedValue<u16>, Error> {
        self.get_field_bytes(2).unpack()
    }

    fn build_number(&self) -> Result<UnpackedValue<u16>, Error> {
        self.get_field_bytes(3).unpack()
    }

    fn revision_number(&self) -> Result<UnpackedValue<u16>, Error> {
        self.get_field_bytes(4).unpack()
    }

    fn flags(&self) -> Result<UnpackedValue<u32>, Error> {
        self.get_field_bytes(5).unpack()
    }

    fn public_key_index(
        &self,
    ) -> Result<UnpackedValue<MetadataBlobIndex>, Error> {
        self.get_field_bytes(6).unpack()
    }

    fn public_key(&self) -> Result<ByteRange, Error> {
        self.tables.get(self.public_key_index()?)
    }

    fn name_index(&self) -> Result<UnpackedValue<MetadataStringIndex>, Error> {
        self.get_field_bytes(7).unpack()
    }

    pub fn name(&self) -> Result<UnpackedValue<&'a str>, Error> {
        self.tables.get(self.name_index()?)
    }
}

impl<'a> MetadataRowUnpacker<'a, AssemblyRef> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator.value(self.major_version()?).name("Major version");
        annotator.value(self.minor_version()?).name("Minor version");
        annotator.value(self.build_number()?).name("Build number");
        annotator
            .value(self.revision_number()?)
            .name("Revision number");

        annotator.value(self.flags()?).name("Flags");

        let name = self.name()?.value;
        annotator
            .value(self.name_index()?)
            .name("name")
            .append_value(name);

        annotator
            .value(self.culture_index()?)
            .name("culture")
            .append_value(self.culture()?.value);

        annotator
            .value(self.public_key_or_token_index()?)
            .name("Public key/token");
        annotator
            .range(self.public_key_or_token()?.into())
            .name(format!("Public key/token, '{name}' AssemblyRef"));

        annotator.value(self.hash_value_index()?).name("Hash value");
        annotator
            .range(self.hash_value()?.into())
            .name(format!("HashValue, '{name}' AssemblyRef"));

        Ok(())
    }

    fn major_version(&self) -> Result<UnpackedValue<u16>, Error> {
        self.get_field_bytes(0).unpack()
    }

    fn minor_version(&self) -> Result<UnpackedValue<u16>, Error> {
        self.get_field_bytes(1).unpack()
    }

    fn build_number(&self) -> Result<UnpackedValue<u16>, Error> {
        self.get_field_bytes(2).unpack()
    }

    fn revision_number(&self) -> Result<UnpackedValue<u16>, Error> {
        self.get_field_bytes(3).unpack()
    }

    fn flags(&self) -> Result<UnpackedValue<u32>, Error> {
        self.get_field_bytes(4).unpack()
    }

    fn public_key_or_token_index(
        &self,
    ) -> Result<UnpackedValue<MetadataBlobIndex>, Error> {
        self.get_field_bytes(5).unpack()
    }

    fn public_key_or_token(&self) -> Result<ByteRange, Error> {
        self.tables.get(self.public_key_or_token_index()?)
    }

    fn name_index(&self) -> Result<UnpackedValue<MetadataStringIndex>, Error> {
        self.get_field_bytes(6).unpack()
    }

    pub fn name(&self) -> Result<UnpackedValue<&'a str>, Error> {
        self.tables.get(self.name_index()?)
    }

    fn culture_index(
        &self,
    ) -> Result<UnpackedValue<MetadataStringIndex>, Error> {
        self.get_field_bytes(7).unpack()
    }

    fn culture(&self) -> Result<UnpackedValue<&'a str>, Error> {
        self.tables.get(self.culture_index()?)
    }

    fn hash_value_index(
        &self,
    ) -> Result<UnpackedValue<MetadataBlobIndex>, Error> {
        self.get_field_bytes(8).unpack()
    }

    fn hash_value(&self) -> Result<ByteRange, Error> {
        self.tables.get(self.hash_value_index()?)
    }
}

impl<'a> MetadataRowUnpacker<'a, ManifestResource> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator.value(self.offset()?).name("Offset");
        annotator.value(self.flags()?).name("Flags");

        annotator
            .value(self.name_index()?)
            .name("name")
            .append_value(self.name()?.value);

        annotator
            .opt_value(self.implementation_index()?)
            .name("Implementation");

        Ok(())
    }

    fn offset(&self) -> Result<UnpackedValue<u32>, Error> {
        self.get_field_bytes(0).unpack()
    }

    fn flags(&self) -> Result<UnpackedValue<u32>, Error> {
        self.get_field_bytes(1).unpack()
    }

    fn name_index(&self) -> Result<UnpackedValue<MetadataStringIndex>, Error> {
        self.get_field_bytes(2).unpack()
    }

    pub fn name(&self) -> Result<UnpackedValue<&'a str>, Error> {
        self.tables.get(self.name_index()?)
    }

    fn implementation_index(
        &self,
    ) -> Result<UnpackedValue<Option<MetadataCodedIndex<Implementation>>>, Error>
    {
        // TODO: When implementing a method to find the location of an
        // implementation, `None` values may require special handling.
        // ECMA-335 (section II.22.24) explicitly calls it this index
        // as optional, and that an index of zero refers to a location
        // within the current file.
        //
        // If it references the current file, `offset` is the number
        // of bytes relative to the `Resources` entry of the CLR
        // header. (`dll_unpacker.clr_runtime_header()?.resource()`,
        // adjusted by `dll_unpacker.virtual_address_to_raw`)
        self.get_field_bytes(3).unpack()
    }

    pub fn implementation(
        &self,
    ) -> Result<Option<MetadataImplementation>, Error> {
        self.tables.get(self.implementation_index()?)
    }
}

impl<'a> MetadataRowUnpacker<'a, NestedClass> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator
            .value(self.nested_class_index()?)
            .name("Nested class")
            .append_value(self.nested_class()?.name()?.value);
        annotator
            .value(self.enclosing_class_index()?)
            .name("Enclosing class")
            .append_value(self.enclosing_class()?.name()?.value);

        Ok(())
    }

    fn nested_class_index(
        &self,
    ) -> Result<UnpackedValue<MetadataTableIndex<TypeDef>>, Error> {
        self.get_field_bytes(0).unpack()
    }

    fn nested_class(&self) -> Result<MetadataRowUnpacker<'a, TypeDef>, Error> {
        self.tables.get(self.nested_class_index()?)
    }

    fn enclosing_class_index(
        &self,
    ) -> Result<UnpackedValue<MetadataTableIndex<TypeDef>>, Error> {
        self.get_field_bytes(1).unpack()
    }

    fn enclosing_class(
        &self,
    ) -> Result<MetadataRowUnpacker<'a, TypeDef>, Error> {
        self.tables.get(self.enclosing_class_index()?)
    }
}

impl<'a> MetadataRowUnpacker<'a, GenericParam> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator.value(self.number()?).name("Param number");
        annotator.value(self.flags()?).name("Flags");
        annotator
            .value(self.owner_index()?)
            .name("Owner index")
            .append_value(self.owner()?.name()?.value);

        annotator
            .value(self.name_index()?)
            .name("name")
            .append_value(self.name()?.value);

        Ok(())
    }

    fn number(&self) -> Result<UnpackedValue<u16>, Error> {
        self.get_field_bytes(0).unpack()
    }

    fn flags(&self) -> Result<UnpackedValue<u16>, Error> {
        self.get_field_bytes(1).unpack()
    }

    fn owner_index(
        &self,
    ) -> Result<UnpackedValue<MetadataCodedIndex<TypeOrMethodDef>>, Error> {
        self.get_field_bytes(2).unpack()
    }

    fn owner(&self) -> Result<MetadataTypeOrMethodDef, Error> {
        self.tables.get(self.owner_index()?)
    }

    fn name_index(&self) -> Result<UnpackedValue<MetadataStringIndex>, Error> {
        self.get_field_bytes(3).unpack()
    }

    pub fn name(&self) -> Result<UnpackedValue<&'a str>, Error> {
        self.tables.get(self.name_index()?)
    }
}

impl<'a> MetadataRowUnpacker<'a, MethodSpec> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator
            .value(self.method_index()?)
            .name("Method")
            .append_value(self.method()?.name()?.value);

        annotator
            .value(self.instantiation_index()?)
            .name("Instantiation");
        annotator
            .range(self.instantiation()?.into())
            .name("MethodSpec instantiation");

        Ok(())
    }

    fn method_index(
        &self,
    ) -> Result<UnpackedValue<MetadataCodedIndex<MethodDefOrRef>>, Error> {
        self.get_field_bytes(0).unpack()
    }

    fn method(&self) -> Result<MetadataMethodDefOrRef, Error> {
        self.tables.get(self.method_index()?)
    }

    fn instantiation_index(
        &self,
    ) -> Result<UnpackedValue<MetadataBlobIndex>, Error> {
        self.get_field_bytes(1).unpack()
    }

    fn instantiation(&self) -> Result<ByteRange, Error> {
        self.tables.get(self.instantiation_index()?)
    }
}

impl<'a> MetadataRowUnpacker<'a, GenericParamConstraint> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator
            .value(self.generic_param_index()?)
            .name("Generic param")
            .append_value(self.generic_param()?.name()?.value);
        annotator
            .value(self.constraint_index()?)
            .name("Constraint")
            .append_value(self.constraint()?.name()?.value);

        Ok(())
    }

    fn generic_param_index(
        &self,
    ) -> Result<UnpackedValue<MetadataTableIndex<GenericParam>>, Error> {
        self.get_field_bytes(0).unpack()
    }

    fn generic_param(
        &self,
    ) -> Result<MetadataRowUnpacker<GenericParam>, Error> {
        self.tables.get(self.generic_param_index()?)
    }

    fn constraint_index(
        &self,
    ) -> Result<UnpackedValue<MetadataCodedIndex<TypeDefOrRef>>, Error> {
        self.get_field_bytes(1).unpack()
    }

    fn constraint(&self) -> Result<MetadataTypeDefOrRef, Error> {
        self.tables.get(self.constraint_index()?)
    }
}
