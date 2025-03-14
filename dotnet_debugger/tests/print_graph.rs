use dotnet_debugger::{RuntimePrimType, SymbolicGraph};
use indoc::indoc;

#[test]
fn print_expanded_graph() {
    let mut graph = SymbolicGraph::new();
    let obj = graph.static_field("class_name", "field_name");
    let subfield = graph.access_field(obj, "subfield");
    let active_index = graph.access_field(subfield, "active_index");
    let array = graph.access_field(subfield, "list._items");
    let item = graph.access_index(array, active_index);
    let func = graph.function_def(vec![], item);
    graph.name(func, "main").unwrap();
    graph.mark_extern_func(func).unwrap();

    let printed = format!("{}", graph.printer().expand_all_expressions());
    let expected = indoc! {"
        let _0 = class_name.field_name;
        let _1 = _0.subfield;
        let _2 = _1.active_index;
        let _3 = _1.list;
        let _4 = _3._items;
        let _5 = _2.prim_cast::<usize>();
        let _6 = _4[_5];
        pub fn main() { _6 }"
    };

    println!("-------------- Expected --------------\n{expected}");
    println!("-------------- Actual   --------------\n{printed}");

    assert_eq!(printed, expected);
}

#[test]
fn print_compact_graph() {
    let mut graph = SymbolicGraph::new();
    let obj = graph.static_field("class_name", "field_name");
    let subfield = graph.access_field(obj, "subfield");
    let active_index = graph.access_field(subfield, "active_index");
    let array = graph.access_field(subfield, "list._items");
    let item = graph.access_index(array, active_index);
    let func = graph.function_def(vec![], item);
    graph.name(func, "main").unwrap();
    graph.mark_extern_func(func).unwrap();

    let printed = format!("{graph}");
    let expected = indoc! {"
       let _1 = class_name.field_name.subfield;
       let _6 = _1.list._items[_1.active_index.prim_cast::<usize>()];
       pub fn main() { _6 }"
    };

    println!("-------------- Expected --------------\n{expected}");
    println!("-------------- Actual   --------------\n{printed}");

    assert_eq!(printed, expected);
}

#[test]
fn print_named_object() {
    // An object may be given an explicit name.  That name should be
    // used when printing the SymbolicGraph.

    let mut graph = SymbolicGraph::new();
    let obj = graph.static_field("class_name", "field_name");
    let subfield = graph.access_field(obj, "subfield");
    graph.name(subfield, "obj").unwrap();
    let active_index = graph.access_field(subfield, "active_index");
    let array = graph.access_field(subfield, "list._items");
    let item = graph.access_index(array, active_index);
    let func = graph.function_def(vec![], item);
    graph.name(func, "main").unwrap();
    graph.mark_extern_func(func).unwrap();

    let printed = format!("{graph}");
    let expected = indoc! {"
        let obj = class_name.field_name.subfield;
        let _6 = obj.list._items[obj.active_index.prim_cast::<usize>()];
        pub fn main() { _6 }"
    };

    println!("-------------- Expected --------------\n{expected}");
    println!("-------------- Actual   --------------\n{printed}");

    assert_eq!(printed, expected);
}

#[test]
fn print_named_intermediate_object() {
    // An intermediate expression may be given an explicit name.  Even
    // though the compact view would usually display these as inline
    // expressions, named intermediates should always be displayed.

    let mut graph = SymbolicGraph::new();
    let obj = graph.static_field("class_name", "field_name");
    let subfield = graph.access_field(obj, "subfield");
    graph.name(subfield, "obj").unwrap();
    let active_index = graph.access_field(subfield, "active_index");
    graph.name(active_index, "index").unwrap();
    let array = graph.access_field(subfield, "list._items");
    let item = graph.access_index(array, active_index);
    let func = graph.function_def(vec![], item);
    graph.name(func, "main").unwrap();
    graph.mark_extern_func(func).unwrap();

    let printed = format!("{graph}");
    let expected = indoc! {"
       let obj = class_name.field_name.subfield;
       let index = obj.active_index;
       let _6 = obj.list._items[index.prim_cast::<usize>()];
       pub fn main() { _6 }"
    };

    println!("-------------- Expected --------------\n{expected}");
    println!("-------------- Actual   --------------\n{printed}");

    assert_eq!(printed, expected);
}

#[test]
fn print_with_duplicate_names() {
    // Names are intended for ease of readability, and are not
    // required to be unique.  When printing, any non-unique names are
    // prefixed to ensure uniqueness.
    //
    // TODO: Strip out the prefix when parsing, so that the name
    //
    // TODO: Raise an error if the name provided would conflict with
    //       an reserved name.  (Reserving all names that start with
    //       an underscore, then a number.)

    let mut graph = SymbolicGraph::new();
    let obj = graph.static_field("class_name", "field_name");
    let subfield = graph.access_field(obj, "subfield");
    graph.name(subfield, "obj").unwrap();
    let active_index = graph.access_field(subfield, "active_index");
    graph.name(active_index, "obj").unwrap();
    let array = graph.access_field(subfield, "list._items");
    let item = graph.access_index(array, active_index);
    let func = graph.function_def(vec![], item);
    graph.name(func, "main").unwrap();
    graph.mark_extern_func(func).unwrap();

    let printed = format!("{graph}");
    let expected = indoc! {"
        let _1_obj = class_name.field_name.subfield;
        let _2_obj = _1_obj.active_index;
        let _6 = _1_obj.list._items[_2_obj.prim_cast::<usize>()];
        pub fn main() { _6 }"
    };

    println!("-------------- Expected --------------\n{expected}");
    println!("-------------- Actual   --------------\n{printed}");

    assert_eq!(printed, expected);
}

#[test]
fn print_nullary_function_definition() {
    let mut graph = SymbolicGraph::new();
    let obj = graph.static_field("class_name", "field_name");
    let subfield = graph.access_field(obj, "subfield");
    let active_index = graph.access_field(subfield, "active_index");
    let array = graph.access_field(subfield, "list._items");
    let item = graph.access_index(array, active_index);

    let func = graph.function_def(vec![], item);
    graph.name(func, "main").unwrap();
    graph.mark_extern_func(func).unwrap();

    let printed = format!("{graph}");
    let expected = indoc! {"
        let _1 = class_name.field_name.subfield;
        let _6 = _1.list._items[_1.active_index.prim_cast::<usize>()];
        pub fn main() { _6 }"
    };

    println!("-------------- Expected --------------\n{expected}");
    println!("-------------- Actual   --------------\n{printed}");

    assert_eq!(printed, expected);
}

#[test]
fn print_unary_function_definition() {
    let mut graph = SymbolicGraph::new();

    let index = graph.function_arg(RuntimePrimType::NativeUInt);
    graph.name(index, "index").unwrap();

    let obj = graph.static_field("class_name", "field_name");
    let subfield = graph.access_field(obj, "subfield");
    let array = graph.access_field(subfield, "list._items");
    let item = graph.access_index(array, index);

    let func = graph.function_def(vec![index], item);
    graph.name(func, "main").unwrap();

    graph.mark_extern_func(func).unwrap();

    let printed = format!("{graph}");
    let expected = indoc! {"
        let _4 = class_name.field_name.subfield.list._items;
        pub fn main(index: usize) { _4[index.prim_cast::<usize>()] }"
    };

    println!("-------------- Expected --------------\n{expected}");
    println!("-------------- Actual   --------------\n{printed}");

    assert_eq!(printed, expected);
}
