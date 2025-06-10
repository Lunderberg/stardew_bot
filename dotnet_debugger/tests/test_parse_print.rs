use dotnet_debugger::{
    Error, RuntimePrimType, RuntimeType, SymbolicGraph, SymbolicType,
    SymbolicValue,
};
use indoc::indoc;
use paste::paste;

trait NormalizeReturn {
    fn normalize_return(self) -> Result<Option<SymbolicValue>, Error>;
}
impl NormalizeReturn for () {
    fn normalize_return(self) -> Result<Option<SymbolicValue>, Error> {
        Ok(None)
    }
}
impl NormalizeReturn for SymbolicValue {
    fn normalize_return(self) -> Result<Option<SymbolicValue>, Error> {
        Ok(Some(self))
    }
}
impl NormalizeReturn for Result<SymbolicValue, Error> {
    fn normalize_return(self) -> Result<Option<SymbolicValue>, Error> {
        self.map(Some)
    }
}
impl NormalizeReturn for Result<(), Error> {
    fn normalize_return(self) -> Result<Option<SymbolicValue>, Error> {
        self.map(|_| None)
    }
}
impl NormalizeReturn for Result<Option<SymbolicValue>, Error> {
    fn normalize_return(self) -> Result<Option<SymbolicValue>, Error> {
        self
    }
}

fn compare_parsed_graph<BuilderFunc, BuilderRes>(
    string: &str,
    graph_builder: BuilderFunc,
) -> Result<(), Error>
where
    BuilderFunc: Fn(&mut SymbolicGraph) -> BuilderRes,
    BuilderRes: NormalizeReturn,
{
    let parsed = {
        let mut graph = SymbolicGraph::new();
        graph.parse(string)?;
        graph
    };

    let expected = {
        let mut graph = SymbolicGraph::new();
        graph_builder(&mut graph).normalize_return()?;
        graph
    };

    println!(
        "----------- Parsed -------------\n{}",
        parsed
            .printer()
            .expand_all_expressions()
            .number_all_expressions()
    );
    println!(
        "----------- Expected -------------\n{}",
        expected
            .printer()
            .expand_all_expressions()
            .number_all_expressions()
    );

    assert!(parsed
        .graph_comparison(&expected)
        .compare_names(true)
        .apply());

    Ok(())
}

fn compare_printed_graph<BuilderFunc, BuilderRes>(
    string: &str,
    graph_builder: BuilderFunc,
) -> Result<(), Error>
where
    BuilderFunc: Fn(&mut SymbolicGraph) -> BuilderRes,
    BuilderRes: NormalizeReturn,
{
    let mut graph = SymbolicGraph::new();
    let opt_root_expr = graph_builder(&mut graph).normalize_return()?;

    let printer = if let Some(root_expr) = opt_root_expr {
        graph.print(root_expr)
    } else {
        graph.printer()
    };
    let printed = format!("{printer}");

    let expected = string.trim_ascii_start().trim_ascii_end();
    println!("-------------- Expected --------------\n{expected}");
    println!("-------------- Actual   --------------\n{printed}");

    assert_eq!(printed, expected);

    Ok(())
}

macro_rules! test_parse {
    ($name:ident, $string:expr, $builder:expr $(,)?) => {
        paste! {
            #[test]
            fn [< parse_ $name >]() -> Result<(), Error> {
                compare_parsed_graph($string, $builder)
            }
        }
    };
}

macro_rules! test_print {
    ($name:ident, $string:expr, $builder:expr $(,)?) => {
        paste! {
            #[test]
            fn [< print_ $name >]() -> Result<(), Error> {
                compare_printed_graph($string, $builder)
            }
        }
    };
}

macro_rules! test_print_and_parse {
    ($name:ident, $string:expr, $builder:expr $(,)?) => {
        test_parse! {$name, $string, $builder}
        test_print! {$name, $string, $builder}
    };
}

test_print_and_parse! {
    static_field,
    indoc!{"
        class_name.field_name
    "},
    |graph| {
        graph.static_field("class_name", "field_name")
    },
}

test_print_and_parse! {
    compact_graph,
    indoc!{"
        let _1 = class_name.field_name.subfield;
        let _5 = _1.list._items[_1.active_index];
        pub fn main() { _5 }
    "},
    |graph| {
        let obj = graph.static_field("class_name", "field_name");
        let subfield = graph.access_field(obj, "subfield");
        let array = graph.access_field(subfield, "list._items");
        let active_index = graph.access_field(subfield, "active_index");
        let item = graph.access_index(array, active_index);
        let func = graph.function_def(vec![], item);
        graph.name(func, "main")?;
        graph.mark_extern_func(func)?;
        Ok(())
    },
}

test_print_and_parse! {
    downcast,
    indoc!{"
        class_name.field_name.as::<namespace.class_name2>()
    "},
    |graph| {
        let obj = graph.static_field("class_name", "field_name");
        graph.downcast(obj, "namespace.class_name2")
    },
}

test_print_and_parse! {
    downcast_with_backtick,
    indoc!{"
        class_name.field_name.as::<\"namespace.class_name2`1\">()
    "},
    |graph| {
        let obj = graph.static_field("class_name", "field_name");
        graph.downcast(obj, "namespace.class_name2`1")
    },
}

test_print_and_parse! {
    downcast_with_type_args,
    "class_name.field_name.as::<other_class<arg1, arg2>>()",
    |graph| {
        let obj = graph.static_field("class_name", "field_name");
        graph.downcast(
            obj,
            SymbolicType {
                full_name: "other_class".into(),
                generics: vec!["arg1".into(), "arg2".into()],
            },
        )
    },
}

test_print_and_parse! {
    field_access,
    "class_name.static_field.a.b.c",
    // Technically, it's ambiguous whether this is a class named
    // "class_name", with static field "static_field", and subfields
    // "a.b.c", or a class named "b" inside namespace
    // "class_name.static_field.a" with static field "c".  To keep the
    // parsing independent of the types that exist in the remote
    // process, this ambiguity is resolved later.
    |graph| {
        let obj = graph.static_field("class_name", "static_field");
        let a = graph.access_field(obj, "a");
        let b = graph.access_field(a, "b");
        graph.access_field(b, "c")
    },
}

test_print_and_parse! {
    static_index_access,
    "class_name.static_field[123]",
    |graph| {
        let obj = graph.static_field("class_name", "static_field");
        graph.access_index(obj, 123usize)
    },
}

test_print_and_parse! {
    dynamic_index_access,
    "class_name.static_field[class2.field2]",
    |graph| {
        let obj = graph.static_field("class_name", "static_field");
        let index = graph.static_field("class2", "field2");
        graph.access_index(obj, index)
    },
}

test_print_and_parse! {
    multi_dim_array_access,
    "class_name.static_field[123, 456, 789]",
    |graph| {
        let obj = graph.static_field("class_name", "static_field");
        graph.access_indices(obj, [123usize, 456usize, 789usize])
    },
}

test_print_and_parse! {
    num_array_elements,
    "class_name.static_field.len()",
    |graph| {
        let obj = graph.static_field("class_name", "static_field");
        graph.num_array_elements(obj)
    },
}

test_print_and_parse! {
    array_extent_static_dim,
    "class_name.static_field.extent(0)",
    |graph| {
        let obj = graph.static_field("class_name", "static_field");
        graph.array_extent(obj, 0usize)
    },
}

test_print_and_parse! {
    array_extent_dynamic_dim,
    "class_name.static_field.extent(class2.field2)",
    |graph| {
        let obj = graph.static_field("class_name", "static_field");
        let dim = graph.static_field("class2", "field2");
        graph.array_extent(obj, dim)
    },
}

test_print_and_parse! {
    array_prim_cast,
    "class_name.static_field.prim_cast::<Ptr>()",
    |graph| {
        let obj = graph.static_field("class_name", "static_field");
        graph.prim_cast(obj, RuntimePrimType::Ptr)
    },
}

test_print_and_parse! {
    shared_expression,

    // If an expression is shared, it will be printed as a variable
    // binding, followed by usage of that binding.  These anonymous
    // variables consist of an underscore followed by an instruction
    // index.  This pattern should be recognized, and should not
    // produce named nodes in the parsed graph.
    indoc!{"
         let _0 = class_name.field_name;
         _0.items[_0.active_slot]
    "},

    |graph| {
        let inventory = graph.static_field("class_name", "field_name");
        let items = graph.access_field(inventory, "items");
        let index = graph.access_field(inventory, "active_slot");
        graph.access_index(items, index)
    },
}

test_print_and_parse! {
    named_expression,
    // An expression may have an explicit name.  When parsing a
    // variable name, any name that is not an anonymous variable
    // should result in a named node in the parsed graph.
    indoc!{"
        let _0 = class_name.field_name;\n\
        let index = _0.active_slot;\n\
        _0.items[index]\
    "},
    |graph| {
        let inventory = graph.static_field("class_name", "field_name");
        let index = graph.access_field(inventory, "active_slot");
        graph.name(index, "index").unwrap();
        let items = graph.access_field(inventory, "items");
        graph.access_index(items, index)
    },
}

test_print_and_parse! {
    named_expressions_with_same_name,
    // Variable names are for human readability, and may be repeated
    // within an expression.  To avoid ambiguity when printing, these
    // duplicate variable names are prefixed with `_{i}_`.  When
    // parsing, this prefix should be stripped from the variable name.
    indoc!{"
        let _0_obj = class_name.static_field;
        let _1_obj = _0_obj.instance_field0;
        let _2_obj = _1_obj.instance_field1;
        _2_obj.instance_field2
    "},
    |graph| {
        let obj = graph.static_field("class_name", "static_field");
        graph.name(obj, "obj").unwrap();
        let obj = graph.access_field(obj, "instance_field0");
        graph.name(obj, "obj").unwrap();
        let obj = graph.access_field(obj, "instance_field1");
        graph.name(obj, "obj").unwrap();
        graph.access_field(obj, "instance_field2")
    },
}

// TODO: Provide a way to parse pointer casts, and to generate the
// .NET runtime pointers that occur in pointer casts.
//
// test_print_and_parse! {
//     array_ptr_cast,
//     "class_name\
//      .static_field\
//      .prim_cast::<Ptr>()\
//      .ptr_cast::<class2>()",
//     |graph| {
//         let obj = graph.static_field("class_name", "static_field");
//         let ptr = graph.prim_cast(obj, RuntimePrimType::Ptr);
//         graph.pointer_cast(ptr, "class2")
//     },
// }

test_print_and_parse! {
    top_level_function,
    indoc!{"
        pub fn main() { 42 }
    "},
    |graph| {
        let func = graph.function_def(vec![], 42usize.into());
        graph.name(func, "main")?;
        graph.mark_extern_func(func)?;
        Ok(())
    },
}

test_print_and_parse! {
    anonymous_function,
    "|arg: usize| { arg*2 }",
    |graph| {
        let arg = graph.function_arg(RuntimePrimType::NativeUInt);
        graph.name(arg, "arg").unwrap();

        let product = graph.mul(arg, 2usize);

        graph.function_def(vec![arg], product)
    },
}

test_parse! {
    braceless_anonymous_function,
    "let _0 = |arg: usize| arg*2;",
    |graph| {
        let arg = graph.function_arg(RuntimePrimType::NativeUInt);
        graph.name(arg, "arg").unwrap();

        let product = graph.mul(arg, 2usize);

        graph.function_def(vec![arg], product)
    },
}

test_parse! {
    nullary_anonymous_function_with_space,
    // With a space between the start and end of the argument list,
    // the two '|' characters are tokenized as two separate
    // TokenKind::Pipe tokens.
    "let _0 = | | 42*2;",
    |graph| {
        let product = graph.mul(42usize, 2usize);
        graph.function_def(vec![], product);
    },
}

test_parse! {
    nullary_anonymous_function_without_space,
    // Without a space between the start and end of the argument list,
    // the two '|' characters are tokenized as a single
    // `TokenKind::DoublePipe` token.  In this context, when the
    // `DoublePipe` appears at the start of an expression, it should
    // be parsed as an anonymous function equivalent to two
    // consecutive `Pipe` tokens with a space between.  Whenever
    // boolean operators are supported, though, a
    // `TokenKind::DoublePipe` could be parsed as a boolean OR
    // operation, whereas the two consecutive `Pipe` characters could
    // not.
    "let _0 = || 42*2;",
    |graph| {
        let product = graph.mul(42usize, 2usize);
        graph.function_def(vec![], product)
    },
}

test_print_and_parse! {
    unary_function_definition,
    indoc! {"
        let _4 = class_name.field_name.subfield.list._items;
        pub fn main(index: usize) { _4[index] }
    "},
    |graph| {
        let index = graph.function_arg(RuntimePrimType::NativeUInt);
        graph.name(index, "index").unwrap();

        let obj = graph.static_field("class_name", "field_name");
        let subfield = graph.access_field(obj, "subfield");
        let array = graph.access_field(subfield, "list._items");
        let item = graph.access_index(array, index);

        let func = graph.function_def(vec![index], item);
        graph.name(func, "main").unwrap();

        graph.mark_extern_func(func).unwrap();
    },
}

test_print_and_parse! {
    function_with_arg,
    indoc!{"
        let arr = class_name.static_field;
        pub fn main(index: usize) { arr[index] }
    "},
    |graph| {
            let arr = graph.static_field("class_name", "static_field");
            graph.name(arr, "arr").unwrap();
            let index = graph.function_arg(RuntimePrimType::NativeUInt);
            graph.name(index, "index").unwrap();
            let item = graph.access_index(arr, index);
            let func = graph.function_def(vec![index], item);
            graph.name(func, "main").unwrap();
            graph.mark_extern_func(func).unwrap();
        },
}

test_print_and_parse! {
    function_with_multiple_returns,

    indoc!{"
        let arr1 = class_name.static_field1;
        let arr2 = class_name.static_field2;
        pub fn main(index: usize) { (arr1[index], arr2[index]) }
    "},
    |graph| {
        let arr1 = graph.static_field("class_name", "static_field1");
        graph.name(arr1, "arr1").unwrap();
        let arr2 = graph.static_field("class_name", "static_field2");
        graph.name(arr2, "arr2").unwrap();
        let index = graph.function_arg(RuntimePrimType::NativeUInt);
        graph.name(index, "index").unwrap();
        let item1 = graph.access_index(arr1, index);
        let item2 = graph.access_index(arr2, index);

        let tuple = graph.tuple(vec![item1, item2]);
        let func = graph.function_def(vec![index], tuple);
        graph.name(func, "main").unwrap();
        graph.mark_extern_func(func).unwrap();
    },
}

test_parse! {
    function_that_shadows_variable,

    indoc! {"
        let x = 1+1;
        fn func(y: usize) {
            let x = 100+100;
            x + y
        }
        func(x)
    "},
    |graph| {
        let x_outer = graph.add(1, 1);
        graph.name(x_outer, "x").unwrap();
        let y = graph.function_arg(RuntimePrimType::NativeUInt);
        graph.name(y, "y").unwrap();
        let x_inner = graph.add(100usize, 100usize);
        graph.name(x_inner, "x").unwrap();
        let sum = graph.add(x_inner, y);
        let func = graph.function_def(vec![y], sum);
        graph.name(func, "func").unwrap();

        graph.function_call(func, vec![x_outer]);
    },
}

test_parse! {
    function_with_param_that_shadows_variable,

    indoc! {"
        let x = 1+1;
        fn func(x: usize) {
            x + 1000
        }
        func(x)
    "},
    |graph| {
        let x_outer = graph.add(1usize, 1usize);
        graph.name(x_outer, "x").unwrap();
        let x_param = graph.function_arg(RuntimePrimType::NativeUInt);
        graph.name(x_param, "x").unwrap();
        let sum = graph.add(x_param, 1000usize);
        let func = graph.function_def(vec![x_param], sum);
        graph.name(func, "func").unwrap();

        graph.function_call(func, vec![x_outer]);
    },
}

test_print_and_parse! {
    addition,
    "pub fn main(a: usize, b: usize) { a + b }",
    |graph| {
        let a = graph.function_arg(RuntimePrimType::NativeUInt);
        graph.name(a, "a").unwrap();
        let b = graph.function_arg(RuntimePrimType::NativeUInt);
        graph.name(b, "b").unwrap();
        let sum = graph.add(a, b);
        let func = graph.function_def(vec![a, b], sum);
        graph.name(func, "main").unwrap();
        graph.mark_extern_func(func).unwrap();
    },
}

test_print_and_parse! {
    positive_i32_literals,
    indoc!{"
        let res_main = 100i32 + 50i32;
        pub fn main() { res_main }
    "},
    |graph| -> Result<_,Error> {
        let res_main = graph.add(100i32, 50i32);
        graph.name(res_main, "res_main")?;

        let main = graph.function_def(vec![], res_main);
        graph.name(main, "main")?;
        graph.mark_extern_func(main)?;
        Ok(())
    },
}

test_print_and_parse! {
    negative_i32_literals,
    indoc!{"
        let res_main = -100i32 + -50i32;
        pub fn main() { res_main }
    "},
    |graph| -> Result<_,Error> {
        let res_main = graph.add(-100i32, -50i32);
        graph.name(res_main, "res_main")?;

        let main = graph.function_def(vec![], res_main);
        graph.name(main, "main")?;
        graph.mark_extern_func(main)?;
        Ok(())
    },
}

test_print_and_parse! {
    subtraction,
    "pub fn main(a: usize, b: usize) { a - b }",
    |graph| {
        let a = graph.function_arg(RuntimePrimType::NativeUInt);
        graph.name(a, "a").unwrap();
        let b = graph.function_arg(RuntimePrimType::NativeUInt);
        graph.name(b, "b").unwrap();
        let sum = graph.sub(a, b);
        let func = graph.function_def(vec![a, b], sum);
        graph.name(func, "main").unwrap();
        graph.mark_extern_func(func).unwrap();
    },
}

test_print_and_parse! {
    multiplication,
    "pub fn main(a: usize, b: usize) { a*b }",
    |graph| {
        let a = graph.function_arg(RuntimePrimType::NativeUInt);
        graph.name(a, "a").unwrap();
        let b = graph.function_arg(RuntimePrimType::NativeUInt);
        graph.name(b, "b").unwrap();
        let prod = graph.mul(a, b);
        let func = graph.function_def(vec![a, b], prod);
        graph.name(func, "main").unwrap();
        graph.mark_extern_func(func).unwrap();
    },
}

test_print_and_parse! {
    repeated_addition,
    "pub fn main(a: usize, b: usize) { a + b + a + b }",
    |graph| {
        let a = graph.function_arg(RuntimePrimType::NativeUInt);
        graph.name(a, "a").unwrap();
        let b = graph.function_arg(RuntimePrimType::NativeUInt);
        graph.name(b, "b").unwrap();

        let sum = graph.add(a, b);
        let sum = graph.add(sum, a);
        let sum = graph.add(sum, b);

        let func = graph.function_def(vec![a, b], sum);
        graph.name(func, "main").unwrap();
        graph.mark_extern_func(func).unwrap();
    },
}

test_print_and_parse! {
    mixed_addition_and_subtraction,
    "pub fn main(a: usize, b: usize) { a - b + a - b }",
    |graph| {
        let a = graph.function_arg(RuntimePrimType::NativeUInt);
        graph.name(a, "a").unwrap();
        let b = graph.function_arg(RuntimePrimType::NativeUInt);
        graph.name(b, "b").unwrap();

        let sum = graph.sub(a, b);
        let sum = graph.add(sum, a);
        let sum = graph.sub(sum, b);

        let func = graph.function_def(vec![a, b], sum);
        graph.name(func, "main").unwrap();
        graph.mark_extern_func(func).unwrap();
    },
}

test_print_and_parse! {
    repeated_multiplication,
    "pub fn main(a: usize, b: usize) { a*b*a*b }",
    |graph| {
        let a = graph.function_arg(RuntimePrimType::NativeUInt);
        graph.name(a, "a").unwrap();
        let b = graph.function_arg(RuntimePrimType::NativeUInt);
        graph.name(b, "b").unwrap();

        let prod = graph.mul(a, b);
        let prod = graph.mul(prod, a);
        let prod = graph.mul(prod, b);

        let func = graph.function_def(vec![a, b], prod);
        graph.name(func, "main").unwrap();
        graph.mark_extern_func(func).unwrap();
    },
}

test_print_and_parse! {
    mixed_operators_with_precedence,
    "pub fn main(a: usize, b: usize) { a*b + b*a }",
    |graph| {
        let a = graph.function_arg(RuntimePrimType::NativeUInt);
        graph.name(a, "a").unwrap();
        let b = graph.function_arg(RuntimePrimType::NativeUInt);
        graph.name(b, "b").unwrap();

        let lhs = graph.mul(a, b);
        let rhs = graph.mul(b, a);
        let sum = graph.add(lhs, rhs);

        let func = graph.function_def(vec![a, b], sum);
        graph.name(func, "main").unwrap();
        graph.mark_extern_func(func).unwrap();
    },
}

test_print_and_parse! {
    mixed_operators_with_parentheses,
    "pub fn main(a: usize, b: usize) { a + (a + b)*(b + a) }",
    |graph| {
        let a = graph.function_arg(RuntimePrimType::NativeUInt);
        graph.name(a, "a").unwrap();
        let b = graph.function_arg(RuntimePrimType::NativeUInt);
        graph.name(b, "b").unwrap();

        let lhs = graph.add(a, b);
        let rhs = graph.add(b, a);
        let prod = graph.mul(lhs, rhs);
        let sum = graph.add(a, prod);

        let func = graph.function_def(vec![a, b], sum);
        graph.name(func, "main").unwrap();
        graph.mark_extern_func(func).unwrap();
    },
}

test_print_and_parse! {
    mixed_operators_with_leading_parentheses,
    "pub fn main(a: usize, b: usize) { (a + b)*(b + a) }",
    |graph| {
        let a = graph.function_arg(RuntimePrimType::NativeUInt);
        graph.name(a, "a").unwrap();
        let b = graph.function_arg(RuntimePrimType::NativeUInt);
        graph.name(b, "b").unwrap();

        let lhs = graph.add(a, b);
        let rhs = graph.add(b, a);
        let prod = graph.mul(lhs, rhs);

        let func = graph.function_def(vec![a, b], prod);
        graph.name(func, "main").unwrap();
        graph.mark_extern_func(func).unwrap();
    },
}

test_print_and_parse! {
    function_call,
    indoc!{"
        fn multiply(lhs: usize, rhs: usize) { lhs*rhs }
        pub fn main(a: usize, b: usize) { multiply(a + b, b + a) }
    "},
    |graph| {
        let lhs = graph.function_arg(RuntimePrimType::NativeUInt);
        graph.name(lhs, "lhs").unwrap();
        let rhs = graph.function_arg(RuntimePrimType::NativeUInt);
        graph.name(rhs, "rhs").unwrap();
        let prod = graph.mul(lhs, rhs);
        let multiply = graph.function_def(vec![lhs, rhs], prod);
        graph.name(multiply, "multiply").unwrap();

        let a = graph.function_arg(RuntimePrimType::NativeUInt);
        graph.name(a, "a").unwrap();
        let b = graph.function_arg(RuntimePrimType::NativeUInt);
        graph.name(b, "b").unwrap();

        let lhs = graph.add(a, b);
        let rhs = graph.add(b, a);
        let call = graph.function_call(multiply, vec![lhs, rhs]);

        let main = graph.function_def(vec![a, b], call);
        graph.name(main, "main").unwrap();
        graph.mark_extern_func(main).unwrap();
    },
}

test_print_and_parse! {
    fragment_using_global_variable,
    // Named global variable may be used within a graph.parse() call.
    indoc!{ "
        let var = 1 + 2;
        let res_main = var + 3;
        pub fn main() { res_main }
    "},
    |graph| {
        let var = graph.add(1usize, 2usize);
        graph.name(var, "var").unwrap();

        graph.parse(stringify!{
            pub fn main() {
                let res_main = var + 3;
                res_main
            }
        }).unwrap();
    },
}

test_print_and_parse! {
    static_range,
    "(0..42)",
    |graph| {
        graph.range(42usize)
    },
}

test_print_and_parse! {
    dynamic_range,
    "(0..5 + 10)",
    |graph| {
        let extent = graph.add(5usize, 10usize);
        graph.range(extent)
    },
}

test_print_and_parse! {
    sum_of_integers,
    indoc!{"
        (0..42).reduce(0, |a: usize, b: usize| { a + b })
    "},
    |graph| {
        let iter = graph.range(42usize);

        let a = graph.function_arg(RuntimePrimType::NativeUInt);
        graph.name(a, "a").unwrap();
        let b = graph.function_arg(RuntimePrimType::NativeUInt);
        graph.name(b, "b").unwrap();

        let sum = graph.add(a, b);
        let func = graph.function_def(vec![a, b], sum);

        graph.reduce(0usize, iter, func)
    },
}

test_print_and_parse! {
    nested_function,
    indoc!{"
        fn func_outer(arg_outer: usize) {
            fn func_inner(arg_inner: usize) {
                let sum = arg_outer + arg_inner;
                let res_inner = sum*10;
                res_inner
            }
            let res_outer = func_inner(1000);
            res_outer
        }
        let res_main = func_outer(42);
        pub fn main() { res_main }
    "},
    |graph| {
        let arg_outer = graph.function_arg(RuntimePrimType::NativeUInt);
        graph.name(arg_outer, "arg_outer").unwrap();

        let arg_inner = graph.function_arg(RuntimePrimType::NativeUInt);
        graph.name(arg_inner, "arg_inner").unwrap();
        let sum = graph.add(arg_outer, arg_inner);
        graph.name(sum, "sum").unwrap();

        let res_inner = graph.mul(sum, 10usize);
        graph.name(res_inner, "res_inner").unwrap();
        let func_inner = graph.function_def(vec![arg_inner], res_inner);
        graph.name(func_inner, "func_inner").unwrap();

        let res_outer = graph.function_call(func_inner, vec![1000usize.into()]);
        graph.name(res_outer, "res_outer").unwrap();
        let func_outer = graph.function_def(vec![arg_outer], res_outer);
        graph.name(func_outer, "func_outer").unwrap();

        let res_main = graph.function_call(func_outer, vec![42usize.into()]);
        graph.name(res_main, "res_main").unwrap();
        let func_main = graph.function_def(vec![], res_main);
        graph.name(func_main, "main").unwrap();
        graph.mark_extern_func(func_main).unwrap();
    },
}

test_print_and_parse! {
    reduce_with_named_reduction,
    indoc!{"
        fn reduction(a: usize, b: usize) { a + b }
        let res_main = (0..10).reduce(0, reduction);
        pub fn main() { res_main }
    "},
    |graph| {
        let a = graph.function_arg(RuntimePrimType::NativeUInt);
        graph.name(a, "a").unwrap();
        let b = graph.function_arg(RuntimePrimType::NativeUInt);
        graph.name(b, "b").unwrap();

        let sum = graph.add(a, b);
        let reduction = graph.function_def(vec![a, b], sum);
        graph.name(reduction, "reduction").unwrap();

        let iterator = graph.range(10usize);
        let res_main = graph.reduce(0usize, iterator, reduction);
        graph.name(res_main, "res_main").unwrap();

        let func_main = graph.function_def(vec![], res_main);
        graph.name(func_main, "main").unwrap();
        graph.mark_extern_func(func_main).unwrap();
    },
}

test_print_and_parse! {
    reduce_with_anonymous_reduction,
    indoc!{"
        let res_main = (0..10).reduce(0, |a: usize, b: usize| { a + b });
        pub fn main() { res_main }
    "},
    |graph| {
        let iterator = graph.range(10usize);

        let a = graph.function_arg(RuntimePrimType::NativeUInt);
        graph.name(a, "a").unwrap();
        let b = graph.function_arg(RuntimePrimType::NativeUInt);
        graph.name(b, "b").unwrap();

        let sum = graph.add(a, b);
        let reduction = graph.function_def(vec![a, b], sum);

        let res_main = graph.reduce(0usize, iterator, reduction);
        graph.name(res_main, "res_main").unwrap();

        let func_main = graph.function_def(vec![], res_main);
        graph.name(func_main, "main").unwrap();
        graph.mark_extern_func(func_main).unwrap();
    },
}

test_print_and_parse! {
    reduce_with_named_reduction_including_named_subexpressions,
    indoc! {"
        fn reduction(a: usize, b: usize) {
            let prod = a*b;
            prod + b
        }
        let res_main = (0..10).reduce(0, reduction);
        pub fn main() { res_main }
    "},
    |graph| {
        let a = graph.function_arg(RuntimePrimType::NativeUInt);
        graph.name(a, "a").unwrap();
        let b = graph.function_arg(RuntimePrimType::NativeUInt);
        graph.name(b, "b").unwrap();

        let prod = graph.mul(a, b);
        graph.name(prod, "prod").unwrap();
        let sum = graph.add(prod, b);
        let reduction = graph.function_def(vec![a, b], sum);
        graph.name(reduction, "reduction").unwrap();

        let iterator = graph.range(10usize);
        let res_main = graph.reduce(0usize, iterator, reduction);
        graph.name(res_main, "res_main").unwrap();

        let func_main = graph.function_def(vec![], res_main);
        graph.name(func_main, "main").unwrap();
        graph.mark_extern_func(func_main).unwrap();
    },
}

test_print_and_parse! {
    reduce_with_anonymous_reduction_including_named_subexpressions,
    indoc! {"
        let res_main = (0..10).reduce(0, |a: usize, b: usize| {
            let prod = a*b;
            prod + b
        });
        pub fn main() { res_main }
    "},
    |graph| {
        let a = graph.function_arg(RuntimePrimType::NativeUInt);
        graph.name(a, "a").unwrap();
        let b = graph.function_arg(RuntimePrimType::NativeUInt);
        graph.name(b, "b").unwrap();

        let prod = graph.mul(a, b);
        graph.name(prod, "prod").unwrap();
        let sum = graph.add(prod, b);
        let reduction = graph.function_def(vec![a, b], sum);

        let iterator = graph.range(10usize);
        let res_main = graph.reduce(0usize, iterator, reduction);
        graph.name(res_main, "res_main").unwrap();

        let func_main = graph.function_def(vec![], res_main);
        graph.name(func_main, "main").unwrap();
        graph.mark_extern_func(func_main).unwrap();
    },
}

test_print_and_parse! {
    map,
    indoc! {"
        let res_main = (0..10)\
            .map(|i: usize| { i*i })\
            .reduce(0, |a: usize, b: usize| { a + b });
        pub fn main() { res_main }
    "},
    |graph| {
        let iterator = graph.range(10usize);

        let map = {
            let i = graph.function_arg(RuntimePrimType::NativeUInt);
            graph.name(i, "i").unwrap();
            let square = graph.mul(i, i);

            graph.function_def(vec![i], square)
        };
        let mapped = graph.map(iterator, map);

        let reduction = {
            let a = graph.function_arg(RuntimePrimType::NativeUInt);
            graph.name(a, "a").unwrap();
            let b = graph.function_arg(RuntimePrimType::NativeUInt);
            graph.name(b, "b").unwrap();

            let sum = graph.add(a, b);
            graph.function_def(vec![a, b], sum)
        };

        let res_main = graph.reduce(0usize, mapped, reduction);
        graph.name(res_main, "res_main").unwrap();

        let func_main = graph.function_def(vec![], res_main);
        graph.name(func_main, "main").unwrap();
        graph.mark_extern_func(func_main).unwrap();
    },
}

test_print_and_parse! {
    if_else,
    indoc! {"
        let a = if true { 20 } else { 30 };
        let b = if false { 2 } else { 3 };
        let res_main = a + b;
        pub fn main() { res_main }
    "},
    |graph| {
        let a = graph.if_else(true, 20usize, 30usize);
        graph.name(a, "a").unwrap();
        let b = graph.if_else(false, 2usize, 3usize);
        graph.name(b, "b").unwrap();

        let res_main = graph.add(a, b);
        graph.name(res_main, "res_main").unwrap();

        let func_main = graph.function_def(vec![], res_main);
        graph.name(func_main, "main").unwrap();
        graph.mark_extern_func(func_main).unwrap();
    },
}

test_print_and_parse! {
    if_else_if_else,
    indoc! {"
        let res_main = if true { 20 } else if false { 30 } else { 40 };
        pub fn main() { res_main }
    "},
    |graph| {
        let else_if = graph.if_else(false, 30usize, 40usize);
        let res_main = graph.if_else(true, 20usize, else_if);
        graph.name(res_main, "res_main").unwrap();

        let func_main = graph.function_def(vec![], res_main);
        graph.name(func_main, "main").unwrap();
        graph.mark_extern_func(func_main).unwrap();
    },
}

test_print_and_parse! {
    comparisons,

    indoc!{"
        let a = 5 == 10;
        let b = 5 != 10;
        let c = 5 < 10;
        let d = 5 > 10;
        let e = 5 <= 10;
        let f = 5 >= 10;
        let res_main = (a, b, c, d, e, f);
        pub fn main() { res_main }
    "},
    |graph| {
        let a = graph.equal(5usize, 10usize);
        let b = graph.not_equal(5usize, 10usize);
        let c = graph.less_than(5usize, 10usize);
        let d = graph.greater_than(5usize, 10usize);
        let e = graph.less_than_or_equal(5usize, 10usize);
        let f = graph.greater_than_or_equal(5usize, 10usize);

        graph.name(a, "a").unwrap();
        graph.name(b, "b").unwrap();
        graph.name(c, "c").unwrap();
        graph.name(d, "d").unwrap();
        graph.name(e, "e").unwrap();
        graph.name(f, "f").unwrap();

        let res_main = graph.tuple(vec![a, b, c, d, e, f]);
        graph.name(res_main, "res_main").unwrap();

        let func_main = graph.function_def(vec![], res_main);
        graph.name(func_main, "main").unwrap();
        graph.mark_extern_func(func_main).unwrap();
    },
}

test_parse! {
    comments,
    indoc!{"
        pub fn main() {
            5 + 10 // + 15
                + 20
        }
    "},
    |graph| {
        let a = graph.add(5usize, 10usize);
        let res_main = graph.add(a, 20usize);

        let func_main = graph.function_def(vec![], res_main);
        graph.name(func_main, "main").unwrap();
        graph.mark_extern_func(func_main).unwrap();
    },
}

test_print_and_parse! {
    div_mod,

    indoc!{"
        let div = 5/3;
        let modulo = 5%3;
        let validation = div*3 + modulo == 5;
        let res_main = (div, modulo, validation);
        pub fn main() { res_main }
    "},
    |graph| {
        let div = graph.div(5usize, 3usize);
        graph.name(div, "div").unwrap();
        let modulo = graph.modulo(5usize, 3usize);
        graph.name(modulo, "modulo").unwrap();

        let mul = graph.mul(div, 3usize);
        let sum = graph.add(mul, modulo);
        let validation = graph.equal(sum, 5usize);
        graph.name(validation, "validation").unwrap();

        let res_main = graph.tuple(vec![div, modulo, validation]);
        graph.name(res_main, "res_main").unwrap();

        let func_main = graph.function_def(vec![], res_main);
        graph.name(func_main, "main").unwrap();
        graph.mark_extern_func(func_main).unwrap();
    },
}

test_print_and_parse! {
    iterator_filter,

    indoc! {"
        let res_main = (0..100)\
            .filter(|i: usize| { i%2 == 0 })\
            .map(|j: usize| { j*j })\
            .reduce(0, |a: usize, b: usize| { a + b });
        pub fn main() { res_main }
    "},
    |graph| {
        let iter = graph.range(100usize);

        let filter = {
            let i = graph.function_arg(RuntimePrimType::NativeUInt);
            graph.name(i, "i").unwrap();

            let rem = graph.modulo(i, 2usize);
            let eq = graph.equal(rem, 0usize);
            graph.function_def(vec![i], eq)
        };
        let iter = graph.filter(iter, filter);

        let map = {
            let j = graph.function_arg(RuntimePrimType::NativeUInt);
            graph.name(j, "j").unwrap();

            let prod = graph.mul(j, j);
            graph.function_def(vec![j], prod)
        };
        let iter = graph.map(iter, map);

        let reduce = {
            let a = graph.function_arg(RuntimePrimType::NativeUInt);
            graph.name(a, "a").unwrap();

            let b = graph.function_arg(RuntimePrimType::NativeUInt);
            graph.name(b, "b").unwrap();

            let sum = graph.add(a, b);
            graph.function_def(vec![a, b], sum)
        };
        let res_main = graph.reduce(0usize, iter, reduce);
        graph.name(res_main, "res_main").unwrap();

        let func_main = graph.function_def(vec![], res_main);
        graph.name(func_main, "main").unwrap();
        graph.mark_extern_func(func_main).unwrap();
    },
}

test_print_and_parse! {
    iterator_collect,

    indoc!{"
        let res_main = (0..100).collect();
        pub fn main() { res_main }
    "},
    |graph| {
        let iter = graph.range(100usize);

        let res_main = graph.collect(iter);
        graph.name(res_main, "res_main").unwrap();

        let func_main = graph.function_def(vec![], res_main);
        graph.name(func_main, "main").unwrap();
        graph.mark_extern_func(func_main).unwrap();
    },
}

test_print_and_parse! {
    iterator_chain,

    indoc!{"
        let iter_doubles = (0..10).map(|i| { i*2 });
        let iter_squares = (0..10).map(|j| { j*j });
        let res_main = iter_doubles.chain(iter_squares).collect();
        pub fn main() { res_main }
    "},
    |graph| -> Result<(), Error> {
        let iter_doubles = {
            let iter = graph.range(10usize);
            let i = graph.function_arg(RuntimeType::Unknown);
            graph.name(i, "i").unwrap();
            let res = graph.mul(i,2usize);
            let map = graph.function_def(vec![i], res);
            let mapped = graph.map(iter, map);
            graph.name(mapped, "iter_doubles")?;
            mapped
        };

        let iter_squares = {
            let iter = graph.range(10usize);
            let j = graph.function_arg(RuntimeType::Unknown);
            graph.name(j, "j").unwrap();
            let res = graph.mul(j,j);
            let map = graph.function_def(vec![j], res);
            let mapped = graph.map(iter, map);
            graph.name(mapped, "iter_squares")?;
            mapped
        };

        let chained = graph.chain(iter_doubles, iter_squares);
        let res_main = graph.collect(chained);
        graph.name(res_main, "res_main").unwrap();

        let func_main = graph.function_def(vec![], res_main);
        graph.name(func_main, "main").unwrap();
        graph.mark_extern_func(func_main).unwrap();

        Ok(())
    },
}

test_print_and_parse! {
    none,

    indoc! {"
        let res_main = if true { 100 } else { None };
        pub fn main() { res_main }
    "},
    |graph| {
        let else_branch = graph.none();
        let res_main = graph.if_else(true, 100usize, else_branch);
        graph.name(res_main, "res_main").unwrap();

        let func_main = graph.function_def(vec![], res_main);
        graph.name(func_main, "main").unwrap();
        graph.mark_extern_func(func_main).unwrap();
    },
}

test_print_and_parse! {
    boolean_or,

    indoc!{"
        pub fn main(a: bool, b: bool) { a || b }
    "},
    |graph| {
        let a = graph.function_arg(RuntimePrimType::Bool);
        graph.name(a, "a").unwrap();
        let b = graph.function_arg(RuntimePrimType::Bool);
        graph.name(b, "b").unwrap();
        let ret = graph.boolean_or(a, b);
        let func = graph.function_def(vec![a, b], ret);
        graph.name(func, "main").unwrap();
        graph.mark_extern_func(func).unwrap();
    },
}

test_print_and_parse! {
    boolean_and,

    indoc! {"
        pub fn main(a: bool, b: bool) { a && b }
    "},
    |graph| {
        let a = graph.function_arg(RuntimePrimType::Bool);
        graph.name(a, "a").unwrap();
        let b = graph.function_arg(RuntimePrimType::Bool);
        graph.name(b, "b").unwrap();
        let ret = graph.boolean_and(a, b);
        let func = graph.function_def(vec![a, b], ret);
        graph.name(func, "main").unwrap();
        graph.mark_extern_func(func).unwrap();
    },
}

test_print_and_parse! {
    boolean_not,

    indoc!{"
        pub fn main(a: bool) { !a }
    "},
    |graph| {
        let a = graph.function_arg(RuntimePrimType::Bool);
        graph.name(a, "a").unwrap();
        let ret = graph.boolean_not(a);
        let func = graph.function_def(vec![a], ret);
        graph.name(func, "main").unwrap();
        graph.mark_extern_func(func).unwrap();
    },
}

test_print_and_parse! {
    repeated_boolean_or,

    "pub fn main(a: bool, b: bool, c: bool) { a || b || c }",
    |graph| {
        let a = graph.function_arg(RuntimePrimType::Bool);
        graph.name(a, "a").unwrap();
        let b = graph.function_arg(RuntimePrimType::Bool);
        graph.name(b, "b").unwrap();
        let c = graph.function_arg(RuntimePrimType::Bool);
        graph.name(c, "c").unwrap();
        let ret = graph.boolean_or(a, b);
        let ret = graph.boolean_or(ret, c);
        let func = graph.function_def(vec![a, b, c], ret);
        graph.name(func, "main").unwrap();
        graph.mark_extern_func(func).unwrap();
    },
}

test_print_and_parse! {
    repeated_boolean_and,

    "pub fn main(a: bool, b: bool, c: bool) { a && b && c }",
    |graph| {
        let a = graph.function_arg(RuntimePrimType::Bool);
        graph.name(a, "a").unwrap();
        let b = graph.function_arg(RuntimePrimType::Bool);
        graph.name(b, "b").unwrap();
        let c = graph.function_arg(RuntimePrimType::Bool);
        graph.name(c, "c").unwrap();
        let ret = graph.boolean_and(a, b);
        let ret = graph.boolean_and(ret, c);
        let func = graph.function_def(vec![a, b, c], ret);
        graph.name(func, "main").unwrap();
        graph.mark_extern_func(func).unwrap();
    },
}

test_print_and_parse! {
    boolean_or_with_not,
    // Boolean NOT has a higher precedence than boolean OR, so this is
    // parsed as "(!a) || b"
    indoc! {"
        pub fn main(a: bool, b: bool) { !a || b }
    "},
    |graph| {
        let a = graph.function_arg(RuntimePrimType::Bool);
        graph.name(a, "a").unwrap();
        let b = graph.function_arg(RuntimePrimType::Bool);
        graph.name(b, "b").unwrap();
        let not_a = graph.boolean_not(a);
        let ret = graph.boolean_or(not_a, b);
        let func = graph.function_def(vec![a, b], ret);
        graph.name(func, "main").unwrap();
        graph.mark_extern_func(func).unwrap();
    },
}

test_print_and_parse! {
    boolean_and_with_not,
    // Boolean NOT has a higher precedence than boolean AND, so this is
    // parsed as "(!a) && b"
    indoc! {"
        pub fn main(a: bool, b: bool) { !a && b }
    "},
    |graph| {
        let a = graph.function_arg(RuntimePrimType::Bool);
        graph.name(a, "a").unwrap();
        let b = graph.function_arg(RuntimePrimType::Bool);
        graph.name(b, "b").unwrap();
        let not_a = graph.boolean_not(a);
        let ret = graph.boolean_and(not_a, b);
        let func = graph.function_def(vec![a, b], ret);
        graph.name(func, "main").unwrap();
        graph.mark_extern_func(func).unwrap();
    },
}

test_print_and_parse! {
    and_with_or_requires_parentheses,
    // Boolean OR and AND have incomparable precedences.  While OR can
    // be chained together as (a || b || c), and AND can be chained
    // together as (a && b && c), they require parentheses when mixed
    // together.
    "pub fn main(a: bool, b: bool, c: bool) { \
        (a && b) || c \
    }",
    |graph| {

        let a = graph.function_arg(RuntimePrimType::Bool);
        graph.name(a, "a").unwrap();
        let b = graph.function_arg(RuntimePrimType::Bool);
        graph.name(b, "b").unwrap();
        let c = graph.function_arg(RuntimePrimType::Bool);
        graph.name(c, "c").unwrap();

        let partial = graph.boolean_and(a, b);
        let ret = graph.boolean_or(partial, c);

        let func = graph.function_def(vec![a, b, c], ret);
        graph.name(func, "main").unwrap();
        graph.mark_extern_func(func).unwrap();
    },
}

test_print_and_parse! {
    or_with_and_requires_parentheses,
    // Boolean OR and AND have incomparable precedences.  While OR can
    // be chained together as (a || b || c), and AND can be chained
    // together as (a && b && c), they require parentheses when mixed
    // together.
    "pub fn main(a: bool, b: bool, c: bool) { \
        (a || b) && c \
    }",
    |graph| {
        let a = graph.function_arg(RuntimePrimType::Bool);
        graph.name(a, "a").unwrap();
        let b = graph.function_arg(RuntimePrimType::Bool);
        graph.name(b, "b").unwrap();
        let c = graph.function_arg(RuntimePrimType::Bool);
        graph.name(c, "c").unwrap();

        let partial = graph.boolean_or(a, b);
        let ret = graph.boolean_and(partial, c);

        let func = graph.function_def(vec![a, b, c], ret);
        graph.name(func, "main").unwrap();
        graph.mark_extern_func(func).unwrap();
    },
}

test_print_and_parse! {
    read_primitive,
    indoc! {"
    pub fn main(a: Ptr) {
        let b = a.read_prim::<usize>();
        let c = b + 10;
        c
    }"},
    |graph| {
        let a = graph.function_arg(RuntimePrimType::Ptr);
        graph.name(a, "a").unwrap();

        let b = graph.read_value(a, RuntimePrimType::NativeUInt);
        graph.name(b, "b").unwrap();
        let c = graph.add(b, 10usize);
        graph.name(c, "c").unwrap();

        let func = graph.function_def(vec![a], c);
        graph.name(func, "main").unwrap();
        graph.mark_extern_func(func).unwrap();
    },
}

test_print_and_parse! {
    read_bytes,
    indoc! {"
    pub fn main(a: Ptr) {
        let bytes = a.read_bytes(8);
        bytes
    }"},
    |graph| {
        let a = graph.function_arg(RuntimePrimType::Ptr);
        graph.name(a, "a").unwrap();

        let bytes = graph.read_bytes(a, 8usize);
        graph.name(bytes, "bytes").unwrap();

        let func = graph.function_def(vec![a], bytes);
        graph.name(func, "main").unwrap();
        graph.mark_extern_func(func).unwrap();
    },
}

test_print_and_parse! {
    cast_bytes,
    indoc! {"
    pub fn main(a: Ptr) {
        let bytes = a.read_bytes(8);
        let value = bytes.cast_bytes::<usize>(0);
        value
    }"},
    |graph| {
        let a = graph.function_arg(RuntimePrimType::Ptr);
        graph.name(a, "a").unwrap();

        let bytes = graph.read_bytes(a, 8usize);
        graph.name(bytes, "bytes").unwrap();

        let value = graph.cast_bytes(bytes, 0usize, RuntimePrimType::NativeUInt);
        graph.name(value, "value").unwrap();

        let func = graph.function_def(vec![a], value);
        graph.name(func, "main").unwrap();
        graph.mark_extern_func(func).unwrap();
    },
}

test_print_and_parse! {
    read_multiple_remote_regions,
    indoc! {"
    pub fn main(ptr_a: Ptr, ptr_b: Ptr) {
        let bytes = read_bytes(ptr_a, 4, ptr_b, 8);
        let value_a = bytes.cast_bytes::<u32>(0).prim_cast::<usize>();
        let value_b = bytes.cast_bytes::<u64>(4).prim_cast::<usize>();
        let sum = value_a + value_b;
        sum
    }"},
    |graph| {
        let ptr_a = graph.function_arg(RuntimePrimType::Ptr);
        graph.name(ptr_a, "ptr_a").unwrap();
        let ptr_b = graph.function_arg(RuntimePrimType::Ptr);
        graph.name(ptr_b, "ptr_b").unwrap();

        let bytes = graph.read_byte_regions([
            (ptr_a, 4usize),
            (ptr_b, 8usize),
        ]);
        graph.name(bytes, "bytes").unwrap();

        let value_a = graph.cast_bytes(bytes, 0usize, RuntimePrimType::U32);
        let value_a = graph.prim_cast(value_a, RuntimePrimType::NativeUInt);
        graph.name(value_a, "value_a").unwrap();
        let value_b = graph.cast_bytes(bytes, 4usize, RuntimePrimType::U64);
        let value_b = graph.prim_cast(value_b, RuntimePrimType::NativeUInt);
        graph.name(value_b, "value_b").unwrap();

        let sum = graph.add(value_a, value_b);
        graph.name(sum, "sum").unwrap();

        let func = graph.function_def(vec![ptr_a, ptr_b], sum);
        graph.name(func, "main").unwrap();
        graph.mark_extern_func(func).unwrap();
    },
}
