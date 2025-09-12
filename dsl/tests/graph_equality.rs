use paste::paste;

use dsl::{GraphComparisonExt as _, SymbolicGraph};

macro_rules! verify_equivalent_graphs {
    ($( $test_name:ident: $builder:expr ),+ $(,)?) => {
        $(
            #[test]
            fn $test_name() {
                let expr_builder = $builder;
                let func_builder = |graph: &mut SymbolicGraph| {
                    let expr = expr_builder(graph);
                    let func = graph.function_def(vec![], expr);
                    graph.name(func, "main").unwrap();
                    graph.mark_extern_func(func).unwrap();
                };

                let mut lhs = SymbolicGraph::new();
                func_builder(&mut lhs);

                let mut rhs = SymbolicGraph::new();
                func_builder(&mut rhs);

                println!("------------- LHS ---------------\n{lhs}");
                println!("------------- RHS ---------------\n{rhs}");

                assert!(lhs
                        .graph_comparison(&rhs)
                        .apply());
            }

            paste!{
            #[test]
            fn [<$test_name _order_dependent>]() {
                let builder = $builder;

                let mut lhs = SymbolicGraph::new();
                builder(&mut lhs);

                let mut rhs = SymbolicGraph::new();
                builder(&mut rhs);

                assert!(lhs
                        .graph_comparison(&rhs)
                        .order_dependent(true)
                        .apply());
            }
            }
        )*
    };
}

verify_equivalent_graphs! {
    // empty_graphs_are_equivalent: |_graph| {},
    equivalent_static_field: |graph: &mut SymbolicGraph| {

        graph.static_field("class_name", "field_name")
    },
    equivalent_instance_field: |graph: &mut SymbolicGraph| {
        let obj = graph.static_field("class_name", "static_field_name");

        graph.access_field(obj, "instance_field_name")
    },
    equivalent_downcast: |graph: &mut SymbolicGraph| {
        let obj = graph.static_field("class_name", "field_name");

        graph.downcast(obj, "subclass_name")
    },
    equivalent_static_index_access: |graph: &mut SymbolicGraph| {
        let obj = graph.static_field("class_name", "field_name");

        graph.access_index(obj, 0)
    },
    equivalent_dynamic_index_access: |graph: &mut SymbolicGraph| {
        let obj = graph.static_field("class1", "field1");
        let index = graph.static_field("class2", "field2");

        graph.access_index(obj, index)
    },
    equivalent_array_length: |graph: &mut SymbolicGraph| {
        let obj = graph.static_field("class_name", "field_name");

        graph.num_array_elements(obj)
    },
    equivalent_multi_dim_array_extent: |graph: &mut SymbolicGraph| {
        let obj = graph.static_field("class_name", "field_name");

        graph.array_extent(obj, 0)
    },
}

#[test]
fn default_comparison_is_order_independent() {
    let lhs = {
        let mut graph = SymbolicGraph::new();
        let x = graph.static_field("class1", "field1");
        let y = graph.static_field("class2", "field2");
        let sum = graph.add(x, y);
        let func = graph.function_def(vec![], sum);
        graph.name(func, "main").unwrap();
        graph.mark_extern_func(func).unwrap();
        graph
    };

    let rhs = {
        let mut graph = SymbolicGraph::new();
        let y = graph.static_field("class2", "field2");
        let x = graph.static_field("class1", "field1");
        let sum = graph.add(x, y);
        let func = graph.function_def(vec![], sum);
        graph.name(func, "main").unwrap();
        graph.mark_extern_func(func).unwrap();
        graph
    };

    assert!(lhs.graph_comparison(&rhs).apply());
}

#[test]
fn order_dependent_comparison_may_be_applied() {
    let lhs = {
        let mut graph = SymbolicGraph::new();
        let x = graph.static_field("class1", "field1");
        let y = graph.static_field("class2", "field2");
        let sum = graph.add(x, y);
        let func = graph.function_def(vec![], sum);
        graph.name(func, "main").unwrap();
        graph.mark_extern_func(func).unwrap();
        graph
    };

    let rhs = {
        let mut graph = SymbolicGraph::new();
        let y = graph.static_field("class2", "field2");
        let x = graph.static_field("class1", "field1");
        let sum = graph.add(x, y);
        let func = graph.function_def(vec![], sum);
        graph.name(func, "main").unwrap();
        graph.mark_extern_func(func).unwrap();
        graph
    };

    assert!(!lhs.graph_comparison(&rhs).order_dependent(true).apply());
}

#[test]
fn default_comparison_ignores_name() {
    let lhs = {
        let mut graph = SymbolicGraph::new();
        let obj = graph.static_field("class_name", "field_name");
        graph.name(obj, "some_name").unwrap();
        let func = graph.function_def(vec![], obj);
        graph.name(func, "main").unwrap();
        graph.mark_extern_func(func).unwrap();
        graph
    };
    let rhs = {
        let mut graph = SymbolicGraph::new();
        let obj = graph.static_field("class_name", "field_name");
        graph.name(obj, "some_other_name").unwrap();
        let func = graph.function_def(vec![], obj);
        graph.name(func, "main").unwrap();
        graph.mark_extern_func(func).unwrap();
        graph
    };

    assert!(lhs.graph_comparison(&rhs).apply());
}

#[test]
fn name_dependent_comparison_may_be_applied() {
    let lhs = {
        let mut graph = SymbolicGraph::new();
        let obj = graph.static_field("class_name", "field_name");
        graph.name(obj, "some_name").unwrap();
        let func = graph.function_def(vec![], obj);
        graph.name(func, "main").unwrap();
        graph.mark_extern_func(func).unwrap();
        graph
    };
    let rhs = {
        let mut graph = SymbolicGraph::new();
        let obj = graph.static_field("class_name", "field_name");
        graph.name(obj, "some_other_name").unwrap();
        let func = graph.function_def(vec![], obj);
        graph.name(func, "main").unwrap();
        graph.mark_extern_func(func).unwrap();
        graph
    };

    assert!(!lhs.graph_comparison(&rhs).compare_names(true).apply());
}

#[test]
fn comparison_may_be_order_and_name_dependent() {
    let lhs = {
        let mut graph = SymbolicGraph::new();
        let obj = graph.static_field("class_name", "field_name");
        graph.name(obj, "some_name").unwrap();
        let func = graph.function_def(vec![], obj);
        graph.name(func, "main").unwrap();
        graph.mark_extern_func(func).unwrap();
        graph
    };
    let rhs = {
        let mut graph = SymbolicGraph::new();
        let obj = graph.static_field("class_name", "field_name");
        graph.name(obj, "some_other_name").unwrap();
        let func = graph.function_def(vec![], obj);
        graph.name(func, "main").unwrap();
        graph.mark_extern_func(func).unwrap();
        graph
    };

    assert!(!lhs
        .graph_comparison(&rhs)
        .order_dependent(true)
        .compare_names(true)
        .apply());
}

#[test]
fn comparison_depends_on_function_visibility() {
    let lhs = {
        let mut graph = SymbolicGraph::new();
        let func = graph.function_def(vec![], 42.into());
        graph.name(func, "main").unwrap();
        graph.mark_extern_func(func).unwrap();
        graph
    };
    let rhs = {
        let mut graph = SymbolicGraph::new();
        let func = graph.function_def(vec![], 42.into());
        graph.name(func, "main").unwrap();
        graph
    };

    assert!(!lhs.graph_comparison(&rhs).apply());
}
