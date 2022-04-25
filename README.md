![crates.io](https://img.shields.io/crates/v/tabbycat.svg)
![tabbycat](https://upload.wikimedia.org/wikipedia/commons/thumb/7/7f/Egyptian_Mau_Bronze.jpg/2560px-Egyptian_Mau_Bronze.jpg)
# About
This crate is used to generate dot graphs with types defined in rust.
The whole crate is implemented following the dot language specification listed at [graphviz's website](https://graphviz.org/doc/info/lang.html).
# Attributes
There is an optional feature `attributes` which implemented a subset of the [attributes list of the dot language](https://graphviz.org/doc/info/attrs.html).
By enabling this feature, you will be able to write something like:
```
use tabbycat::attributes::*;
use tabbycat::AttrList;
let attrlist =  AttrList::new()
    .add_pair(fontsize(12.0))
    .add_pair(label("test"))
    .new_bracket()
    .add_pair(fillcolor(Color::Blue))
    .add_pair(arrowhead(ArrowShape::Orinv));
assert_eq!("[fontsize=12;label=\"test\";][fillcolor=blue;arrowhead=orinv;]", attrlist.to_string())
```
# Example
```
use tabbycat::attributes::*;
use tabbycat::{AttrList, GraphBuilder, GraphType, Identity, StmtList, Edge, SubGraph};
let graph = GraphBuilder::default()
    .graph_type(GraphType::DiGraph)
    .strict(false)
    .id(Identity::id("G").unwrap())
    .stmts(StmtList::new()
        .add_node(Identity::id("A").unwrap(), None, Some(AttrList::new().add_pair(color(Color::Red))))
        .add_edge(Edge::head_node(Identity::id("B").unwrap(), None)
            .arrow_to_node(Identity::id("C").unwrap(), None)
            .add_attrpair(arrowhead(ArrowShape::Diamond)))
        .add_subgraph(SubGraph::subgraph(Some(Identity::id("D").unwrap()),
            StmtList::new()
                .add_edge(Edge::head_node(Identity::id("E").unwrap(), None)
                    .arrow_to_node(Identity::id("F").unwrap(), None)))))
    .build()
    .unwrap();
println!("{}", graph);
```
This will generate an output like:
```plaintext
digraph G{A[color=red;];B->C[arrowhead=diamond;];subgraph D{E->F;};}
```
