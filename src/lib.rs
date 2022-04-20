// Copyright 2020
// Schrodinger ZHU <i@zhuyi.fan>

//! ![tabbycat](https://upload.wikimedia.org/wikipedia/commons/thumb/7/7f/Egyptian_Mau_Bronze.jpg/2560px-Egyptian_Mau_Bronze.jpg)
//! # About
//! This crate is used to generate dot graphs with types defined in rust.
//! The whole crate is implemented following the dot language specification listed at [graphviz's website](https://graphviz.org/doc/info/lang.html).
//!
//! # Attributes
//! This crate implements a subset of the [attributes list of the dot language](https://graphviz.org/doc/info/attrs.html).
//! You can write something like:
//!
//! ```
//! use tabbycat::attributes::*;
//! use tabbycat::AttrList;
//! let attrlist =  AttrList::new()
//!     .add_pair(fontsize(12.0))
//!     .add_pair(label("test"))
//!     .new_bracket()
//!     .add_pair(fillcolor(Color::Blue))
//!     .add_pair(arrowhead(ArrowShape::Orinv));
//! assert_eq!("[fontsize=12;label=\"test\";][fillcolor=blue;arrowhead=orinv;]", attrlist.to_string())
//! ```
//!
//! # Example
//!
//! ```
//! use tabbycat::attributes::*;
//! use tabbycat::{AttrList, GraphBuilder, GraphType, Identity, StmtList, Edge, SubGraph};
//! let graph = GraphBuilder::default()
//!     .graph_type(GraphType::DiGraph)
//!     .strict(false)
//!     .id(Identity::id("G").unwrap())
//!     .stmts(StmtList::new()
//!         .add_node(Identity::id("A").unwrap(), None, Some(AttrList::new().add_pair(color(Color::Red))))
//!         .add_edge(Edge::head_node(Identity::id("B").unwrap(), None)
//!             .arrow_to_node(Identity::id("C").unwrap(), None)
//!             .add_attrpair(arrowhead(ArrowShape::Diamond)))
//!         .add_subgraph(SubGraph::subgraph(Some(Identity::id("D").unwrap()),
//!             StmtList::new()
//!                 .add_edge(Edge::head_node(Identity::id("E").unwrap(), None)
//!                     .arrow_to_node(Identity::id("F").unwrap(), None)))))
//!     .build()
//!     .unwrap();
//! println!("{}", graph);
//! ```
//!
//! This will generate an output like:
//! ```plaintext
//! digraph G{A[color=red;];B->C[arrowhead=diamond;];subgraph D{E->F;};}
//! ```
pub use graph::*;

mod graph;

pub mod attributes;

#[cfg(test)]
mod test {
    use crate::attributes::*;
    use crate::Compass::NorthEast;
    use crate::Identity;

    #[test]
    fn codegen_compass() {
        use crate::Compass::*;
        assert_eq!("n", North.to_string());
        assert_eq!("ne", NorthEast.to_string());
    }

    #[test]
    fn codegen_id() {
        use crate::Identity;
        assert_eq!("a123", Identity::id("a123").unwrap().to_string());
        assert_eq!("\"123\"", Identity::quoted("123").to_string());
    }

    #[test]
    fn codegen_port() {
        use crate::Compass as C;
        use crate::Identity as I;
        use crate::Port;
        {
            let a = Port::id_compass(I::from(1.5), C::NorthEast);
            assert_eq!(":1.5:ne", a.to_string())
        }

        {
            let a = Port::ID(I::String("a".to_string()), None);
            assert_eq!(":a", a.to_string())
        }

        {
            let a = Port::Compass(C::SouthWest);
            assert_eq!(":sw", a.to_string())
        }
    }

    #[test]
    fn codegen_attrlist() -> anyhow::Result<()> {
        use crate::AttrList;
        use crate::Identity as I;
        let attrlist = AttrList::new()
            .add(I::id("name")?, I::id("abc")?)
            .add(I::id("color")?, I::id("red")?)
            .new_bracket()
            .add(I::id("size")?, I::from(12_isize));
        {
            let attrlist = attrlist
                .add_pair(fontsize(12.0))
                .add_pair(label("test"))
                .add_pair(fillcolor(Color::Blue))
                .add_pair(arrowhead(ArrowShape::Orinv));
            Ok(assert_eq!("[name=abc;color=red;][size=12;fontsize=12;label=\"test\";fillcolor=blue;arrowhead=orinv;]", attrlist.to_string()))
        }
    }

    #[test]
    fn codegen_subgraph() {
        use crate::{Compass, Identity, Port, Stmt, StmtList, SubGraph};
        let g = SubGraph::SubGraph {
            id: Some(Identity::String("G".to_string())),
            stmts: Box::new(StmtList(vec![Stmt::Node {
                id: Identity::String("g".to_string()),
                port: Some(Port::ID(
                    Identity::String("h".to_string()),
                    Some(Compass::SouthWest),
                )),
                attr: None,
            }])),
        };
        assert_eq!("subgraph G {g:h:sw;}", g.to_string())
    }

    #[test]
    fn codegen_edge() -> anyhow::Result<()> {
        use crate::{Edge, StmtList, SubGraph};
        let edge = Edge::head_node(Identity::id("a")?, None)
            .add_attribute(Identity::id("color")?, Identity::id("pink")?)
            .arrow_to_node(Identity::id("b")?, None)
            .arrow_to_subgraph(SubGraph::cluster(
                StmtList::new()
                    .add_node(Identity::id("c")?, None, None)
                    .add_node(Identity::id("d")?, None, None),
            ));
        assert_eq!("a->b->{c;d;}[color=pink;]", edge.to_string());
        Ok(())
    }

    #[test]
    fn codegen_graph() -> anyhow::Result<()> {
        use crate::*;
        let g = GraphBuilder::default()
            .graph_type(GraphType::DiGraph)
            .strict(true)
            .id(Identity::Double(1.1))
            .stmts(
                StmtList::new()
                    .add_node(
                        Identity::from(1),
                        Some(Port::Compass(NorthEast)),
                        Some(AttrList::new().add(Identity::id("color")?, Identity::id("red")?)),
                    )
                    .add_subgraph(SubGraph::subgraph(
                        Some(Identity::from(2)),
                        StmtList::new()
                            .add_edge(
                                Edge::head_node(Identity::from(3), None)
                                    .arrow_to_node(Identity::from(4), None)
                                    .arrow_to_node(Identity::from(5), None)
                                    .arrow_to_node(Identity::from(6), None)
                                    .add_attribute(Identity::id("color")?, Identity::id("purple")?),
                            )
                            .add_subgraph(SubGraph::subgraph(
                                Some(Identity::from(2)),
                                StmtList::new().add_edge(
                                    Edge::head_node(Identity::from(3), None)
                                        .arrow_to_node(Identity::from(4), None)
                                        .arrow_to_node(Identity::from(5), None)
                                        .arrow_to_node(Identity::from(6), None)
                                        .add_attribute(
                                            Identity::id("color")?,
                                            Identity::id("purple")?,
                                        ),
                                ),
                            )),
                    ))
                    .add_node(Identity::from(7), None, None)
                    .add_edge(
                        Edge::head_node(Identity::from(3), None)
                            .arrow_to_node(Identity::from(7), None)
                            .arrow_to_node(Identity::from(1), None),
                    ),
            )
            .build()
            .unwrap();
        println!("{:#}", g);
        Ok(())
    }
}
