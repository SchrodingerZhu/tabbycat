pub mod graph;

#[cfg(test)]
mod test {
    use crate::graph::Identity;
    use crate::graph::Compass::NorthEast;

    #[test]
    fn codegen_compass() {
        use crate::graph::Compass::*;
        assert_eq!("n", North.to_string());
        assert_eq!("ne", NorthEast.to_string());
    }

    #[test]
    fn codegen_id() {
        use crate::graph::Identity;
        assert_eq!("a123", Identity::id("a123").unwrap().to_string());
        assert_eq!("\"123\"", Identity::quoted("123").to_string());
    }

    #[test]
    fn codegen_port() {
        use crate::graph::Port;
        use crate::graph::Identity as I;
        use crate::graph::Compass as C;
        {
            let a = Port::id_compass(I::from(1.5), C::NorthEast);
            assert_eq!(":1.5:ne", a.to_string())
        }

        {
            let a = Port::ID(I::String("a"), None);
            assert_eq!(":a", a.to_string())
        }

        {
            let a = Port::Compass(C::SouthWest);
            assert_eq!(":sw", a.to_string())
        }
    }

    #[test]
    fn codegen_attrlist() -> anyhow::Result<()> {
        use crate::graph::Identity as I;
        use crate::graph::AttrList;
        let attrlist = AttrList::new()
            .add(I::id("name")?, I::id("abc")?)
            .add(I::id("color")?, I::id("red")?)
            .new_bracket()
            .add(I::id("size")?, I::from(12_isize));
        Ok(assert_eq!("[name=abc;color=red;][size=12;]", attrlist.to_string()))
    }


    #[test]
    fn codegen_subgraph() {
        use crate::graph::{Stmt, StmtList, SubGraph, Identity, Port, Compass};
        let g = SubGraph::SubGraph {
            id: Some(Identity::String("G")),
            stmts: Box::new(StmtList(
                vec![Stmt::Node {
                    id: Identity::String("g"),
                    port: Some(Port::ID(
                        Identity::String("h"),
                        Some(Compass::SouthWest),
                    )),
                    attr: None,
                }]
            )),
        };
        assert_eq!("subgraph G {g:h:sw;}", g.to_string())
    }

    #[test]
    fn codegen_edge() -> anyhow::Result<()> {
        use crate::graph::{SubGraph, StmtList, Edge};
        let edge = Edge::head_node(Identity::id("a")?, None)
            .add_attribute(Identity::id("color")?, Identity::id("pink")?)
            .arrow_to_node(Identity::id("b")?, None)
            .arrow_to_subgraph(
                SubGraph::cluster(StmtList::new()
                    .add_node(Identity::id("c")?, None, None)
                    .add_node(Identity::id("d")?, None, None)
                    ));
        assert_eq!("a->b->{c;d;}[color=pink;]", edge.to_string());
        Ok(())
    }

    #[test]
    fn codegen_graph() -> anyhow::Result<()> {
        use crate::graph::*;
        let g = GraphBuilder::default()
            .graph_type(GraphType::DiGraph)
            .strict(true)
            .id(Identity::Double(1.1))
            .stmts(StmtList::new()
                .add_node(Identity::from(1), Some(Port::Compass(NorthEast)), Some(AttrList::new()
                    .add(Identity::id("color")?, Identity::id("red")?)))
                .add_subgraph(SubGraph::subgraph(
                    Some(Identity::from(2)),
                    StmtList::new()
                        .add_edge(Edge::head_node(Identity::from(3), None)
                            .arrow_to_node(Identity::from(4), None)
                            .arrow_to_node(Identity::from(5), None)
                            .arrow_to_node(Identity::from(6), None)
                            .add_attribute(Identity::id("color")?, Identity::id("purple")?))
                ))
                .add_node(Identity::from(7), None, None)
                .add_edge(Edge::head_node(Identity::from(3), None)
                    .arrow_to_node(Identity::from(7), None)
                    .arrow_to_node(Identity::from(1), None)))
            .build()
            .unwrap();
        println!("{}", g);
        Ok(())
    }
}