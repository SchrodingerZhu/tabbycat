use std::fmt::{Formatter, Result};

use derive_builder::Builder;

/// The list of attributes
#[derive(Clone, Debug)]
pub struct AttrList<'a> (pub(crate) Vec<Vec<(Identity<'a>, Identity<'a>)>>);

/// The list of statements, including:
/// - node declaration
/// - edge declaration
/// - subgraph declaration
/// - global attributes
#[derive(Clone, Debug)]
pub struct StmtList<'a>(pub(crate) Vec<Stmt<'a>>);

/// The types of graphs
#[derive(Copy, Clone, Debug)]
pub enum GraphType {
    /// undirected graph
    Graph,
    /// directed graph
    DiGraph,
}

/// The types of global attributes
#[derive(Copy, Clone, Debug)]
pub enum AttrType {
    /// attributes for graph
    Graph,
    /// attributes for node
    Node,
    /// attributes for edge
    Edge,
}

/// An identity in the dot language. You are recommended to construct it in one of the following ways:
/// - attributes method
/// - `Identity::id` for checked id strings
/// - `Identity::quoted` for quoted safe strings
/// - `Identity::from` for numeral types
///
/// However, if you need to create some special identities like `HTML`, you can use `Identity::String` directly.
#[derive(Clone, Debug)]
pub enum Identity<'a> {
    String(&'a str),
    Usize(usize),
    ISize(isize),
    I8(i8),
    U8(u8),
    I16(i16),
    U16(u16),
    I32(i32),
    U32(u32),
    Bool(bool),
    I64(i64),
    U64(u64),
    I128(i128),
    U128(u128),
    Float(f32),
    Double(f64),
    Quoted(&'a str),
    #[cfg(feature = "attributes")]
    ArrowName([Option<&'a str>; 4]),
    #[cfg(feature = "attributes")]
    RGBA(u8, u8, u8, u8),
    #[cfg(feature = "attributes")]
    HSV(f32, f32, f32),
    #[cfg(feature = "attributes")]
    Point2D(f32, f32, bool),
    #[cfg(feature = "attributes")]
    Point3D(f32, f32, f32, bool),
}

/// Graph in the dot language. You can construct it with the `GraphBuilder`.
#[derive(Builder, Clone, Debug)]
#[builder(pattern = "owned")]
pub struct Graph<'a> {
    graph_type: GraphType,
    strict: bool,
    #[builder(setter(strip_option))]
    id: Option<Identity<'a>>,
    stmts: StmtList<'a>,
}

/// A single line of statement. You should not construct it directly in most cases.
/// We still expose this type because we only implement a subset of dot language so
/// you may need to write special statements on your own.
#[derive(Clone, Debug)]
pub enum Stmt<'a> {
    Edge(Edge<'a>),
    Node {
        id: Identity<'a>,
        port: Option<Port<'a>>,
        attr: Option<AttrList<'a>>,
    },
    Attr(AttrType, AttrList<'a>),
    Equation(Identity<'a>, Identity<'a>),
    SubGraph(SubGraph<'a>),
}

/// An edge in the dot language.
#[derive(Clone, Debug)]
pub struct Edge<'a> {
    pub(crate) node: EdgeNode<'a>,
    pub(crate) body: Vec<EdgeBody<'a>>,
    pub(crate) attr: Option<AttrList<'a>>,
}

/// The tag of the edge operation
#[derive(Copy, Clone, Debug)]
pub enum EdgeOp {
    Arrow,
    Line,
}

/// A body part of edge
#[derive(Clone, Debug)]
pub struct EdgeBody<'a> {
    pub(crate) node: EdgeNode<'a>,
    pub(crate) op: EdgeOp,
}

/// A node of the edge
#[derive(Clone, Debug)]
pub enum EdgeNode<'a> {
    Node {
        id: Identity<'a>,
        port: Option<Port<'a>>,
    },
    SubGraph(SubGraph<'a>),
}

/// A subgraph in the dot language
#[derive(Clone, Debug)]
pub enum SubGraph<'a> {
    SubGraph {
        id: Option<Identity<'a>>,
        stmts: Box<StmtList<'a>>,
    },
    Cluster(Box<StmtList<'a>>),
}

impl<'a> SubGraph<'a> {
    /// create a cluster, for example you may need to following structure in your graph:
    /// ```plaintext
    /// {A;B;}
    /// ```
    pub fn cluster(list: StmtList<'a>) -> Self {
        SubGraph::Cluster(Box::new(list))
    }
    /// create a subgraph, which will output something like:
    /// ```plaintext
    /// subgraph G {
    ///     A -> B;
    /// }
    /// ```
    pub fn subgraph(id: Option<Identity<'a>>, list: StmtList<'a>) -> Self {
        SubGraph::SubGraph { id, stmts: Box::new(list) }
    }
}

/// The port suffix.
#[derive(Clone, Debug)]
pub enum Port<'a> {
    ID(Identity<'a>, Option<Compass>),
    Compass(Compass),
}

/// Directions
#[derive(Copy, Clone, Debug)]
pub enum Compass {
    North,
    NorthEast,
    East,
    SouthEast,
    South,
    SouthWest,
    West,
    NorthWest,
    Central,
}

impl<'a> IntoIterator for StmtList<'a> {
    type Item = Stmt<'a>;
    type IntoIter = std::vec::IntoIter<Stmt<'a>>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a> IntoIterator for AttrList<'a> {
    type Item = Vec<(Identity<'a>, Identity<'a>)>;
    type IntoIter = std::vec::IntoIter<Vec<(Identity<'a>, Identity<'a>)>>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a> From<bool> for Identity<'a> {
    fn from(flag: bool) -> Self {
        Identity::Bool(flag)
    }
}

impl<'a> From<isize> for Identity<'a> {
    fn from(number: isize) -> Self {
        Identity::ISize(number)
    }
}

impl<'a> From<usize> for Identity<'a> {
    fn from(number: usize) -> Self {
        Identity::Usize(number)
    }
}

impl<'a> From<i8> for Identity<'a> {
    fn from(number: i8) -> Self {
        Identity::I8(number)
    }
}

impl<'a> From<u8> for Identity<'a> {
    fn from(number: u8) -> Self {
        Identity::U8(number)
    }
}

impl<'a> From<u16> for Identity<'a> {
    fn from(number: u16) -> Self {
        Identity::U16(number)
    }
}

impl<'a> From<i16> for Identity<'a> {
    fn from(number: i16) -> Self {
        Identity::I16(number)
    }
}

impl<'a> From<u32> for Identity<'a> {
    fn from(number: u32) -> Self {
        Identity::U32(number)
    }
}

impl<'a> From<i32> for Identity<'a> {
    fn from(number: i32) -> Self {
        Identity::I32(number)
    }
}

impl<'a> From<u64> for Identity<'a> {
    fn from(number: u64) -> Self {
        Identity::U64(number)
    }
}

impl<'a> From<i64> for Identity<'a> {
    fn from(number: i64) -> Self {
        Identity::I64(number)
    }
}

impl<'a> From<i128> for Identity<'a> {
    fn from(number: i128) -> Self {
        Identity::I128(number)
    }
}


impl<'a> From<u128> for Identity<'a> {
    fn from(number: u128) -> Self {
        Identity::U128(number)
    }
}

impl<'a> From<f32> for Identity<'a> {
    fn from(number: f32) -> Self {
        Identity::Float(number)
    }
}

impl<'a> From<f64> for Identity<'a> {
    fn from(number: f64) -> Self {
        Identity::Double(number)
    }
}

impl<'a> Identity<'a> {
    /// create a checked id string, the lexical rule is:
    /// `^[a-zA-Z\x{80}-\x{ff}_][a-zA-Z\x{80}-\x{ff}\d_]*$`
    pub fn id(data: &'a str) -> anyhow::Result<Self> {
        static PATTERN: &str = r#"^[a-zA-Z\x{80}-\x{ff}_][a-zA-Z\x{80}-\x{ff}\d_]*$"#;
        let re = regex::Regex::new(PATTERN).unwrap();
        if re.is_match(data) {
            Ok(Identity::String(data))
        } else {
            Err(anyhow::anyhow!("invalid identity format"))
        }
    }
    /// create a quoted string
    pub fn quoted(data: &'a str) -> Self {
        Identity::Quoted(data)
    }
}

impl<'a> Port<'a> {
    /// corresponds to `:id`
    pub fn id(i: Identity<'a>) -> Self {
        Port::ID(i, None)
    }
    /// corresponds to `:id:<direction>`
    pub fn id_compass(i: Identity<'a>, c: Compass) -> Self {
        Port::ID(i, Some(c))
    }
    /// corresponds to `:<direction>`
    pub fn compass(c: Compass) -> Self {
        Port::Compass(c)
    }
}

impl<'a> std::fmt::Display for Graph<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        if self.strict {
            write!(f, "strict ")
        } else {
            Ok(())
        }.and(
            match self.graph_type {
                GraphType::Graph =>
                    write!(f, "graph "),
                GraphType::DiGraph =>
                    write!(f, "digraph ")
            }
        ).and(
            match &self.id {
                Some(id) => if f.alternate() { write!(f, "{} ", id) } else { write!(f, "{}", id) },
                _ => Ok(())
            }
        ).and(
            if f.alternate() {
                let padding = f.width().unwrap_or(0) + 4;
                let buffer = format!("{:width$}", self.stmts, width = padding);
                write!(f, "{{\n").and(
                    buffer.trim().split("\n").fold(Ok(()), |x, y| {
                        x.and(
                            write!(f, "{}", " ".repeat(padding))
                        ).and(
                            write!(f, "{}\n", y)
                        )
                    }).and(write!(f, "}}"))
                )
            } else {
                write!(f, "{{{}}}", self.stmts)
            }
        )
    }
}

impl std::fmt::Display for Compass {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Compass::North => write!(f, "n"),
            Compass::NorthEast => write!(f, "ne"),
            Compass::East => write!(f, "e"),
            Compass::SouthEast => write!(f, "se"),
            Compass::South => write!(f, "s"),
            Compass::SouthWest => write!(f, "sw"),
            Compass::West => write!(f, "w"),
            Compass::NorthWest => write!(f, "nw"),
            Compass::Central => write!(f, "c")
        }
    }
}

impl<'a> std::fmt::Display for Identity<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        use Identity::*;
        match self {
            #[cfg(feature = "attributes")]
            RGBA(r, g, b, a) => write!(f, "\"#{:x}{:x}{:x}{:x}\"", r, g, b, a),
            #[cfg(feature = "attributes")]
            HSV(h, s, v) => write!(f, "\"{},+{},+{}\"", h, s, v),
            #[cfg(feature = "attributes")]
            Point2D(x, y, fixed) =>
                write!(f, "\"{},{}\"", x, y)
                    .and(
                        if *fixed {
                            write!(f, "!")
                        } else { Ok(()) }
                    ),
            #[cfg(feature = "attributes")]
            Point3D(x, y, z, fixed) =>
                write!(f, "\"{},{},{}\"", x, y, z)
                    .and(
                        if *fixed {
                            write!(f, "!")
                        } else { Ok(()) }
                    ),
            String(id) => write!(f, "{}", id),
            Usize(id) => write!(f, "{}", id),
            Float(id) => write!(f, "{}", id),
            Double(id) => write!(f, "{}", id),
            Quoted(id) => write!(f, "{:?}", id),
            ISize(id) => write!(f, "{}", id),
            I8(id) => write!(f, "{}", id),
            U8(id) => write!(f, "{}", id),
            I16(id) => write!(f, "{}", id),
            U16(id) => write!(f, "{}", id),
            I32(id) => write!(f, "{}", id),
            U32(id) => write!(f, "{}", id),
            I64(id) => write!(f, "{}", id),
            U64(id) => write!(f, "{}", id),
            I128(id) => write!(f, "{}", id),
            U128(id) => write!(f, "{}", id),
            Bool(flag) => write!(f, "{}", flag),
            #[cfg(feature = "attributes")]
            ArrowName(names) => {
                names.iter().fold(Ok(()), |acc, x| {
                    acc.and(
                        match x {
                            None => Ok(()),
                            Some(e) => {
                                write!(f, "{}", e)
                            }
                        })
                })
            }
        }
    }
}

impl<'a> std::fmt::Display for Port<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Port::ID(id, Some(c)) =>
                write!(f, ":{}:{}", id, c),
            Port::ID(x, None) =>
                write!(f, ":{}", x),
            Port::Compass(x) =>
                write!(f, ":{}", x)
        }
    }
}

impl<'a> std::fmt::Display for AttrList<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.0.iter()
            .fold(Ok(()), |acc, list| {
                acc.and(write!(f, "["))
                    .and(list
                        .iter()
                        .fold(Ok(()), |acc, (x, y)| {
                            if f.width().is_some() {
                                acc.and(write!(f, "{}={}; ", x, y))
                            } else {
                                acc.and(write!(f, "{}={};", x, y))
                            }
                        }))
                    .and(write!(f, "]"))
            })
    }
}

impl<'a> std::fmt::Display for Stmt<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        use Stmt as S;
        match self {
            S::Equation(a, b) =>
                write!(f, "{}={}", a, b),
            S::Edge(edge) =>
                if let Some(w) = f.width() {
                    write!(f, "{:width$}", edge, width=w)
                } else {
                    write!(f, "{}", edge)
                }
            S::Node { id, port, attr } => {
                write!(f, "{}", id)
                    .and(match port {
                        None => Ok(()),
                        Some(p) => write!(f, "{}", p)
                    })
                    .and(match attr {
                        None => Ok(()),
                        Some(a) => if let Some(w) = f.width() {
                            write!(f, " {:width$}", a, width = w)
                        } else {
                            write!(f, "{}", a)
                        }
                    })
            }
            S::Attr(t, list) => {
                if let Some(w) = f.width() {
                    match t {
                        AttrType::Node => write!(f, "node {:width$}", list, width = w),
                        AttrType::Graph => write!(f, "graph {:width$}", list, width = w),
                        AttrType::Edge => write!(f, "edge {:width$}", list, width = w)
                    }
                } else {
                    match t {
                        AttrType::Node => write!(f, "node {}", list),
                        AttrType::Graph => write!(f, "graph {}", list),
                        AttrType::Edge => write!(f, "edge {}", list)
                    }
                }
            }
            S::SubGraph(sub) => {
                if let Some(w) = f.width() {
                    write!(f, "{:width$}", sub, width = w)
                } else {
                    write!(f, "{}", sub)
                }
            }
        }
    }
}

impl<'a> std::fmt::Display for StmtList<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        if let Some(w) = f.width() {
            self.0
                .iter()
                .fold(Ok(()), |acc, x| {
                    acc.and(write!(f, "{:width$};\n", x, width = w))
                })
        } else {
            self.0
                .iter()
                .fold(Ok(()), |acc, x| {
                    acc.and(write!(f, "{};", x))
                })
        }
    }
}

impl<'a> std::fmt::Display for SubGraph<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            SubGraph::SubGraph { id, stmts } => {
                write!(f, "subgraph ")
                    .and(
                        match id {
                            Some(id) => {
                                write!(f, "{} ", id)
                            }
                            _ => Ok(())
                        }
                    ).and(
                    if let Some(w) = f.width() {
                        let buffer = format!("{:width$}", stmts, width = w);
                        write!(f, "{{\n").and(
                            buffer.trim().split("\n").fold(Ok(()), |x, y| {
                                x.and(
                                    write!(f, "{}", " ".repeat(w))
                                ).and(
                                    write!(f, "{}\n", y)
                                )
                            }).and(write!(f, "}}"))
                        )
                    } else {
                        write!(f, "{{{}}}", stmts)
                    }
                )
            }
            SubGraph::Cluster(stmts) => {
                if let Some(w) = f.width() {
                    let buffer = format!("{:width$}", stmts, width = w);
                    write!(f, "{{\n").and(
                        buffer.trim().split("\n").fold(Ok(()), |x, y| {
                            x.and(
                                write!(f, "{}", " ".repeat(w))
                            ).and(
                                write!(f, "{}\n", y)
                            )
                        }).and(write!(f, "}}"))
                    )
                } else {
                    write!(f, "{{{}}}", stmts)
                }
            }
        }
    }
}

impl<'a> std::fmt::Display for EdgeNode<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            EdgeNode::Node { id, port } =>
                write!(f, "{}", id)
                    .and(match port {
                        Some(port) => write!(f, "{}", port),
                        _ => Ok(())
                    }),
            EdgeNode::SubGraph(graph) => {
                if let Some(w) = f.width() {
                    write!(f, "{:width$}", graph, width = w)
                } else {
                    write!(f, "{}", graph)
                }
            }
        }
    }
}

impl<'a> std::fmt::Display for EdgeBody<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self.op {
            EdgeOp::Arrow => write!(f, "->"),
            EdgeOp::Line => write!(f, "--")
        }.and(
            if let Some(w) = f.width() {
                write!(f, "{:width$}", self.node, width = w)
            } else {
                write!(f, "{}", self.node)
            }
        )
    }
}

impl<'a> std::fmt::Display for Edge<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        if let Some(w) = f.width() {
            write!(f, "{:width$}", self.node, width = w)
        } else {
            write!(f, "{}", self.node)
        }
            .and(self.body.iter().fold(Ok(()), |acc, x| {
                acc.and(if let Some(w) = f.width() {
                    write!(f, "{:width$}", x, width = w)
                } else {
                    write!(f, "{}", x)
                })
            }))
            .and(match &self.attr {
                Some(x) => if let Some(w) = f.width() {
                    write!(f, " {:width$}", x, width = w)
                } else {
                    write!(f, "{}", x)
                },
                _ => Ok(())
            })
    }
}

impl<'a> AttrList<'a> {
    /// Create an empty attribute list
    pub fn new() -> Self {
        AttrList(Vec::new())
    }
    /// The dot language support multiple attribute lists with a syntax like:
    /// ```dot
    /// A->B [color=red;][label="abc";];
    /// ```
    /// This function is used to open a new bracket and later attributes will be added to the new one.
    pub fn new_bracket(mut self) -> Self {
        self.0.push(Vec::new());
        self
    }
    /// Append a list of new attributes to the current bracket
    pub fn extend<I: IntoIterator<Item=AttrPair<'a>>>(mut self, iter: I) -> Self {
        if self.0.is_empty() {
            self = self.new_bracket();
        }
        self.0.last_mut().unwrap().extend(iter);
        self
    }
    /// Add a new attribute list
    pub fn extend_list<I: IntoIterator<Item=Vec<AttrPair<'a>>>>(mut self, iter: I) -> Self {
        self.0.extend(iter);
        self
    }
    /// Add a new attribute
    pub fn add(mut self, key: Identity<'a>, value: Identity<'a>) -> Self {
        if self.0.is_empty() {
            self = self.new_bracket();
        }
        self.0.last_mut().unwrap().push((key, value));
        self
    }
    /// Add a new attribute (in pair)
    pub fn add_pair(self, pair: AttrPair<'a>) -> Self {
        self.add(pair.0, pair.1)
    }
}

impl<'a> StmtList<'a> {
    /// Create a new statement list
    pub fn new() -> Self {
        StmtList(Vec::new())
    }
    /// Add a statement
    pub fn add(mut self, stmt: Stmt<'a>) -> Self {
        self.0.push(stmt);
        self
    }
    /// Append a list a statements
    pub fn extend<I: IntoIterator<Item=Stmt<'a>>>(mut self, iter: I) -> Self {
        self.0.extend(iter);
        self
    }
    /// Add a node statement
    pub fn add_node(mut self, id: Identity<'a>, port: Option<Port<'a>>, attr: Option<AttrList<'a>>) -> Self {
        self.0.push(Stmt::Node {
            id,
            port,
            attr,
        });
        self
    }
    /// Add a global attribute
    pub fn add_attr(mut self, attr_type: AttrType, attr_list: AttrList<'a>) -> Self {
        self.0.push(Stmt::Attr(
            attr_type,
            attr_list,
        ));
        self
    }
    /// Add an edge statement
    pub fn add_edge(mut self, edge: Edge<'a>) -> Self {
        self.0.push(Stmt::Edge(
            edge
        ));
        self
    }
    /// Add a subgraph statement
    pub fn add_subgraph(mut self, sub: SubGraph<'a>) -> Self {
        self.0.push(Stmt::SubGraph(
            sub
        ));
        self
    }
    /// Add an equation
    pub fn add_equation(mut self, a: Identity<'a>, b: Identity<'a>) -> Self {
        self.0.push(Stmt::Equation(
            a, b,
        ));
        self
    }
}

impl<'a> Edge<'a> {
    /// Start a new edge with a node
    pub fn head_node(id: Identity<'a>, port: Option<Port<'a>>) -> Self {
        Edge {
            node: EdgeNode::Node {
                id,
                port,
            },
            body: vec![],
            attr: None,
        }
    }
    /// Start a new edge with a subgraph
    pub fn head_subgraph(sub: SubGraph<'a>) -> Self {
        Edge {
            node: EdgeNode::SubGraph(sub),
            body: vec![],
            attr: None,
        }
    }
    /// Connect to a new node with line
    /// Notice that you should not use this in a directed graph. Unfortunately, this crate does not check this for you.
    pub fn line_to_node(mut self, id: Identity<'a>, port: Option<Port<'a>>) -> Self {
        self.body.push(
            EdgeBody {
                node: EdgeNode::Node {
                    id,
                    port,
                },
                op: EdgeOp::Line,
            }
        );
        self
    }
    /// Connect to a new subgraph with line
    /// Notice that you should not use this in a directed graph. Unfortunately, this crate does not check this for you.
    pub fn line_to_subgraph(mut self, sub: SubGraph<'a>) -> Self {
        self.body.push(
            EdgeBody {
                node: EdgeNode::SubGraph(sub),
                op: EdgeOp::Line,
            }
        );
        self
    }
    /// Connect to a new node with arrow
    /// Notice that you should not use this in a undirected graph. Unfortunately, this crate does not check this for you.
    pub fn arrow_to_node(mut self, id: Identity<'a>, port: Option<Port<'a>>) -> Self {
        self.body.push(
            EdgeBody {
                node: EdgeNode::Node {
                    id,
                    port,
                },
                op: EdgeOp::Arrow,
            }
        );
        self
    }
    /// Connect to a new subgraph with arrow
    /// Notice that you should not use this in a undirected graph. Unfortunately, this crate does not check this for you.
    pub fn arrow_to_subgraph(mut self, sub: SubGraph<'a>) -> Self {
        self.body.push(
            EdgeBody {
                node: EdgeNode::SubGraph(sub),
                op: EdgeOp::Arrow,
            }
        );
        self
    }
    /// Add an attribute list to the edge
    pub fn add_attrlist(mut self, list: AttrList<'a>) -> Self {
        if self.attr.is_none() {
            self.attr.replace(list);
        } else {
            self.attr.as_mut().unwrap().0.extend(list.0);
            self.attr.as_mut().unwrap().0.push(Vec::new());
        }
        self
    }
    /// Add an attribute to the edge
    pub fn add_attribute(mut self, key: Identity<'a>, value: Identity<'a>) -> Self {
        if self.attr.is_none() {
            self.attr.replace(AttrList(vec![vec![(key, value)]]));
        } else {
            let vec = &mut self.attr.as_mut().unwrap().0;
            if vec.is_empty() {
                vec.push(vec![(key, value)]);
            } else {
                vec.last_mut().unwrap().push((key, value));
            }
        }
        self
    }
    /// Add an attribute to the edge (in pair)
    pub fn add_attrpair(self, pair: AttrPair<'a>) -> Self {
        self.add_attribute(pair.0, pair.1)
    }
}

pub type AttrPair<'a> = (Identity<'a>, Identity<'a>);

