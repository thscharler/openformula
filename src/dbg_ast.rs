use crate::ast::{
    BinaryNode, Node, OFAst, OFFnCall, OFParens, OFPostfix, OFPrefix, Operator, Span,
};
use std::fmt;
use std::fmt::{Display, Formatter};

pub fn indent(indent: u32, f: &mut Formatter<'_>) -> fmt::Result {
    for _ in 0..indent * 4 {
        write!(f, " ")?;
    }
    Ok(())
}

pub fn arrow(indent: u32, f: &mut Formatter<'_>) -> fmt::Result {
    if indent > 0 {
        for _ in 0..((indent - 1) * 4) {
            write!(f, " ")?;
        }
        for _ in ((indent - 1) * 4)..(indent * 4) - 2 {
            write!(f, "-")?;
        }
        write!(f, "> ")?;
    }
    Ok(())
}

pub fn debug_self<'a>(node: &impl Node<'a>, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "{}    ", node.name())?;
    debug_span(node.span(), f)?;
    writeln!(f)?;

    Ok(())
}

pub fn debug_op<'a>(op: &impl Operator<'a>, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "{}    ", op)?;
    debug_span(op.span(), f)?;
    writeln!(f)?;

    Ok(())
}

pub fn debug_elem<'a, T>(v: &T, f: &mut Formatter<'_>) -> fmt::Result
where
    T: Display,
    T: Node<'a>,
{
    write!(f, "{}    ", v)?;
    debug_span(v.span(), f)?;
    writeln!(f)?;

    Ok(())
}

pub fn debug_span(span: Span<'_>, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "off:{}:'{}'", span.location_offset(), *span)
}

pub fn debug_ast<'a, 'b>(ast: &OFAst<'a>, indent: u32, f: &mut Formatter<'b>) -> fmt::Result {
    match ast {
        OFAst::NodeEmpty(v) => debug_empty(v, f),
        OFAst::NodeCompare(ex) => debug_binary(ex, indent, f),
        OFAst::NodeAdd(ex) => debug_binary(ex, indent, f),
        OFAst::NodeMul(ex) => debug_binary(ex, indent, f),
        OFAst::NodePow(ex) => debug_binary(ex, indent, f),
        OFAst::NodePostfix(ex) => debug_postfix(ex, indent, f),
        OFAst::NodePrefix(ex) => debug_prefix(ex, indent, f),
        OFAst::NodeNumber(v) => debug_elem(v, f),
        OFAst::NodeString(v) => debug_elem(v, f),
        OFAst::NodeCellRef(v) => debug_elem(v, f),
        OFAst::NodeCellRange(v) => debug_elem(v, f),
        OFAst::NodeColRange(v) => debug_elem(v, f),
        OFAst::NodeRowRange(v) => debug_elem(v, f),
        OFAst::NodeParens(ex) => debug_parens(ex, indent, f),
        OFAst::NodeFnCall(ff) => debug_fn_call(ff, indent, f),
        OFAst::NodeNamed(v) => debug_elem(v, f),
    }
}

pub fn debug_empty<'a, 'b>(node: &impl Node<'a>, f: &mut Formatter<'b>) -> fmt::Result {
    write!(f, "()    ")?;
    debug_span(node.span(), f)?;
    writeln!(f)?;
    Ok(())
}

pub fn debug_binary<'a, 'b>(
    node: &impl BinaryNode<'a>,
    ind: u32,
    f: &mut Formatter<'b>,
) -> fmt::Result {
    debug_self(node, f)?;

    arrow(ind + 1, f)?;
    debug_ast(node.left(), ind + 1, f)?;
    indent(ind + 1, f)?;
    debug_op(node.op(), f)?;
    indent(ind + 1, f)?;
    debug_ast(node.right(), ind + 1, f)?;
    Ok(())
}

pub fn debug_prefix<'a, 'b>(node: &OFPrefix<'a>, ind: u32, f: &mut Formatter<'b>) -> fmt::Result {
    debug_self(node, f)?;

    arrow(ind + 1, f)?;
    debug_op(&node.op, f)?;
    indent(ind + 1, f)?;
    debug_ast(&node.expr, ind + 1, f)?;
    Ok(())
}

pub fn debug_postfix<'a, 'b>(node: &OFPostfix<'a>, ind: u32, f: &mut Formatter<'b>) -> fmt::Result {
    debug_self(node, f)?;

    arrow(ind + 1, f)?;
    debug_ast(&node.expr, ind + 1, f)?;
    indent(ind + 1, f)?;
    debug_op(&node.op, f)?;
    Ok(())
}

pub fn debug_parens<'a, 'b>(node: &OFParens<'a>, ind: u32, f: &mut Formatter<'b>) -> fmt::Result {
    debug_self(node, f)?;

    arrow(ind + 1, f)?;
    debug_ast(&node.expr, ind + 1, f)?;
    Ok(())
}

pub fn debug_fn_call<'a, 'b>(node: &OFFnCall<'a>, ind: u32, f: &mut Formatter<'b>) -> fmt::Result {
    debug_self(node, f)?;

    arrow(ind + 1, f)?;
    write!(f, "{} (   ", node.name)?;
    debug_span(node.name.span(), f)?;
    writeln!(f)?;

    for ex in &node.arg {
        indent(ind + 2, f)?;
        debug_ast(ex, ind + 2, f)?;
    }

    indent(ind + 1, f)?;
    writeln!(f, ")")?;
    Ok(())
}
