use crate::ast::{
    BinaryNode, Node, OFAst, OFFnCall, OFParens, OFPostfix, OFPrefix, Operator, Span,
};
use std::fmt;
use std::fmt::{Display, Formatter};

pub fn indent(f: &mut Formatter<'_>, indent: u32) -> fmt::Result {
    for _ in 0..indent * 4 {
        write!(f, " ")?;
    }
    Ok(())
}

pub fn arrow(f: &mut Formatter<'_>, indent: u32) -> fmt::Result {
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

pub fn debug_self<'a>(f: &mut Formatter<'_>, node: &impl Node<'a>) -> fmt::Result {
    write!(f, "{}    ", node.name())?;
    debug_span(f, node.span())?;
    writeln!(f)?;

    Ok(())
}

pub fn debug_none(f: &mut Formatter<'_>) -> fmt::Result {
    writeln!(f, "None    ")?;
    Ok(())
}

pub fn debug_op<'a>(f: &mut Formatter<'_>, op: &impl Operator<'a>) -> fmt::Result {
    write!(f, "{}    ", op)?;
    debug_span(f, op.span())?;
    writeln!(f)?;

    Ok(())
}

pub fn debug_elem<'a, T>(f: &mut Formatter<'_>, v: &T) -> fmt::Result
where
    T: Display,
    T: Node<'a>,
{
    write!(f, "[{}] ", v)?;
    debug_span(f, v.span())?;

    Ok(())
}

pub fn debug_span(f: &mut Formatter<'_>, span: Span<'_>) -> fmt::Result {
    write!(f, "\"{}\" {} ", *span, span.location_offset())
}

pub fn debug_ast<'a, 'b>(f: &mut Formatter<'b>, ast: &OFAst<'a>, indent: u32) -> fmt::Result {
    match ast {
        OFAst::NodeEmpty(v) => debug_empty(f, v),
        OFAst::NodeCompare(ex) => debug_binary(f, ex, indent),
        OFAst::NodeAdd(ex) => debug_binary(f, ex, indent),
        OFAst::NodeMul(ex) => debug_binary(f, ex, indent),
        OFAst::NodePow(ex) => debug_binary(f, ex, indent),
        OFAst::NodePostfix(ex) => debug_postfix(f, ex, indent),
        OFAst::NodePrefix(ex) => debug_prefix(f, ex, indent),
        OFAst::NodeNumber(v) => debug_elem(f, v),
        OFAst::NodeString(v) => debug_elem(f, v),
        OFAst::NodeCellRef(v) => debug_elem(f, v),
        OFAst::NodeCellRange(v) => debug_elem(f, v),
        OFAst::NodeColRange(v) => debug_elem(f, v),
        OFAst::NodeRowRange(v) => debug_elem(f, v),
        OFAst::NodeParens(ex) => debug_parens(f, ex, indent),
        OFAst::NodeFnCall(ff) => debug_fn_call(f, ff, indent),
        OFAst::NodeNamed(v) => debug_elem(f, v),
    }
}

pub fn debug_empty<'a, 'b>(f: &mut Formatter<'b>, node: &impl Node<'a>) -> fmt::Result {
    write!(f, "()    ")?;
    debug_span(f, node.span())?;
    writeln!(f)?;
    Ok(())
}

pub fn debug_binary<'a, 'b>(
    f: &mut Formatter<'b>,
    node: &impl BinaryNode<'a>,
    ind: u32,
) -> fmt::Result {
    debug_self(f, node)?;

    arrow(f, ind + 1)?;
    debug_ast(f, node.left(), ind + 1)?;
    indent(f, ind + 1)?;
    debug_op(f, node.op())?;
    indent(f, ind + 1)?;
    debug_ast(f, node.right(), ind + 1)?;
    Ok(())
}

pub fn debug_prefix<'a, 'b>(f: &mut Formatter<'b>, node: &OFPrefix<'a>, ind: u32) -> fmt::Result {
    debug_self(f, node)?;

    arrow(f, ind + 1)?;
    debug_op(f, &node.op)?;
    indent(f, ind + 1)?;
    debug_ast(f, &node.expr, ind + 1)?;
    Ok(())
}

pub fn debug_postfix<'a, 'b>(f: &mut Formatter<'b>, node: &OFPostfix<'a>, ind: u32) -> fmt::Result {
    debug_self(f, node)?;

    arrow(f, ind + 1)?;
    debug_ast(f, &node.expr, ind + 1)?;
    indent(f, ind + 1)?;
    debug_op(f, &node.op)?;
    Ok(())
}

pub fn debug_parens<'a, 'b>(f: &mut Formatter<'b>, node: &OFParens<'a>, ind: u32) -> fmt::Result {
    debug_self(f, node)?;

    arrow(f, ind + 1)?;
    debug_ast(f, &node.expr, ind + 1)?;
    Ok(())
}

pub fn debug_fn_call<'a, 'b>(f: &mut Formatter<'b>, node: &OFFnCall<'a>, ind: u32) -> fmt::Result {
    debug_self(f, node)?;

    arrow(f, ind + 1)?;
    write!(f, "{} (   ", node.name)?;
    debug_span(f, node.name.span())?;
    writeln!(f)?;

    for ex in &node.arg {
        indent(f, ind + 2)?;
        debug_ast(f, ex, ind + 2)?;
    }

    indent(f, ind + 1)?;
    writeln!(f, ")")?;
    Ok(())
}
