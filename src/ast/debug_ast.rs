use crate::ast::{
    BinaryNode, Node, OFAdd, OFAst, OFCellRange, OFCellRef, OFCol, OFColRange, OFCompare, OFEmpty,
    OFFnCall, OFIri, OFMul, OFNamed, OFNumber, OFParens, OFPostfix, OFPow, OFPrefix, OFRow,
    OFRowRange, OFSheetName, OFSimpleNamed, OFString, Operator, Span,
};
use crate::iparse::error::DebugWidth;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};

impl<'a> Debug for OFAst<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        debug_ast(f, DebugWidth::from(f.width()), self, 0)
    }
}

impl<'a> Debug for OFEmpty<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        debug_empty(f, DebugWidth::from(f.width()), self)
    }
}

impl<'a> Debug for OFCompare<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        debug_binary(f, DebugWidth::from(f.width()), self, 0)
    }
}

impl<'a> Debug for OFAdd<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        debug_binary(f, DebugWidth::from(f.width()), self, 0)
    }
}

impl<'a> Debug for OFMul<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        debug_binary(f, DebugWidth::from(f.width()), self, 0)
    }
}

impl<'a> Debug for OFPow<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        debug_binary(f, DebugWidth::from(f.width()), self, 0)
    }
}

impl<'a> Debug for OFPostfix<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        debug_postfix(f, DebugWidth::from(f.width()), self, 0)
    }
}

impl<'a> Debug for OFPrefix<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        debug_prefix(f, DebugWidth::from(f.width()), self, 0)
    }
}

impl<'a> Debug for OFNumber<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        debug_elem(f, DebugWidth::from(f.width()), self)
    }
}

impl<'a> Debug for OFString<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        debug_elem(f, DebugWidth::from(f.width()), self)
    }
}

impl<'a> Debug for OFIri<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        debug_elem(f, DebugWidth::from(f.width()), self)
    }
}

impl<'a> Debug for OFSheetName<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        debug_elem(f, DebugWidth::from(f.width()), self)
    }
}

impl<'a> Debug for OFRow<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        debug_elem(f, DebugWidth::from(f.width()), self)
    }
}

impl<'a> Debug for OFCol<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        debug_elem(f, DebugWidth::from(f.width()), self)
    }
}

impl<'a> Debug for OFSimpleNamed<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        debug_elem(f, DebugWidth::from(f.width()), self)
    }
}

impl<'a> Debug for OFNamed<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        debug_elem(f, DebugWidth::from(f.width()), self)
    }
}

impl<'a> Debug for OFCellRef<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        debug_elem(f, DebugWidth::from(f.width()), self)
    }
}

impl<'a> Debug for OFCellRange<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        debug_elem(f, DebugWidth::from(f.width()), self)
    }
}

impl<'a> Debug for OFRowRange<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        debug_elem(f, DebugWidth::from(f.width()), self)
    }
}

impl<'a> Debug for OFColRange<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        debug_elem(f, DebugWidth::from(f.width()), self)
    }
}

impl<'a> Debug for OFParens<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        debug_parens(f, DebugWidth::from(f.width()), self, 0)
    }
}

impl<'a> Debug for OFFnCall<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        debug_fn_call(f, DebugWidth::from(f.width()), self, 0)
    }
}

// -----------------------------------------------------------------------

fn indent(f: &mut Formatter<'_>, indent: u32) -> fmt::Result {
    writeln!(f)?;
    for _ in 0..indent * 4 {
        write!(f, " ")?;
    }
    Ok(())
}

fn arrow(f: &mut Formatter<'_>, indent: u32) -> fmt::Result {
    writeln!(f)?;
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

fn debug_self<'a>(f: &mut Formatter<'_>, w: DebugWidth, node: &impl Node<'a>) -> fmt::Result {
    write!(f, "{} ", node.name())?;
    debug_span(f, w, node.span())?;

    Ok(())
}

fn debug_op<'a>(f: &mut Formatter<'_>, w: DebugWidth, op: &impl Operator<'a>) -> fmt::Result {
    write!(f, "{} ", op)?;
    debug_span(f, w, op.span())?;

    Ok(())
}

fn debug_elem<'a, T>(f: &mut Formatter<'_>, w: DebugWidth, v: &T) -> fmt::Result
where
    T: Display,
    T: Node<'a>,
{
    write!(f, "{} ", v)?;
    debug_span(f, w, v.span())?;

    Ok(())
}

fn debug_span(f: &mut Formatter<'_>, w: DebugWidth, span: Span<'_>) -> fmt::Result {
    if w == DebugWidth::Long {
        write!(f, "{}:\"{}\"", span.location_offset(), *span)
    } else {
        Ok(())
    }
}

fn debug_ast<'a, 'b>(
    f: &mut Formatter<'b>,
    w: DebugWidth,
    ast: &OFAst<'a>,
    indent: u32,
) -> fmt::Result {
    match ast {
        OFAst::NodeEmpty(v) => debug_empty(f, w, v),
        OFAst::NodeCompare(ex) => debug_binary(f, w, ex, indent),
        OFAst::NodeAdd(ex) => debug_binary(f, w, ex, indent),
        OFAst::NodeMul(ex) => debug_binary(f, w, ex, indent),
        OFAst::NodePow(ex) => debug_binary(f, w, ex, indent),
        OFAst::NodePostfix(ex) => debug_postfix(f, w, ex, indent),
        OFAst::NodePrefix(ex) => debug_prefix(f, w, ex, indent),
        OFAst::NodeNumber(v) => debug_elem(f, w, v),
        OFAst::NodeString(v) => debug_elem(f, w, v),
        OFAst::NodeCellRef(v) => debug_elem(f, w, v),
        OFAst::NodeCellRange(v) => debug_elem(f, w, v),
        OFAst::NodeColRange(v) => debug_elem(f, w, v),
        OFAst::NodeRowRange(v) => debug_elem(f, w, v),
        OFAst::NodeParens(ex) => debug_parens(f, w, ex, indent),
        OFAst::NodeFnCall(ff) => debug_fn_call(f, w, ff, indent),
        OFAst::NodeNamed(v) => debug_elem(f, w, v),
    }
}

fn debug_empty<'a, 'b>(f: &mut Formatter<'b>, w: DebugWidth, node: &impl Node<'a>) -> fmt::Result {
    debug_self(f, w, node)?;

    writeln!(f)?;
    Ok(())
}

fn debug_binary<'a, 'b>(
    f: &mut Formatter<'b>,
    w: DebugWidth,
    node: &impl BinaryNode<'a>,
    ind: u32,
) -> fmt::Result {
    debug_self(f, w, node)?;

    arrow(f, ind + 1)?;
    debug_ast(f, w, node.left(), ind + 1)?;
    indent(f, ind + 1)?;
    debug_op(f, w, node.op())?;
    indent(f, ind + 1)?;
    debug_ast(f, w, node.right(), ind + 1)?;
    Ok(())
}

fn debug_prefix<'a, 'b>(
    f: &mut Formatter<'b>,
    w: DebugWidth,
    node: &OFPrefix<'a>,
    ind: u32,
) -> fmt::Result {
    debug_self(f, w, node)?;

    arrow(f, ind + 1)?;
    debug_op(f, w, &node.op)?;
    indent(f, ind + 1)?;
    debug_ast(f, w, &node.expr, ind + 1)?;
    Ok(())
}

fn debug_postfix<'a, 'b>(
    f: &mut Formatter<'b>,
    w: DebugWidth,
    node: &OFPostfix<'a>,
    ind: u32,
) -> fmt::Result {
    debug_self(f, w, node)?;

    arrow(f, ind + 1)?;
    debug_ast(f, w, &node.expr, ind + 1)?;
    indent(f, ind + 1)?;
    debug_op(f, w, &node.op)?;
    Ok(())
}

fn debug_parens<'a, 'b>(
    f: &mut Formatter<'b>,
    w: DebugWidth,
    node: &OFParens<'a>,
    ind: u32,
) -> fmt::Result {
    debug_self(f, w, node)?;

    arrow(f, ind + 1)?;
    debug_ast(f, w, &node.expr, ind + 1)?;
    Ok(())
}

fn debug_fn_call<'a, 'b>(
    f: &mut Formatter<'b>,
    w: DebugWidth,
    node: &OFFnCall<'a>,
    ind: u32,
) -> fmt::Result {
    debug_self(f, w, node)?;

    arrow(f, ind + 1)?;
    write!(f, "{} ( ", node.name)?;
    debug_span(f, w, node.name.span())?;

    for ex in &node.arg {
        indent(f, ind + 2)?;
        debug_ast(f, w, ex, ind + 2)?;
    }

    indent(f, ind + 1)?;
    write!(f, ")")?;

    Ok(())
}
