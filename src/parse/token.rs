//!
//! Parser tokens
//!

/// All known tokens.
#[allow(missing_docs)]
#[non_exhaustive]
#[derive(Debug, Eq, PartialEq)]
pub enum Token<I> {
    ErrRef(I),
    CellRef(I, CellRefToken<I>),
    RangeRef(I, CellRangeToken<I>),
    ColRef(I, ColRangeToken<I>),
    RowRef(I, RowRangeToken<I>),
    CellCuboid(I, CellCuboidToken<I>),
    ColCuboid(I, ColCuboidToken<I>),
    RowCuboid(I, RowCuboidToken<I>),
}

/// Cell reference
#[derive(Debug, Eq, PartialEq)]
pub struct CellRefToken<I> {
    /// External reference.
    pub iri: Option<I>,
    /// Different table.
    pub table: Option<(Option<I>, I)>,
    /// Cell
    pub cell: CellToken<I>,
}

/// Cell range
#[derive(Debug, Eq, PartialEq)]
pub struct CellRangeToken<I> {
    /// External reference.
    pub iri: Option<I>,
    /// Different table.
    pub table: Option<(Option<I>, I)>,
    /// Cell
    pub from: CellToken<I>,
    /// Cell
    pub to: CellToken<I>,
}

/// Column range
#[derive(Debug, Eq, PartialEq)]
pub struct ColRangeToken<I> {
    /// External reference.
    pub iri: Option<I>,
    /// Different table.
    pub table: Option<(Option<I>, I)>,
    /// Cell
    pub from: (Option<I>, I),
    /// Cell
    pub to: (Option<I>, I),
}

/// Row range
#[derive(Debug, Eq, PartialEq)]
pub struct RowRangeToken<I> {
    /// External reference.
    pub iri: Option<I>,
    /// Different table.
    pub table: Option<(Option<I>, I)>,
    /// Cell
    pub from: (Option<I>, I),
    /// Cell
    pub to: (Option<I>, I),
}

/// Cell cuboid spanning multiple tables.
#[derive(Debug, Eq, PartialEq)]
pub struct CellCuboidToken<I> {
    /// External reference.
    pub iri: Option<I>,
    /// Different table.
    pub from_table: I,
    /// Cell
    pub from: CellToken<I>,
    /// Different table.
    pub to_table: I,
    /// Cell
    pub to: CellToken<I>,
}

/// Column cuboid spanning multiple tables.
#[derive(Debug, Eq, PartialEq)]
pub struct ColCuboidToken<I> {
    /// External reference.
    pub iri: Option<I>,
    /// Different table.
    pub from_table: I,
    /// Cell
    pub from: (Option<I>, I),
    /// Different table.
    pub to_table: I,
    /// Cell
    pub to: (Option<I>, I),
}

/// Row cuboid spanning multiple tables.
#[derive(Debug, Eq, PartialEq)]
pub struct RowCuboidToken<I> {
    /// External reference.
    pub iri: Option<I>,
    /// Different table.
    pub from_table: I,
    /// Cell
    pub from: (Option<I>, I),
    /// Different table.
    pub to_table: I,
    /// Cell
    pub to: (Option<I>, I),
}

/// Basic cell reference token.
#[derive(Debug, Eq, PartialEq)]
pub struct CellToken<I> {
    /// Cell abs-flag + column
    pub col: (Option<I>, I),
    /// Cell abs_flag + row
    pub row: (Option<I>, I),
}
