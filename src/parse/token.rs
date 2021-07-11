#[non_exhaustive]
#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub struct CellRefToken<I> {
    pub iri: Option<I>,
    pub sheet: Option<(Option<I>, I)>,
    pub cell: CellToken<I>,
}

#[derive(Debug, PartialEq)]
pub struct CellRangeToken<I> {
    pub iri: Option<I>,
    pub sheet: Option<(Option<I>, I)>,
    pub from: CellToken<I>,
    pub to: CellToken<I>,
}

#[derive(Debug, PartialEq)]
pub struct ColRangeToken<I> {
    pub iri: Option<I>,
    pub sheet: Option<(Option<I>, I)>,
    pub from: (Option<I>, I),
    pub to: (Option<I>, I),
}

#[derive(Debug, PartialEq)]
pub struct RowRangeToken<I> {
    pub iri: Option<I>,
    pub sheet: Option<(Option<I>, I)>,
    pub from: (Option<I>, I),
    pub to: (Option<I>, I),
}

#[derive(Debug, PartialEq)]
pub struct CellCuboidToken<I> {
    pub iri: Option<I>,
    pub from_sheet: I,
    pub from: CellToken<I>,
    pub to_sheet: I,
    pub to: CellToken<I>,
}

#[derive(Debug, PartialEq)]
pub struct ColCuboidToken<I> {
    pub iri: Option<I>,
    pub from_sheet: I,
    pub from: (Option<I>, I),
    pub to_sheet: I,
    pub to: (Option<I>, I),
}

#[derive(Debug, PartialEq)]
pub struct RowCuboidToken<I> {
    pub iri: Option<I>,
    pub from_sheet: I,
    pub from: (Option<I>, I),
    pub to_sheet: I,
    pub to: (Option<I>, I),
}

#[derive(Debug, PartialEq)]
pub struct CellToken<I> {
    pub col: (Option<I>, I),
    pub row: (Option<I>, I),
}
