pub struct Position {
    /// 1-indexed row.
    pub row: usize,
    /// 1-indexed column.
    pub column: usize,
}

impl Position {
    pub fn start() -> Position {
        Position {
            row: 1,
            column: 1,
        }
    }
}
