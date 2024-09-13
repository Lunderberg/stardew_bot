use ratatui::{
    buffer::Buffer,
    layout::{Constraint, Layout, Rect},
    style::{Style, Styled},
    text::{Line, Text},
    widgets::{StatefulWidget, TableState, Widget},
};

pub struct DynamicTable<'a, FGenerateCell> {
    generate_cell: FGenerateCell,
    num_rows: usize,
    widths: Vec<Constraint>,
    header: Option<Vec<Cell<'a>>>,
    header_margin: u16,
    header_style: Option<Style>,
    highlight_symbol: Text<'a>,
    highlight_style: Style,
    column_spacing: u16,
}

pub struct Cell<'a> {
    content: Text<'a>,
    style: Style,
}

impl<'a> Cell<'a> {
    pub fn new<T>(content: impl Into<Text<'a>>) -> Self {
        Self {
            content: content.into(),
            style: Style::default(),
        }
    }

    pub fn style<S: Into<Style>>(self, style: S) -> Self {
        Self {
            style: style.into(),
            ..self
        }
    }
}

impl<'a, T> From<T> for Cell<'a>
where
    T: Into<Text<'a>>,
{
    fn from(content: T) -> Self {
        Self::new::<T>(content)
    }
}

impl<'a> Styled for Cell<'a> {
    type Item = Self;

    fn style(&self) -> Style {
        self.style
    }

    fn set_style<S: Into<Style>>(self, style: S) -> Self::Item {
        self.style(style)
    }
}

impl<'a, FGenerateCell> DynamicTable<'a, FGenerateCell> {
    pub fn new(
        generate_cell: FGenerateCell,
        num_rows: usize,
        widths: Vec<Constraint>,
    ) -> Self {
        Self {
            generate_cell,
            num_rows,
            widths,
            header: Default::default(),
            header_margin: 1,
            header_style: None,
            highlight_symbol: Default::default(),
            highlight_style: Default::default(),
            column_spacing: 1,
        }
    }

    pub fn header(self, header: Vec<Cell<'a>>) -> Self {
        Self {
            header: Some(header),
            ..self
        }
    }

    pub fn header_style(self, header_style: impl Into<Style>) -> Self {
        Self {
            header_style: Some(header_style.into()),
            ..self
        }
    }

    pub fn highlight_symbol<T: Into<Text<'a>>>(
        self,
        highlight_symbol: T,
    ) -> Self {
        Self {
            highlight_symbol: highlight_symbol.into(),
            ..self
        }
    }

    pub fn highlight_style(self, highlight_style: impl Into<Style>) -> Self {
        Self {
            highlight_style: highlight_style.into(),
            ..self
        }
    }

    pub fn column_spacing(self, column_spacing: u16) -> Self {
        Self {
            column_spacing,
            ..self
        }
    }

    fn layout(&self, area: Rect) -> (Rect, Rect) {
        let header_top_margin = 0;
        let header_height = 1;
        let header_bottom_margin = self.header_margin;

        let layout = Layout::vertical([
            Constraint::Length(header_top_margin),
            Constraint::Length(header_height),
            Constraint::Length(header_bottom_margin),
            Constraint::Min(0),
        ])
        .split(area);
        let (header_area, rows_area) = (layout[1], layout[3]);
        (header_area, rows_area)
    }

    fn selection_width(&self) -> u16 {
        self.highlight_symbol.width() as u16
    }

    fn get_column_widths(
        &self,
        max_width: u16,
        selection_width: u16,
    ) -> Vec<(u16, u16)> {
        let [_selection_area, columns_area] = Layout::horizontal([
            Constraint::Length(selection_width),
            Constraint::Fill(0),
        ])
        .areas(Rect::new(0, 0, max_width, 1));
        let rects = Layout::horizontal(self.widths.clone())
            // .flex(self.flex)
            .spacing(self.column_spacing)
            .split(columns_area);
        rects.iter().map(|c| (c.x, c.width)).collect()
    }
}

impl<'a> Widget for Cell<'a> {
    fn render(self, area: Rect, buf: &mut Buffer)
    where
        Self: Sized,
    {
        buf.set_style(area, self.style);
        self.content.render(area, buf);
    }
}

impl<'a, 'b, FGenerateCell> StatefulWidget for DynamicTable<'a, FGenerateCell>
where
    FGenerateCell: Fn(usize, usize) -> Option<Line<'b>>,
{
    type State = TableState;

    fn render(
        self,
        area: ratatui::prelude::Rect,
        buf: &mut ratatui::prelude::Buffer,
        state: &mut Self::State,
    ) {
        let table_area = area;

        let selection_width = self.selection_width();
        let column_widths =
            self.get_column_widths(table_area.width, selection_width);

        let (header_area, rows_area) = self.layout(table_area);

        if let Some(style) = self.header_style {
            buf.set_style(header_area, style);
        }

        if let Some(header) = self.header {
            for ((x, width), cell) in
                column_widths.iter().zip(header.into_iter())
            {
                cell.render(
                    Rect::new(
                        header_area.x + x,
                        header_area.y,
                        *width,
                        header_area.height,
                    ),
                    buf,
                );
            }
        }

        if let Some(selected) = state.selected_mut() {
            *selected = (*selected).min(self.num_rows.saturating_sub(1));
        }

        if let Some(selected) = state.selected() {
            *state.offset_mut() = {
                let padding = 2;
                let height = rows_area.height as usize;

                let offset = state.offset();

                offset
                    // Move the view window upward to contain selection
                    .min(selected.saturating_sub(padding))
                    // Move the view window downward to contain selection
                    .max((selected + padding + 1).saturating_sub(height))
                    // But not so far that it would show empty rows at the bottom
                    .min(self.num_rows.saturating_sub(height))
            };

            let row_area = Rect::new(
                rows_area.x,
                rows_area.y + ((selected - state.offset()) as u16),
                rows_area.width,
                1,
            );
            let cell_area = Rect::new(
                row_area.x,
                row_area.y,
                selection_width,
                row_area.height,
            );
            buf.set_style(row_area, self.highlight_style);
            self.highlight_symbol.render(cell_area, buf);
        }

        for view_row in 0..rows_area.height {
            let table_row = (view_row as usize) + state.offset();
            let row_area = Rect::new(
                rows_area.x,
                rows_area.y + view_row,
                rows_area.width,
                1,
            );

            for (i_column, (column_start, column_width)) in
                column_widths.iter().enumerate()
            {
                if let Some(cell) = (self.generate_cell)(i_column, table_row) {
                    let cell_area = Rect::new(
                        row_area.x + column_start,
                        row_area.y,
                        *column_width,
                        row_area.height,
                    );
                    cell.render(cell_area, buf);
                }
            }
        }

        // todo!()
    }
}
