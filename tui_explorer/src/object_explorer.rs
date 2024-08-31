use dotnet_debugger::{PersistentState, RuntimeObject, TypedPointer};
use ratatui::{text::Line, widgets::Widget as _};

use crate::{extended_tui::WidgetWindow, Error};

pub struct ObjectExplorer {
    state: PersistentState,
    top_object: Option<TypedPointer<RuntimeObject>>,
}

impl ObjectExplorer {
    pub fn new(top_object: Option<TypedPointer<RuntimeObject>>) -> Self {
        ObjectExplorer {
            state: PersistentState::new(),
            top_object,
        }
    }
}

impl WidgetWindow for ObjectExplorer {
    fn title(&self) -> std::borrow::Cow<str> {
        "ObjectExplorer".into()
    }

    fn draw<'a>(
        &'a mut self,
        globals: crate::extended_tui::WidgetGlobals<'a>,
        area: ratatui::layout::Rect,
        buf: &mut ratatui::prelude::Buffer,
    ) {
        let Some(object_ptr) = self.top_object else {
            return;
        };

        let res = || -> Result<_, Error> {
            let mut reader = self.state.cached_reader(globals.reader);

            let top_object = reader.object(object_ptr)?;

            let class_name: Line = reader.class_name(&top_object)?.into();

            let widget: ratatui::widgets::List = std::iter::once(class_name)
                .chain(
                    reader
                        .iter_fields(&top_object)?
                        .into_iter()
                        .map(Into::into),
                )
                .collect();

            widget.render(area, buf);
            Ok(())
        }();

        res.unwrap()
    }
}
