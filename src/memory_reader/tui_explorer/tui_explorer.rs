use crate::memory_reader::{Error, Result, SigintHandler};

use super::{DetailView, MemoryTable, RunningLog, TerminalContext};

use crossterm::event::Event;
use tui::{
    backend::Backend,
    layout::{Constraint, Direction, Layout},
    Frame,
};

pub struct TuiExplorer {
    _pid: u32,
    running_log: RunningLog,
    memory_table: MemoryTable,
    detail_view: DetailView,
}

impl TuiExplorer {
    pub fn new(pid: u32) -> Result<Self> {
        let mut detail_view = DetailView::new();
        detail_view.load_details(
            vec!["a", "b"]
                .into_iter()
                .enumerate()
                .map(|(i, name)| (name.to_string(), i.to_string())),
        );
        Ok(Self {
            _pid: pid,
            running_log: RunningLog::new(100),
            memory_table: MemoryTable::new(pid)?,
            detail_view,
        })
    }

    pub fn run(&mut self) -> Result<()> {
        use crossterm::event;

        let mut context = TerminalContext::new()?;
        let handler = SigintHandler::new();

        while !handler.received() {
            context
                .draw(|frame| self.draw(frame))
                .map_err(|err| Error::TuiIoError { err })?;

            let timeout = std::time::Duration::from_millis(100);
            let poll = event::poll(timeout)
                .map_err(|err| Error::TuiIoError { err })?;

            if poll {
                let event_received =
                    event::read().map_err(|err| Error::TuiIoError { err })?;
                let should_exit = self.update_state(event_received)?;
                if should_exit {
                    break;
                }
            }
        }
        Ok(())
    }

    fn draw<B: Backend>(&mut self, frame: &mut Frame<B>) {
        let chunks = Layout::default()
            .direction(Direction::Horizontal)
            .margin(1)
            .constraints(
                [
                    Constraint::Percentage(20),
                    Constraint::Percentage(60),
                    Constraint::Percentage(20),
                ]
                .as_ref(),
            )
            .split(frame.size());

        self.running_log.draw(frame, chunks[0]);
        self.memory_table.draw(frame, chunks[1]);
        self.detail_view.draw(frame, chunks[2]);
    }

    fn update_state(&mut self, event: Event) -> Result<bool> {
        use crossterm::event::{KeyCode, KeyModifiers};
        if let Event::Key(key) = event {
            match key.code {
                KeyCode::Char('c')
                    if key.modifiers.contains(KeyModifiers::CONTROL) =>
                {
                    return Ok(true)
                }
                KeyCode::Down => {
                    self.memory_table.move_selection_down();
                }
                KeyCode::Up => self.memory_table.move_selection_up(),
                _ => {
                    self.running_log.add_log(format!(
                        "{:?}, {:?}",
                        key.code, key.modifiers
                    ));
                }
            }
        }
        Ok(false)
    }
}
