use std::{
    collections::VecDeque,
    time::{Duration, Instant},
};

pub struct RateCounter {
    events: VecDeque<Instant>,
    max_history: Duration,
}

impl RateCounter {
    pub fn new() -> Self {
        Self {
            events: Default::default(),
            max_history: Duration::from_secs(5),
        }
    }

    pub fn max_history(self, max_history: Duration) -> Self {
        Self {
            max_history,
            ..self
        }
    }

    fn pre_history_size(&self, now: Instant) -> usize {
        self.events
            .partition_point(|event| *event < now - self.max_history)
    }

    pub fn record_event(&mut self) {
        let now = Instant::now();
        let pre_history = self.pre_history_size(now);
        for _ in 0..pre_history {
            self.events.pop_front();
        }

        self.events.push_back(now);
    }

    pub fn rate_per_second(&self) -> f32 {
        let pre_history = self.pre_history_size(Instant::now());
        let events = self.events.len() - pre_history;

        (events as f32) / self.max_history.as_secs_f32()
    }
}
