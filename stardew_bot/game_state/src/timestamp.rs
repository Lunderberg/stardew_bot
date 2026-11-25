use std::fmt::Display;

/// Stardew stores times throughout the day as (100*hour + minute), so
/// that they show up as a human-readable HHMM when printed.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct Timestamp(pub i32);

/// The difference between two timestamps
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct TimeDelta {
    minutes: i32,
}

impl Timestamp {
    pub fn new(timestamp: i32) -> Self {
        Self(timestamp)
    }

    pub fn hours(&self) -> i32 {
        self.0 / 100
    }

    pub fn minutes(&self) -> i32 {
        self.0 % 100
    }

    fn total_minutes(&self) -> i32 {
        self.hours() * 60 + self.minutes()
    }

    fn from_total_minutes(total_minutes: i32) -> Self {
        let hours = total_minutes / 60;
        let minutes = total_minutes % 60;
        Self(hours * 100 + minutes)
    }

    pub fn raw(&self) -> i32 {
        self.0
    }
}

impl TimeDelta {
    pub fn from_minutes(minutes: i32) -> Self {
        TimeDelta { minutes }
    }

    pub fn from_hours(hours: i32) -> Self {
        Self::from_minutes(hours * 60)
    }

    pub fn minutes(&self) -> i32 {
        self.minutes
    }
}

impl From<i32> for Timestamp {
    fn from(timestamp: i32) -> Self {
        Timestamp::new(timestamp)
    }
}

impl std::ops::Sub for Timestamp {
    type Output = TimeDelta;

    fn sub(self, rhs: Self) -> Self::Output {
        TimeDelta::from_minutes(self.total_minutes() - rhs.total_minutes())
    }
}

impl std::ops::Add<TimeDelta> for Timestamp {
    type Output = Timestamp;

    fn add(self, rhs: TimeDelta) -> Self::Output {
        Self::from_total_minutes(self.total_minutes() + rhs.minutes())
    }
}

impl std::ops::Add<Timestamp> for TimeDelta {
    type Output = Timestamp;

    fn add(self, rhs: Timestamp) -> Self::Output {
        rhs + self
    }
}

impl std::ops::Sub<TimeDelta> for Timestamp {
    type Output = Timestamp;

    fn sub(self, rhs: TimeDelta) -> Self::Output {
        Self::from_total_minutes(self.total_minutes() - rhs.minutes())
    }
}

impl std::ops::Add for TimeDelta {
    type Output = TimeDelta;

    fn add(self, rhs: Self) -> Self::Output {
        TimeDelta::from_minutes(self.minutes() + rhs.minutes())
    }
}
impl std::ops::Sub for TimeDelta {
    type Output = TimeDelta;

    fn sub(self, rhs: Self) -> Self::Output {
        TimeDelta::from_minutes(self.minutes() - rhs.minutes())
    }
}

impl std::ops::Mul<i32> for TimeDelta {
    type Output = TimeDelta;

    fn mul(self, rhs: i32) -> Self::Output {
        TimeDelta::from_minutes(self.minutes() * rhs)
    }
}
impl std::ops::Mul<TimeDelta> for i32 {
    type Output = TimeDelta;

    fn mul(self, rhs: TimeDelta) -> Self::Output {
        rhs * self
    }
}

impl Display for Timestamp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:02}:{:02}", self.hours(), self.minutes())
    }
}

impl Display for TimeDelta {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let hours = self.minutes / 60;
        let minutes = self.minutes % 60;
        if hours == 0 {
            write!(f, "{minutes}m")
        } else {
            write!(f, "{hours}h{minutes:02}m")
        }
    }
}
