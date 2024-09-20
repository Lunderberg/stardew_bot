use std::path::PathBuf;

use thiserror::Error;

use serde::{Deserialize, Serialize};

fn xdg_home() -> PathBuf {
    std::env::var("XDG_CONFIG_HOME")
        .map(|dir| -> PathBuf { dir.into() })
        .unwrap_or_else(|_| {
            let home: PathBuf = std::env::var("HOME")
                .expect("Could not find $HOME environment var")
                .into();
            home.join(".config")
        })
}

#[derive(Default, Serialize, Deserialize)]
pub(crate) struct UserConfig {
    pub(crate) object_explorer_sort_top: Vec<String>,
    pub(crate) object_explorer_sort_bottom: Vec<String>,

    #[serde(default)]
    pub(crate) object_explorer_alias: Vec<(String, String)>,
}

impl UserConfig {
    fn default_save_location() -> PathBuf {
        xdg_home().join("stardew_bot/user_config.json")
    }

    pub(crate) fn load_default() -> Result<Self, Error> {
        let file_location = Self::default_save_location();

        let config = if file_location.exists() {
            let config_str = std::fs::read_to_string(file_location)?;
            serde_json::from_str(&config_str)?
        } else {
            Self::default()
        };

        Ok(config)
    }

    pub(crate) fn save_to_default_location(&self) -> Result<(), Error> {
        let config_str = serde_json::to_string_pretty(self)?;

        let file_location = Self::default_save_location();

        std::fs::create_dir_all(
            file_location
                .parent()
                .expect("Save location is not the root directory"),
        )?;

        std::fs::write(file_location, config_str)?;

        Ok(())
    }
}

#[derive(Error)]
pub enum Error {
    #[error("std::io::Error {0}")]
    IOError(#[from] std::io::Error),

    #[error("serde_json::Error {0}")]
    SerdeError(#[from] serde_json::Error),
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}
