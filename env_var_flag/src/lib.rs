pub fn env_var_flag(name: &'static str) -> bool {
    std::env::var(name)
        .map(|var| {
            if var.is_empty() {
                false
            } else if var.eq_ignore_ascii_case("true") {
                true
            } else if let Ok(value) = var.parse::<usize>() {
                value > 0
            } else {
                false
            }
        })
        .unwrap_or(false)
}
