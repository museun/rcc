#[macro_export]
macro_rules! fail {
    ($($arg:tt)*) => {{
        eprintln!($($arg)*);
        if cfg!(test) || cfg!(feature="dump") {
            panic!();
        }
        ::std::process::exit(1);
    }};
}

#[derive(Clone, Copy)]
pub enum Color {
    Red,
    Green,
    Yellow,
    Cyan,
    Magenta,
    Blue,
    White,
}

impl Color {
    pub fn get(self) -> &'static str {
        match self {
            Color::Red => "\x1B[31m",
            Color::Green => "\x1B[32m",
            Color::Yellow => "\x1B[33m",
            Color::Cyan => "\x1B[36m",
            Color::Magenta => "\x1B[35m",
            Color::Blue => "\x1B[34m",
            Color::White => "\x1B[37m",
        }
    }

    pub fn reset() -> &'static str {
        "\x1B[m"
    }
}

#[macro_export]
macro_rules! wrap_color {
    //, $(arg:tt),* $(,)*

    // TODO fix this
    ($color:expr, $fmt:expr) => {{
        format!("{}{}{}", $color.get(), $fmt, Color::reset())
    }};

    ($color:expr, $fmt:expr, $($arg:tt)*) => {{
        format!("{}{}{}", $color.get(), format!($fmt, $($arg)*), Color::reset())
    }};
}
