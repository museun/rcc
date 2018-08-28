#[macro_export]
macro_rules! fail {
    ($($arg:tt)*) => {{
        eprintln!("{}: {}", wrap_color!(Color::Red{}, "failure"), format!($($arg)*));
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

/// gets +/- `width` around the `cursor`
pub fn midpoint(input: &str, cursor: usize, width: usize) -> (&str, usize) {
    let half = width / 2;
    if input.len() > width {
        if cursor < half {
            (&input[..half], cursor)
        } else {
            (&input[cursor - half..], half)
        }
    } else {
        (input, cursor)
    }
}

pub fn draw_caret(width: usize, color: Color) {
    let s = ::std::iter::repeat(" ").take(width).collect::<String>();
    eprintln!("{}{}", s, wrap_color!(color, "^"));
}

pub fn count_digits(n: usize) -> usize {
    if n == 0 {
        return 1;
    }

    let mut n = n;
    let mut x = 0;
    while n > 0 {
        n /= 10;
        x += 1;
    }
    x
}

/// rounds to the alignment
pub fn round(x: i32, align: i32) -> i32 {
    (x + align - 1) & !(align - 1)
}

pub fn join_with<S, I, T>(mut iter: I, sep: S) -> String
where
    S: AsRef<str>,
    T: AsRef<str>,
    I: Iterator<Item = T>,
{
    let mut buf = String::new();
    if let Some(s) = iter.next() {
        buf.push_str(s.as_ref());
    }
    for i in iter {
        buf.push_str(sep.as_ref());
        buf.push_str(i.as_ref());
    }
    buf
}
