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
    Blue,
    Magenta,
    Cyan,
    White,

    BrightRed,
    BrightGreen,
    BrightYellow,
    BrightBlue,
    BrightMagenta,
    BrightCyan,
    BrightWhite,
}

impl Color {
    pub fn len() -> usize {
        7 * 2
    }

    pub fn get(self) -> &'static str {
        match self {
            Color::Red => "\x1B[31m",
            Color::Green => "\x1B[32m",
            Color::Yellow => "\x1B[33m",
            Color::Blue => "\x1B[34m",
            Color::Magenta => "\x1B[35m",
            Color::Cyan => "\x1B[36m",
            Color::White => "\x1B[37m",

            Color::BrightRed => "\x1B[91m",
            Color::BrightGreen => "\x1B[92m",
            Color::BrightYellow => "\x1B[93m",
            Color::BrightBlue => "\x1B[94m",
            Color::BrightMagenta => "\x1B[95m",
            Color::BrightCyan => "\x1B[96m",
            Color::BrightWhite => "\x1B[97m",
        }
    }

    pub fn reset() -> &'static str {
        "\x1B[m"
    }
}

#[macro_export]
macro_rules! wrap_color {
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

use std::borrow::Cow;
use std::sync::atomic::{AtomicUsize, Ordering};

const IDENTATION: usize = 4;
static TRACER_DEPTH: AtomicUsize = AtomicUsize::new(0);

pub fn indent() {
    let _ = TRACER_DEPTH.fetch_add(IDENTATION, Ordering::Relaxed);
}

pub fn dedent() {
    let _ = TRACER_DEPTH.fetch_sub(IDENTATION, Ordering::Relaxed);
}

pub fn reset_level() {
    TRACER_DEPTH.store(0, Ordering::Relaxed)
}

pub fn level() -> usize {
    TRACER_DEPTH.load(Ordering::Relaxed)
}

#[macro_export]
macro_rules! tracer {
    ($e:expr, $($args:tt)*) => {{
         Tracer::new($e, &format!($($args)*))
    }};
}

pub struct Tracer<'a> {
    label: &'a str,
    pad: Cow<'a, str>,
}

impl<'a> Tracer<'a> {
    pub fn new(label: &'a str, data: &str) -> Self {
        let pad = ::std::iter::repeat(".")
            .take(level())
            .collect::<String>()
            .into();
        let next = COLORS[(level() + COLORS.len() - 1) % COLORS.len()];
        let lede = wrap_color!(next, "{}>", pad);
        eprintln!("{}{}: {}", lede, label, data);
        indent();
        Tracer { label, pad }
    }

    pub fn writeln(&self, data: &str) {
        let next = COLORS[(level() + COLORS.len() - 1) % COLORS.len()];
        let lede = wrap_color!(next, "{}?", self.pad);
        eprintln!("{}{}", lede, data);
    }
}

impl<'a> Drop for Tracer<'a> {
    fn drop(&mut self) {
        dedent();
        let next = COLORS[(level() + COLORS.len() - 1) % COLORS.len()];
        let lede = wrap_color!(next, "<{}", self.pad);
        eprintln!("{}{}", lede, self.label);
    }
}

static COLORS: [Color; 14] = [
    Color::Red,
    Color::Green,
    Color::Yellow,
    Color::Blue,
    Color::Magenta,
    Color::Cyan,
    Color::White,
    Color::BrightRed,
    Color::BrightGreen,
    Color::BrightYellow,
    Color::BrightBlue,
    Color::BrightMagenta,
    Color::BrightCyan,
    Color::BrightWhite,
];
