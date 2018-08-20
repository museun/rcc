#[macro_export]
macro_rules! fail {
    ($($arg:tt)*) => {{
        eprintln!($($arg)*);
        ::std::process::exit(1)
    }};
}
