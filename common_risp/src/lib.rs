pub use risp_macro::compile;

pub fn print<T: std::fmt::Display>(value: T) {
    println!("{}", value);
}
