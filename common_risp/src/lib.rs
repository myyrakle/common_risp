pub use risp_macro::compile;

pub fn print<T: std::fmt::Display>(value: T) {
    println!("{}", value);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add_expression() {
        let result = compile!(
            (+ 10 20)
        );

        assert_eq!(result, 30);
    }

    #[test]
    fn test_subtract_expression() {
        let result = compile!(
            (- 10 20)
        );

        assert_eq!(result, -10);
    }
}
