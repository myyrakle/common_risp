pub use risp_macro::compile;

pub fn print<T: std::fmt::Display>(value: T) {
    println!("{}", value);
}

#[cfg(test)]
mod test_arithmetic {
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

    #[test]
    fn test_multiply_expression() {
        let result = compile!(
            (* 10 20)
        );

        assert_eq!(result, 200);
    }

    #[test]
    fn test_divide_expression() {
        let result = compile!(
            (/ 40 20)
        );

        assert_eq!(result, 2);
    }

    #[test]
    fn test_add_subtract_expression() {
        let result = compile!(
            (+ 10 (- 20 5))
        );

        assert_eq!(result, 25);
    }
}
