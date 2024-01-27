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

    #[test]
    fn test_rem_expression() {
        let result = compile!(
            (rem 10 3)
        );

        assert_eq!(result, 1);
    }
}

#[cfg(test)]
mod test_comparison {
    use super::*;

    #[test]
    fn test_less_than_expression() {
        let result = compile!(
            (< 10 20)
        );

        assert_eq!(result, true);
    }

    #[test]
    fn test_greater_than_expression() {
        let result = compile!(
            (> 10 20)
        );

        assert_eq!(result, false);
    }

    #[test]
    fn test_less_than_or_equal_to_expression() {
        let result = compile!(
            (<= 10 20)
        );

        assert_eq!(result, true);
    }

    #[test]
    fn test_greater_than_or_equal_to_expression() {
        let result = compile!(
            (>= 10 20)
        );

        assert_eq!(result, false);
    }

    #[test]
    fn test_equal_to_expression() {
        let result = compile!(
            (= 10 20)
        );

        assert_eq!(result, false);
    }

    #[test]
    fn test_not_equal_to_expression() {
        let result = compile!(
            (/= 10 20)
        );

        assert_eq!(result, true);
    }

    #[test]
    fn test_less_than_expression_three_args() {
        let result = compile!(
            (< 10 20 30)
        );

        assert_eq!(result, true);
    }
}

#[cfg(test)]
mod test_boolean {
    use super::*;

    #[test]
    fn test_and_expression() {
        let result = compile!(
            (and true false)
        );

        assert_eq!(result, false);
    }

    #[test]
    fn test_or_expression() {
        let result = compile!(
            (or true false)
        );

        assert_eq!(result, true);
    }

    #[test]
    fn test_not_expression() {
        let result = compile!(
            (not true)
        );

        assert_eq!(result, false);
    }
}
