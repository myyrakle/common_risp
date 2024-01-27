/*!
Common RISP is a LISP language implementation that operates embedded in Rust code.

## Get Started

The code below uses LISP format to calculate and print a numeric value.
```
fn main() {
    common_risp::compile!(
        (print 99)
        (print (+ 1 2 (* 3 4) 5))
    );
}
```

The execution result is as follows:
```bash
 Compiling common_risp v0.0.0 (/home/myyrakle/Codes/Rust/common_risp/common_risp)
    Finished dev [unoptimized + debuginfo] target(s) in 0.10s
     Running `target/debug/common_risp`
99
20
```

## Operators

RISP provides +, -, *, /, rem, mod as the basic arithmetic operators.
The behavior is almost identical to LISP.
```
fn main() {
    common_risp::compile!(
        (print (+ 1 2 3 4 5))
        (print (- 10 20))
        (print (* 10 20))
        (print (/ 40 20))
        (print (rem 10 3))
        (print (mod 10 3))
    );
}
```

Comparison operators are provided as <, >, <=, >=, =, and /=.
```
fn main() {
    common_risp::compile!(
        (print (< 10 20))
        (print (< 10 20 30))
        (print (> 10 20))
        (print (<= 10 20))
        (print (>= 10 20))
        (print (= 10 20))
        (print (/= 10 20))
    );
}
```

Logical operators and, or, and not are provided.
```
fn main() {
    common_risp::compile!(
        (print (and true false))
        (print (or true false))
        (print (not true))
    );
}
```


## Variables

RISP provides variable declaration functionality through the defvar and defparameter functions.

```
fn main() {
    common_risp::compile!(
        (defvar x 10)
        (defparameter y 20)
        (print (+ x y))
    );
}
```

And, you can change the value of a variable through setq.

```
fn main() {
    common_risp::compile!(
        (defvar x 10)
        (defparameter y 20)
        (print (+ x y))
        (setq x 30)
        (setq y 40)
        (print (+ x y))
    );
}
```

## Function
RISP provides function declaration functionality through the defun function.

```
fn main() {
    common_risp::compile!(
        (defun add (x:i32 y:i32) i32 (+ x y))
        (print (add 10 20))
    );
}
```
The difference between RISP and LISP is that parameter types and return types were added for type stability.
However, if there is no return type, it can be omitted.

```
fn main() {
    common_risp::compile!(
        (defun print_add (x:i32 y:i32) (print (+ x y)))
        (print_add 10 20)
    );
}
```

## IF
RISP provides an IF statement.


```
fn main() {
    common_risp::compile!(
        (defvar x 10)
        (defvar y 20)
        (if (< x y) (print "x is less than y") (print "x is greater than or equal to y"))
    );
}
```

## Interoperability

Variables and functions defined within RISP code can also be used outside RISP code.
```
fn main() {
    common_risp::compile!(
        (defvar x 10)
        (defun add (x:i32 y:i32) i32 (+ x y))
    );

    assert_eq!(x, 10);
    assert_eq!(add(10, 20), 30);
}
```

Also, the opposite is possible.
```
fn main() {
    let x = 10;

    common_risp::compile!(
        (defun add (x:i32 y:i32) i32 (+ x y))
        (print (add x 20))
    );
}
```
*/

#[allow(unused_imports)]
use crate as common_risp;

/// Compile RISP code and execute it.
pub use risp_macro::compile;

/// There is no need to use it outside of RISP code.
///
/// Print a value.
pub fn print<T: std::fmt::Display>(value: T) {
    println!("{}", value);
}

/// There is no need to use it outside of RISP code.
///
/// Performs modulo operations on numbers.
pub fn mod_<T>(lhs: T, rhs: T) -> T
where
    T: std::ops::Rem<Output = T>
        + std::ops::Add<Output = T>
        + std::ops::AddAssign
        + std::cmp::PartialOrd<T>
        + std::default::Default
        + Copy,
{
    let mut result = lhs % rhs;
    if result < T::default() {
        result += rhs;
    }
    result
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

        let result = compile!(
            (rem -1 5)
        );

        assert_eq!(result, -1);
    }

    #[test]
    fn test_mod_expression() {
        let result = compile!(
            (mod 10 3)
        );

        assert_eq!(result, 1);

        let result = compile!(
            (mod -1 5)
        );

        assert_eq!(result, 4);
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

#[cfg(test)]
mod test_variables {
    use super::*;

    #[test]
    fn test_defvar() {
        compile!(
            (defvar x 10)
        );

        assert_eq!(x, 10);
    }

    #[test]
    fn test_defparameter() {
        compile!(
            (defparameter x 10)
        );

        assert_eq!(x, 10);
    }

    #[test]
    fn test_setq() {
        compile!(
            (defparameter x 10)
        );

        assert_eq!(x, 10);

        compile!(
            (setq x 20)
        );

        assert_eq!(x, 20);
    }
}

#[cfg(test)]
mod test_functions {
    use super::*;

    #[test]
    fn test_defun() {
        compile!(
            (defun add (x:i32 y:i32) i32 (+ x y))
        );

        assert_eq!(add(10, 20), 30);
    }

    #[test]
    fn test_defun_with_no_return_type() {
        compile!(
            (defun add () (print "Hello, World!"))
        );
    }

    #[test]
    fn test_defun_with_no_args() {
        compile!(
            (defun add () i32 (+ 10 20))
        );

        assert_eq!(add(), 30);
    }

    #[test]
    fn test_defun_with_multiline() {
        compile!(
            (defun add () i32 (defvar x 10) (defvar y 20) (+ x y))
        );

        assert_eq!(add(), 30);
    }
}

#[cfg(test)]
mod test_scope {
    use super::*;

    #[test]
    fn test_scope() {
        mod foo {
            pub fn add(lhs: i32, rhs: i32) -> i32 {
                lhs + rhs
            }
        }

        let result = compile!(
            (foo::add 10 20)
        );

        assert_eq!(result, 30);
    }
}

#[cfg(test)]
mod test_if {
    use super::*;

    #[test]
    fn test_if() {
        let result = compile!(
            (if true 10 20)
        );

        assert_eq!(result, 10);
    }

    #[test]
    fn test_if_false() {
        let result = compile!(
            (if false 10 20)
        );

        assert_eq!(result, 20);
    }
}
