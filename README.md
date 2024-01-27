# common_risp

![](https://img.shields.io/badge/language-Rust-red) ![](https://img.shields.io/badge/version-0.1.1-brightgreen) [![GitHub license](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/myyrakle/common_risp/blob/master/LICENSE)

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
```
 Compiling common_risp v0.0.0 (/home/myyrakle/Codes/Rust/common_risp/common_risp)
    Finished dev [unoptimized + debuginfo] target(s) in 0.10s
     Running `target/debug/common_risp`
99
20
```

## More Details

Please refer to [the documentation](https://docs.rs/common_risp/latest/common_risp) for more details.