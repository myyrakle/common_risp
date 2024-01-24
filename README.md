# common_risp

Provides a representation of LISP through macro expansion.


## Get Started 

The code below uses LISP format to calculate and print a numeric value.
```
fn main() {
    common_risp::compile!(
        (print 99)
        (print (+ 1 2 (*3 4) 5))
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