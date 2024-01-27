fn main() {
    common_risp::compile!(
        (defun add (x:i32 y:i32) i32 (+ x y))
    );

    assert_eq!(add(10, 20), 30);
}
