fn main() {
    common_risp::compile!(
        (print 99)
        (print (+ 1 2 (*3 4) 5))
    );
}
