fn main() {
    common_risp::compile!(
        (print 99)
        (print (+ 1 2 (*3 4) 5))
        (print (>= 10 20 30))
        (print (not(and true false)))
        (print (rem 1 5))
    );
}
