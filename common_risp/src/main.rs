fn main() {
    common_risp::compile!(
        (print 99)
        (print (+ 1 2 (* 3 4) 5))
    );

    let result = common_risp::compile!(
        (+ 10 20)
    );
    println!("{}", result);
}
