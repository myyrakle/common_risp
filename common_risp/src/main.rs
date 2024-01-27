fn main() {
    let result = common_risp::compile!(
        (+ 10 20)
    );
    println!("{}", result);
}
