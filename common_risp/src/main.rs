use risp_macro::compile;

fn main() {
    compile!(
        println!("Hello, world!");
        {
            let x = 1;
            let y = 2;
            println!("x + y = {}", x + y);
        }
    );

    println!("Hello, world!");
}
