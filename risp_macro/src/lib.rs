use proc_macro::{TokenStream, TokenTree};

fn replace_spetial_function(name: &str) -> String {
    match name {
        "print" => "common_risp::print".to_string(),
        _ => name.to_string(),
    }
}

fn compile_expression(function_name: &str, tokens: Vec<TokenTree>) -> String {
    let mut rust_code = "".to_string();

    match function_name {
        "defparameter" => {
            panic!("defparameter is not supported.")
        }
        "defun" => {
            panic!("defun is not supported.")
        }
        _ => {
            let function_name = replace_spetial_function(function_name);

            let mut parameters = vec![];

            for token in tokens {
                match token {
                    TokenTree::Group(group)
                        if group.delimiter() == proc_macro::Delimiter::Parenthesis =>
                    {
                        parameters.push(compile_expression(
                            &group.stream().into_iter().next().unwrap().to_string(),
                            group.stream().into_iter().skip(1).collect::<Vec<_>>(),
                        ));
                    }
                    _ => {
                        parameters.push(token.to_string());
                    }
                }
            }

            rust_code.push_str(&format!("{}({});\n", function_name, parameters.join(", ")));
        }
    }

    rust_code
}

fn compile_root_expression(token_tree: TokenTree) -> String {
    match token_tree {
        TokenTree::Group(group) if group.delimiter() == proc_macro::Delimiter::Parenthesis => {
            let mut rust_code = "".to_string();

            let mut group_iter = group.stream().into_iter();

            let function_name = group_iter
                .next()
                .expect("Every root expression must have a function name.")
                .to_string();

            let parameters = group_iter.collect::<Vec<_>>();

            rust_code.push_str(&compile_expression(&function_name, parameters));

            rust_code
        }
        _ => panic!("Every root expression must be a pair of parentheses."),
    }
}

#[proc_macro]
pub fn compile(item: TokenStream) -> TokenStream {
    let mut rust_code = "".to_string();

    for token in item {
        rust_code.push_str(&compile_root_expression(token));
    }

    rust_code.parse().unwrap()
}
