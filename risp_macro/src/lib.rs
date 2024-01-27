use proc_macro::{Group, TokenStream, TokenTree};

fn replace_spetial_function(name: &str) -> String {
    match name {
        "print" => "common_risp::print".to_string(),
        "mod" => "common_risp::mod_".to_string(),
        _ => name.to_string(),
    }
}

enum ProcessedToken {
    String(String),
    Group(Group),
}

fn preprocess_tokens(tokens: Vec<TokenTree>) -> Vec<ProcessedToken> {
    let mut processed_tokens = vec![];

    let mut token_iter = tokens.into_iter().peekable();

    while let Some(token) = token_iter.next() {
        match token {
            TokenTree::Group(group) if group.delimiter() == proc_macro::Delimiter::Parenthesis => {
                processed_tokens.push(ProcessedToken::Group(group));
            }
            _ => {
                let mut token = token.to_string();

                // 마이너스 부호 처리
                match token.as_str() {
                    "-" => {
                        let next_token = token_iter
                            .next()
                            .expect("There must be a number after the minus sign.")
                            .to_string();

                        if next_token.parse::<i128>().is_err() && next_token.parse::<f64>().is_err()
                        {
                            panic!("There must be a number after the minus sign.");
                        }

                        token.push_str(next_token.as_str());
                    }
                    _ => {}
                }

                // 식별자 내 :: 연산자 처리
                while let Some(peeked) = token_iter.peek() {
                    match peeked.to_string().as_str() {
                        ":" => {
                            token_iter.next().unwrap();

                            if let Some(expect_second_colon) = token_iter.next() {
                                if expect_second_colon.to_string() != ":" {
                                    panic!(
                                        "The only operator that can occur in an identifier expression is ::"
                                    );
                                }
                            } else {
                                panic!("The only operator that can occur in an identifier expression is ::");
                            }

                            token.push_str("::");

                            let next_token = token_iter
                                .next()
                                .expect("The :: operator must be followed by an identifier.")
                                .to_string();

                            token.push_str(next_token.as_str());
                        }
                        _ => {
                            break;
                        }
                    }
                }

                processed_tokens.push(ProcessedToken::String(token));
            }
        }
    }

    processed_tokens
}

fn compile_expression(expression: Group) -> String {
    let mut group_iter = expression.stream().into_iter().peekable();

    let function_name = group_iter
        .next()
        .expect("Every expression must have a function name.")
        .to_string();
    let mut function_name = function_name;

    // Handles special operator patterns.
    if function_name == ">" || function_name == "<" || function_name == "/" {
        let next_token = group_iter.peek().unwrap().to_string();
        if next_token == "=" {
            group_iter.next();
            function_name += next_token.as_str();
        }
    }

    // Standardize function names.
    while let Some(peeked) = group_iter.peek() {
        match peeked.to_string().as_str() {
            ":" => {
                group_iter.next().unwrap();

                if let Some(expect_second_colon) = group_iter.next() {
                    if expect_second_colon.to_string() != ":" {
                        panic!(
                            "The only operator that can occur in an identifier expression is ::"
                        );
                    }
                } else {
                    panic!("The only operator that can occur in an identifier expression is ::");
                }

                function_name += "::";

                let next_token = group_iter
                    .next()
                    .expect("The :: operator must be followed by an identifier.")
                    .to_string();

                function_name += next_token.as_str();
            }
            _ => {
                break;
            }
        }
    }

    let tokens = group_iter.collect::<Vec<_>>();

    let tokens = preprocess_tokens(tokens);

    let mut rust_code = "".to_string();

    match function_name.as_str() {
        "+" | "-" | "*" | "/" => {
            let mut operand = vec![];

            for token in tokens {
                match token {
                    ProcessedToken::Group(group) => {
                        operand.push(compile_expression(group));
                    }
                    ProcessedToken::String(token) => {
                        operand.push(token);
                    }
                }
            }

            rust_code.push_str(&format!("({})", operand.join(function_name.as_str())));
        }
        "<" | ">" | "<=" | ">=" | "=" | "/=" => {
            let mut operand = vec![];

            let operator = match function_name.as_str() {
                "<" => "<",
                ">" => ">",
                "<=" => "<=",
                ">=" => ">=",
                "=" => "==",
                "/=" => "!=",
                _ => unreachable!(),
            };

            for token in tokens {
                match token {
                    ProcessedToken::Group(group) => {
                        operand.push(compile_expression(group));
                    }
                    ProcessedToken::String(token) => {
                        operand.push(token);
                    }
                }
            }

            if operand.len() < 2 {
                panic!("{} must have at least two operands.", function_name);
            }

            let mut binary_expressions = vec![];
            for i in 0..operand.len() - 1 {
                binary_expressions.push(format!(
                    "({} {} {})",
                    operand[i],
                    operator,
                    operand[i + 1]
                ));
            }

            rust_code.push_str(&format!("({})", binary_expressions.join("&&")));
        }
        "rem" => {
            let mut operand = vec![];

            for token in tokens {
                match token {
                    ProcessedToken::Group(group) => {
                        operand.push(compile_expression(group));
                    }
                    ProcessedToken::String(token) => {
                        operand.push(token);
                    }
                }
            }

            if operand.len() != 2 {
                panic!("{} must have two operands.", function_name);
            }

            rust_code.push_str(&format!("({} % {})", operand[0], operand[1]));
        }
        "and" | "or" => {
            let mut operand = vec![];

            let operator = match function_name.as_str() {
                "and" => "&&",
                "or" => "||",
                _ => unreachable!(),
            };

            for token in tokens {
                match token {
                    ProcessedToken::Group(group) => {
                        operand.push(compile_expression(group));
                    }
                    ProcessedToken::String(token) => {
                        operand.push(token);
                    }
                }
            }

            if operand.len() < 2 {
                panic!("{} must have at least two operands.", function_name);
            }

            rust_code.push_str(&format!("({})", operand.join(operator)));
        }
        "not" => {
            let mut operand = vec![];

            for token in tokens {
                match token {
                    ProcessedToken::Group(group) => {
                        operand.push(compile_expression(group));
                    }
                    ProcessedToken::String(token) => {
                        operand.push(token);
                    }
                }
            }

            if operand.len() != 1 {
                panic!("{} must have one operand.", function_name);
            }

            rust_code.push_str(&format!("(!{})", operand[0]));
        }
        "defparameter" | "defvar" => {
            if tokens.len() != 2 {
                panic!("{} must have two operands.", function_name);
            }

            let variable_name = match &tokens[0] {
                ProcessedToken::String(token) => token,
                _ => panic!(
                    "The first operand of {} must be a variable name.",
                    function_name
                ),
            };

            let value = match &tokens[1] {
                ProcessedToken::Group(group) => compile_expression(group.to_owned()),
                ProcessedToken::String(token) => token.to_string(),
            };

            rust_code.push_str(&format!("let mut {variable_name} = {value};\n"));
        }
        "defun" => {
            panic!("defun is not supported.")
        }
        _ => {
            let function_name = replace_spetial_function(function_name.as_str());

            let mut parameters = vec![];

            for token in tokens {
                match token {
                    ProcessedToken::Group(group) => {
                        parameters.push(compile_expression(group));
                    }
                    ProcessedToken::String(token) => {
                        parameters.push(token);
                    }
                }
            }

            rust_code.push_str(&format!("{}({})", function_name, parameters.join(", ")));
        }
    }

    rust_code
}

fn compile_root_expression(token_tree: TokenTree) -> String {
    match token_tree {
        TokenTree::Group(group) if group.delimiter() == proc_macro::Delimiter::Parenthesis => {
            let mut rust_code = "".to_string();

            rust_code.push_str(&compile_expression(group));

            rust_code
        }
        _ => panic!("Every root expression must be a pair of parentheses."),
    }
}

#[proc_macro]
pub fn compile(item: TokenStream) -> TokenStream {
    let mut rust_lines = vec![];

    for token in item {
        rust_lines.push(compile_root_expression(token));
    }

    let rust_code = rust_lines.join(";\n");
    println!("{}", rust_code);

    rust_code.parse().unwrap()
}
