use crate::lisp::scrunch::LispVal;

fn str_to_val(str: String) -> Option<LispVal>{
    match str.as_str() {
        "+" => Some(LispVal::Ident("+".to_string())),
        "*" => Some(LispVal::Ident("*".to_string())),
        "-" => Some(LispVal::Ident("-".to_string())),
        "/" => Some(LispVal::Ident("/".to_string())),
        "list" => Some(LispVal::Ident("list".to_string())),
        _ => match str.parse::<i32>(){
            Ok(n) => Some(LispVal::Int(n)),
            Err(_) => None,
        }
    }
}

fn add_to_list(val: &mut LispVal, elem: Option<LispVal>){
    if let LispVal::List(xs) = val {
        match elem {
            Some(x) => xs.push(x),
            None => (),
        }
    }
}

fn skip_parens(str: String) -> usize {
    let mut count = 1;
    for (e, c) in str.chars().enumerate(){
        if c == ')' {
            count -= 1;
        } else if c == '(' {
            count += 1;
        }
        if count <= 0 {
            return e;
        }
    }
    return 0;
}

pub fn parse_expr(input: String) -> LispVal{
    let mut res: LispVal = LispVal::List(vec![]);
    let mut acc: Vec<char> = vec![];
    let mut i: usize = 1;
    while i < input.as_str().len() as usize {
        let c = input.chars().nth(i).unwrap();
        if c == '(' {
            let next_paren = skip_parens(input[i+1..].to_string());
            add_to_list(&mut res, Some(parse_expr(input[i..i+next_paren+2].to_string())));
            i += next_paren;
        }
        else if c == ' ' || c == ')' {
            let str: String = acc.into_iter().collect();
            acc = vec![];

            add_to_list(&mut res, str_to_val(str));
        } else {
            acc.push(c);
        }
        i += 1;
    }
    return res;
}
