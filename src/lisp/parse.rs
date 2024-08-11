use crate::lisp::scrunch::LispVal;
use crate::lisp::scrunch::IdentType;

fn str_to_val(str: String) -> Option<LispVal>{
    let str = str.trim().to_string();
    if str.starts_with("#\\"){
        let parse_char = &str[2..];
        match parse_char {
            "space" => return Some(LispVal::Char(' ')),
            "newline" => return Some(LispVal::Char('\n')),
            _ => if parse_char.len() == 1 && parse_char.is_ascii() {
                return Some(LispVal::Char(parse_char.chars().nth(0).unwrap()));
            },
        }
    }
    if str.starts_with("\"") && str.starts_with("\""){
        return Some(LispVal::String(str[1..str.len()-1].to_string()))
    }
    match str.as_str() {
        "+" => Some(LispVal::Ident(IdentType::Add)),
        "-" => Some(LispVal::Ident(IdentType::Sub)),
        "*" => Some(LispVal::Ident(IdentType::Mult)),
        "/" => Some(LispVal::Ident(IdentType::Div)),
        "%" => Some(LispVal::Ident(IdentType::Mod)),
        "eq?" => Some(LispVal::Ident(IdentType::Eq)),
        "#t" => Some(LispVal::Bool(true)),
        "#f" => Some(LispVal::Bool(false)),
        "list" => Some(LispVal::Ident(IdentType::List)),
        _ => match str.parse::<i32>(){
            Ok(n) => Some(LispVal::Int(n)),
            Err(_) => None,
        }
    }
}

fn add_to_list(val: &mut LispVal, elem: Option<LispVal>){
    if let LispVal::List(xs) = val {
        match elem {
            Some(x) => if xs.len() == 0{
                *xs = if let LispVal::List(lst) = x {
                    lst
                } else { vec![x] }
            } else {
                xs.push(x)
            },
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
    let mut i: usize = 0;
    while i < input.as_str().len() as usize {
        let c = input.chars().nth(i).unwrap();
        if c == '(' && i != 0{
            let next_paren = skip_parens(input[i+1..].to_string());
            let in_expr = parse_expr(input[i..i+next_paren+2].to_string());
            if acc.clone().into_iter().collect::<String>() == "'"{
                if let LispVal::List(mut xs) = in_expr {
                    xs.insert(0, LispVal::Ident(IdentType::List));
                    let new_in_expr = LispVal::List(xs);
                    add_to_list(&mut res, Some(new_in_expr));
                }
            } else {
                add_to_list(&mut res, Some(in_expr));
            }
            i += next_paren;
        }
        else if c == ' ' || c == ')' {
            let str: String = acc.into_iter().collect();
            acc = vec![];

            add_to_list(&mut res, str_to_val(str));
        } else if c != '('{
            acc.push(c);
        }
        i += 1;
    }
    return res;
}
