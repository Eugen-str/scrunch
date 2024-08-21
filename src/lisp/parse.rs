use crate::lisp::scrunch::LispVal;
use crate::lisp::scrunch::IdentType;

use super::scrunch::LispErr;

fn check_ident(name: String) -> Result<LispVal, LispErr>{
    return Ok(LispVal::Ident(IdentType::Name(name)));
}

fn str_to_val(str: String) -> Result<LispVal, LispErr>{
    if str.starts_with("#\\"){
        let parse_char = &str[2..];
        match parse_char {
            "space" => return Ok(LispVal::Char(' ')),
            "newline" => return Ok(LispVal::Char('\n')),
            _ => if parse_char.len() == 1 && parse_char.is_ascii() {
                return Ok(LispVal::Char(parse_char.chars().nth(0).unwrap()));
            },
        }
    }
    if str.starts_with("\"") && str.ends_with("\""){
        return Ok(LispVal::String(str[1..str.len()-1].to_string()))
    }
    match str.as_str() {
        "+" => Ok(LispVal::Ident(IdentType::Add)),
        "-" => Ok(LispVal::Ident(IdentType::Sub)),
        "*" => Ok(LispVal::Ident(IdentType::Mult)),
        "/" => Ok(LispVal::Ident(IdentType::Div)),
        "%" => Ok(LispVal::Ident(IdentType::Mod)),
        "eq?" => Ok(LispVal::Ident(IdentType::Eq)),
        "#t" => Ok(LispVal::Bool(true)),
        "#f" => Ok(LispVal::Bool(false)),
        "list" => Ok(LispVal::Ident(IdentType::List)),
        "append" => Ok(LispVal::Ident(IdentType::Append)),
        "println" => Ok(LispVal::Ident(IdentType::Println)),
        "write" => Ok(LispVal::Ident(IdentType::Write)),
        "writeln" => Ok(LispVal::Ident(IdentType::Writeln)),
        "display" => Ok(LispVal::Ident(IdentType::Display)),
        "lambda" => Ok(LispVal::Ident(IdentType::Lambda)),
        "define" => Ok(LispVal::Ident(IdentType::Define)),
        "macro" => Ok(LispVal::Ident(IdentType::Macro)),
        "import" => Ok(LispVal::Ident(IdentType::Import)),
        "error" => Ok(LispVal::Ident(IdentType::ThrowError)),
        "export" => Ok(LispVal::Ident(IdentType::Export)),
        "if" => Ok(LispVal::Ident(IdentType::If)),
        "cond" => Ok(LispVal::Ident(IdentType::Cond)),
        "do" => Ok(LispVal::Ident(IdentType::Do)),
        "<" => Ok(LispVal::Ident(IdentType::Lt)),
        ">" => Ok(LispVal::Ident(IdentType::Gt)),
        "<=" => Ok(LispVal::Ident(IdentType::LtEq)),
        ">=" => Ok(LispVal::Ident(IdentType::GtEq)),
        "car" => Ok(LispVal::Ident(IdentType::Car)),
        "cdr" => Ok(LispVal::Ident(IdentType::Cdr)),
        "cons" => Ok(LispVal::Ident(IdentType::Cons)),
        _ => match str.parse::<i32>(){
            Ok(n) => Ok(LispVal::Int(n)),
            Err(_) => check_ident(str),
        }
    }
}

fn add_to_list(val: &mut LispVal, elem: LispVal){
    if let LispVal::List(xs) = val {
        xs.push(elem)
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

pub fn parse_expr(input: String) -> Result<LispVal, LispErr>{
    let input = input.trim();
    let mut res: LispVal = LispVal::List(vec![]);
    let mut acc: Vec<char> = vec![];
    let mut i: usize = 0;
    let mut paren_count = if input.chars().nth(0).unwrap() == '(' { 1 } else { 0 };

    while i < input.len() as usize {
        let c = input.chars().nth(i).unwrap();

        if c == ';' {
            if paren_count != 0 {
                return Err(LispErr::ErrSyntax("parentheses mismatch".to_string()));
            }

            return Ok(res);
        } else if c == '(' && i != 0 {
            let next_paren = skip_parens(input[i+1..].to_string());
            let in_expr;

            paren_count += 1;

            match parse_expr(input[i..i+next_paren+2].to_string()) {
                Ok(expr) => in_expr = expr,
                Err(err) => return Err(err),
            }

            if acc.clone().into_iter().collect::<String>() == "'"{
                if let LispVal::List(mut xs) = in_expr {
                    xs.insert(0, LispVal::Ident(IdentType::List));
                    let new_in_expr = LispVal::List(xs);
                    add_to_list(&mut res, new_in_expr);
                }
            } else {
                add_to_list(&mut res, in_expr);
            }
            i += next_paren;
        }
        else if c == ' ' || c == ')' {
            let mut str: String = acc.into_iter().collect::<String>().trim().to_string();
            acc = vec![];

            if str.starts_with("\"") && !str.ends_with("\""){
                let mut c_ = input.chars().nth(i).unwrap();
                while c_ != '"' {
                    if c_ == '\n' || c_ == ')' {
                        return Err(LispErr::ErrSyntax("unterminated string".to_string()));
                    }

                    c_ = input.chars().nth(i).unwrap();
                    str.push(c_);
                    i += 1;
                }
                i -= 1;
            }

            if c == ')'{
                paren_count -= 1;
            }

            if str != "" && str != "'"{
                let parsed_val;
                match str_to_val(str) {
                    Ok(val) => parsed_val = val,
                    Err(err) => return Err(err),
                }

                add_to_list(&mut res, parsed_val);
            }

            if paren_count == 0{
                return Ok(res);
            }

        } else if c != '('{
            acc.push(c);
        }
        i += 1;
    }
    if paren_count != 0 {
        return Err(LispErr::ErrSyntax("parentheses mismatch".to_string()));
    }

    return Ok(res);
}
