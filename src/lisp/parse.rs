use crate::lisp::scrunch::LispVal;
use crate::lisp::scrunch::IdentType;

use super::scrunch::Either::{self, *};
use super::scrunch::LispErr;

fn check_ident(name: String) -> Either<LispErr, LispVal>{
    return Right(LispVal::Ident(IdentType::Name(name)));
}

fn str_to_val(str: String) -> Either<LispErr, LispVal>{
    if str.starts_with("#\\"){
        let parse_char = &str[2..];
        match parse_char {
            "space" => return Right(LispVal::Char(' ')),
            "newline" => return Right(LispVal::Char('\n')),
            _ => if parse_char.len() == 1 && parse_char.is_ascii() {
                return Right(LispVal::Char(parse_char.chars().nth(0).unwrap()));
            },
        }
    }
    if str.starts_with("\"") && str.ends_with("\""){
        return Right(LispVal::String(str[1..str.len()-1].to_string()))
    }
    match str.as_str() {
        "+" => Right(LispVal::Ident(IdentType::Add)),
        "-" => Right(LispVal::Ident(IdentType::Sub)),
        "*" => Right(LispVal::Ident(IdentType::Mult)),
        "/" => Right(LispVal::Ident(IdentType::Div)),
        "%" => Right(LispVal::Ident(IdentType::Mod)),
        "eq?" => Right(LispVal::Ident(IdentType::Eq)),
        "#t" => Right(LispVal::Bool(true)),
        "#f" => Right(LispVal::Bool(false)),
        "list" => Right(LispVal::Ident(IdentType::List)),
        "println" => Right(LispVal::Ident(IdentType::Println)),
        "lambda" => Right(LispVal::Ident(IdentType::Lambda)),
        "define" => Right(LispVal::Ident(IdentType::Define)),
        "if" => Right(LispVal::Ident(IdentType::If)),
        "<" => Right(LispVal::Ident(IdentType::Lt)),
        ">" => Right(LispVal::Ident(IdentType::Gt)),
        "<=" => Right(LispVal::Ident(IdentType::LtEq)),
        ">=" => Right(LispVal::Ident(IdentType::GtEq)),
        _ => match str.parse::<i32>(){
            Ok(n) => Right(LispVal::Int(n)),
            Err(_) => check_ident(str),
        }
    }
}

fn add_to_list(val: &mut LispVal, elem: LispVal){
    if let LispVal::List(xs) = val {
        if xs.len() == 0{
            *xs = if let LispVal::List(lst) = elem {
                lst
            } else { vec![elem] }
        } else {
            xs.push(elem)
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

pub fn parse_expr(input: String) -> Either<LispErr, LispVal>{
    let mut res: LispVal = LispVal::List(vec![]);
    let mut acc: Vec<char> = vec![];
    let mut i: usize = 0;
    let mut paren_count = 1;
    while i < input.as_str().len() as usize {
        let c = input.chars().nth(i).unwrap();
        if c == '(' && i != 0{
            let next_paren = skip_parens(input[i+1..].to_string());
            let in_expr;

            paren_count += 1;

            match parse_expr(input[i..i+next_paren+2].to_string()) {
                Right(expr) => in_expr = expr,
                Left(err) => return Left(err),
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
            let str: String = acc.into_iter().collect::<String>().trim().to_string();
            acc = vec![];

            if c == ')'{
                paren_count -= 1;
            }

            if str != "" && str != "'"{
                let parsed_val;
                match str_to_val(str) {
                    Right(val) => parsed_val = val,
                    Left(err) => return Left(err),
                }

                add_to_list(&mut res, parsed_val);
            }

            if paren_count == 0{
                return Right(res);
            }

        } else if c != '('{
            acc.push(c);
        }
        i += 1;
    }
    if paren_count != 0 {
        return Left(LispErr::ErrSyntax("parentheses mismatch".to_string()));
    }

    return Right(res);
}
