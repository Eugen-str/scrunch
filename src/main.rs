mod lisp;

use std::{env, fs};

use lisp::scrunch::{eval, Either, LispVal};
use lisp::parse::parse_expr;
use lisp::env::Env;

use linefeed::{Interface, ReadResult};

fn repl(){
    let reader = Interface::new("scrunch").unwrap();
    reader.set_prompt("scrunch> ").unwrap();

    let mut env = Env::new();

    while let ReadResult::Input(input) = reader.read_line().unwrap() {
        if input == "quit" || input == "exit" {
            break;
        }

        reader.add_history(input.clone());
        let expr;
        match parse_expr(input){
            lisp::scrunch::Either::Right(e) => expr = e,
            lisp::scrunch::Either::Left(err) => {
                println!("{}", err);
                continue;
            },
        }

        match eval(&expr, &mut env) {
            lisp::scrunch::Either::Left(err) => {
                println!("{}", err);
            }
            lisp::scrunch::Either::Right(val) => {
                println!("{}", val);
            }
        };
    }
}

fn get_exprs(contents: String) -> Vec<String>{
    let mut str_acc: Vec<char> = vec![];
    let mut res: Vec<String> = vec![];
    let mut paren_count = 0;

    for c in contents.chars() {
        if c == '\n'{ continue; }

        str_acc.push(c);
        if c == '(' {
            paren_count += 1;
        } else if c == ')' {
            paren_count -= 1;

            if paren_count == 0 {
                res.push(str_acc.clone().into_iter().collect());
                str_acc = vec![];
            }
        }
    }

    return res;
}

fn eval_file(path: String){
    let contents = fs::read_to_string(path)
        .expect("Should have been able to read the file");

    let mut env = Env::new();

    let mut exprs: Vec<LispVal> = vec![];
    for expr in get_exprs(contents){
        match parse_expr(expr){
            lisp::scrunch::Either::Right(e) => exprs.push(e),
            lisp::scrunch::Either::Left(err) => {
                println!("{}", err);
                return;
            },
        }
    }

    for expr in exprs {
        let res = eval(&expr, &mut env);
        if let Either::Left(err) = res {
            println!("{}", err);
        }
    }
}

fn main() {
    let cmd_args: Vec<String> = env::args().skip(1).collect();
    if cmd_args.len() == 0 {
        repl();
    } else if cmd_args.len() == 1 {
        let filename;
        let arg0 = cmd_args.get(0).unwrap();
        if arg0.ends_with(".lisp") {
            filename = arg0.clone();
        } else {
            println!("ERROR: Filename must end in .lisp, provided `{}`", arg0);
            return;
        }
        eval_file(filename);
    }
}
