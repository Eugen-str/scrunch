mod lisp;

use lisp::scrunch::eval;
use lisp::parse::parse_expr;

use linefeed::{Interface, ReadResult};

fn main() {
    let reader = Interface::new("scrunch").unwrap();
    reader.set_prompt("scrunch> ").unwrap();

    while let ReadResult::Input(input) = reader.read_line().unwrap() {
        if input == "quit" || input == "exit" {
            break;
        }

        reader.add_history(input.clone());
        let expr = parse_expr(input);

        //println!("{}", expr);

        match eval(&expr) {
            lisp::scrunch::Either::Left(err) => {
                println!("{}", err);
            }
            lisp::scrunch::Either::Right(val) => {
                println!("{}", val);
            }
        };
    }
}
