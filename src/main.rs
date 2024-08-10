mod lisp;

use lisp::scrunch::eval;
use lisp::parse::parse_expr;

fn main() {
    use linefeed::{Interface, ReadResult};
    let reader = Interface::new("scrunch").unwrap();
    reader.set_prompt("scrunch> ").unwrap();

    while let ReadResult::Input(input) = reader.read_line().unwrap() {
        if input == "quit" || input == "exit" {
            break;
        }

        let expr = parse_expr(input);

        println!("{}", expr);
        println!("{}", eval(&expr));
    }
}
