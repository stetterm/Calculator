use calculator::expression::{self, Expression};

use std::io::{self, Write};

fn main() {
    if let Err(e) = cli_loop() {
        println!("{}", e);
    }
}

///
/// Main application loop for the CLI
/// that allows you to evaluate mathematical
/// expressions using the expression module.
/// 
fn cli_loop() -> Result<(), Box<dyn std::error::Error>> {
    let mut user_in = String::new();
    loop {
        print!("Enter expression: ");
        io::stdout().flush()?;
        io::stdin().read_line(&mut user_in)?;
        if user_in.starts_with("quit") {
            return Ok(())
        }
        let expr = match Expression::parse(&user_in) {
            Ok(e) => e,
            Err(e) => {
                println!("{}", e);
                continue;
            }
        };
        println!("Value: {}", expr.solve());
        user_in.clear();
    }
}