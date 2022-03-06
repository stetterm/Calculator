///
/// Module to evaluate an expression
/// using mathematical infix notation.
/// 
pub mod expression {
    use std::io::ErrorKind;
    use std::error::Error;

    ///
    /// Holds the various types of
    /// mathematical operators currently
    /// supported by the expression evaluator.
    /// 
    pub enum Operator {
        Addition,
        Subtraction,
        Multiplication,
        Division,
        Value(f64),
    }

    ///
    /// Matches the character with the
    /// corresponding operator if it exists.
    /// 
    pub fn get_operator(symb: char) -> Option<Operator> {
        match symb {
            '+' => Some(Operator::Addition),
            '-' => Some(Operator::Subtraction),
            '*' => Some(Operator::Multiplication),
            '/' => Some(Operator::Division),
            _ => None,
        }
    }

    ///
    /// Represents an expression that has an infix
    /// operator and two expressions to evaluate.
    /// 
    pub struct Expression {
        pub operator: Operator,
        pub left: Option<Box<Expression>>,
        pub right: Option<Box<Expression>>,
    }

    #[derive(Debug, Clone)]
    pub struct ParenthesesError;
    #[derive(Debug, Clone)]
    pub struct InvalidToken;

    impl std::fmt::Display for ParenthesesError {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            write!(f, "Invalid use of parentheses")
        }
    }

    impl std::fmt::Display for InvalidToken {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            write!(f, "Invalid character in sequence")
        }
    }

    impl std::error::Error for ParenthesesError {}

    impl std::error::Error for InvalidToken {}

    // Implementaion block for an expression
    impl Expression {

        ///
        /// Solves the current expression by evaluating
        /// the left and right expressions recursively, and then applying
        /// the correct operation to the results.
        /// 
        pub fn solve(&self) -> f64 {
            if let Operator::Value(n) = self.operator {
                return n;
            }
            let left_val = match &self.left {
                Some(v) => v.solve(),
                None => 0.,
            };
            let right_val = match &self.right {
                Some(v) => v.solve(),
                None => 0.,
            };
            match self.operator {
                Operator::Addition => left_val + right_val,
                Operator::Subtraction => left_val - right_val,
                Operator::Multiplication => left_val * right_val,
                Operator::Division => left_val / right_val,
                Operator::Value(n) => n,
            }
        }
        
        pub fn parse(tokens: &str) -> Result<Expression, Box<dyn std::error::Error>> {
            let mut expr_list: Vec<Expression> = vec![];
            let cpy = tokens.chars()
                .filter(|c| *c != ' ')
                .collect::<Vec<char>>();
            let mut i = 0;
            loop {
                let cur_token = cpy[i];
                match get_operator(cur_token) {
                    Some(o) => expr_list.push(Expression {
                        operator: o,
                        left: None,
                        right: None,
                    }),
                    None => if cur_token.is_numeric() {
                        let temp = i;
                        let mut str_val = String::new();
                        str_val += &cur_token.to_string();
                        while cpy[temp].is_numeric() && i < cpy.len() {
                            i += 1;
                        }
                        let val: f64 = str_val.parse()?;
                        expr_list.push(Expression { 
                            operator: Operator::Value(val),
                            left: None,
                            right: None, 
                        });
                    } else if cur_token == '(' {
                        let mut x = 1;
                        let mut par = String::new();
                        while x != 0 && i < cpy.len() {
                            i += 1;
                            if cpy[i] == '(' {
                                x += 1;
                            } else if cpy[i] == ')' {
                                x -= 1;
                            }
                            par.push(cpy[i]);
                        }
                        if x != 0 {
                            return Err(Box::new(ParenthesesError));
                        }
                        let paren_expr = Expression::parse(&par[1..i])?;
                        let paren_expr = paren_expr.solve();
                        expr_list.push(Expression {
                            operator: Operator::Value(paren_expr),
                            left: None,
                            right: None,
                        });
                    } else {
                        return Err(Box::new(InvalidToken));
                    },
                }
                if i == cpy.len() - 1 {
                    break;
                }
                i += 1;
            }
            

            Ok(Expression { operator: Operator::Value(0.), left: None, right: None })
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::expression::{Expression, Operator};


    ///
    /// Evaluates the expression: 2 * (3 + 2)
    /// 
    #[test]
    fn simple_expression() {
        let i = Expression {
            operator: Operator::Addition,
            left: Some(Box::new(Expression { 
                operator: Operator::Value(3.), left: None, right: None,
            })),
            right: Some(Box::new(Expression {
                operator: Operator::Value(2.), left: None, right: None,
            }))
        };
        let j = Expression {
            operator: Operator::Multiplication,
            left: Some(Box::new(Expression {
                operator: Operator::Value(2.), left: None, right: None,
            })),
            right: Some(Box::new(i)),
        };
        assert_eq!(10., j.solve());
    }

    #[test]
    fn test_iteration() {
        let test = "3 + 5 * (3 + 4)";
        for partition in test.trim().split_inclusive(&['+', '*', '(', ')'][..]) {
            println!("{partition}");
        }
    }

    #[test]
    #[ignore]
    fn test_output() {
        // Expression::parse("2 * (3+ 5) * 6");
        let x: f64 = "55".parse().unwrap();
        dbg!(x);
    }
}