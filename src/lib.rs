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
    #[derive(Debug, Clone, PartialEq)]
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
    #[derive(Debug, Clone)]
    pub struct Expression {
        pub operator: Operator,
        pub left: Option<Box<Expression>>,
        pub right: Option<Box<Expression>>,
    }

    #[derive(Debug, Clone)]
    pub struct UnbalancedParentheses;
    #[derive(Debug, Clone)]
    pub struct InvalidToken;

    impl std::fmt::Display for UnbalancedParentheses {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            write!(f, "Invalid use of parentheses")
        }
    }

    impl std::fmt::Display for InvalidToken {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            write!(f, "Invalid character in sequence")
        }
    }

    impl std::error::Error for UnbalancedParentheses {}

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
            // filter token characters into expressions
            let mut expr_list: Vec<Expression> = vec![];
            let cpy = tokens.chars()
                .filter(|c| *c != ' ')
                .collect::<Vec<char>>();
            let mut i = 0;
            loop {
                if i > cpy.len() - 1 {
                    break;
                }
                let cur_token = cpy[i];
                match get_operator(cur_token) {
                    Some(o) => expr_list.push(Expression {
                        operator: o,
                        left: None,
                        right: None,
                    }),
                    None => if cur_token.is_numeric() {
                        let mut str_val = String::new();
                        str_val += &cur_token.to_string();
                        while i < cpy.len() && cpy[i].is_numeric() {
                            i += 1;
                        }
                        let val: f64 = str_val.parse()?;
                        expr_list.push(Expression { 
                            operator: Operator::Value(val),
                            left: None,
                            right: None, 
                        });
                        i -= 1;
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
                            return Err(Box::new(UnbalancedParentheses));
                        }
                        let paren_expr = Expression::parse(&par[0..par.len()-1])?;
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
                i += 1;
            }
            // loop for multiplication/division
            let mut i = 0;
            loop {
                if i >= expr_list.len() {
                    break;
                }
                if expr_list[i].operator == Operator::Multiplication
                    || expr_list[i].operator == Operator::Division {
                    if i < expr_list.len()-1 {
                        let new_expr = Expression {
                            operator: expr_list[i].operator.clone(),
                            left: Some(Box::new(expr_list.remove(i-1).clone())),
                            right: Some(Box::new(expr_list.remove(i).clone())),
                        };
                        expr_list.remove(i-1);
                        expr_list.insert(0, new_expr);
                    }
                } else {
                    i += 1;
                }
            }
            let mut expr_root = expr_list[0].clone();
            let mut i = 1;
            loop {
                if i >= expr_list.len()-1 {
                    break;
                }
                expr_root = Expression {
                    operator: expr_list[i].operator.clone(),
                    left: Some(Box::new(expr_root)),
                    right: Some(Box::new(expr_list[i+1].clone())),
                };
                i += 2;
            }
            Ok(expr_root)
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
    fn test_output() {
        let x = Expression::parse("2 * (3+ 5) * 6").unwrap();
        assert_eq!(96., x.solve());
    }

    #[test]
    fn test_output_2() {
        let x = Expression::parse("2 * (3/6 + 5) * 7 - (1 * 5 + (1-3))").unwrap();
        assert_eq!(74., x.solve());
    }
}