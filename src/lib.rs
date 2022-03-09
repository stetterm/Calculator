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
        
        ///
        /// Main function for parsing string mathematical
        /// expressions into an Expression which has a method
        /// "solve()" to return the numeric solution.
        /// 
        /// # Examples
        /// ```
        /// // Here we assert on the result of calling "solve"
        /// // on the Expression returned from this mathematical
        /// // string.
        /// # use calculator::expression::Expression;
        /// let x = Expression::parse("3 - 6 * 5 + (4 * 14 - 17) / 5").unwrap();
        /// assert_eq!(-19.2, x.solve());
        /// 
        /// // Here is another example:
        /// let x = Expression::parse("4 * 33 - 6 / 9 + (3 * (4 + 6) - 4)").unwrap();
        /// assert_eq!(472./3., x.solve());
        /// 
        /// // When an empty string is passed, 0 is returned:
        /// let x = Expression::parse("").unwrap();
        /// assert_eq!(0., x.solve());
        /// ```
        /// 
        /// # Errors
        /// ```
        /// // This function returns has the potential to return
        /// // two different errors.
        /// 
        /// // The first of these errors is UnbalancedParentheses.
        /// // The function cannot easily assume what the user wants
        /// // when the parentheses are not matched, so it will return
        /// // an error and not continue to parse if: a closing parenthesis
        /// // character (")") comes before an opening parenthesis ("("),
        /// // or if the number of opening and closing parentheses do not
        /// // match (e.g. "(5 + 6 - (10)" ).
        /// # use calculator::expression::{self, Expression};
        /// let x = Expression::parse("(5 + 6 - (10)");
        /// match x {
        ///     Ok(e) => panic!(),
        ///     Err(e) => assert_eq!("Invalid use of parentheses",
        ///         format!("{}", e)
        ///     ),
        /// }
        /// 
        /// let x = Expression::parse(") 3 - 5 * 4");
        /// match x {
        ///     Ok(e) => panic!(),
        ///     Err(e) => assert_eq!("Invalid use of parentheses",
        ///         format!("{}", e)
        ///     ),
        /// }
        /// ```
        /// 
        /// // The second of these two errors is InvalidToken.
        /// // This error is returned if an unsupported character
        /// // (i.e. any character that is not a paranthesis, number,
        /// // decimal, or operator (+-*/)).
        /// ```
        /// # use calculator::expression::{self, Expression};
        /// let x = Expression::parse("3 + & - @ * 5");
        /// match x {
        ///     Ok(e) => panic!(),
        ///     Err(e) => assert_eq!("Invalid character in sequence",
        ///         format!("{}", e)
        ///     ),
        /// }
        /// ```
        /// 
        pub fn parse(tokens: &str) -> Result<Expression, Box<dyn std::error::Error>> {
            // filter out white spaces
            let mut expr_list: Vec<Expression> = vec![];
            let cpy = tokens.chars()
                .filter(|c| *c != ' ' && *c != '\n' && *c != '\r')
                .collect::<Vec<char>>();

            // If nothing is in the expression,
            // a 0 value expression is returned.
            if cpy.len() == 0 {
                return Ok(Expression {
                    operator: Operator::Value(0.),
                    left: None,
                    right: None,
                });
            }

            // loop through token characters and
            // convert them into Expressions.
            let mut i = 0;
            loop {
                if i > cpy.len() - 1 {
                    break;
                }
                let cur_token = cpy[i];

                // If the character is an operator (+-*/),
                // make an operator Expression.
                match get_operator(cur_token) {
                    Some(o) => expr_list.push(Expression {
                        operator: o,
                        left: None,
                        right: None,
                    }),

                // If not, check whether the character is numeric,
                // and parse it into f64 if it is numeric.
                    None => if cur_token.is_numeric() {
                        let mut str_val = String::new();
                        while i < cpy.len() && cpy[i].is_numeric() {
                            str_val += &cpy[i].to_string();
                            i += 1;
                        }
                        let val: f64 = str_val.parse()?;
                        expr_list.push(Expression { 
                            operator: Operator::Value(val),
                            left: None,
                            right: None, 
                        });
                        i -= 1;

                // If not, it must be an opening parethensis, or
                // and invalid character, because closing parethenses
                // cannot come before opening parentheses.
                    } else if cur_token == '(' {
                        let mut x = 1;
                        let mut par = String::new();
                        while x != 0 && i < cpy.len() {
                            i += 1;
                            if i < cpy.len() {
                                if cpy[i] == '(' {
                                    x += 1;
                                } else if cpy[i] == ')' {
                                    x -= 1;
                                }
                                par.push(cpy[i]);
                            }
                        }

                        // If an uneven amount of parentheses are found,
                        // an error is returned.
                        if x != 0 {
                            return Err(Box::new(UnbalancedParentheses));
                        }

                        // Otherwise, the characters inside the parentheses are
                        // recursively parsed.
                        let paren_expr = Expression::parse(&par[0..par.len()-1])?;
                        let paren_expr = paren_expr.solve();
                        expr_list.push(Expression {
                            operator: Operator::Value(paren_expr),
                            left: None,
                            right: None,
                        });

                    // If closing parentheses come before opening
                    // parentheses, an error is returned.
                    } else if cur_token == ')' {
                        return Err(Box::new(UnbalancedParentheses));
                    
                    // If an unsupported character is found, an
                    // error is returned.
                    } else {
                        return Err(Box::new(InvalidToken));
                    },
                }
                i += 1;
            }

            // Loop for multiplication/division.
            let mut i = 0;
            loop {
                if i >= expr_list.len() {
                    break;
                }

                // Condense all multiplication and division
                // using surrounding numbers into their own
                // Expressions, and replace the previously
                // existing Expressions with the new Expression.
                if expr_list[i].operator == Operator::Multiplication
                    || expr_list[i].operator == Operator::Division {
                    if i < expr_list.len()-1 {
                        let new_expr = Expression {
                            operator: expr_list[i].operator.clone(),
                            left: Some(Box::new(expr_list.remove(i-1).clone())),
                            right: Some(Box::new(expr_list.remove(i).clone())),
                        };
                        expr_list.remove(i-1);
                        expr_list.insert(i-1, new_expr);
                    }
                } else {
                    i += 1;
                }
            }

            // Loop for addition/subtraction.
            let mut expr_root = expr_list[0].clone();
            let mut i = 1;
            loop {

                // Use a "root" Expression to add itself into
                // a new larger Expression containing more values, 
                // until the list has been parsed into a single Expression.
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

    #[test]
    fn test_output_3() {
        let x = Expression::parse("2 * (3 * 5 + 6 * (1 - 4 * 6 + 5) - 16 * 4) * 13").unwrap();
        assert_eq!(-4082., x.solve());
    }

    #[test]
    fn test_output_4() {
        let x = Expression::parse("13 - 5 * 6 * ((2 + 5 - 15 / 23) / 10 - 6)").unwrap();
        assert_eq!(173.95652173913044, x.solve());
    }
}