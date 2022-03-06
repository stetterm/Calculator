///
/// Module to evaluate an expression
/// using mathematical infix notation.
/// 
pub mod expression {

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

        
        // pub fn parse(tokens: &str) -> Expression {

        // }
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
}