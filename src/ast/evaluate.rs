use super::ast_node::*;

// Trait to implement for AST nodes that can be evaluated (stepped)
pub trait Evaluate: Sized {
    // Whether self has already been evaluated to a value, and thus no more evaluation is possible
    // ie. an Expr::Integer(5) is a value and cannot be evaluated, while
    // a binary expression of (5 + 3) is not yet a value, but when stepped becomes
    // Expr::Integer(8), which is a value
    fn is_value(&self) -> bool;

    fn subst(self, variable_name: &str, replace_with: Expr) -> Self;

    // Step once in the evaluation
    fn step(self) -> Self;

    fn evaluate(mut self) -> Self {
        while !self.is_value() {
            self = self.step();
        }

        self
    }
}

impl Evaluate for Expr {
    fn is_value(&self) -> bool {
        todo!();
    }

    fn step(self) -> Self {
        todo!();
    }

    fn subst(self, variable_name: &str, replace_with: Expr) -> Self {
        todo!()
    }
}

impl Evaluate for Statement {
    fn is_value(&self) -> bool {
        todo!();
    }

    fn step(self) -> Self {
        todo!();
    }

    fn subst(self, variable_name: &str, replace_with: Expr) -> Self {
        todo!()
    }
}

impl Evaluate for Decl {
    fn is_value(&self) -> bool {
        todo!();
    }

    fn step(self) -> Self {
        todo!();
    }

    fn subst(self, variable_name: &str, replace_with: Expr) -> Self {
        todo!()
    }
}

impl Evaluate for Program {
    fn is_value(&self) -> bool {
        todo!();
    }

    fn step(self) -> Self {
        todo!();
    }

    fn subst(self, variable_name: &str, replace_with: Expr) -> Self {
        todo!()
    }
}
