use super::ast_node::{Type::*, *};

// Kantsii tehä lisää jos tarve löytyy
pub enum TypeError {
    UndefinedVariable(String),
    Fail,
}

// Variable name -> Type of variable
type Context = std::collections::HashMap<String, Type>;

pub trait InferType {
    // Infer the type of self in the given context
    fn infer_type(&self, ctx: Context) -> Result<Type, TypeError>;

    // Check the type of self with no context
    fn type_check(&self) -> Result<Type, TypeError> {
        self.infer_type(Context::new())
    }
}

impl InferType for Expr {
    fn infer_type(&self, ctx: Context) -> Result<Type, TypeError> {
        todo!()
    }
}

impl InferType for Statement {
    fn infer_type(&self, ctx: Context) -> Result<Type, TypeError> {
        todo!()
    }
}

impl InferType for Decl {
    fn infer_type(&self, ctx: Context) -> Result<Type, TypeError> {
        todo!()
    }
}

impl Program {
    // Check whether type checking all declarations succeed
    fn type_check(&self) -> Result<Type, TypeError> {
        self.declarations
            .iter()
            .try_for_each(|declaration| declaration.type_check().map(|_| ()))?;

        Ok(Void)
    }
}
