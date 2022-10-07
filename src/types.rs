use std::collections::HashMap;

#[derive(Clone, Copy)]
pub enum Binop {
    Plus,
    Minus,
    Mul,
    Div,
    Eq,
    Ne,
    Lt,
    Le,
    And,
    Or,
    Cons,
}

pub type Id = String;

pub type Env = HashMap<String, Value>;

pub enum Expr {
    EInt(i64),
    EBool(bool),
    ENil,
    EVar(Id),
    EBin(Binop, Box<Expr>, Box<Expr>),
    EIf(Box<Expr>, Box<Expr>, Box<Expr>),
    ELet(Id, Box<Expr>, Box<Expr>),
    EApp(Box<Expr>, Box<Expr>),
    ELam(Id, Box<Expr>),
}

impl Clone for Expr {
    fn clone(&self) -> Self {
        match self {
            Expr::EInt(x) => Expr::EInt(*x),
            Expr::EBool(b) => Expr::EBool(*b),
            Expr::ENil => Expr::ENil,
            Expr::EVar(id) => Expr::EVar(id.clone()),
            Expr::EBin(binop, , )
        }
    }
}

pub enum Value {
    VInt(i64),
    VBool(bool),
    VClos(Env, Id, Expr),
    VNil,
    VPair(Box<Value>, Box<Value>),
    VErr(String),
    VPrim(fn(Value) -> Value),
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::VInt(x1), Value::VInt(x2)) => x1 == x2,
            (Value::VBool(x1), Value::VBool(x2)) => x1 == x2,
            (Value::VNil, Value::VNil) => true,
            (Value::VPair(x1, y1), Value::VPair(x2, y2)) => x1 == x2 && y1 == y2,
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn value_equality() {
        assert!(Value::VInt(5) == Value::VInt(5));
        assert!(Value::VInt(69) != Value::VInt(13));
        assert!(Value::VBool(false) == Value::VBool(false));
        assert!(Value::VBool(true) != Value::VBool(false));
        assert!(Value::VNil == Value::VNil);
        assert!(
            Value::VPair(Box::new(Value::VInt(69)), Box::new(Value::VInt(27)))
                == Value::VPair(Box::new(Value::VInt(69)), Box::new(Value::VInt(27)))
        );
    }
}
