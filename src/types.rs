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
            Expr::EBin(binop, e1, e2) => Expr::EBin(binop.clone(), e1.clone(), e2.clone()),
            Expr::EIf(e1, e2, e3) => Expr::EIf(e1.clone(), e2.clone(), e3.clone()),
            Expr::ELet(id, e1, e2) => Expr::ELet(id.clone(), e1.clone(), e2.clone()),
            Expr::EApp(e1, e2) => Expr::EApp(e1.clone(), e2.clone()),
            Expr::ELam(id, e) => Expr::ELam(id.clone(), e.clone()),
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

impl Clone for Value {
    fn clone(&self) -> Self {
        match self {
            Value::VInt(x) => Value::VInt(*x),
            Value::VBool(b) => Value::VBool(*b),
            Value::VClos(env, id, expr) => Value::VClos(env.clone(), id.clone(), expr.clone()),
            Value::VNil => Value::VNil,
            Value::VPair(v1, v2) => Value::VPair(v1.clone(), v2.clone()),
            Value::VErr(s) => Value::VErr(s.clone()),
            Value::VPrim(prim) => Value::VPrim(prim.clone()),
        }
    }
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
