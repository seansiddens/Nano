use std::collections::HashMap;
use std::fmt;
use std::hash::Hash;

#[derive(Debug, Clone, Copy)]
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

impl fmt::Display for Binop {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let binop_string = match self {
            Binop::Plus => "+",
            Binop::Minus => "-",
            Binop::Mul => "*",
            Binop::Div => "/",
            Binop::Eq => "=",
            Binop::Ne => "!=",
            Binop::Lt => "<",
            Binop::Le => "<=",
            Binop::And => "&&",
            Binop::Or => "||",
            Binop::Cons => ":",
        };
        write!(f, "{}", binop_string)
    }
}

pub type Id = String;

pub type Env = HashMap<String, Value>;

#[derive(Debug)]
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

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let expr_string = match self {
            Expr::EInt(x) => format!("{}", x),
            Expr::EBool(b) => format!("{}", b),
            Expr::EVar(x) => x.to_string(),
            Expr::EBin(o, e1, e2) => format!("({} {} {})", *e1, o, *e2),
            Expr::EIf(c, t, e) => format!("if {} then {} else {}", *c, *t, *e),
            Expr::ELet(x, e1, e2) => format!("let {} = {} in \n {}", x, *e1, *e2),
            Expr::EApp(e1, e2) => format!("({} {})", *e1, *e2),
            Expr::ELam(x, e) => format!("\\{} -> {}", x, *e),
            Expr::ENil => "[]".to_string(),
        };
        write!(f, "{}", expr_string)
    }
}

#[derive(Debug)]
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

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let value_string = match self {
            Value::VInt(x) => format!("{}", x),
            Value::VBool(b) => format!("{}", b),
            Value::VClos(env, x, v) => format!("<<{:#?}, \\{} -> {}>>", env, x, v),
            Value::VPair(v, w) => format!("({} : {})", *v, *w),
            Value::VErr(s) => format!("ERROR: {}", s),
            Value::VNil => "[]".to_string(),
            Value::VPrim(_) => "<<primitive-function".to_string(),
        };
        write!(f, "{}", value_string)
    }
}

// Type alias.
pub type TVar = String;

// Mapping of program variables to poly-types.
pub type TypeEnv = HashMap<Id, Poly>;

// Mapping of type variables to the types they should be replaced with.
pub type Subst = HashMap<TVar, Type>;

#[derive(Debug, PartialEq)]
pub enum Type {
    TInt,                            // Int
    TBool,                           // Bool
    TFunction(Box<Type>, Box<Type>), // function type: T1 -> T2
    TVar(TVar),                      //  type variable: a, b, c
    TList(Box<Type>),                // list type: [T]
}

#[derive(Debug, PartialEq)]
pub enum Poly {
    Mono(Type),              // mono-type
    Forall(TVar, Box<Poly>), // Polymorphic type
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
