use core::panic;

use crate::types::*;

/// Returns the most recent binding for the variable 'id' in the HashMap
/// representing the environment. Panics if variable not present in environment.
fn lookup_id(env: &Env, id: &Id) -> Value {
    match env.get(id) {
        Some(value) => return value.clone(),
        None => panic!("runtime error: unbound variable {}", id),
    }
}

fn extend_env(env: &mut Env, id: &Id, val: &Value) {
    // TODO: Do we have to do clones here?
    env.insert(id.clone(), val.clone());
}

pub fn eval(env: Env, e: Expr) -> Value {
    match (env, e) {
        // Constants, just return values.
        (_, Expr::EInt(n)) => Value::VInt(n),
        (_, Expr::EBool(b)) => Value::VBool(b),
        (_, Expr::ENil) => Value::VNil,
        (env, Expr::ELam(id, e_body)) => Value::VClos(env, id, *e_body),
        // Lookup var in env.
        (env, Expr::EVar(id)) => lookup_id(&env, &id),
        // Eval each arg and then carry out the op
        (env, Expr::EBin(op, e1, e2)) => eval_op(op, eval(env.clone(), *e1), eval(env, *e2)),
        (env, Expr::EIf(e1, e2, e3)) => {
            // e1 must evaluate to a VBool
            match eval(env.clone(), *e1) {
                Value::VBool(true) => eval(env, *e2),
                Value::VBool(false) => eval(env, *e3),
                _ => panic!("type error"),
            }
        }
        // Eval body of let in env extended w/ (id, val)
        (env, Expr::ELet(id, e1, e2)) => {
            let value = eval(env.clone(), *e1);
            let mut env = env;
            extend_env(&mut env, &id, &value);
            eval(env, *e2)
        }
        // Eval arg (e2) in env, then eval body of closure extended by (id, arg)
        (env, Expr::EApp(e1, e2)) => {
            let arg = eval(env.clone(), *e2);
            // e1 must evaluate to a function.
            match eval(env.clone(), *e1) {
                Value::VClos(mut closure_env, id, e_body) => {
                    extend_env(&mut closure_env, &id, &arg);
                    closure_env.extend(env);
                    eval(closure_env, e_body)
                }
                Value::VPrim(prim) => prim(arg),
                _ => panic!("type error"),
            }
        }
    }
}

fn eval_op(binop: Binop, val1: Value, val2: Value) -> Value {
    match (binop, val1, val2) {
        // Both values must be VInts
        (Binop::Plus, Value::VInt(x), Value::VInt(y)) => Value::VInt(x + y),
        (Binop::Plus, _, _) => panic!("type error: Plus must be on ints!"),
        (Binop::Minus, Value::VInt(x), Value::VInt(y)) => Value::VInt(x - y),
        (Binop::Minus, _, _) => panic!("type error: Minus must be on ints!"),
        (Binop::Mul, Value::VInt(x), Value::VInt(y)) => Value::VInt(x * y),
        (Binop::Mul, _, _) => panic!("type error: Mul must be on ints!"),
        (Binop::Div, Value::VInt(x), Value::VInt(y)) => Value::VInt(x / y),
        (Binop::Div, _, _) => panic!("type error: Div must be on ints!"),
        (Binop::Lt, Value::VInt(x), Value::VInt(y)) => Value::VBool(x < y),
        (Binop::Lt, _, _) => panic!("type error: Lt must be on ints!"),
        (Binop::Le, Value::VInt(x), Value::VInt(y)) => Value::VBool(x <= y),
        (Binop::Le, _, _) => panic!("type error: Le must be on ints!"),

        // Must both be VBools
        (Binop::And, Value::VBool(x), Value::VBool(y)) => Value::VBool(x && y),
        (Binop::And, _, _) => panic!("type error: And must be on ints!"),
        (Binop::Or, Value::VBool(x), Value::VBool(y)) => Value::VBool(x || y),
        (Binop::Or, _, _) => panic!("type error: Or must be on ints!"),
        // Can be on anything of same type?
        (Binop::Eq, Value::VInt(x), Value::VInt(y)) => Value::VBool(x == y),
        (Binop::Eq, Value::VBool(x), Value::VBool(y)) => Value::VBool(x == y),
        (Binop::Eq, Value::VNil, Value::VNil) => Value::VBool(true),
        (Binop::Eq, _, _) => panic!("type error: Ne must be on the same type!"),
        (Binop::Ne, Value::VInt(x), Value::VInt(y)) => Value::VBool(x != y),
        (Binop::Ne, Value::VBool(x), Value::VBool(y)) => Value::VBool(x != y),
        (Binop::Ne, _, _) => panic!("type error: Ne must be on the same type!"),

        // Can be anything.
        (Binop::Cons, v1, v2) => Value::VPair(Box::new(v1), Box::new(v2)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lookup_test() {
        let mut env = Env::new();
        env.insert("x0".to_string(), Value::VInt(0));
        env.insert("b0".to_string(), Value::VBool(true));
        env.insert("nil".to_string(), Value::VNil);

        assert!(lookup_id(&env, &"x0".to_string()) == Value::VInt(0));
        assert!(lookup_id(&env, &"b0".to_string()) == Value::VBool(true));
        assert!(lookup_id(&env, &"nil".to_string()) == Value::VNil);
    }

    #[test]
    #[should_panic]
    fn lookup_unbound_var() {
        let mut env = Env::new();
        env.insert("x0".to_string(), Value::VInt(10));

        let _ = lookup_id(&env, &"foo".to_string());
    }

    #[test]
    fn env_extend_test() {
        let mut env = Env::new();
        env.insert("x0".to_string(), Value::VInt(0));
        env.insert("b0".to_string(), Value::VBool(true));
        env.insert("nil".to_string(), Value::VNil);

        extend_env(&mut env, &"foo".to_string(), &Value::VInt(69));
        assert!(lookup_id(&env, &"foo".to_string()) == Value::VInt(69));

        extend_env(&mut env, &"foo".to_string(), &Value::VInt(28));
        assert!(lookup_id(&env, &"foo".to_string()) == Value::VInt(28));
    }

    #[test]
    fn eval_test() {
        let mut env = Env::new();
        env.insert("x0".to_string(), Value::VInt(0));
        env.insert("b0".to_string(), Value::VBool(true));
        env.insert("nil".to_string(), Value::VNil);

        // Constant expressions.
        assert!(eval(env.clone(), Expr::EInt(5)) == Value::VInt(5));
        assert!(eval(env.clone(), Expr::EBool(true)) == Value::VBool(true));
        assert!(eval(env.clone(), Expr::ENil) == Value::VNil);

        // Nested Expressions.

        // (2 + 3) * (4 + 5)
        let expr = Expr::EBin(
            Binop::Mul,
            Box::new(Expr::EBin(
                Binop::Plus,
                Box::new(Expr::EInt(2)),
                Box::new(Expr::EInt(3)),
            )),
            Box::new(Expr::EBin(
                Binop::Plus,
                Box::new(Expr::EInt(4)),
                Box::new(Expr::EInt(5)),
            )),
        );
        assert!(eval(env.clone(), expr) == Value::VInt(45));

        // let z = 3 in
        // let y = 2 in
        // let x = 1 in
        // let w = 0 in
        //  (x + y) - (z + w)
        let expr = Expr::ELet(
            "z".to_string(),
            Box::new(Expr::EInt(3)),
            Box::new(Expr::ELet(
                "y".to_string(),
                Box::new(Expr::EInt(2)),
                Box::new(Expr::ELet(
                    "x".to_string(),
                    Box::new(Expr::EInt(1)),
                    Box::new(Expr::ELet(
                        "w".to_string(),
                        Box::new(Expr::EInt(0)),
                        Box::new(Expr::EBin(
                            Binop::Minus,
                            Box::new(Expr::EBin(
                                Binop::Plus,
                                Box::new(Expr::EVar("x".to_string())),
                                Box::new(Expr::EVar("y".to_string())),
                            )),
                            Box::new(Expr::EBin(
                                Binop::Plus,
                                Box::new(Expr::EVar("z".to_string())),
                                Box::new(Expr::EVar("w".to_string())),
                            )),
                        )),
                    )),
                )),
            )),
        );
        assert!(eval(env.clone(), expr) == Value::VInt(0));

        // let z = 3 in
        // let y = 2 in
        // let x = 1 in
        // let w = 0 in
        //   if ((x + y) - (z + w)) == 0 then 3 else 7
        let expr = Expr::ELet(
            "z".to_string(),
            Box::new(Expr::EInt(3)),
            Box::new(Expr::ELet(
                "y".to_string(),
                Box::new(Expr::EInt(2)),
                Box::new(Expr::ELet(
                    "x".to_string(),
                    Box::new(Expr::EInt(1)),
                    Box::new(Expr::ELet(
                        "w".to_string(),
                        Box::new(Expr::EInt(0)),
                        Box::new(Expr::EBin(
                            Binop::Minus,
                            Box::new(Expr::EBin(
                                Binop::Plus,
                                Box::new(Expr::EVar("x".to_string())),
                                Box::new(Expr::EVar("y".to_string())),
                            )),
                            Box::new(Expr::EBin(
                                Binop::Plus,
                                Box::new(Expr::EVar("z".to_string())),
                                Box::new(Expr::EVar("w".to_string())),
                            )),
                        )),
                    )),
                )),
            )),
        );
    }

    #[test]
    fn eval_plus() {
        assert!(eval_op(Binop::Plus, Value::VInt(3), Value::VInt(7)) == Value::VInt(10));
        assert!(eval_op(Binop::Plus, Value::VInt(1), Value::VInt(2)) != Value::VInt(10));
    }

    #[test]
    #[should_panic]
    fn eval_plus_fail() {
        eval_op(Binop::Plus, Value::VInt(17), Value::VBool(true));
    }

    #[test]
    fn eval_minus() {
        assert!(eval_op(Binop::Minus, Value::VInt(3), Value::VInt(7)) == Value::VInt(-4));
        assert!(eval_op(Binop::Minus, Value::VInt(1), Value::VInt(2)) != Value::VInt(10));
    }

    #[test]
    #[should_panic]
    fn eval_minus_fail() {
        eval_op(Binop::Minus, Value::VInt(17), Value::VBool(true));
    }

    #[test]
    fn eval_mul() {
        assert!(eval_op(Binop::Mul, Value::VInt(3), Value::VInt(7)) == Value::VInt(21));
        assert!(eval_op(Binop::Mul, Value::VInt(1), Value::VInt(2)) != Value::VInt(10));
    }

    #[test]
    #[should_panic]
    fn eval_mul_fail() {
        eval_op(Binop::Mul, Value::VInt(17), Value::VBool(true));
    }

    #[test]
    fn eval_div() {
        assert!(eval_op(Binop::Div, Value::VInt(27), Value::VInt(7)) == Value::VInt(3));
        assert!(eval_op(Binop::Div, Value::VInt(1), Value::VInt(2)) != Value::VInt(10));
    }

    #[test]
    #[should_panic]
    fn eval_div_fail() {
        eval_op(Binop::Div, Value::VInt(17), Value::VBool(true));
    }

    #[test]
    fn eval_lt() {
        assert!(eval_op(Binop::Lt, Value::VInt(27), Value::VInt(7)) == Value::VBool(false));
        assert!(eval_op(Binop::Lt, Value::VInt(1), Value::VInt(2)) != Value::VBool(false));
    }

    #[test]
    #[should_panic]
    fn eval_lt_fail() {
        eval_op(Binop::Lt, Value::VInt(17), Value::VBool(true));
    }
}
