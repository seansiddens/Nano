use core::panic;

use crate::types::*;

/// Returns the most recent binding for the variable 'id' in the HashMap
/// representing the environment. Panics if variable not present in environment.
fn lookup_id(id: &Id, env: &Env) -> Value {
    if env.contains_key(id) {
        return env[id];
    }
    panic!("Error! Unbound variable {}", id);
}

pub fn eval(env: Env, e: Expr) -> Value {
    match (env, e) {
        // Constants, just return values.
        (_, Expr::EInt(n)) => Value::VInt(n),
        (_, Expr::EBool(b)) => Value::VBool(b),
        (_, Expr::ENil) => Value::VNil,
        // TOOD: Finish rest of these.
        _ => Value::VNil,
    }
}
