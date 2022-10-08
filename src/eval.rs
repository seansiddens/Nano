use core::panic;

use crate::types::*;

/// Returns the most recent binding for the variable 'id' in the HashMap
/// representing the environment. Panics if variable not present in environment.
fn lookup_id(env: &Env, id: &Id) -> Value {
    match env.get(id) {
        Some(value) => return value.clone(),
        None => panic!("Error! Unbound variable."),
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

        
        _ => Value::VNil,
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
}
