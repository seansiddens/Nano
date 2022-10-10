use crate::types::*;

trait HasTVars {
    fn free_tvars(&self) -> Vec<TVar>;
}

impl HasTVars for Type {
    fn free_tvars(&self) -> Vec<TVar> {
        match self {
            Self::TInt => Vec::new(),
            Self::TBool => Vec::new(),
            Self::TFunction(t1, t2) => {
                // Union of free tvars of t1 and t2.
                let mut tvars = (*t1).free_tvars();
                tvars.extend((*t2).free_tvars());
                tvars
            },
            Self::TVar(var) => vec![var.to_string()],
            Self::TList(t) => t.free_tvars(),
        }        
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn free_tvars_type() {
        let t = Type::TInt;
        assert!(t.free_tvars() == Vec::<TVar>::new());

        let t = Type::TVar("a".to_string());
        assert!(t.free_tvars() == vec!["a"]);

        let t = Type::TFunction(
            Box::new(Type::TVar("a".to_string())),
            Box::new(Type::TVar("b".to_string()))
        );
        assert!(t.free_tvars() == vec!["a", "b"]);
    }

}