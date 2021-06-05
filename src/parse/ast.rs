#[derive(Debug, PartialEq)]
pub struct ASTree {
    root: Node,
}

impl ASTree {
    pub fn new(root: Node) -> ASTree {
        ASTree { root }
    }
}

#[derive(Debug, PartialEq)]
pub enum Node {
    Expr(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Identifier(String),
    BinaryOp {
        op: String,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Cond {
        cond: Box<Expr>,
        then: Box<Expr>,
        r#else: Box<Expr>,
    },
    LogicalOr {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    LogicalAnd {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
}

impl Expr {
    pub fn binary_op(op: &str, lhs: Expr, rhs: Expr) -> Expr {
        Expr::BinaryOp {
            op: op.to_string(),
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }
    pub fn cond(cond: Expr, then: Expr, r#else: Expr) -> Expr {
        Expr::Cond {
            cond: Box::new(cond),
            then: Box::new(then),
            r#else: Box::new(r#else),
        }
    }
    pub fn logical_or(lhs: Expr, rhs: Expr) -> Expr {
        Expr::LogicalOr {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }
    pub fn logical_and(lhs: Expr, rhs: Expr) -> Expr {
        Expr::LogicalAnd {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Integer(i64),
    Character(u8),
}
