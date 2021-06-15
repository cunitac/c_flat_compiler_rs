use super::Span;

pub trait Located<'a> {
    fn position(&self) -> Span<'a>;
}

impl<'a> Located<'a> for Span<'a> {
    fn position(&self) -> Span<'a> {
        *self
    }
}

#[derive(Debug, PartialEq)]
pub struct ASTree<'a> {
    pub position: Span<'a>,
    pub declarations: Vec<DefinedFunction<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct DefinedFunction<'a> {
    pub ret_type: TypeRef<'a>,
    pub name: Span<'a>,
    pub params: Vec<Param<'a>>,
    pub body: Block<'a>,
}

#[derive(Debug, PartialEq)]
pub struct TypeRef<'a> {
    pub name: Span<'a>,
}

#[derive(Debug, PartialEq)]
pub struct Param<'a> {
    pub r#type: Span<'a>,
    pub name: Span<'a>,
}

#[derive(Debug, PartialEq)]
pub struct Block<'a> {
    pub stmts: Vec<Stmt<'a>>,
}

#[derive(Debug, PartialEq)]
pub enum Stmt<'a> {
    Expr(Expr<'a>),
}

impl<'a> Located<'a> for ASTree<'a> {
    fn position(&self) -> Span<'a> {
        self.position
    }
}

type BoxExpr<'a> = Box<Expr<'a>>;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'a> {
    Literal(Literal<'a>),
    Identifier(Span<'a>),
    BinaryOp {
        op: String,
        lhs: BoxExpr<'a>,
        rhs: BoxExpr<'a>,
    },
    Cond {
        cond: BoxExpr<'a>,
        then: BoxExpr<'a>,
        r#else: BoxExpr<'a>,
    },
    LogicalOr {
        lhs: BoxExpr<'a>,
        rhs: BoxExpr<'a>,
    },
    LogicalAnd {
        lhs: BoxExpr<'a>,
        rhs: BoxExpr<'a>,
    },
}

impl Expr<'_> {
    pub fn binary_op(op: &str, lhs: Self, rhs: Self) -> Self {
        Expr::BinaryOp {
            op: op.to_string(),
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }
    pub fn cond(cond: Self, then: Self, r#else: Self) -> Self {
        Expr::Cond {
            cond: Box::new(cond),
            then: Box::new(then),
            r#else: Box::new(r#else),
        }
    }
    pub fn logical_or(lhs: Self, rhs: Self) -> Self {
        Expr::LogicalOr {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }
    pub fn logical_and(lhs: Self, rhs: Self) -> Self {
        Expr::LogicalAnd {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }
}

impl<'a> Located<'a> for Expr<'a> {
    fn position(&self) -> Span<'a> {
        use Expr::*;
        match self {
            Literal(literal) => literal.position(),
            Identifier(identifier) => identifier.position(),
            BinaryOp { lhs, .. } => lhs.position(),
            Cond { cond, .. } => cond.position(),
            LogicalOr { lhs, .. } => lhs.position(),
            LogicalAnd { lhs, .. } => lhs.position(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Literal<'a> {
    Integer { val: i64, position: Span<'a> },
    Character { val: u8, position: Span<'a> },
}

impl PartialEq for Literal<'_> {
    fn eq(&self, rhs: &Self) -> bool {
        use Literal::*;
        match (self, rhs) {
            (Integer { val, .. }, Integer { val: rval, .. }) => val == rval,
            (Character { val, .. }, Character { val: rval, .. }) => val == rval,
            _ => false,
        }
    }
}

impl<'a> Located<'a> for Literal<'a> {
    fn position(&self) -> Span<'a> {
        use Literal::*;
        match self {
            Integer { position, .. } => position.position(),
            Character { position, .. } => position.position(),
        }
    }
}
