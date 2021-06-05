use super::*;

macro_rules! assert_parse {
    ($parse:ident($src:expr), $rest:expr, $ast:expr $(,)?) => {
        assert_eq!($parse(Span::new($src)).unwrap(), ($rest, $ast))
    };
    ($parse:ident($src:expr), $ast:expr $(,)?) => {
        assert_eq!($parse(Span::new($src)).unwrap(), ("", $ast))
    };
}

#[test]
fn test_expr10() {
    assert_parse!(expr10("4 ?\t 2 :\n  0"), Expr::cond(int(4), int(2), int(0)));
    assert_parse!(
        expr10("0 ? 1 ? 2 : 3 : 4 ? 5 : 6"),
        Expr::cond(
            int(0),
            Expr::cond(int(1), int(2), int(3)),
            Expr::cond(int(4), int(5), int(6))
        )
    );
    assert_parse!(
        expr10("0 ? 1 : 2 ? 3 : 4 ? 5 : 6"),
        Expr::cond(
            int(0),
            int(1),
            Expr::cond(int(2), int(3), Expr::cond(int(4), int(5), int(6)))
        )
    );
}
#[test]
fn test_expr9() {
    assert_parse!(
        expr9("4 || 5 || 99"),
        Expr::logical_or(Expr::logical_or(int(4), int(5)), int(99))
    );
    assert_parse!(
        expr9("4 || 5 && 99"),
        Expr::logical_or(int(4), Expr::logical_and(int(5), int(99)))
    );
}
#[test]
fn test_expr8() {
    assert_parse!(
        expr8("4 && 5 && 99"),
        Expr::logical_and(Expr::logical_and(int(4), int(5)), int(99))
    );
    assert_parse!(
        expr8("4 + 5 && 99"),
        Expr::logical_and(Expr::binary_op("+", int(4), int(5)), int(99))
    );
}
#[test]
fn test_expr7() {
    assert_parse!(
        expr7("4 > 5 <= 99"),
        Expr::binary_op("<=", Expr::binary_op(">", int(4), int(5)), int(99))
    );
    assert_parse!(
        expr7("4 + 5 == 99"),
        Expr::binary_op("==", Expr::binary_op("+", int(4), int(5)), int(99))
    );
}
#[test]
fn test_expr6() {
    assert_parse!(
        expr6("4 | 5 | 99"),
        Expr::binary_op("|", Expr::binary_op("|", int(4), int(5)), int(99))
    );
    assert_parse!(
        expr6("4 | 5 ^ 99"),
        Expr::binary_op("|", int(4), Expr::binary_op("^", int(5), int(99)))
    );
}
#[test]
fn test_expr5() {
    assert_parse!(
        expr5("4 ^ 5 ^ 99"),
        Expr::binary_op("^", Expr::binary_op("^", int(4), int(5)), int(99))
    );
    assert_parse!(
        expr5("4 ^ 5 & 99"),
        Expr::binary_op("^", int(4), Expr::binary_op("&", int(5), int(99)))
    );
}
#[test]
fn test_expr4() {
    assert_parse!(
        expr4("4 & 5 & 99"),
        Expr::binary_op("&", Expr::binary_op("&", int(4), int(5)), int(99))
    );
    assert_parse!(
        expr4("4 + 5 & 99"),
        Expr::binary_op("&", Expr::binary_op("+", int(4), int(5)), int(99))
    );
}
#[test]
fn test_expr3() {
    assert_parse!(
        expr4("4 >> 5 << 99"),
        Expr::binary_op("<<", Expr::binary_op(">>", int(4), int(5)), int(99))
    );
    assert_parse!(
        expr4("4 << 5 - 99"),
        Expr::binary_op("<<", int(4), Expr::binary_op("-", int(5), int(99)))
    );
}
#[test]
fn test_expr2() {
    assert_parse!(
        expr2("3 * 9 - 4"),
        Expr::binary_op("-", Expr::binary_op("*", int(3), int(9)), int(4)),
    );
    assert_parse!(
        expr2("4\n+\r3 *     \n\t\r 2"),
        Expr::binary_op("+", int(4), Expr::binary_op("*", int(3), int(2)))
    );
}
#[test]
fn test_expr1() {
    assert_parse!(
        expr2("3 + 9 - 4"),
        Expr::binary_op("-", Expr::binary_op("+", int(3), int(9)), int(4)),
    );
    assert_parse!(
        expr2("4\n-\r3 +     \n\t\r 2"),
        Expr::binary_op("+", Expr::binary_op("-", int(4), int(3)), int(2))
    );
}
#[test]
fn test_term() {
    assert_parse!(term("42"), int(42));
}

#[test]
fn test_character() {
    assert_parse!(character("'a'"), b'a');
    assert_parse!(character("'\\n'"), b'\n');
    assert_parse!(character("'\\060'"), b'0');
}

fn int(val: i64) -> Expr<'static> {
    Expr::Literal(Literal::Integer {
        val,
        position: Span::new(""),
    })
}
