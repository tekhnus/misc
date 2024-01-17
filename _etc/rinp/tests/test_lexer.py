import pytest
from rinp.sql.lexer import tokenize, Piece, Literal


@pytest.mark.parametrize(
    ["given_text", "expect_tokens"],
    [
        # Test basic functionality.
        [
            "select 'Hello, world';",
            [Piece("select"), Literal("Hello, world", "'"), Piece(";")],
        ],
        [
            'select "Hello, world";',
            [Piece("select"), Literal("Hello, world", '"'), Piece(";")],
        ],
        [
            "select  Hello, world ;",
            [Piece("select"), Piece("Hello"), Piece(","), Piece("world"), Piece(";")],
        ],
        # Test that space is assumed where needed.
        ["select 3x", [Piece("select"), Piece("3"), Piece("x")]],
        ["select x3", [Piece("select"), Piece("x3")]],
        ["1>2", [Piece("1"), Piece(">"), Piece("2")]],
        ["a>b", [Piece("a"), Piece(">"), Piece("b")]],
        ["1!=2", [Piece("1"), Piece("!="), Piece("2")]],
        ["1=-2", [Piece("1"), Piece("=-"), Piece("2")]],
        ["a'abc'a", [Piece("a"), Literal("abc", "'"), Piece("a")]],
        ["&'abc'&", [Piece("&"), Literal("abc", "'"), Piece("&")]],
        # Test that space is always respected.
        ["1 ! = 2", [Piece("1"), Piece("!"), Piece("="), Piece("2")]],
    ],
)
def test_tokenize(given_text, expect_tokens):
    assert tokenize(given_text) == expect_tokens
