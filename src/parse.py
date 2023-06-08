class Expr:
    def __str__(self) -> str:
        return "Expr"

class BinaryExpr(Expr):
    def __init__(self, left: Expr, op: str, right: Expr):
        self.left = left
        self.op = op
        self.right = right
    
    def __str__(self) -> str:
        """
        example: [+]─[-]─1
                  │   └──2
                  └──3
        """
        result = str(self.left).split('\n')
        result[0] = f"[{self.op}]─{result[0]}"
        for i in range(1, len(result)):
            result[i] = f" │  {result[i]}"
        right = str(self.right).split('\n')
        right[0] = f" └──{right[0]}"
        for i in range(1, len(right)):
            right[i] = f"    {right[i]}"
        result.extend(right)
        return "\n".join(result)
        
class UnaryExpr(Expr):
    def __init__(self, op: str, expr: Expr):
        self.op = op
        self.expr = expr
        
    def __str__(self) -> str:
        """
        example: [&]─[+]─1
                      └──2
        """
        result = str(self.expr).split('\n')
        result[0] = f"[{self.op}]─{result[0]}"
        for i in range(1, len(result)):
            result[i] = f"    {result[i]}"
        return "\n".join(result)
        
class LiteralExpr(Expr):
    def __init__(self, value: str):
        self.value = value
    
    def __str__(self) -> str:
        return self.value

class Token:
    """kind: number or operator"""
    precedence_table = {
        "+": 2,
        "-": 2,
        "*": 3,
        "/": 3,
        "%": 1,
        "!": 2,
        "&": 3,
    }
    def __init__(self, kind: str, value: str) -> None:
        self.kind = kind
        self.value = value
        
    def __str__(self) -> str:
        return f"Token({self.kind}, {self.value})"
    
    def is_unary(self) -> bool:
        return self.kind == "operator" and self.value in "&!%"
    
    def is_binary(self) -> bool:
        return self.kind == "operator" and self.value in "+-*/"
    
    def precedence(self) -> int:
        return self.precedence_table[self.value]
    
class Lexer:
    def __init__(self, text: str) -> None:
        self.text = text
        self.pos = 0
        
    def next_token(self) -> Token:
        if self.pos >= len(self.text):
            return Token("eof", "")
        c = self.text[self.pos]
        if c in "0123456789":
            self.pos += 1
            return Token("number", c)
        if c in "+-*/&!%":
            self.pos += 1
            return Token("operator", c)
        raise Exception(f"Unknown token: {c}")

class Parser:
    def __init__(self, tokens: list[str]) -> None:
        self.tokens = tokens
        self.pos = 0
    
    def peek(self) -> Token:
        if self.pos >= len(self.tokens):
            return Token("eof", "")
        return self.tokens[self.pos]
    
    def advance(self) -> Token:
        token = self.peek()
        self.pos += 1
        return token
    
    def parse_literal(self) -> LiteralExpr:
        return LiteralExpr(self.advance().value)
    
    def parse_expr(self, min_precedence: int) -> Expr:
        lookahead = self.peek()
        if lookahead.is_unary():
            op = self.advance()
            lhs = UnaryExpr(op.value, self.parse_expr(op.precedence() + 1))
        else:
            lhs = self.parse_literal()
        lookahead = self.peek()
        while lookahead.is_binary() and lookahead.precedence() >= min_precedence:
            op = self.advance()
            rhs = self.parse_expr(op.precedence())
            lhs = BinaryExpr(lhs, op.value, rhs)
            lookahead = self.peek()
        return lhs
    
if __name__ == "__main__":
    text = "!1*4/&3+&%2*3+%1*2"
    lexer = Lexer(text)
    tokens = []
    while True:
        token = lexer.next_token()
        tokens.append(token)
        if token.kind == "eof":
            break
    parser = Parser(tokens)
    expr = parser.parse_expr(0)
    print(text)
    print(expr)