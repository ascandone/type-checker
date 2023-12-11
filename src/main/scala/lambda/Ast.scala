package lambda

enum Declaration:
  case Let(name: String, value: Expr)

enum Expr:
  case Var(name: String)
  case App(f: Expr, x: Expr)
  case Abs(param: String, body: Expr)
  case Let(name: String, value: Expr, body: Expr)
