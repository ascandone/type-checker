package lambda

enum Declaration:
  case Let(name: String, value: Expr)

enum Expr:
  case Variable(name: String)
  case App(f: Expr, x: Expr)
  case Fn(param: String, body: Expr)
