type t(_) =
  | And(t(bool), t(bool)): t(bool)
  | Or(t(bool), t(bool)): t(bool)
  | Greater(t(int), t(int)): t(bool)
  | Less(t(int), t(int)): t(bool)
  | GreaterEq(t(int), t(int)): t(bool)
  | LessEq(t(int), t(int)): t(bool)
  | Equal(t(int), t(int)): t(bool)
  | Not(t(bool)): t(bool)
  | Literal(int): t(int)
  | N: t(int)
  | Mod(t(int), t(int)): t(int)
  | If(t(bool), t(int), t(int)): t(int);

[@unboxed]
type any =
  | Any(t(_)): any;

let int_of_bool = b => if (b) {1} else {0};

let rec eval: type a. (int, t(a)) => a =
  n => {
    let eval = x => eval(n, x);
    fun
    | Literal(x) => x
    |  If(b, then', else') =>
      eval(
        if (eval(b)) {
          then';
        } else {
          else';
        },
      )
    |  Greater(a, b) => eval(a) > eval(b)
    |  Less(a, b) => eval(a) < eval(b)
    |  GreaterEq(a, b) => eval(a) >= eval(b)
    |  LessEq(a, b) => eval(a) <= eval(b)
    | Not(a) => !eval(a)
    |  And(x, y) => eval(x) && eval(y)
    |  Or(x, y) => eval(x) || eval(y)
    |  Equal(x, y) => eval(x) == eval(y)
    | N => n
    |  Mod(x, y) => eval(x) mod eval(y);
  };

let eval_int = (n, Any(x)) => {
  let eval = x => eval(n, x);
  switch (x) {
  | Literal(x) => x
  | If(_) as x => eval(x)
  | And(_) as x => int_of_bool(eval(x))
  | Or(_) as x => int_of_bool(eval(x))
  | Not(_) as x => int_of_bool(eval(x))
  | Equal(_) as x => int_of_bool(eval(x))
  | N => n
  | Mod(_) as x => eval(x)
  | Greater(_) as x => int_of_bool(eval(x))
  | Less(_) as x => int_of_bool(eval(x))
  | GreaterEq(_) as x => int_of_bool(eval(x))
  | LessEq(_) as x => int_of_bool(eval(x))
  };
};
