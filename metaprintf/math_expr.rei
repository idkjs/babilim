/** Math expression for plural form

    The notion of singular and plural varies across languages.
    For instance, French consider that any number [n>1] should be plural and
    [0] or [1] is singular. Contrarily, English only uses plural for [n=1].
    Similarly dual (aka a special case for [n=2]) is relatively common across
    languages, and Czech (and other slavic language like Polish)
    makes a grammatical distinction between [n=1],[n=2],[n=3],[n=4]
    and [n=5+].

    The mini math interpreter is then used to project a number
    [n] to the right grammatical category.

*/;

/** Math expression */

type t('a) =
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

/** Boxed math expression */

[@unboxed]
type any =
  | Any(t(_)): any;

/** [eval_int n expr] evals the value of the expression for [n],
    converting bool to integer using the C-convention [true=1]
    and [false=0] */

let eval_int: (int, any) => int;
