/** Type-level witness */;

type eq(_, _) =
  | /** Equality type */
    Eq: eq('x, 'x);

type show('driver, 'c) =
  | /** [Show(f,x)] groups together the two part of a %a specifier */
    Show(
      ('driver, 'a) => 'c,
      'a,
    )
    : show('driver, 'c);

/** Simple specifier */

type exs('x, 'm) = {
  .
  x: 'x,
  l: 'l,
  fl: 'x => 'l,
  driver: 'driver,
  mid: 'c,
}
constraint 'm = ('l, 'driver, 'c);

/** [%a] specifier */

type alpha('m) = {
  .
  x: show('driver, 'c),
  l: 'l,
  fl: (('driver, 'y) => 'c, 'y) => 'l,
  driver: 'driver,
  mid: 'c,
}
constraint 'm = ('l, 'driver, 'y, 'c);

/** [%t] specifier */

type theta('m) = {
  .
  x: 'driver => 'c as 't,
  l: 'l,
  fl: 't => 'l,
  driver: 'driver,
  mid: 'c,
}
constraint 'm = ('l, 'driver, 'c);

/** Simple specifier witness */

type s(_) =
  | Char: s(char)
  | Int: s(int)
  | Int32: s(int32)
  | Int64: s(int64)
  | Nativeint: s(nativeint)
  | Bool: s(bool)
  | Float: s(float)
  | String: s(string); /***/

/** Witness the equality between two simple specifier */

let (\===): (s('x), s('y)) => option(eq('x, 'y));

/** Generic argument type for witness list */

type arg(_) =
  | S(s('x)): arg(exs('x, _))
  | Int_param(s('y))
    : arg({
        .
        x: (int, 'y),
        l: 'l,
        fl: (int, 'y) => 'l,
        driver: 'driver,
        mid: 'c,
      })
  | Int2_param(s('y))
    : arg({
        .
        x: (int, int, 'y),
        l: 'l,
        fl: (int, int, 'y) => 'l,
        driver: 'driver,
        mid: 'c,
      })
  | A: arg(alpha(_))
  | T: arg(theta(_)) /***/

/** Witness for the type of a list of specifier */

and l(_, _, _, _) =
  | []: l(('tail, 'moretail), ('tail, 'moretail), 'driver, 'mid)
  | ::(
      arg({
        .
        x: 'x,
        l: 'm,
        fl: 'fm,
        driver: 'driver,
        mid: 'mid,
      }),
      l(('l, 'm), 'tail, 'driver, 'mid),
    )
    : l(('x => 'l, 'fm), 'tail, 'driver, 'mid); /***/

/** Witness equality for the metafmt argument convention
    (aka one argument by specifier)
*/

let leq:
  (l(('a, _), ('r, _), 'd, 'm), l(('b, _), ('r, _), 'd, 'm)) =>
  option(eq('a, 'b));

/** Append two specifier list witnesses */

let (@):
  (l('lists, 'middle_lists, 'd, 'm), l('middle_lists, 'results, 'd, 'm)) =>
  l('lists, 'results, 'd, 'm);

type dyn('b, 'd, 'mid, 'driver) =
  | /** Forget the types of the arguments in both convention */
    Dyn(
      l(('a, 'b), ('c, 'd), 'mid, 'driver),
    )
    : dyn('b, 'd, 'mid, 'driver);

type h(_, _, _, _, _) =
  | /** Only remember the argument types in the format convention */
    H(
      l(('a, 'b), ('c, 'd), 'dr, 'm),
    )
    : h('b, 'd, 'c, 'dr, 'm);

/** Append two format-side specifier lists */

let (@/):
  (h('list, 'mid_list, 'y, 'd, 'm), h('mid_list, 'result, 'y, 'd, 'm)) =>
  h('list, 'result, 'y, 'd, 'm);
