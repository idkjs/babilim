/** Metafmt is a Format overlay that implements positional format specifier.

    For instance the format string ["%2$s %1$s %2$s"] requires two strings
    arguments and print the second string twice.

    Currently, this comes at the cost of advanced features of Format:
    subformat nesting ["%( %)"], user-provided padding and precision
    ["%*.*d"], format printing ["%{ %}"] and nested format inside tag and
    box specification ["@{<@[<%s>%s@]>"].

*/;

/** empty type */

type empty = Witness.eq(int, char);

/** {1 Heterogeneous list} */;

module L: {
  type t('a, 'res) =
    | []: t('res, 'res)
    | ::('a, t('list, 'res)): t('a => 'list, 'res);
  type fix('a) = t('a, empty);
};

/** [apply f [x_1;…;x_n]] computes [f x_1 … x_n] */

let apply: ('f, L.t('f, 'result)) => 'result;

type index('x, 'list) =
  | Z: index('x, 'x => _)
  | /** The unary integer, n= S (…(Z)…), select the type of the n-th element
    of the heterogeneous list L.t */
    S(
      index('x, 'list),
    )
    : index('x, _ => 'list);

/** [nth k l] returns the k-th element of the list.
    Raise [Invalid_argument] if the index is greater
    than the list length. Note that this cannot happen
    if the type of the list is fixed to [L.fix] .
 */

let nth: (index('x, 'list), L.t('list, 'r)) => 'x;

/** Heterogeneous list of indices of an heterogeneous list */

module Meta: {
  type t('indexed, 'source, 'final) =
    | []: t('res, 'source, 'res)
    | ::(index('x, 'source), t('indexed, 'source, 'final))
      : t('x => 'indexed, 'source, 'final);
};

/** [f [k_1;…;k_l] [x_1;…;x_n]] computes [f x_{k_1} x_{k_2} … x_{k_l}].
      Note that this functions will not type-check if [max_l k_l > n] */

let indexed_apply:
  ('f, Meta.t('f, 'arguments, 'result), L.fix('arguments)) => 'result;

/** {1 Auxiliary types } */

module Modal: {
  /** Modal modifier to the specifier */;

  type modifier =
    | Plus
    | Space
    | Hash;
  type ext =
    | Lit(int)
    | Star;

  type t = {
    modifier: option(modifier),
    padding: option(ext),
    precision: option(ext),
    variant: option(string),
  };

  let default: t;

  let modifier: string => option(modifier);
  let ext: string => option(ext);

  let lexmodal: (~m: string, ~pa: string, ~pr: option(string), string) => t;
};

module Formatting_box: {
  /** Formatting boxes */

  type t =
    | HV
    | V
    | HOV
    | C;

  let to_string: t => string;
};

/** {1 Format atom} */

type atom('a, 'driver, 'mid) =
  | Text(string): atom(_)
  | Hole({
      modal: Modal.t,
      arg:
        Witness.arg({
          .
          x: 'x,
          fl: _,
          l: _,
          driver: 'driver,
          mid: 'mid,
        }),
      pos: index('x, 'src),
    })
    : atom('src, 'driver, 'mid)
  | Break({
      space: int,
      indent: int,
    })
    : atom(_)
  | Open_box({
      kind: Formatting_box.t,
      indent: int,
    })
    : atom(_)
  | Open_tag(string): atom(_)
  | Close_box: atom(_)
  | Close_tag: atom(_)
  | Flush: atom(_)
  | Fullstop: atom(_)
  | Newline: atom(_);

/** Main type */

type t('a, 'driver, 'mid) =
  | []: t(_)
  | ::(atom('a, 'driver, 'mid), t('a, 'driver, 'mid))
    : t('a, 'driver, 'mid);

/** kfprintf with arguments boxed inside a heterogeneous list */

let kfprintf:
  (
    Format.formatter => 'result,
    Format.formatter,
    t('args, Format.formatter, unit),
    L.t('args, 'result)
  ) =>
  'result;

/** [expand_full spec f] takes an uncurried function [f]
    as argument and returns a curryied function with the format calling
    convention */

let expand_full:
  (
    Witness.l(
      ('metafmt_args, 'format_args),
      ('result, 'result),
      'formatter,
      'mid,
    ),
    L.t('metafmt_args, 'result) => 'result
  ) =>
  'format_args;

/** [expand spec f] takes an uncurried function [f]
    as argument and returns a curryied function with the metafmt calling
    convention */

let expand:
  (
    Witness.l(
      ('metafmt_args, 'format_args),
      ('result, 'result),
      'formatter,
      'mid,
    ),
    L.t('metafmt_args, 'result) => 'result
  ) =>
  'metafmt_args;

let int: index(int, 'l) => atom('l, _, _);
let float: index(float, 'l) => atom('l, _, _);
let str: index(string, 'l) => atom('l, _, _);
let show: index(Witness.show('a, 'b), 'l) => atom('l, 'a, 'b);

let _0: index('x, 'x => 'rest);
let _1: index('x, ('first, 'x) => 'rest);
let _2: index('x, ('first, 'second, 'x) => 'rest);
let _3: index('x, ('first, 'second, 'third, 'x) => 'rest);
let _4: index('x, ('first, 'second, 'third, 'fourth, 'x) => 'rest);

module Box: {
  /** Metafmt format specifier boxed together with a type witness */;

  type b('fin, 'driver, 'mid) =
    | /** This type forget the argument used by the metafmt format */
      Box(
        Witness.l(('core, _), ('fin, _), 'driver, 'mid),
        t('core, 'driver, 'mid),
      )
      : b('fin, 'driver, 'mid);

  /** This type adds an universal quantification on the result,
       intermediary result and formatter type */

  [@unboxed]
  type u = {u: 'fin 'driver 'mid. b('fin, 'driver, 'mid)};

  /** A shortcut avoiding the need to prove the fact that
      box types can always be universally quatified */

  let unsafe: b('fin, 'driver, 'mid) => u;

  exception Metafmt_type_error;

  /** [kfprintf k spec box ppf] checks that the boxed type witness
        and the type witness provided as an argument are compatible and then call
        the unboxed version of [kfprintf] if they are or
        raise [Metafmt_type_error] otherwise
    */

  let kfprintf:
    (
      Format.formatter => 'r,
      Witness.h('x, 'r, 'r, Format.formatter, unit),
      u,
      Format.formatter
    ) =>
    'x;

  let fprintf:
    (
      Witness.h('x, unit, unit, Format.formatter, unit),
      u,
      Format.formatter
    ) =>
    'x;

  let sprintf:
    (Witness.h('x, string, string, Format.formatter, unit), u) => 'x;
};
