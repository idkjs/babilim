/** Dynamically typed parsers

    Parse dynamically format strings and metafmt strings from
    untyped strings.
*/;

/** Dynamic type means potential runtime errors */

exception Dynamic_type_error(string);

module Cfmt: {
  /** Dynamic parsing of format strings */;

  /** Forget the type of specifiers */

  type t('fmter, 'mid, 'fin) =
    | Dyn(CamlinternalFormatBasics.fmt('a, 'fmter, 'mid, 'd, 'e, 'fin))
      : t('fmter, 'mid, 'fin); /***/

  /** Universally quantify over formatter, result and intermediary types */

  [@unboxed]
  type u = {u: 'fmter 'mid 'fin. t('fmter, 'mid, 'fin)};

  /** shortcut */

  let unsafe: t('a, 'b, 'c) => u;

  /** Empty format string */

  let nil: t('a, 'b, 'c);

  /** [cons (_,spec) fmt] adds a specifier to the format string [fmt] */

  let scons: (('any, string), t('a, 'b, 'c)) => t('a, 'b, 'c);

  /** [tcons text fmt] adds a text element to the format string [fmt] */

  let tcons: (string, t('a, 'b, 'c)) => t('a, 'b, 'c);

  /** [tcons flit fmt] adds a formatting element to the format string [fmt] */

  let fcons:
    (CamlinternalFormatBasics.formatting_lit, t('a, 'b, 'c)) => t('a, 'b, 'c);

  /** [box_cons kind indent fmt] open a formatting box [@[<kind indent]
      inside the format string [fmt] */

  let box_cons:
    (Metafmt.Formatting_box.t, int, t('a, 'b, 'c)) => t('a, 'b, 'c);

  /** [tag_cons text fmt] open a tag [text] inside the format string [fmt] */

  let tag_cons: (string, t('a, 'b, 'c)) => t('a, 'b, 'c);
};

/** {1 Main types } */;

type dyn('finl, 'finr, 'fmter, 'mid) =
  | /** [Dyn {fmt;spec;ref} Dynamically typed metafmt [fmt] expected to respect
    the specifier list [spec] extracted from the format string [ref]
*/
    Dyn({
      spec: Witness.l(('a, 'm), ('finl, 'finr), 'fmter, 'mid),
      ref: format6('m, 'fmter, 'mid, 'd, 'e, 'finr),
      fmt: Metafmt.t('a, 'fmter, 'mid),
    })
    : dyn('finl, 'finr, 'fmter, 'mid);

/** Dynamically typed atom */

[@unboxed]
type atom('driver, 'mid) =
  | A(Metafmt.atom('a, 'driver, 'mid)): atom('driver, 'mid);

/** Dynamically typed index */

[@unboxed]
type nat =
  | I(Metafmt.index('x, 'src)): nat;

/** Dynamically argument atom */

[@unboxed]
type arg('d, 'mid) =
  | W(
      Witness.arg({
        .
        x: 'x,
        fl: _,
        l: _,
        mid: 'mid,
        driver: 'd,
      }),
    )
    : arg('d, 'mid);

exception Unsupported(string);

/** [nil fmt] creates an empty metafmt, expected to be compatible with
    the format [fmt] */

let nil: format6('a, 'b, 'c, 'd, 'e, 'f) => dyn('g, 'f, 'b, 'c);

/** [cons atom metafmt] appends [atom] to the metafmt string */

let cons:
  (atom('d, 'mid), dyn('finl, 'finr, 'd, 'mid)) =>
  dyn('finl, 'finr, 'd, 'mid);

/** [hcons (m,arg,n) metafmt] appends the hole [Hole(m,arg,n) ] to the
    metafmt string */

let hcons:
  ((Metafmt.Modal.t, arg('a, 'b), nat), dyn('finl, 'finr, 'a, 'b)) =>
  dyn('finl, 'finr, 'a, 'b);

/** [integer n] computes the dynamically typed index [n] */

let integer: int => nat;

/** [arg s] computes the dynamically typed arg */

let arg: (Metafmt.Modal.t, string) => arg('a, 'b);
