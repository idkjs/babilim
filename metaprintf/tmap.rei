/** Translation map */;

type t;
type tmap = t;
let empty: t;

/** [box_then_add ?num ?ctx fmt metafmt tmap] registers the
    translations metafmt for fmt. The [num] and [ctx]
    optional arguments precises respectively the grammatical number of
    the translation and its context */

let box_then_add:
  (
    ~num: int=?,
    ~ctx: string=?,
    format6('a, _, _, _, _, _),
    Witness.l(('ab, 'a), (unit, unit), Format.formatter, unit),
    Metafmt.t('ab, Format.formatter, unit),
    t
  ) =>
  t;

/** [xfprintf tmap ppf ?num ?ctx fmt] uses the translation
    [tmap] to find a potential translation to [fmt] and print
    this translation with the same expected arguments than
    the original format
*/

let xfprintf:
  (
    t,
    Format.formatter,
    ~num: int=?,
    ~ctx: string=?,
    format('a, Format.formatter, unit)
  ) =>
  'a;

/** [xkfprintf tmap k ppf ?num ?ctx fmt] uses the translation
    [tmap] to find a potential translation to [fmt] and print
    this translation with the same expected arguments than
    the original format and applies the [k] to the formatter
    once the printing is finished
*/

let xkfprintf:
  (
    t,
    Format.formatter => 'r,
    Format.formatter,
    ~num: int=?,
    ~ctx: string=?,
    format6('a, Format.formatter, unit, 'c, 'd, 'r)
  ) =>
  'a;

/** [xsprintf tmap k ppf ?num ?ctx fmt] uses the translation
    [tmap] to find a potential translation to [fmt] and print
    this translation inside an internal buffer with the same expected arguments
    than the original format. Once the printing finished it outputs the
    printed string
*/

let xsprintf:
  (
    t,
    ~num: int=?,
    ~ctx: string=?,
    format6('a, Format.formatter, unit, 'c, 'd, string)
  ) =>
  'a;

/** [add ~num ~ctx (Dyn {ref;fmt;_}) tmap] registers the metafmt [fmt] as
    a translations for the reference format [ref]. The [num] and [ctx]
    argument precises respectively the grammatical number of the translation
    and its context */

let add:
  (
    ~num: int=?,
    ~ctx: string=?,
    Untyped.dyn(unit, unit, Format.formatter, unit),
    t
  ) =>
  t;

module Store: {
  /** Storing translation maps */;

  /** Stored translation: include the lang and the projection expression
      for computing grammatical number classes from a given integer */

  type t = {
    lang: string,
    plural: Math_expr.any,
    translations: tmap,
  };

  /** [write translations filename] */

  let write: (t, string) => unit;

  /** [read filename] try to read the file [filename] and returns
        the stored translation info if successful */

  let read: string => option(t);
};

module Implementation: {
  /** Implementation function */;

  type t = {
    /** Singular form printing function */
    kfprintf:
      'a 'r.
      (
        Format.formatter => 'r,
        Format.formatter,
        format4('a, Format.formatter, unit, 'r)
      ) =>
      'a,

    /** Plural form printing function */
    knfprintf:
      'a 'r.
      (
        Format.formatter => 'r,
        Format.formatter,
        int,
        format4('a, Format.formatter, unit, 'r) as 'f,
        'f
      ) =>
      'a,

  };

  /** Default implementation for singular form is
        [Format.kfprintf]. And the plural [knfprintf] is using
        the english/germanic distinction between singular and
        plural.
    */

  let default: t;

  /** Create an implementation from grammatical number classifier and
    a translation map */

  let from_map: (Math_expr.any, tmap) => t;

  /** [from_store filename] creates an implementation from a stored translation
      map */

  let from_store: string => option(t);
};
