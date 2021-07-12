/** Dynamicaly parse from strings */;

/** Parse a format string */

let fmt: string => Untyped.Cfmt.t('a, 'b, 'c);

/** [metafmt src ref] Parse a metaformat string from [src],
      compatible with the format string [src] */

let metafmt:
  (string, format6('a, 'b, 'c, 'd, 'e, 'f)) => Untyped.dyn('g, 'f, 'b, 'c);
