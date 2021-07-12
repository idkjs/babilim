/** Format string analysis */;

/** Nested format argument [%( %)], format printing
    [%{ %}] and custom printer are not supported */

exception Unsupported(string);

/** Recompute a list of specificiers from a format string */

let typer:
  CamlinternalFormatBasics.fmt('a, 'b, 'c, 'd, 'e, 'f) =>
  Witness.h('a, 'f, 'g, 'b, 'c);
