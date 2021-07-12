module W = Witness;

open CamlinternalFormatBasics;

let grouped =
    (
      type a,
      type b,
      type c,
      type x,
      type d,
      type f,
      type otr,
      type dr,
      type mid,
      pa: padding(a, b),
      pr: precision(b, x => d),
      x: Witness.s(x),
      W.H(l): W.h(d, f, otr, dr, mid),
    )
    : W.h(a, f, otr, dr, mid) =>
  switch (pa, pr) {
  | (Arg_padding(_), Arg_precision) => W.(H([Int2_param(x), ...l]))
  | (Arg_padding(_), Lit_precision(_)) => W.(H([Int_param(x), ...l]))
  | (Arg_padding(_), No_precision) => W.(H([Int_param(x), ...l]))
  | (Lit_padding(_), Arg_precision) => W.(H([Int_param(x), ...l]))
  | (No_padding, Arg_precision) => W.(H([Int_param(x), ...l]))
  | (Lit_padding(_), Lit_precision(_)) => W.(H([S(x), ...l]))
  | (No_padding, Lit_precision(_)) => W.(H([S(x), ...l]))
  | (No_padding, No_precision) => W.(H([S(x), ...l]))
  | (Lit_padding(_), No_precision) => W.(H([S(x), ...l]))
  };

let padded = (pa, x, l) => grouped(pa, No_precision, x, l);

exception Unsupported(string);
let unsupported = s => raise(Unsupported(s));
open W;
let (@) = (@/);
let rec typer:
  type a b c d e f g. fmt(a, b, c, d, e, f) => W.h(a, f, g, b, c) =
  fmt =>
    switch (fmt) {
    | Char(f) => Char <::> f
    | Caml_char(f) => Char <::> f
    |  String(p, f) => padded(p, String, typer(f))
    |  Caml_string(p, f) => padded(p, String, typer(f))
    |  Int(k, pa, pre, f) =>
      grouped(pa, pre, Int, typer(f))
    |  Int32(k, pa, pre, f) =>
      grouped(pa, pre, Int32, typer(f))
    |  Int64(k, pa, pre, f) =>
      grouped(pa, pre, Int64, typer(f))
    |  Nativeint(k, pa, pre, f) =>
      grouped(pa, pre, Nativeint, typer(f))
    |  Bool(pa, f) => padded(pa, Bool, typer(f))
    |  Float(_, pa, pre, f) =>
      grouped(pa, pre, Float, typer(f))

    | Flush(f) => typer(f)
    |  String_literal(_, f) => typer(f)
    |  Char_literal(_, f) => typer(f)
    |  Formatting_lit(_, f) => typer(f)

    |  Formatting_gen(f, f') => boxes(f) @/ typer(f')

    | End_of_format => H([])

    | Alpha(f) =>
      let W.H(r) = typer(f);
      H([A, ...r]);
    | Theta(f) =>
      let W.H(r) = typer(f);
      H([T, ...r]);

    | Format_subst(_) => unsupported("Format substitution %(fmt%)")
    | Format_arg(_) => unsupported("Format arg %{fmt%}")
    | Reader(_) => unsupported("Reader r")
    | Scan_char_set(_) => unsupported("Scanf char set […]")
    | Scan_get_counter(_) => unsupported("Scanf get_counter %n/%l/%N/µL")
    | Scan_next_char(_) => unsupported("Scanf next_char %0c")
    | Ignored_param(_) => unsupported("Scanf %_…")
    | Custom(_) => unsupported("Custom")
    }

and boxes:
  type a b c d e f g. formatting_gen(a, b, c, d, e, f) => W.h(a, f, g, b, c) =
  fun
  | Open_tag( Format(f, _)) => typer(f)
  | Open_box( Format(f, _)) => typer(f)

and (<::>):
  type b c x ll lm r d e f g.
    (s(x), fmt(r, b, c, d, e, f)) => h(x => r, f, g, b, c) =
  (x, f) => {
    let W.H(r) = typer(f);
    H([S(x), ...r]);
  };
