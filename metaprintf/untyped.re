module Internal = CamlinternalFormatBasics;

exception Dynamic_type_error(string);
open Metafmt;
module W = Witness;

module Cfmt = {
  type s = {
    label: string,
    pa: int,
    pr: string,
    mode: string,
  };
  type t('fmter, 'mid, 'fin) =
    | Dyn(Internal.fmt('a, 'fmter, 'mid, 'd, 'e, 'fin))
      : t('fmter, 'mid, 'fin);

  [@unboxed]
  type u = {u: 'fmter 'mid 'fin. t('fmter, 'mid, 'fin)};
  let unsafe = (d: t(_, _, _)) => {u: Obj.magic(d)};

  let nil = Dyn(Internal.End_of_format);
  exception Not_implemented(string);
  open Internal;
  let scons = ((_m, s), Dyn(l)) =>
    switch (s) {
    | "d" => Dyn( Int(Int_d, No_padding, No_precision, l))
    | "i" => Dyn( Int(Int_i, No_padding, No_precision, l))
    | "x" => Dyn( Int(Int_x, No_padding, No_precision, l))
    | "X" => Dyn( Int(Int_X, No_padding, No_precision, l))
    | "o" => Dyn( Int(Int_o, No_padding, No_precision, l))
    | "O" => Dyn( Int(Int_o, No_padding, No_precision, l))
    | "ld" =>
      Dyn( Int32(Int_d, No_padding, No_precision, l))
    | "li" =>
      Dyn( Int32(Int_i, No_padding, No_precision, l))
    | "lx" =>
      Dyn( Int32(Int_x, No_padding, No_precision, l))
    | "lX" =>
      Dyn( Int32(Int_X, No_padding, No_precision, l))
    | "lo" =>
      Dyn( Int32(Int_o, No_padding, No_precision, l))
    | "lO" =>
      Dyn( Int32(Int_o, No_padding, No_precision, l))
    | "Ld" =>
      Dyn( Int64(Int_d, No_padding, No_precision, l))
    | "Li" =>
      Dyn( Int64(Int_i, No_padding, No_precision, l))
    | "Lx" =>
      Dyn( Int64(Int_x, No_padding, No_precision, l))
    | "LX" =>
      Dyn( Int64(Int_X, No_padding, No_precision, l))
    | "Lo" =>
      Dyn( Int64(Int_o, No_padding, No_precision, l))
    | "LO" =>
      Dyn( Int64(Int_o, No_padding, No_precision, l))
    | "nd" =>
      Dyn( Nativeint(Int_d, No_padding, No_precision, l))
    | "ni" =>
      Dyn( Nativeint(Int_i, No_padding, No_precision, l))
    | "nx" =>
      Dyn( Nativeint(Int_x, No_padding, No_precision, l))
    | "nX" =>
      Dyn( Nativeint(Int_X, No_padding, No_precision, l))
    | "no" =>
      Dyn( Nativeint(Int_o, No_padding, No_precision, l))
    | "nO" =>
      Dyn( Nativeint(Int_o, No_padding, No_precision, l))
    | "f" =>
      Dyn( Float(Float_f, No_padding, No_precision, l))
    | "g" =>
      Dyn( Float(Float_g, No_padding, No_precision, l))
    | "e" =>
      Dyn( Float(Float_e, No_padding, No_precision, l))
    | "b"
    | "B" => Dyn( Bool(No_padding, l))
    | "c" => Dyn(Char(l))
    | "C" => Dyn(Caml_char(l))
    | "s" => Dyn( String(No_padding, l))
    | "S" => Dyn( Caml_string(No_padding, l))
    | "a" => Dyn(Alpha(l))
    | "t" => Dyn(Theta(l))
    | "!" => Dyn(Flush(l))

    | s =>
      raise(Not_implemented(Format.sprintf("Untyped.Cfmt.scons: %s", s)))
    };

  let tcons = (t, Dyn(l)) =>
    if (String.length(t) == 1) {
      Dyn( Char_literal(t.[0], l));
    } else {
      Dyn( String_literal(t, l));
    };

  let fcons = (f, Dyn(l)) => Dyn( Formatting_lit(f, l));

  let box_cons = (kind, indent, Dyn(l)) => {
    let box =
      Format.sprintf(
        "<%s %d>",
        Metafmt.Formatting_box.to_string(kind),
        indent,
      );
    Dyn(
       
      Formatting_gen(
        Open_box(
           
          Format(
             String_literal(box, End_of_format),
            "@[" ++ box,
          ),
        ),
        l,
      ),
    );
  };

  let tag_cons = (s, Dyn(l)) => {
    let tag = Format.sprintf("<%s>", s);
    Dyn(
       
      Formatting_gen(
        Open_tag(
           
          Format(
             String_literal(tag, End_of_format),
            "@{" ++ tag,
          ),
        ),
        l,
      ),
    );
  };
};

type dyn('finl, 'finr, 'fmter, 'mid) =
  | Dyn({
      spec: W.l(('a, 'm), ('finl, 'finr), 'fmter, 'mid),
      ref: format6('m, 'fmter, 'mid, 'd, 'e, 'finr),
      fmt: t('a, 'fmter, 'mid),
    })
    : dyn('finl, 'finr, 'fmter, 'mid);

[@unboxed]
type atom('driver, 'mid) =
  | A(Metafmt.atom('a, 'driver, 'mid)): atom('driver, 'mid);

[@unboxed]
type nat =
  | I(index('x, 'src)): nat;
[@unboxed]
type arg('d, 'mid) =
  | W(
      W.arg({
        .
        x: 'x,
        fl: _,
        l: _,
        mid: 'mid,
        driver: 'd,
      }),
    )
    : arg('d, 'mid);

let error = s => raise(Dynamic_type_error(s));

let (%=%) = (type a, type b, x: W.s(a), y: W.s(b)): W.eq(a, b) =>
  switch (W.(x \=== y)) {
  | None => error("Incompatible element types")
  | Some(W.Eq) => W.Eq
  };

let rec compat:
  type x x2 src dest a b c d e dr fl fr mid.
    (
      W.arg({
        .
        x: x,
        l: d,
        fl: e,
        driver: dr,
        mid: mid,
      }),
      index(x2, src),
      W.l((dest, a), (fl, fr), dr, mid)
    ) =>
    index(x, dest) =
  W.(
    (arg, index, spec) =>
      switch (arg, index, spec) {
      | (_, _, W.[]) => error("Mismatched position and number of arguments")
      | (W.S(x), Z, [W.S(y), ..._]) =>
        switch (x %=% y) {
        | Eq => Z
        }
      | (W.Int_param(x), Z, [W.Int_param(y), ..._]) =>
        switch (x %=% y) {
        | Eq => Z
        }
      | (W.Int2_param(x), Z, [W.Int2_param(y), ..._]) =>
        switch (x %=% y) {
        | Eq => Z
        }
      | (W.A, Z, [W.A, ..._]) => Z
      | (W.T, Z, [W.T, ..._]) => Z
      | (_, S(n), [_, ...q]) => S(compat(arg, n, q))

      /* Error messages */
      | (W.A, Z, [W.S(_), ..._]) => error("Expexted %a got %simple")
      | (W.T, Z, [W.S(_), ..._]) => error("Expexted %t got %simple")
      | (W.S(_), Z, [W.T, ..._]) => error("Expexted %simple got %t")
      | (W.S(_), Z, [W.A, ..._]) => error("Expexted %simple got %a")
      | (W.T, Z, [W.A, ..._]) => error("Expected %t got %a")
      | (W.A, Z, [W.T, ..._]) => error("Expected %a got %t")

      | (W.Int_param(_), Z, [W.S(_), ..._]) =>
        error("Expected int parameter %* got %simple")
      | (W.Int_param(_), Z, [W.A, ..._]) =>
        error("Expected int parameter %* got %a")
      | (W.Int_param(_), Z, [W.T, ..._]) =>
        error("Expected int parameter %* got %t")
      | (W.Int_param(_), Z, [W.Int2_param(_), ..._]) =>
        error("Expected int parameter %*, got %*.* int parameter")

      | (W.Int2_param(_), Z, [W.S(_), ..._]) =>
        error("Expected 2 int parameters %*.* got %simple")
      | (W.Int2_param(_), Z, [W.A, ..._]) =>
        error("Expected 2 int parameters %*.* got %a")
      | (W.Int2_param(_), Z, [W.T, ..._]) =>
        error("Expected 2 int parameters %*.* got %t")
      | (W.Int2_param(_), Z, [W.Int_param(_), ..._]) =>
        error("Expected 2 int parameters %*.*, got %* single int parameter")

      | (W.S(_), Z, [W.Int_param(_), ..._]) =>
        error("Expected %simple, got int parameter %*")
      | (W.A, Z, [W.Int_param(_), ..._]) =>
        error("Expected %a, got int parameter %*")
      | (W.T, Z, [W.Int_param(_), ..._]) =>
        error("Expected %t, got int parameter %*")
      | (W.S(_), Z, [W.Int2_param(_), ..._]) =>
        error("Expected %simple, got 2 int parameters %*.*")
      | (W.A, Z, [W.Int2_param(_), ..._]) =>
        error("Expected %simple, got 2 int parameters %*.*")
      | (W.T, Z, [W.Int2_param(_), ..._]) =>
        error("Expected %simple, got 2 int parameters %*.*")
      }
  );

let nil = ( Internal.Format(core, _) as fmt) => {
  let W.H(spec) = Conv.typer(core);
  Dyn({spec, ref: fmt, fmt: []});
};

let cons =
    (
      type finl,
      type finr,
      type d,
      type mid,
      A(atom): atom(d, mid),
      Dyn(r): dyn(finl, finr, d, mid),
    ) =>
  switch (atom) {
  | Text(s) => Dyn({...r, fmt: [Text(s), ...r.fmt]})
  | Open_box(b) => Dyn({...r, fmt: [Open_box(b), ...r.fmt]})
  | Open_tag(x) => Dyn({...r, fmt: [Open_tag(x), ...r.fmt]})
  | Close_box => Dyn({...r, fmt: [Close_box, ...r.fmt]})
  | Close_tag => Dyn({...r, fmt: [Close_tag, ...r.fmt]})
  | Fullstop => Dyn({...r, fmt: [Fullstop, ...r.fmt]})
  | Newline => Dyn({...r, fmt: [Newline, ...r.fmt]})
  | Break(b) => Dyn({...r, fmt: [Break(b), ...r.fmt]})
  | Flush => Dyn({...r, fmt: [Flush, ...r.fmt]})

  | Hole({modal, arg: W.S(x) as arg, pos}) =>
    let pos = compat(arg, pos, r.spec);
    Dyn({...r, fmt: [Hole({modal, arg: S(x), pos}), ...r.fmt]});

  | Hole({modal, arg: W.Int_param(x) as arg, pos}) =>
    let pos = compat(arg, pos, r.spec);
    Dyn({...r, fmt: [Hole({modal, arg: W.Int_param(x), pos}), ...r.fmt]});

  | Hole({modal, arg: W.Int2_param(x) as arg, pos}) =>
    let pos = compat(arg, pos, r.spec);
    Dyn({...r, fmt: [Hole({modal, arg: W.Int2_param(x), pos}), ...r.fmt]});

  | Hole({modal, arg: W.A as arg, pos}) =>
    let pos = compat(arg, pos, r.spec);
    Dyn({...r, fmt: [Hole({modal, arg: W.A, pos}), ...r.fmt]});
  | Hole({modal, arg: W.T as arg, pos}) =>
    let pos = compat(arg, pos, r.spec);
    Dyn({...r, fmt: [Hole({modal, arg: W.T, pos}), ...r.fmt]});
  };

let hcons =
    (
      type d,
      type m,
      (modal, W(arg): arg(d, m), I(n)),
      Dyn(r): dyn('finl, 'finr, d, m),
    ) =>
  switch (arg) {
  | W.S(x) =>
    let pos = compat(arg, n, r.spec);
    Dyn({...r, fmt: [Hole({modal, arg: S(x), pos}), ...r.fmt]});

  | W.Int_param(x) =>
    let pos = compat(arg, n, r.spec);
    Dyn({...r, fmt: [Hole({modal, arg: Int_param(x), pos}), ...r.fmt]});

  | W.Int2_param(x) =>
    let pos = compat(arg, n, r.spec);
    Dyn({...r, fmt: [Hole({modal, arg: Int2_param(x), pos}), ...r.fmt]});

  | W.A =>
    let pos = compat(arg, n, r.spec);
    Dyn({...r, fmt: [Hole({modal, arg: W.A, pos}), ...r.fmt]});
  | W.T =>
    let pos = compat(arg, n, r.spec);
    Dyn({...r, fmt: [Hole({modal, arg: W.T, pos}), ...r.fmt]});
  };

let rec integer = n =>
  if (n <= 1) {
    I(Z);
  } else {
    let I(n') = integer(n - 1);
    I(S(n'));
  };

exception Unsupported(string);
let unsupported = s => raise(Unsupported(s));

let is_star = x => x == Some(Modal.Star);
let promote = (type x, modal: Modal.t, x: W.s(x)) =>
  switch (is_star(modal.padding), is_star(modal.precision)) {
  | (true, true) => W(Int2_param(x))
  | (false, true)
  | (true, false) => W(Int_param(x))
  | (false, false) => W(S(x))
  };

let arg = modal => {
  let promote = x => promote(modal, x);
  fun
  | "d"
  | "i"
  | "x"
  | "o"
  | "X"
  | "O" => promote(Int)
  | "f"
  | "e"
  | "g" => promote(Float)
  | "b"
  | "B" => promote(Bool)
  | "s"
  | "S" => promote(String)
  | "ld"
  | "li"
  | "lx"
  | "lo"
  | "lX"
  | "lO" => promote(Int32)
  | "Ld"
  | "Li"
  | "Lx"
  | "Lo"
  | "LX"
  | "LO" => promote(Int64)
  | "nd"
  | "ni"
  | "nx"
  | "no"
  | "nX"
  | "nO" => promote(Nativeint)
  | "c"
  | "C" => W(S(Char))
  | "a" => W(A)
  | "t" => W(T)
  | s => unsupported(s);
};

module Gen = {
  let ff = Format.fprintf;

  let rec gen_pos = ppf =>
    fun
    | I(Z) => ff(ppf, "Z")
    | I(S(n)) => ff(ppf, "(S %a)", gen_pos, I(n));

  let gen_s = (ppf, type x, x: W.s(x)) =>
    switch (x) {
    | W.Int => ff(ppf, "Int")
    | W.Float => ff(ppf, "Float")
    | W.Int32 => ff(ppf, "Int32")
    | W.Int64 => ff(ppf, "Int64")
    | W.Nativeint => ff(ppf, "Nativeint")
    | W.Char => ff(ppf, "Char")
    | W.Bool => ff(ppf, "Bool")
    | W.String => ff(ppf, "String")
    };

  let rec gen_w = (type d, type m, ppf, x: arg(d, m)) =>
    switch (x) {
    | W(W.A) => ff(ppf, "A")
    | W(W.T) => ff(ppf, "T")
    | W(W.(S(x))) => ff(ppf, "S %a", gen_s, x)
    | W(W.(Int_param(x))) => ff(ppf, "Int_param %a", gen_s, x)
    | W(W.(Int2_param(x))) => ff(ppf, "Int2_param %a", gen_s, x)
    };

  let gen_elt = ppf =>
    fun
    | Text(s) => ff(ppf, "Text %S", s)
    | Hole({modal, arg, pos}) =>
      ff(ppf, "Hole(Modal.default, %a,%a)", gen_w, W(arg), gen_pos, I(pos))
    | Break(r) => ff(ppf, "Break{space=%d; indent=%d}", r.space, r.indent)
    | Open_box(r) => ff(ppf, "Open_tag{indent=%d; kind=H}", r.indent)
    | Close_tag => ff(ppf, "Close_tag")
    | Close_box => ff(ppf, "Close_box")
    | Open_tag(s) => ff(ppf, "Open_tag %s", s)
    | Newline => ff(ppf, "Newline")
    | Fullstop => ff(ppf, "Fullstop")
    | Flush => ff(ppf, "Flush");

  let rec gen_list = (ppf, Dyn(r)) =>
    switch (r.fmt) {
    | [] => ()
    | [a] => gen_elt(ppf, a)
    | [a, ...[_, ..._] as q] =>
      ff(ppf, "%a; %a", gen_elt, a, gen_list, Dyn({...r, fmt: q}))
    };

  let gen = ppf => ff(ppf, "Metaprintf.(Metafmt.[%a])", gen_list);
};
