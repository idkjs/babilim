type empty = Witness.eq(int, char);

module L = {
  type t('a, 'res) =
    | []: t('res, 'res)
    | ::('a, t('list, 'res)): t('a => 'list, 'res);

  type fix('a) = t('a, empty);
};

type index('x, 'list) =
  | Z: index('x, 'x => _)
  | S(index('x, 'list)): index('x, _ => 'list);

let rec nth: type x fin list r. (index(x, list), L.t(list, r)) => x =
  (index, l) =>
    switch (index, l) {
    | (Z, L.[a, ..._]) => a
    | (S(n), L.[_, ...q]) => nth(n, q)
    | (_, L.[]) => raise(Invalid_argument("Metafmt.nth"))
    };

let m = nth(Z, [0]);
let k = nth(S(Z), [0, "zero"]);

module Meta = {
  type t('indexed, 'source, 'final) =
    | []: t('res, 'source, 'res)
    | ::(index('x, 'source), t('indexed, 'source, 'final))
      : t('x => 'indexed, 'source, 'final);

  let x = [Z, Z, S(Z)];
};

let rec apply: type a b. (a, L.t(a, b)) => b =
  (f, l) =>
    switch (l) {
    | L.[] => f
    | L.[a] => f(a)
    | L.[a, ...q] => apply(f(a), q)
    };

let rec indexed_apply: type a src b. (a, Meta.t(a, src, b), L.fix(src)) => b =
  (f, indices, src) =>
    switch (indices) {
    | Meta.[] => f
    | Meta.[n] => f(nth(n, src))
    | Meta.[n, ...q] => indexed_apply(f(nth(n, src)), q, src)
    };

module W = Witness;

module Modal = {
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

  let default = {
    modifier: None,
    padding: None,
    precision: None,
    variant: None,
  };

  let modifier =
    fun
    | "" => None
    | "+" => Some(Plus)
    | " " => Some(Space)
    | "#" => Some(Hash)
    | _ => None;

  exception Unsupported(string);
  let unsupported = x => raise(Unsupported(x));

  let ext =
    fun
    | "" => None
    | "*" => Some(Star)
    | x =>
      try(Some(Lit(int_of_string(x)))) {
      | _ => None
      };

  let lexmodal = (~m, ~pa, ~pr, l) => {
    modifier: modifier(m),
    padding: ext(pa),
    precision:
      switch (pr) {
      | None => None
      | Some(pr) => ext(pr)
      },
    variant: Some(l),
  };
};

module Formatting_box = {
  type t =
    | HV
    | V
    | HOV
    | C;
  let to_string =
    fun
    | V => "v"
    | HOV => "hov"
    | HV => "hv"
    | C => "";
};

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

type t('a, 'driver, 'mid) =
  | []: t(_)
  | ::(atom('a, 'driver, 'mid), t('a, 'driver, 'mid))
    : t('a, 'driver, 'mid);

module I = CamlinternalFormatBasics;
open Format;

let print_int = (params: list(_), f, ppf, modal, n) => {
  open Modal;
  let (padding, params) =
    switch (modal.padding, params) {
    | (None, _) => (I.No_padding, params)
    | (Some(Lit(k)), _) => (
         I.Lit_padding(Right, k),
        params,
      )
    | (Some(Star), [a, ...q]) => (
         I.Lit_padding(Right, a),
        q,
      )
    | (Some(Star), []) => assert(false)
    };

  let precision =
    switch (modal.precision, params) {
    | (None, _) => I.No_precision
    | (Some(Lit(k)), _) => I.Lit_precision(k)
    | (Some(Star), [a, ..._]) => I.Lit_precision(a)
    | (Some(Star), []) => assert(false)
    };
  let variant =
    I.(
      switch (modal.modifier) {
      | None =>
        switch (modal.variant) {
        | None => Int_d
        | Some(x) =>
          switch (x) {
          | "i" => Int_i
          | "o" => Int_o
          | "x" => Int_x
          | "X" => Int_X
          | "u" => Int_u
          | "d"
          | _ => Int_d
          }
        }
      | Some(m) =>
        switch (modal.variant) {
        | None =>
          switch (m) {
          | Space => Int_sd
          | Plus => Int_pd
          | _ => Int_d
          }
        | Some(s) =>
          switch (m, s) {
          | (Space, "i") => Int_si
          | (Plus, "i") => Int_pi
          | (Space, "d") => Int_sd
          | (Plus, "d") => Int_pd
          | (Hash, "x") => Int_Cx
          | (Hash, "X") => Int_CX
          | (Hash, "o") => Int_Co
          | _ => Int_d
          }
        }
      }
    );

  Format.fprintf(
    ppf,
    I.( Format(f(variant, padding, precision), "")),
    n,
  );
};

let print_float = (params: list(_), ppf, modal, n) => {
  open Modal;
  let (padding, params) =
    switch (modal.padding, params) {
    | (None, _) => (I.No_padding, params)
    | (Some(Lit(k)), _) => (
         I.Lit_padding(Right, k),
        params,
      )
    | (Some(Star), [k, ...q]) => (
         I.Lit_padding(Right, k),
        q,
      )
    | (Some(Star), []) => assert(false)
    };
  let precision =
    switch (modal.precision, params) {
    | (None, _) => I.No_precision
    | (Some(Lit(k)), _) => I.Lit_precision(k)
    | (Some(Star), [k, ..._]) => I.Lit_precision(k)
    | (Some(Star), []) => assert(false)
    };
  let variant =
    I.(
      switch (modal.modifier) {
      | None =>
        switch (modal.variant) {
        | None => Float_f
        | Some(x) =>
          switch (x) {
          | "e" => Float_e
          | "f" => Float_f
          | "g" => Float_g
          | "h" => Float_h
          | "E" => Float_E
          | "F" => Float_F
          | "G" => Float_G
          | "H" => Float_H
          | _ => Float_f
          }
        }
      | Some(m) =>
        switch (modal.variant) {
        | None =>
          switch (m) {
          | Space => Float_sf
          | Plus => Float_pf
          | _ => Float_f
          }
        | Some(s) =>
          switch (m, s) {
          | (Space, "e") => Float_se
          | (Space, "f") => Float_sf
          | (Space, "g") => Float_sg
          | (Space, "h") => Float_sh
          | (Space, "E") => Float_sE
          | (Space, "G") => Float_sG
          | (Space, "H") => Float_sH
          | (Plus, "e") => Float_pe
          | (Plus, "f") => Float_pf
          | (Plus, "g") => Float_pg
          | (Plus, "h") => Float_ph
          | (Plus, "E") => Float_pE
          | (Plus, "G") => Float_pG
          | (Plus, "H") => Float_pH
          | _ => Float_f
          }
        }
      }
    );

  Format.fprintf(
    ppf,
    I.(
       
      Format(
         Float(variant, padding, precision, End_of_format),
        "",
      )
    ),
    n,
  );
};

let int = (v, pa, pr) => I.( Int(v, pa, pr, End_of_format));
let int32 = (v, pa, pr) =>
  I.( Int32(v, pa, pr, End_of_format));
let int64 = (v, pa, pr) =>
  I.( Int64(v, pa, pr, End_of_format));
let nativeint = (v, pa, pr) =>
  I.( Nativeint(v, pa, pr, End_of_format));

let print_string = (params: list(_), ppf, modal, n) => {
  open Modal;
  let padding =
    switch (modal.padding, params) {
    | (None, _) => I.No_padding
    | (Some(Lit(k)), _) =>  I.Lit_padding(Right, k)
    | (Some(Star), [k, ..._]) =>  I.Lit_padding(Right, k)
    | (Some(Star), []) => assert(false)
    };
  let variant =
    I.(
      switch (modal.variant) {
      | Some("S") =>  Caml_string(padding, End_of_format)
      | None
      | Some("s")
      | Some(_) =>  String(padding, End_of_format)
      }
    );
  Format.fprintf(ppf, I.( Format(variant, "")), n);
};

let print_bool = (params: list(int), ppf, modal, n) => {
  open Modal;
  let padding =
    switch (modal.padding, params) {
    | (None, _) => I.No_padding
    | (Some(Lit(k)), _) =>  I.Lit_padding(Right, k)
    | (Some(Star), [k, ..._]) =>  I.Lit_padding(Right, k)
    | (Some(Star), []) => assert(false)
    };
  let variant =
    I.(
      switch (modal.variant) {
      | Some("B") =>  Bool(padding, End_of_format)
      | None
      | Some("b")
      | Some(_) =>  Bool(padding, End_of_format)
      }
    );
  Format.fprintf(ppf, I.( Format(variant, "")), n);
};

let print_char = (ppf, modal, n) => {
  open Modal;
  let variant =
    I.(
      switch (modal.variant) {
      | Some("C") => Caml_char(End_of_format)
      | None
      | Some("c")
      | Some(_) => Char(End_of_format)
      }
    );
  Format.fprintf(ppf, I.( Format(variant, "")), n);
};

let print_elt = (type x, ~params=([]: list(_)), ppf, modal, w: W.s(x), x: x) =>
  switch (w) {
  | W.Int => print_int(params, int, ppf, modal, x)
  | W.Char => print_char(ppf, modal, x)
  | W.Bool => print_bool(params, ppf, modal, x)
  | W.Int32 => print_int(params, int32, ppf, modal, x)
  | W.Int64 => print_int(params, int64, ppf, modal, x)
  | W.Nativeint => print_int(params, nativeint, ppf, modal, x)
  | W.Float => print_float(params, ppf, modal, x)
  | W.String => print_string(params, ppf, modal, x)
  };

let print_hole =
    (
      type x,
      type l,
      type fl,
      ppf,
      modal,
      w:
        W.arg({
          .
          x: x,
          l: l,
          fl: fl,
          driver: Format.formatter,
          mid: unit,
        }),
      x: x,
    ) =>
  switch (w) {
  | W.A =>
    let  W.Show(f, x) = x;
    (f(ppf, x): unit);
  | W.T => (x(ppf): unit)
  | W.(S(w)) => print_elt(ppf, modal, w, x)
  | W.(Int_param(w)) => print_elt(~params=[fst(x)], ppf, modal, w, snd(x))
  | W.(Int2_param(w)) =>
    let (pa, pr, x) = x;
    print_elt(~params=[pa, pr], ppf, modal, w, x);
  };

let rec kfprintf:
  type l r.
    (formatter => r, formatter, t(l, formatter, unit), L.t(l, r)) => r =
  (k, ppf, x, args) =>
    switch (x) {
    | [] => k(ppf)
    | [Text(s), ...q] =>
      pp_print_string(ppf, s);
      kfprintf(k, ppf, q, args);
    | [Hole({modal, arg, pos}), ...q] =>
      print_hole(ppf, modal, arg, nth(pos, args));
      kfprintf(k, ppf, q, args);
    | [Open_box({kind, indent}), ...q] =>
      Format.fprintf(
        ppf,
        "@[<%s%d>",
        Formatting_box.to_string(kind),
        indent,
      );
      kfprintf(k, ppf, q, args);
    | [Open_tag(s), ...q] =>
      Format.pp_open_tag(ppf, s);
      kfprintf(k, ppf, q, args);
    | [Close_box, ...q] =>
      Format.pp_close_box(ppf, ());
      kfprintf(k, ppf, q, args);
    | [Close_tag, ...q] =>
      Format.pp_close_tag(ppf, ());
      kfprintf(k, ppf, q, args);
    | [Break({space, indent}), ...q] =>
      Format.pp_print_break(ppf, space, indent);
      kfprintf(k, ppf, q, args);
    | [Fullstop, ...q] =>
      Format.fprintf(ppf, "@.");
      kfprintf(k, ppf, q, args);
    | [Newline, ...q] =>
      Format.pp_force_newline(ppf, ());
      kfprintf(k, ppf, q, args);
    | [Flush, ...q] =>
      Format.fprintf(ppf, "%!");
      kfprintf(k, ppf, q, args);
    };

let rec expand_full:
  type l m r r f mid. (W.l((l, m), (r, r), f, mid), L.t(l, r) => r) => m =
  (spec, f) =>
    switch (spec) {
    | W.[S(x), ...q] => (n => expand_full(q, l => f(L.[n, ...l])))
    | W.[A, ...q] => (
        (show, x) =>
          expand_full(q, l =>
            f(L.[ W.Show(show, x), ...l])
          )
      )
    | W.[T, ...q] => (t => expand_full(q, l => f(L.[t, ...l])))
    | W.[Int_param(x), ...q] => (
        (n, x) => expand_full(q, l => f(L.[(n, x), ...l]))
      )
    | W.[Int2_param(x), ...q] => (
        (n, m, x) => expand_full(q, l => f(L.[(n, m, x), ...l]))
      )
    | W.[] => f([])
    };

let rec expand:
  type f l m r mid. (W.l((l, m), (r, r), f, mid), L.t(l, r) => r) => l =
  (spec, f) =>
    switch (spec) {
    | W.[S(_), ...q] => (n => expand(q, l => f(L.[n, ...l])))
    | W.[A, ...q] => (x => expand(q, l => f(L.[x, ...l])))
    | W.[T, ...q] => (t => expand(q, l => f(L.[t, ...l])))
    | W.[Int_param(_), ...q] => (t => expand(q, l => f(L.[t, ...l])))
    | W.[Int2_param(_), ...q] => (t => expand(q, l => f(L.[t, ...l])))
    | W.[] => f([])
    };

let int = pos => Hole({modal: Modal.default, arg: S(Int), pos});
let float = pos => Hole({modal: Modal.default, arg: S(Float), pos});
let str = pos => Hole({modal: Modal.default, arg: S(String), pos});
let show = pos => Hole({modal: Modal.default, arg: A, pos});

let _0 = Z;
let _1 = S(Z);
let _2 = S(_1);
let _3 = S(_2);
let _4 = S(_3);

module Box = {
  type b('fin, 'driver, 'mid) =
    | Box(
        W.l(('core, _), ('fin, _), 'driver, 'mid),
        t('core, 'driver, 'mid),
      )
      : b('fin, 'driver, 'mid);

  [@unboxed]
  type u = {u: 'fin 'driver 'mid. b('fin, 'driver, 'mid)};
  exception Metafmt_type_error;

  let unsafe = (b: b('f, 'driver, 'mid)): u => {u: Obj.magic(b)};

  let kfprintf =
      (
        type x,
        type y,
        type r,
        k: Format.formatter => r,
        W.H(spec): W.h(x, r, r, Format.formatter, unit),
        {u:  Box(spec', metafmt)},
      )
      : (Format.formatter => x) =>
    switch (W.leq(spec, spec')) {
    | Some(W.Eq) => (ppf => expand_full(spec) @@ kfprintf(k, ppf, metafmt))
    | None => raise(Metafmt_type_error)
    };

  let fprintf = spec => kfprintf(_ => (), spec);

  let sprintf = (spec, u) => {
    let b = Buffer.create(10);
    kfprintf(
      _ => Buffer.contents(b),
      spec,
      u,
      Format.formatter_of_buffer(b),
    );
  };
};
