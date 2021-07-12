module Inner = CamlinternalFormatBasics;
module U = Untyped.Cfmt;

type gkey('a) = {
  id: 'a,
  ctx: string,
  num: option(int),
};
type dynkey = gkey(U.u);

module M =
  Map.Make({
    type t = dynkey;
    let compare = compare;
  });
module W = Witness;

type t = M.t(Metafmt.Box.u);
type tmap = t;
let empty: t = (M.empty: t);

let fmt = ( Inner.Format(c, _)) => c;
let dynamic = key => U.unsafe(U.Dyn(fmt(key)));
let make = (~num=?, ~ctx="", id) => {id, ctx, num};

let box_then_add:
  (
    ~num: int=?,
    ~ctx: string=?,
    format6('a, _, _, _, _, _),
    W.l(('ab, 'a), (unit, unit), Format.formatter, unit),
    Metafmt.t('ab, Format.formatter, unit),
    t
  ) =>
  t = (
  (~num=?, ~ctx="", id, spec, f, m) =>
    M.add(
      {id: dynamic(id), ctx, num},
      Metafmt.Box.(unsafe( Box(spec, f))),
      m,
    ):
    (
      ~num: int=?,
      ~ctx: string=?,
      format6('a, _, _, _, _, _),
      W.l(('ab, 'a), (unit, unit), Format.formatter, unit),
      Metafmt.t('ab, Format.formatter, unit),
      t
    ) =>
    t
);

let xfprintf = (m, ppf, ~num=?, ~ctx="", id) => {
  let dynkey = {id: dynamic(id), ctx, num};
  let box = M.find(dynkey, m);
  let expected_spec = Conv.typer(fmt(id));
  Metafmt.Box.fprintf(expected_spec, box, ppf);
};

let xkfprintf = (m, k, ppf, ~num=?, ~ctx="", id) => {
  let dynkey = {id: dynamic(id), ctx, num};
  let box = M.find(dynkey, m);
  let expected_spec = Conv.typer(fmt(id));
  Metafmt.Box.kfprintf(k, expected_spec, box, ppf);
};

let xsprintf = (m, ~num=?, ~ctx="", id) => {
  let dynkey = {id: dynamic(id), ctx, num};
  let box = M.find(dynkey, m);
  let expected_spec = Conv.typer(fmt(id));
  Metafmt.Box.sprintf(expected_spec, box);
};

let add = (~num=?, ~ctx="", Untyped.Dyn({spec, ref, fmt}), m) =>
  box_then_add(~num?, ~ctx, ref, spec, fmt, m);

module Store = {
  let magic = "Babilim.Translation.Store";
  type nonrec t = {
    lang: string,
    plural: Math_expr.any,
    translations: t,
  };
  let write = (map: t, s) => {
    let ch = open_out_bin(s);
    output_string(ch, magic);
    output_value(ch, map);
  };

  let read = s => {
    let ch = open_in_bin(s);
    if (really_input_string(ch, String.length(magic)) == magic) {
      let map: t = (input_value(ch): t);
      Some(map);
    } else {
      None;
    };
  };
};

module Implementation = {
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

  let default = {
    kfprintf: Format.kfprintf,
    knfprintf: (k, ppf, n, s, pl) =>
      if (n == 1) {
        Format.kfprintf(k, ppf, s);
      } else {
        Format.kfprintf(k, ppf, pl);
      },
  };

  let from_map = (expr, tmap) => {
    let kfprintf = (k, ppf, fmt) =>
      try(xkfprintf(tmap, ~num=?None, ~ctx=?None, k, ppf, fmt)) {
      | Not_found => default.kfprintf(k, ppf, fmt)
      };

    let knfprintf = (k, ppf, num, fmts, fmtpl) => {
      let num = Math_expr.eval_int(num, expr);
      let CamlinternalFormatBasics.( Format(_, ctx)) = fmts;
      try(xkfprintf(tmap, ~num, ~ctx, k, ppf, fmtpl)) {
      | Not_found => default.knfprintf(k, ppf, num, fmts, fmtpl)
      };
    };

    {kfprintf, knfprintf};
  };

  let from_store = f =>
    switch (Store.read @@ f ++ ".bo") {
    | None => None
    | Some(m) => Some(from_map(m.Store.plural, m.Store.translations))
    };
};
