let input = ref(None);
let output = ref(None);

let optref = r => Arg.String(s => r := Some(s));
let args = [
  ("-input", optref(input), "input po(t) files"),
  ("-output", optref(output), "output binary map"),
];

module Mf = Metaprintf;

let flatten = String.concat("");
let is_format = entry => List.mem("c-format", entry.Po.Types.flags);

let metaformattify = (ref, s) =>
  Metaprintf.Untyped.(Dyn({ref, spec: [], fmt: [Text(s)]}));
let formattify = s =>
  CamlinternalFormatBasics.(
     
    Format( String_literal(s, End_of_format), s)
  );

let to_format = (fmt, id) =>
   CamlinternalFormatBasics.Format(fmt, flatten(id));

let add_str = (ctx, entry: Po.Types.entry, map) =>
  switch (entry.msg) {
  | Singular({id, translation}) =>
    let fmt = formattify(flatten(id));
    let metafmt = metaformattify(fmt, flatten(translation));
    Mf.Tmap.add(~ctx, metafmt, map);
  | Plural({id, plural, translations}) =>
    let fmt = formattify(flatten(plural));
    let ctx = flatten([ctx, ...id]);
    let add = (m, (num, translation)) => {
      let metafmt = metaformattify(fmt, flatten(translation));
      Mf.Tmap.add(~num, ~ctx, metafmt, m);
    };
    List.fold_left(add, map, translations);
  };

let add_fmt = (ctx, entry: Po.Types.entry, map) =>
  switch (entry.msg) {
  | Singular({id, translation}) =>
    let Mf.Untyped.Cfmt.Dyn(fmt) = Mf.Parse.fmt(flatten(id));
    let metafmt =
      Mf.Parse.metafmt(flatten(translation), to_format(fmt, id));
    Mf.Tmap.add(~ctx, metafmt, map);
  | Plural({id, plural, translations}) =>
    let Mf.Untyped.Cfmt.Dyn(fmt) = Mf.Parse.fmt(flatten(plural));
    let ctx = flatten([ctx, ...id]);
    let add = (m, (num, translation)) => {
      let metafmt =
        Mf.Parse.metafmt(flatten(translation), to_format(fmt, plural));
      Mf.Tmap.add(~num, ~ctx, metafmt, m);
    };
    List.fold_left(add, map, translations);
  };

let add = (_, entry: Po.Types.entry, map) => {
  let ctx = flatten(entry.context);
  try(
    if (is_format(entry)) {
      add_fmt(ctx, entry, map);
    } else {
      add_str(ctx, entry, map);
    }
  ) {
  | e =>
    Format.eprintf(
      "@[<v 2>Error with entry:@,%a@]",
      Po.Types.Pp.entry,
      entry,
    );
    raise(e);
  };
};

let transform = file => {
  let data =
    try(Po.Parse.file(file)) {
    | exn =>
      Format.eprintf("Po file parsing failure@.");
      raise(exn);
    };

  let map = Po.Types.Map.fold(add, data.map, Mf.Tmap.empty);
  let output =
    switch (output^) {
    | None => Filename.chop_extension(file) ++ ".bo"
    | Some(x) => x
    };
  Mf.Tmap.Store.write(
    {lang: data.lang, plural: data.plural, translations: map},
    output,
  );
};

let () = {
  Arg.parse(args, ignore, "translator -input file");
  Po.Option.(input^ >> transform);
};
