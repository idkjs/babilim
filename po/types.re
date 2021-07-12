type multiline = list(string);
type comments = {
  translator: multiline,
  programmer: multiline,
};

type loc = {
  file: string,
  line: int,
};

type msg =
  | Singular({
      id: multiline,
      translation: multiline,
    })
  | Plural({
      id: multiline,
      plural: multiline,
      translations: list((int, multiline)),
    });

type entry = {
  comments,
  location: loc,
  flags: list(string),
  context: multiline,
  previous: option(msg),
  msg,
};

let id =
  fun
  | Singular({id, _}) => id
  | Plural({id, _}) => id;

type key = {
  id: multiline,
  ctx: multiline,
};

module Header = {
  module Map =
    Map.Make({
      type t = string;
      let compare = compare;
    });

  type entry = {
    extra: list(string),
    keyed: Map.t(string),
  };
  type t = Map.t(entry);

  let parse_entry = (map, s) =>
    if (s == "") {
      map;
    } else {
      let s = String.trim(Scanf.unescaped(s));
      let p =
        try(String.index(s, ':')) {
        | Not_found => String.length(s)
        };
      let section = String.sub(s, 0, p);
      let entry = {
        let start = {extra: [], keyed: Map.empty};
        String.sub(s, p + 1, String.length(s) - p - 1)
        |> String.split_on_char(';')
        |> List.fold_left(
             (entry, x) =>
               switch (String.split_on_char('=', x)) {
               | [] => assert(false)
               | [""] => entry
               | [x] =>
                 let x = String.trim(x);
                 {...entry, extra: [x, ...entry.extra]};
               | [k, ...x] =>
                 let (k, x) = String.(trim(k), concat("=", x));
                 {...entry, keyed: Map.add(k, x, entry.keyed)};
               },
             start,
           );
      };
      Map.add(section, entry, map);
    };

  let parse = s => List.fold_left(parse_entry, Map.empty, s);
};

module Map =
  Map.Make({
    type t = key;
    let compare = (x: t, y: t) => compare(x, y);
  });

type map = Map.t(entry);

type po = {
  header: Header.t,
  lang: string,
  plural: Metaprintf.Math_expr.any,
  map,
};

let add = (entry, map) =>
  Map.add({id: id(entry.msg), ctx: entry.context}, entry, map);

let of_list = List.fold_left((m, x) => add(x, m), Map.empty);

let make = (msg, map) => {
  let contents =
    switch (msg) {
    | Singular(x) => x.translation
    | Plural({translations: [(_, a), ..._], _}) => a
    | Plural(_) => failwith("Ill-formed header")
    };
  let header =
    try(Header.parse(contents)) {
    | e =>
      Format.eprintf("Header parsing failure@.");
      raise(e);
    };

  open Header;
  let lang =
    try(
      switch (Map.find("Language", header)) {
      | {extra: [a, ..._], _} => a
      | _ => assert(false)
      }
    ) {
    | exn =>
      Format.eprintf("Language not found @.");
      exit(2);
    };

  let p =
    try(Map.find("Plural-Forms", header)) {
    | Not_found =>
      Format.eprintf("Not found: Plural-Forms @. ");
      exit(2);
    };

  let e =
    try(Map.find("plural", p.keyed)) {
    | Not_found =>
      Format.eprintf("Not found: Plural-Forms.plural@.");
      exit(2);
    };

  let plural =
    try(Math.expr(Math_lex.main, Lexing.from_string(e))) {
    | exn =>
      Format.eprintf("Math parser failure while parsing [%s]@.", e);
      raise(exn);
    };

  {header, lang, plural, map};
};

module Pp = {
  let text = (ppf, s) => Format.fprintf(ppf, "\"%s\"", Escape.string(s));

  let list = Format.pp_print_list;

  let fp = Format.fprintf;
  let premtext = (~pre="", ppf) =>
    list(
      ~pp_sep=(ppf, ()) => fp(ppf, "@,"),
      ppf => fp(ppf, "%s%a", pre, text),
      ppf,
    );

  let mtext = premtext(~pre="");

  let msg = (~pre="", ppf) =>
    fun
    | Singular({id, translation}) => {
        fp(ppf, "%smsgid %a@,", pre, mtext, id);
        if (translation == []) {
          ();
        } else {
          fp(ppf, "%smsgstr %a@,", pre, mtext, translation);
        };
      }
    | Plural({id, plural, translations}) => {
        fp(ppf, "%smsgid %a@,", pre, mtext, id);
        fp(ppf, "%smsgid_plural %a@,", pre, mtext, plural);
        let pr = ((n, l)) =>
          switch (l) {
          | [] => ()
          | [a, ...q] =>
            fp(ppf, "%smsgstr[%d] %a@,", pre, n, text, a);
            premtext(~pre, ppf, q);
          };
        List.iter(pr, translations);
      };

  let entry = (ppf, entry: entry) => {
    let f = x => Format.fprintf(ppf, x);
    f("@[<v>");
    List.iter(f("# %s@,"), entry.comments.translator);
    List.iter(f("#. %s@,"), entry.comments.programmer);
    List.iter(f("#, %s@,"), entry.flags);
    f("#: %s:%d@,", entry.location.file, entry.location.line);
    List.iter(f("msgctxt %a@,", text), entry.context);
    Option.(entry.previous >> msg(~pre="#| ", ppf));
    msg(ppf, entry.msg);
    f("@]@.");
  };
};
