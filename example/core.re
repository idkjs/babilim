module T = Metaprintf.Tmap;
module I18n = {
  let implementation = ref(T.Implementation.default);

  let kfprintf = (k, ppf, fmt) => implementation^.kfprintf(k, ppf, fmt);
  let sprintf = fmt => {
    let b = Buffer.create(10);
    let ppf = Format.formatter_of_buffer(b);
    kfprintf(
      ppf => {
        Format.pp_print_flush(ppf, ());
        Buffer.contents(b);
      },
      ppf,
      fmt,
    );
  };

  let s = x =>
    sprintf(
      CamlinternalFormat.(
         
        Format( String_literal(x, End_of_format), x)
      ),
    );
};

let set_map = f =>
  switch (T.Implementation.from_store(f)) {
  | None => ()
  | Some(t) => I18n.implementation := t
  };

let () =
  Arg.parse(
    [("-lang", Arg.String(set_map), "Set the translation map used")],
    ignore,
    "core -lang name",
  );

let () = {
  I18n.kfprintf(
    ignore,
    Format.std_formatter,
    "This is the message %d over %d",
    1,
    2,
  );
  let s =
    I18n.s(
      "This message with \"[@@attribute]\" should not be interpreted as a format",
    );
  let s' = I18n.s(/**I remember*/ "This message is commented");
  let s'' = I18n.s([@i18n {context: "complex analysis"}] "Pole");
  Format.printf("@.%s\n%s\n%s", s, s', s'');
  Format.printf("@.");
};
