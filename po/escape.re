let string = s => {
  /* Escape only C0 control characters (bytes <= 0x1F), DEL(0x7F), '\\' and '"' */
  let n = ref(0);
  for (i in 0 to String.length(s) - 1) {
    n :=
      n^
      + (
        switch (String.unsafe_get(s, i)) {
        | '"'
        | '\\'
        | '\n'
        | '\t'
        | '\r'
        | '\b' => 2
        | '\000' .. '\031'
        | '\127' => 4
        | _ => 1
        }
      );
  };
  if (n^ == String.length(s)) {
    s;
  } else {
    let s' = Bytes.create(n^);
    n := 0;
    for (i in 0 to String.length(s) - 1) {
      switch (String.unsafe_get(s, i)) {
      | ('"' | '\\') as c =>
        Bytes.unsafe_set(s', n^, '\\');
        incr(n);
        Bytes.unsafe_set(s', n^, c);
      | '\n' =>
        Bytes.unsafe_set(s', n^, '\\');
        incr(n);
        Bytes.unsafe_set(s', n^, 'n');
      | '\t' =>
        Bytes.unsafe_set(s', n^, '\\');
        incr(n);
        Bytes.unsafe_set(s', n^, 't');
      | '\r' =>
        Bytes.unsafe_set(s', n^, '\\');
        incr(n);
        Bytes.unsafe_set(s', n^, 'r');
      | '\b' =>
        Bytes.unsafe_set(s', n^, '\\');
        incr(n);
        Bytes.unsafe_set(s', n^, 'b');
      | ('\000' .. '\031' | '\127') as c =>
        let a = Char.code(c);
        Bytes.unsafe_set(s', n^, '\\');
        incr(n);
        Bytes.unsafe_set(s', n^, Char.chr(48 + a / 100));
        incr(n);
        Bytes.unsafe_set(s', n^, Char.chr(48 + a / 10 mod 10));
        incr(n);
        Bytes.unsafe_set(s', n^, Char.chr(48 + a mod 10));
      | c => Bytes.unsafe_set(s', n^, c)
      };
      incr(n);
    };
    Bytes.to_string(s');
  };
};
