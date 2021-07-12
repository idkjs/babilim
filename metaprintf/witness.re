type eq(_, _) =
  | Eq: eq('x, 'x);
type cp('a, 'c, 'driver) = ('driver, 'a) => 'c;
type show('driver, 'c) =
  | Show(('driver, 'a) => 'c, 'a): show('driver, 'c);

type exs('x, 'm) = {
  .
  x: 'x,
  l: 'l,
  fl: 'x => 'l,
  driver: 'driver,
  mid: 'c,
}
constraint 'm = ('l, 'driver, 'c);

type alpha('m) = {
  .
  x: show('driver, 'c),
  l: 'l,
  fl: (('driver, 'y) => 'c, 'y) => 'l,
  driver: 'driver,
  mid: 'c,
}
constraint 'm = ('l, 'driver, 'y, 'c);

type theta('m) = {
  .
  x: 'driver => 'c as 't,
  l: 'l,
  fl: 't => 'l,
  driver: 'driver,
  mid: 'c,
}
constraint 'm = ('l, 'driver, 'c);

type s(_) =
  | Char: s(char)
  | Int: s(int)
  | Int32: s(int32)
  | Int64: s(int64)
  | Nativeint: s(nativeint)
  | Bool: s(bool)
  | Float: s(float)
  | String: s(string);

let (\===) = (type a, type b, x: s(a), y: s(b)): option(eq(a, b)) =>
  switch (x, y) {
  | (Char, Char) => Some(Eq)
  | (Int, Int) => Some(Eq)
  | (Int32, Int32) => Some(Eq)
  | (Int64, Int64) => Some(Eq)
  | (Nativeint, Nativeint) => Some(Eq)
  | (Bool, Bool) => Some(Eq)
  | (Float, Float) => Some(Eq)
  | (String, String) => Some(Eq)
  | _ => None
  };

type complement =
  | Precision
  | Padding;

type arg(_) =
  | S(s('x)): arg(exs('x, _))
  | Int_param(s('y))
    : arg({
        .
        x: (int, 'y),
        l: 'l,
        fl: (int, 'y) => 'l,
        driver: 'driver,
        mid: 'c,
      })
  | Int2_param(s('y))
    : arg({
        .
        x: (int, int, 'y),
        l: 'l,
        fl: (int, int, 'y) => 'l,
        driver: 'driver,
        mid: 'c,
      })
  | A: arg(alpha(_))
  | T: arg(theta(_))

and l(_, _, _, _) =
  | []: l(('tail, 'moretail), ('tail, 'moretail), 'driver, 'mid)
  | ::(
      arg({
        .
        x: 'x,
        l: 'm,
        fl: 'fm,
        driver: 'driver,
        mid: 'mid,
      }),
      l(('l, 'm), 'tail, 'driver, 'mid),
    )
    : l(('x => 'l, 'fm), 'tail, 'driver, 'mid);

let rec leq:
  type a b c d e f g h dr mid.
    (l((a, b), (c, d), dr, mid), l((e, f), (c, g), dr, mid)) =>
    option(eq(a, e)) =
  (x, y) =>
    switch (x, y) {
    | ([], []) => Some(Eq)

    | ([S(x), ...l], [S(y), ...r]) =>
      switch (x \=== y) {
      | None => None
      | Some(Eq) =>
        switch (leq(l, r)) {
        | None => None
        | Some(Eq) => Some(Eq)
        }
      }

    | ([Int_param(x), ...l], [Int_param(y), ...r]) =>
      switch (x \=== y) {
      | None => None
      | Some(Eq) =>
        switch (leq(l, r)) {
        | None => None
        | Some(Eq) => Some(Eq)
        }
      }

    | ([Int2_param(x), ...l], [Int2_param(y), ...r]) =>
      switch (x \=== y) {
      | None => None
      | Some(Eq) =>
        switch (leq(l, r)) {
        | None => None
        | Some(Eq) => Some(Eq)
        }
      }

    | ([A, ...l], [A, ...r]) =>
      switch (leq(l, r)) {
      | None => None
      | Some(Eq) => Some(Eq)
      }
    | ([T, ...l], [T, ...r]) =>
      switch (leq(l, r)) {
      | None => None
      | Some(Eq) => Some(Eq)
      }

    | ([S(_), ..._], [A, ..._]) => None
    | ([S(_), ..._], [T, ..._]) => None
    | ([S(_), ..._], [Int_param(_), ..._]) => None
    | ([S(_), ..._], [Int2_param(_), ..._]) => None
    | ([Int_param(_), ..._], [A, ..._]) => None
    | ([Int_param(_), ..._], [S(_), ..._]) => None
    | ([Int_param(_), ..._], [T, ..._]) => None
    | ([Int_param(_), ..._], [Int2_param(_), ..._]) => None
    | ([Int2_param(_), ..._], [S(_), ..._]) => None
    | ([Int2_param(_), ..._], [A, ..._]) => None
    | ([Int2_param(_), ..._], [T, ..._]) => None
    | ([Int2_param(_), ..._], [Int_param(_), ..._]) => None
    | ([A, ..._], [S(_), ..._]) => None
    | ([A, ..._], [T, ..._]) => None
    | ([A, ..._], [Int_param(_), ..._]) => None
    | ([A, ..._], [Int2_param(_), ..._]) => None
    | ([T, ..._], [S(_), ..._]) => None
    | ([T, ..._], [A, ..._]) => None
    | ([T, ..._], [Int_param(_), ..._]) => None
    | ([T, ..._], [Int2_param(_), ..._]) => None

    | ([_, ..._], []) => None
    | ([], [_, ..._]) => None
    };

let canary = [S(Int), S(Int), A];
type dyn('b, 'd, 'mid, 'driver) =
  | Dyn(l(('a, 'b), ('c, 'd), 'mid, 'driver)): dyn('b, 'd, 'mid, 'driver);

let rec (@):
  type ls l2 m d t. (l(ls, l2, d, m), l(l2, t, d, m)) => l(ls, t, d, m) =
  (l, r) =>
    switch (l) {
    | [] => r
    | [a, ...q] => [a, ...q @ r]
    };

let l = [A, A, S(Int), S(Float)];

type box('r, 'd, 'm) =
  | Box(l(('core, _), ('r, _), 'd, 'm), 'd => 'core): box('r, 'd, 'm);

type h(_, _, _, _, _) =
  | H(l(('a, 'b), ('c, 'd), 'dr, 'm)): h('b, 'd, 'c, 'dr, 'm);

let rec (@/):
  type ls l2 m t x y z d.
    (h(ls, l2, x, d, m), h(l2, t, y, d, m)) => h(ls, t, y, d, m) =
  (H(x), H(y)) =>
    switch (x) {
    | [] => H(y)
    | [S(x), ...q] => single(x, q, H(y))
    | [Int_param(a), ...q] => int_param(a, q, H(y))
    | [Int2_param(a), ...q] => int2_param(a, q, H(y))
    | [A, ...q] =>
      let H(r) = H(q) @/ H(y);
      H([A, ...r]);
    | [T, ...q] =>
      let H(r) = H(q) @/ H(y);
      H([T, ...r]);
    }

and single:
  type x m ll lm r rf t tm t2 d.
    (s(x), l((ll, lm), (tm, r), d, m), h(r, t, t2, d, m)) =>
    h(x => lm, t, t2, d, m) =
  (x, q, y) => {
    let H(r) = H(q) @/ y;
    H([S(x), ...r]);
  }

and int_param:
  type x y m ll lm r rf t tm t2 d.
    (s(x), l((ll, lm), (tm, r), d, m), h(r, t, t2, d, m)) =>
    h((int, x) => lm, t, t2, d, m) =
  (x, q, r) => {
    let H(r) = H(q) @/ r;
    H([Int_param(x), ...r]);
  }

and int2_param:
  type x y m ll lm r rf t tm t2 d.
    (s(x), l((ll, lm), (tm, r), d, m), h(r, t, t2, d, m)) =>
    h((int, int, x) => lm, t, t2, d, m) =
  (x, q, r) => {
    let H(r) = H(q) @/ r;
    H([Int2_param(x), ...r]);
  };

let rec return: type a b c r r2 d m. (r, h(a, r, r2, d, m)) => a =
  (r, H(spec)) =>
    switch (spec) {
    | [] => r
    | [S(_), ...q] => (_ => return(r, H(q)))
    | [T, ...q] => (_ => return(r, H(q)))
    | [A, ...q] => ((_, _) => return(r, H(q)))
    | [Int_param(_), ...q] => ((_, _) => return(r, H(q)))
    | [Int2_param(_), ...q] => ((_, _, _) => return(r, H(q)))
    };

exception Unbox_error;
let rec unbox: type a b m c r r2 d. (h(b, r, r2, d, m), box(r, d, m), d) => b =
  (H(spec),  Box(spec', f), ppf) =>
    switch (spec, spec') {
    | ([], []) => f(ppf)
    | ([S(x), ...l], [S(y), ...r]) =>
      switch (x \=== y) {
      | None => raise(Unbox_error)
      | Some(Eq) => (
          x => unbox(H(l),  Box(r, ppf => f(ppf, x)), ppf)
        )
      }
    | ([A, ...l], [A, ...r]) => (
        (show, x) =>
          unbox(
            H(l),
             
            Box(r, ppf => f(ppf,  Show(show, x))),
            ppf,
          )
      )
    | ([T, ...l], [T, ...r]) => (
        g => unbox(H(l),  Box(r, ppf => f(ppf, g)), ppf)
      )
    | ([Int_param(a), ...l], [Int_param(a'), ...r]) =>
      switch (a \=== a') {
      | Some(Eq) => (
          (x, y) =>
            unbox(
              H(l),
               Box(r, ppf => f(ppf, (x, y))),
              ppf,
            )
        )
      | _ => raise(Unbox_error)
      }
    | ([Int2_param(a), ...l], [Int2_param(a'), ...r]) =>
      switch (a \=== a') {
      | Some(Eq) => (
          (n, m, y) =>
            unbox(
              H(l),
               Box(r, ppf => f(ppf, (n, m, y))),
              ppf,
            )
        )
      | _ => raise(Unbox_error)
      }
    | _ => raise(Unbox_error)
    };
