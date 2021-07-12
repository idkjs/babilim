let maybe = (f, x) =>
  switch (x) {
  | Some(x) => f(x)
  | None => ()
  };

let bind = (f, x) =>
  switch (x) {
  | Some(x) => f(x)
  | None => None
  };

let (>>) = (x, f) => maybe(f, x);
let (>>=) = (x, f) => bind(f, x);
let (>>|) = (x, f) => x >>= (x => Some(f(x)));
