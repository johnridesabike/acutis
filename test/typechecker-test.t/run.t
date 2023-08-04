Echoes typecheck correctly
  $ acutis echoes.acutis --printtypes
  a = string
  c = ?string

Nullables typecheck correctly
  $ acutis nullables.acutis --printtypes
  a = ?int
  b = ??string

Nested patterns typecheck correctly
  $ acutis nested.acutis --printtypes
  a = {b: {c: ?int}, d: string}
  e = [{f: int, h: string}]
  i = [{@tag: 0, j: int} | {@tag: 1, k: string} | ...]
  x = {z: ?{za: int}}
  y = int

Type narrowing works
  $ acutis narrowing.acutis --printtypes
  x = {a: string, b: string}
  y = {a: string, c: string}
  z = {a: string}

Record field access infers correctly
  $ acutis record_fields.acutis --printtypes
  x = {a: string, b: int, c: {d: string}, e: {f: ?int, g: false | true}}

Open enums are inferred correctly
  $ acutis enum_open.acutis --printtypes
  a = @"a" | @"b" | @"c" | ...
  b = @1 | @2 | @3 | ...
  c = <@"a" | @"b" | @"c" | ...>
  d = {a: int, b: @1 | ...}
  e = {a: string, b: @1 | ...}

Closed enums are inferred correctly
  $ acutis enum_closed.acutis --printtypes
  a = @"a" | @"b"
  b = @1 | @2
  c = @"a" | @"b"
  d = @"a" | @"b" | @"c" | @"d" | ...
  e = @1 | @2
  f = @1 | @2 | @3 | @4 | ...
  g = {a: @"a" | @"b"}
  h = {a: @1 | @2}
  i = @"a" | @"b"
  j = @1 | @2
  k = {a: @"a" | @"b"}
  l = {a: @"a" | @"b" | @"c" | ...}
  m = {a: @1 | @2}
  n = {a: @1 | @2 | @3 | ...}

Booleans are inferred correctly
  $ acutis boolean.acutis --printtypes
  a = true
  b = false
  c = false | true
  d = false | true

Open tagged unions are inferred correctly
  $ acutis union_open.acutis --printtypes
  a = {@tag: "a", b: string} | {@tag: "b", b: int} | ...
  b = {@tag: 0, b: string} | {@tag: 1, b: int} | ...
  c =
    {@tag: "a", b: string} | {@tag: "b", b: int} | {@tag: "c", b: float} | ...
  d = <{@tag: "a"} | {@tag: "b"} | {@tag: "c"} | ...>

Closed unions are inferred correctly
  $ acutis union_closed.acutis --printtypes
  a = {@tag: 1, b: string} | {@tag: 2, b: int}
  b = {@tag: "a", b: int} | {@tag: "b", b: string}
  c = {@tag: "a", b: int} | {@tag: "b", b: string}
  d =
    {@tag: "a", b: int} |
    {@tag: "b", b: string} |
    {@tag: "c", b: float} |
    {@tag: "d", b: [int]} |
    ...
  e = {@tag: 0, b: int} | {@tag: 1, b: string}
  f =
    {@tag: 0, b: int} |
    {@tag: 1, b: string} |
    {@tag: 2, b: float} |
    {@tag: 3, b: [int]} |
    ...
  g = {@tag: 0, w: int, y: string} | {@tag: 1, x: int, z: string}

Boolean unions work
  $ acutis union_boolean.acutis --printtypes
  a = {@tag: true, b: string}
  b = {@tag: false, b: string}
  c = {@tag: false, c: string} | {@tag: true, b: string}
  d = {@tag: false} | {@tag: true, b: string}

The typechecker update context works
  $ acutis context.acutis --printtypes
  collections =
    {
      frontPage:
        [
          {
            data:
              {
                isoDate: string,
                page: {excerpt: string},
                pub: {@pub: false} | {@pub: true, absoluteUrl: string},
                title: string
              },
            templateContent: string
          }
        ]
    }

Other cases
  $ acutis other.acutis --printtypes
  a = {a: int, b: int, x: string}

Components infer correctly.
  $ acutis components.acutis component_a.acutis --printtypes
  a = int
  b = string

Interfaces
  $ acutis interface.acutis --printtypes
  a = ?int
  b = @"a" | @"b"
  open_enum = {@tag: 0} | {@tag: 1, a: int} | {@tag: 2} | ...
  open_union = {@tag: 0} | {@tag: 1, a: int} | {@tag: 2} | ...
  record = {a: int, b: string}
  unknown_int = int

The typescheme pretty-printer prints valid (and pretty) syntax
  $ acutis prettyprint.acutis --printtypes
  a = {a: @0 | @1, b: @"a" | @"b"}
  b = {@tag: false, a: <?string>} | {@tag: true, a: [int]}
  c = {@tag: 0} | {@tag: 1, a: (float, false | true)}
  d = {@tag: "a", a: float} | {@tag: "b", a: @0 | @1 | ...}
  e =
    {@tag: 0, a: _} |
    {@tag: 1, b: @"a" | @"b" | ...} |
    {@tag: 2, b: int, c: false | true} |
    ...
  f =
    @"a very looooong string enum type" |
    @"it is so very loooooong! It just keeps going!" |
    ...
  g =
    {
      a:
        [
          @"another loooong enum type!" |
          @"it is so very loooooong! It just keeps going!" |
          @"this time it is inside a record!" |
          ...
        ]
    }

Pathologic cases

Enums that come after variables are open
  $ acutis pathologic_enum.acutis --printtypes
  File "pathologic_enum.acutis", 2:4-2:5
  Type error.
  Type mismatch.
  Expected:
    {b: string}
  Received:
    {b: @1 | ...}
  [1]
