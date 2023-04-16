import main from "./tojs_main_test.mjs";

let result = await main({
  big_int: 100_000_000,
  big_float: 1234.56789,
  bool1: true,
  bool2: false,
  dangerous: "&\"'></`=",
  record: { int_enum: 8, string_enum: "yes" },
  tagged_record_bool: { tag: false, a: "a" },
  null_string_dict: { a: "a", b: null },
  int_list: [-5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5],
  tagged_record_int: { tag: 1, tuple: [1.5, "snd", true] },
  tagged_record_string: {
    tag: "a",
    record_list: [{ name: "John", job: "librarian" }],
  },
  tagged_record_open: {
    tag: 999,
    b: "200",
  },
  unknown: new Date(),
  nested_list: [
    [
      [1, 1, 1],
      [2, 2, 2],
    ],
    [[3, 3, 3]],
  ],
  nested_nullable_list: [null, true, false, null],
});

console.log(result);
