import main from "./tojs_error.mjs";

try {
  console.log(
    await main({ bool: true, int_enum: 1, string_enum: "a", tuple: ["a", "b"] })
  );
} catch (e) {
  console.log(e.message);
}

try {
  console.log(
    await main({
      bool: false,
      int_enum: 3,
      string_enum: "a",
      tuple: ["a", "b"],
    })
  );
} catch (e) {
  console.log(e.message);
}

try {
  console.log(
    await main({
      bool: false,
      int_enum: 1,
      string_enum: "c",
      tuple: ["a", "b"],
    })
  );
} catch (e) {
  console.log(e.message);
}

try {
  console.log(
    await main({ bool: false, int_enum: 1, string_enum: "a", tuple: ["a"] })
  );
} catch (e) {
  console.log(e.message);
}
