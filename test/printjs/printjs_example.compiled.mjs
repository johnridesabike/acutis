/* THIS FILE WAS GENERATED BY ACUTIS. */

function decode_error(expected, recieved, debug_stack) {
  throw new Error([
    "Decode error in field: ",
    debug_stack.join(" -> "),
    "\nExpected type:\n",
    expected,
    "\nRecieved value:\n",
    recieved,
  ].join(""));
}

function decode_error_field(field, debug_stack) {
  throw new Error([
    "Decode error.\nAn object is missing the field: ",
    field,
    "\nIn field: ",
    debug_stack.join(" -> "),
  ].join(""));
}

function acutis_escape(str) {
  let result = "";
  for (let index = 0; index < str.length; index++) {
    let c = str[index];
    switch (c) {
      case "&": result += "&amp;"; break;
      case "\"": result += "&quot;"; break;
      case "'": result += "&apos;"; break;
      case ">": result += "&gt;"; break;
      case "<": result += "&lt;"; break;
      case "/": result += "&#x2F;"; break;
      case "`": result += "&#x60;"; break;
      case "=": result += "&#x3D;"; break;
      default: result += c;
    }
  }
  return result;
}

export default async function main(input1) {
  let data = new Map();
  let debug_stack = new Array();
  debug_stack.push("<input>");
  if ("blogPosts" in input1) {
    debug_stack.push("blogPosts");
    let input2 = input1.blogPosts;
    if (input2 instanceof Array) {
      let dst_base1 = new Array(2);
      let dst1 = dst_base1;
      for (let index = 0; index < input2.length; index++) {
        let input_hd1 = input2[index];
        debug_stack.push(index);
        let dst_new1 = new Array(2);
        let record1 = new Map();
        dst_new1[0] = record1;
        if ("author" in input_hd1) {
          debug_stack.push("author");
          let input3 = input_hd1.author;
          let record2 = new Map();
          record1.set("author", record2);
          if ("name" in input3) {
            debug_stack.push("name");
            let input4 = input3.name;
            if (input4 === null || input4 === undefined) {
              record2.set("name", null);
            } else {
              let nullable1 = new Array(1);
              record2.set("name", nullable1);
              if (typeof input4 === "string") {
                nullable1[0] = input4;
              } else {
                return decode_error("string", input4, debug_stack);
              }
            }
            debug_stack.pop();
          } else {
            record2.set("name", null);
          }
          debug_stack.pop();
        } else {
          return decode_error_field("author", debug_stack);
        }
        if ("content" in input_hd1) {
          debug_stack.push("content");
          let input5 = input_hd1.content;
          if (typeof input5 === "string") {
            record1.set("content", input5);
          } else {
            return decode_error("string", input5, debug_stack);
          }
          debug_stack.pop();
        } else {
          return decode_error_field("content", debug_stack);
        }
        if ("date" in input_hd1) {
          debug_stack.push("date");
          let input6 = input_hd1.date;
          if (typeof input6 === "string") {
            record1.set("date", input6);
          } else {
            return decode_error("string", input6, debug_stack);
          }
          debug_stack.pop();
        } else {
          return decode_error_field("date", debug_stack);
        }
        if ("image" in input_hd1) {
          debug_stack.push("image");
          let input7 = input_hd1.image;
          if (input7 === null || input7 === undefined) {
            record1.set("image", null);
          } else {
            let nullable2 = new Array(1);
            record1.set("image", nullable2);
            let record3 = new Map();
            nullable2[0] = record3;
            if ("alt" in input7) {
              debug_stack.push("alt");
              let input8 = input7.alt;
              if (typeof input8 === "string") {
                record3.set("alt", input8);
              } else {
                return decode_error("string", input8, debug_stack);
              }
              debug_stack.pop();
            } else {
              return decode_error_field("alt", debug_stack);
            }
            if ("src" in input7) {
              debug_stack.push("src");
              let input9 = input7.src;
              if (typeof input9 === "string") {
                record3.set("src", input9);
              } else {
                return decode_error("string", input9, debug_stack);
              }
              debug_stack.pop();
            } else {
              return decode_error_field("src", debug_stack);
            }
          }
          debug_stack.pop();
        } else {
          record1.set("image", null);
        }
        if ("title" in input_hd1) {
          debug_stack.push("title");
          let input10 = input_hd1.title;
          if (typeof input10 === "string") {
            record1.set("title", input10);
          } else {
            return decode_error("string", input10, debug_stack);
          }
          debug_stack.pop();
        } else {
          return decode_error_field("title", debug_stack);
        }
        dst1[1] = dst_new1;
        dst1 = dst_new1;
        debug_stack.pop();
      }
      dst1[1] = null;
      data.set("blogPosts", dst_base1[1]);
    } else {
      return decode_error(
        "[\n\
  {\n\
    author: {name: ?string},\n\
    content: string,\n\
    date: string,\n\
    image: ?{alt: string, src: string},\n\
    title: string\n\
  }\n\
]",
        input2,
        debug_stack
      );
    }
    debug_stack.pop();
  } else {
    return decode_error_field("blogPosts", debug_stack);
  }
  if ("siteTitle" in input1) {
    debug_stack.push("siteTitle");
    let input11 = input1.siteTitle;
    if (typeof input11 === "string") {
      data.set("siteTitle", input11);
    } else {
      return decode_error("string", input11, debug_stack);
    }
    debug_stack.pop();
  } else {
    return decode_error_field("siteTitle", debug_stack);
  }
  return (await Promise.all([
    "<h1> Blog posts for ",
    acutis_escape(data.get("siteTitle")),
    " </h1>",
    (async function () {
      let result = new Array();
      let index = 0;
      let arg0 = data.get("blogPosts");
      while (arg0 !== null) {
        let data1 = new Map(data);
        let exit = null;
        exit = 0;
        data1.set("content", arg0[0].get("content"));
        data1.set("date", arg0[0].get("date"));
        data1.set("image", arg0[0].get("image"));
        data1.set("name", arg0[0].get("author").get("name"));
        data1.set("title", arg0[0].get("title"));
        result.push(
          "\n  <article class=\"h-entry\">\n    <header>\n      ",
          (async function () {
            let data2 = new Map(data1);
            let exit = null;
            let arg0 = data1.get("image");
            if (arg0 === null) {
              exit = 0;
            } else {
              exit = 1;
              data2.set("alt", arg0[0].get("alt"));
              data2.set("src", arg0[0].get("src"));
            }
            switch (exit) {
              case 0: return (await Promise.all([])).join("");
              case 1:
                return (await Promise.all([
                  "<img src=\"",
                  acutis_escape(data2.get("src")),
                  "\" alt=\"",
                  acutis_escape(data2.get("alt")),
                  "\">\n      ",
                ])).join("");
            }
          })(),
          "<h2 class=\"p-name\"> ",
          acutis_escape(data1.get("title")),
          " </h2>\n      <span class=\"p-author\"> By ",
          acutis_escape(
            data1.get("name") !== null ? data1.get("name")[0] : "Anonymous"
          ),
          " </span>\n      <span class=\"dt-published\"> Posted on ",
          acutis_escape(data1.get("date")),
          " </span>\n    </header>\n    <div class=\"e-content\"> ",
          data1.get("content"),
          " </div>\n  </article>\n"
        );
        index++;
        arg0 = arg0[1];
      }
      return (await Promise.all(result)).join("");
    })(),
    "\n",
  ])).join("");
}
