Both the JavaScript and native runtimes should decode data identically.
  $ node run.js template.acutis
  Record:
  a 1 1.
  
  Dictionary:
  a b c 
  
  List:
  a b 
  
  Nullables:
  a null 
  
  Tuple:
  a 1. 
  
  Booleans:
  pass
  
  Union:
  pass

  $ acutis template.acutis < data.json
  Record:
  a 1 1.
  
  Dictionary:
  a b c 
  
  List:
  a b 
  
  Nullables:
  a null 
  
  Tuple:
  a 1. 
  
  Booleans:
  pass
  
  Union:
  pass

Encoding data should work. If a function component takes an 'unknown' prop, then
unknown values are passed as-is, and passing concrete values is not an error but
is unspecified behavior (currently they're made null).
  $ node run.js encode.acutis
  Encoding:
  {
    "arr": [
      "x",
      "y"
    ],
    "arrEmpty": [],
    "dict": {
      "a": 0,
      "b": "b"
    },
    "f": false,
    "none": null,
    "some": "some",
    "t": true,
    "unknown": [
      true,
      null,
      "string",
      100,
      {
        "key": 1.5
      }
    ]
  }
  Encoding unknowns:
  {
    "arr": null,
    "arrEmpty": null,
    "dict": null,
    "f": null,
    "none": null,
    "some": null,
    "t": null,
    "unknown": [
      true,
      null,
      "string",
      100,
      {
        "key": 1.5
      }
    ]
  }
