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
unknown values are passed as-is, and passing concrete values returns their
internal representation. The latter case is not included in this test because
that representation is not deterministic across all build profiles.
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
