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
the value given keeps its internal representation intact.
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
    "t": true
  }
  Encoding unknowns:
  {
    "arr": [
      "x",
      [
        "y",
        0
      ]
    ],
    "arrEmpty": 0,
    "dict": {
      "a": 0,
      "b": "b"
    },
    "f": 0,
    "none": 0,
    "some": [
      "some"
    ],
    "t": 1
  }
