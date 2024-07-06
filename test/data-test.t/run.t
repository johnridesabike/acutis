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
    "none": null,
    "f": false,
    "t": true,
    "arr": [
      "x",
      "y"
    ],
    "dict": {
      "a": 0,
      "b": "b"
    },
    "some": "some",
    "arrEmpty": []
  }
  Encoding unknowns:
  {
    "none": 0,
    "f": 0,
    "t": 1,
    "arr": [
      "x",
      [
        "y",
        0
      ]
    ],
    "dict": {
      "a": 0,
      "b": "b"
    },
    "some": [
      "some"
    ],
    "arrEmpty": 0
  }
