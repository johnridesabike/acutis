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

If a function component takes an 'unknown' prop, then the value given keeps its
internal representation intact.
  $ node run.js unknowns.acutis
  {
    "arr": [
      "x",
      [
        "y",
        null
      ]
    ],
    "arrEmpty": null,
    "dict": {
      "a": 0,
      "b": "b"
    },
    "f": 0,
    "none": null,
    "some": [
      "some"
    ],
    "t": 1
  }
