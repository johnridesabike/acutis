  $ acutis echoes.acutis --data echoes.json
  Echoes work:
  Hello World! &amp;&quot;&quot;&apos;&apos;&gt;&lt;&#x2F;&#x60;&#x3D; &""''></`= &lt; < d
  
  Echo formats work:
  %i: 123456 %f: 1234.56789 %b: false true

  $ acutis other.acutis --data other.json
  Underscore was successfully ignored.
  
  Record field access works:
  pass pass

  $ acutis \
  >   whitespace_control.acutis components/ohHai.acutis \
  >   --data whitespace_control.json
  _hi_
  
  I did not. Oh hai Mark.
   Cheep cheep cheep. Oh hai Lisa.
  

  $ acutis \
  >   nullables.acutis \
  >   components/nullProps.acutis \
  >   components/nullPropsDefault.acutis \
  >   --data nullables.json
  b
  b
  
  z
  
  
  Nullable props default to null:
  pass pass
  Unabound variables successfully default to null

  $ acutis map.acutis --data map.json
  Mapping list variables works:
  John Carlo 
  
  Mapping list literals works:
  John Carlo 
  
  Mapping list variables appended with literals works
  Paul John Carlo 
  
  Mapping lists with the index works:
  John Carlo
  
  Mapping dictionary variables works:
  Lisa: computers. Tommy: banking. 
  
  Mapping dictionary literals works:
  Lisa: computers. Tommy: banking. 

  $ acutis \
  >   component_tests.acutis \
  >   components/childrenProp.acutis \
  >   components/passthroughChild.acutis \
  >   components/passthroughChildNest.acutis \
  >   components/passthroughChildNestPun.acutis \
  >   --data empty.json
  The default 'children' child works:
  pass
  
  pass
  
  
  Children are passed correctly
  pass
  
  
  pass
  
  

  $ acutis tagged_unions.acutis --data tagged_unions.json
  Tagged unions work:
  success success success success success success 
  
  Decoding open tagged unions works:
  success

  $ acutis constructing.acutis --data constructing.json
  Constructing values works:
  success
  
  Constructing nested template blocks inside data patterns works:
  success
  
  Appending to a list literal works:pass pass pass pass 


