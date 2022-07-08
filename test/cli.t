Create the test artifiacts.
  $ cat > data.json << EOF
  > {"name": "Carlo"}
  > EOF
  $ cat > a.acutis << EOF
  > {{ content ~}}
  > EOF
  $ cat > layout.acutis << EOF
  > {%~ A content=name / %}
  > EOF

Process data through stdin.
  $ cat data.json | acutis layout.acutis a.acutis
  Carlo

Process data through an argument.
  $ acutis --data=data.json layout.acutis a.acutis
  Carlo

Output to a file.
  $ acutis --data=data.json --output="result.txt" layout.acutis a.acutis
  $ cat result.txt
  Carlo
