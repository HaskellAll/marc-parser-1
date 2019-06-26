# marc-parser
 Parser from MARC standards to JSON format implemented in Haskell.
 
Input for parser is file which contains one or more MARC records (5 types of record formats are supported), output is file with structured MARC records in JSON format.

How to use?
<br/>Load `parser.hs`, run main with 2 command line arguments, path to input file and output file respectively. 
<br/>`:main ./test_files/in04.txt ./parsed_files/out04.txt`
