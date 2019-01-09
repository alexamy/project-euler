USING: kernel combinators math namespaces
       io io.files io.encodings.utf8
       sequences splitting sorting prettyprint ;
IN: project-euler.problem22

SYMBOL: path
: set-path ( -- )
  "C:\\Users\\Alex\\Desktop\\project-euler\\problem22\\names.txt"
  path set ;

: read-file ( -- s )
  path get utf8 [ readln ] with-file-reader ;

: remove-quotes ( s -- s )
  [ length ] keep
  [ 1 - ] dip
  [ 1 ] 2dip
  subseq ;

: get-names ( s -- seq )
  "," split
  [ remove-quotes ] map
  natural-sort ;

: problem-run ( -- )
  set-path read-file get-names . ;

MAIN: problem-run
