USING: kernel combinators math namespaces arrays assocs
       io io.files io.encodings.utf8
       sequences splitting sorting prettyprint ;
IN: factor.problem22

SYMBOL: path
: set-path ( -- )
  "C:\\Users\\Alex\\Desktop\\project-euler\\factor\\problem22\\problem22_names.txt"
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
  [ remove-quotes ] map ;

: name-score ( s -- n )
  >array [ 64 - ] map sum ;

: zip-index-1 ( seq -- seq )
  { f } swap append zip-index 1 tail ;

: get-scores ( seq -- seq )
  [ name-score ] map zip-index-1 [ product ] map ;

: problem-run ( -- )
  set-path read-file get-names natural-sort
  get-scores sum . ;

MAIN: problem-run
