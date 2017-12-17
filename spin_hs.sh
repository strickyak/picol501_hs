while sleep 1
do
  ( set -x
    ghc --make -XScopedTypeVariables tcl.hs repl.hs &&
    ci-l tcl.hs repl.hs &&
    ./repl <<"END"
proc foo x {
  k 7
}
assert [== 7 [foo 23]]
assert [== 15 [+ 1 2 3 4 5]]
proc plus {x y} {
  + $x $y
}
assert [== 42 [plus 35 7]] plus_35_7
assert [== 42 [plus [plus 30 8] [plus 2 2]]] 30+8+2+8
assert [== 15 [+ 1 [+ 2 3] [plus 4 5]]] 15
proc tri {n} {
  if [< $n 1] { + 0 } else { + $n [tri [+ $n -1]] }
}
if 0 { + 42 } else { + 23 }
if 1 { + 42 } else { + 23 }
assert 1 Just1
assert [catch {assert 0 Failure}] CheckFailure
assert [== 15 [tri 5]] "tri 15"
assert [eq abc [set x a; set y b; set z c; k "$x$y$z"]] abc->xyz
k OKAY-OKAY-OKAY.
END
date
echo Total Lines: `wc -l tcl.hs`
echo Work Lines: `cat tcl.hs | grep -v '^[ \t]*$' | grep -v '^[ \t]*[-][-]' | grep -v :: | wc -l`
  ) >_ 2>&1
  clear
  cat _
  test -z "$1" || break
done
