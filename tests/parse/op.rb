foo <=> bar
foo == bar
foo === bar
foo != bar
foo =~ bar
foo !~ bar
foo == bar < baz
foo < bar == baz

foo < bar
foo <= bar
foo > bar
foo >= bar
foo < bar | baz
foo | bar < baz

foo | bar ^ baz
foo ^ bar | baz
foo | bar & baz
foo & bar | baz

foo & bar & baz
foo & bar << baz
foo << bar & baz

foo << bar >> baz
foo >> bar << baz
foo + bar << baz
foo << bar + baz

foo + bar - baz
foo - bar + baz
foo * bar + baz
foo + bar * baz
-foo + bar
foo + -bar

foo * bar / baz % quux
foo % bar / baz * quux
foo ** bar * baz
foo * bar ** baz

foo ** bar
foo ** bar ** baz
!foo ** bar
~foo ** bar
+foo ** bar
# ** takes precedence
-foo ** bar

!foo
~foo
+foo
-foo
!! ~+~!!foo

+123 # literal
+ 123 # op + literal
-123 # literal
- 123 # op + literal
