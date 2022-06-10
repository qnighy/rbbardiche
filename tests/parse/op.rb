foo ? bar : baz
a ? b : c ? d : e
a ? b ? c : d : e
foo .. bar ? baz : quux
foo ? bar .. baz : quux
foo ? bar : baz .. quux

foo .. bar
foo ... bar
foo || bar .. baz
foo .. bar || baz
.. foo
+ .. foo
.. + foo
+ .. foo + bar
.. + foo + bar
# (foo ..)
# (~ foo ..)
# (foo + bar ..)
# (~ foo + bar ..)

foo || bar || baz
foo || bar && baz
foo && bar || baz

foo && bar && baz
foo && bar == baz
foo == bar && baz

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

foo
Foo
::Foo
Foo::Bar
foo::Bar

+123 # literal
+ 123 # op + literal
-123 # literal
- 123 # op + literal
