class Foo end
class Foo; end
class Foo 42 end
class Foo 42; 100 end

class Foo
end

class Foo
  x = 42
  # def foo; end
end

class Foo < Bar; end
class Foo < Bar; 42 end
class Foo < Bar; 42; end
class Foo < Bar; 42; 100 end
class Foo < Bar
end
class Foo < Bar
  42
end
class Foo < Bar
  42
  100
end
class Foo < 1 + 2; end
class Foo < 1 + 2; 42 end
class Foo < 1 + 2; 42; end
class Foo < 1 + 2; 42; 100 end
class Foo < 1 + 2
end
class Foo < 1 + 2
  42
end
class Foo < 1 + 2
  42
  100
end
class Foo < f g; end
class Foo < f g; 42 end
class Foo < f g
end
class Foo < f g
  42
end
# class <<foo; end
