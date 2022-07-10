begin
  x
rescue ArgumentError
  y
ensure
  z
end

begin
  x
rescue ArgumentError
  y
rescue TypeError
  z
ensure
  w
end

begin
  x
rescue ArgumentError
  y
else
  z
ensure
  w
end
