begin
  x
rescue; y; end

begin
  x
rescue ArgumentError; y; end

begin
  x
rescue => e; y; end

begin
  x
rescue ArgumentError => e; y; end
