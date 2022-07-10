begin
  x
rescue then y; end

begin
  x
rescue ArgumentError then y; end

begin
  x
rescue => e then y; end

begin
  x
rescue ArgumentError => e then y; end

begin
  x
rescue
then y; end

begin
  x
rescue ArgumentError
then y; end

begin
  x
rescue => e
then y; end

begin
  x
rescue ArgumentError => e
then y; end

begin
  x
rescue; then y; end

begin
  x
rescue ArgumentError; then y; end

begin
  x
rescue => e; then y; end

begin
  x
rescue ArgumentError => e; then y; end
