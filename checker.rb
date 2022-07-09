require "parser/current"

total = 0
errored = []
Dir["tests/parse/**/*.rb"].each do |file|
  expect_error = false
  parser = Parser::CurrentRuby.new
  parser.diagnostics.consumer = -> (diag) { expect_error = true }
  expect_result = parser.parse(Parser::Source::Buffer.new("(string)", source: File.read(file)))
  actual_error = File.exist?(file.sub(/\.rb$/, ".errors.txt"))
  actual_result = File.read(file.sub(/\.rb$/, ".pgem.txt"))
  if expect_error
    ok = actual_error
  else
    ok = !actual_error && "#{expect_result.inspect}\n" == actual_result
    # if !actual_error && "#{expect_result.inspect}\n" != actual_result
    #   File.write(file.sub(/\.rb$/, ".pgem.expected.txt"), "#{expect_result.inspect}\n")
    # end
  end
  total += 1
  errored.push(file) unless ok
end
puts "Success: #{total - errored.size} / #{total}"
unless errored.empty?
  puts "Errored:"
  errored.each do |errored_file|
    puts "  #{errored_file}"
  end
end
