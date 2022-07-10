#!/usr/bin/env ruby

require "bundler/setup"
require "parser/ruby31"

Dir["tests/parse/**/*.rb"].each do |path|
  errors_path = path.sub(/\.rb$/, ".errors.txt")
  ast_path = path.sub(/\.rb$/, ".pgem.txt")
  skip_directive_path = path.sub(/\.rb$/, ".skip_pgem.txt")

  next if File.exist?(skip_directive_path)

  errored = false
  parser = Parser::Ruby31.new
  parser.diagnostics.consumer = -> (diag) do
    if diag.level == :error
      errored = true
    end
  end
  ast = parser.parse(Parser::Source::Buffer.new("(string)", source: File.read(path)))
  if errored
    File.write(errors_path, "") unless File.exist?(errors_path)
  else
    File.delete(errors_path) if File.exist?(errors_path)
    File.write(ast_path, "#{ast.inspect}\n")
  end
  $stderr.puts "Processed: #{path}"
end
