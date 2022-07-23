#!/usr/bin/env ruby

require "bundler/setup"
require "parser/ruby31"
require "ripper"
require "pp"

def main(parser = :pgem)
  Dir["tests/parse/**/*.rb"].each do |path|
    errors_path = path.sub(/\.rb$/, ".errors.txt")
    ast_path = path.sub(/\.rb$/, ".pgem.txt")
    skip_directive_path = path.sub(/\.rb$/, ".skip_#{parser}.txt")

    next if File.exist?(skip_directive_path)

    ast, errored = \
      if parser == :pgem
        parse_with_pgem(File.read(path))
      else
        parse_with_ripper(File.read(path))
      end
    if errored
      File.write(errors_path, "") unless File.exist?(errors_path)
    else
      File.delete(errors_path) if File.exist?(errors_path)
      File.write(ast_path, "#{ast.inspect}\n")
    end
    $stderr.puts "Processed: #{path}"
  end
end

def parse_with_ripper(source)
  builder = RipperWrapper.new(source, "-", 1)
  ast = builder.parse
  if builder.error?
    errored = true
  else
    errored = false
  end
  [ast, errored]
end

def parse_with_pgem(source)
  errored = false
  parser = Parser::Ruby31.new
  parser.builder.emit_file_line_as_literals = false
  Parser::Builders::Default.emit_encoding = true
  parser.diagnostics.consumer = -> (diag) do
    if diag.level == :error
      errored = true
    end
  end
  ast = parser.parse(Parser::Source::Buffer.new("(string)", source: source))
  [ast, errored]
end

class CustomBuilder < Ripper::SexpBuilderPP
  private

  def on_args_add_star(list, star)
    list.push([:splat, star])
  end
end

class RipperWrapper < ::Ripper   #:nodoc:
  attr_reader :error

  private

  def s(name, *args)
    Parser::AST::Node.new(name, args)
  end

  def collapse(stmts)
    stmts = stmts.compact
    if stmts.size == 0
      nil
    elsif stmts.size == 1
      stmts[0]
    else
      s(:begin, *stmts)
    end
  end

  def arrayify(obj)
    case obj
    when Array
      obj
    else
      [s(:NON_ARRAY, obj.inspect)]
    end
  end

  def node_type(node)
    if node.is_a?(Parser::AST::Node)
      node.type
    else
      nil
    end
  end

  def tokname(tok)
    case node_type(tok)
    when :@cvar, :@const, :@gvar, :@ident, :@ivar, :@kw, :@op
      tok.children[0]
    else
      :"RAW_TOK_#{tok.inspect}"
    end
  end

  PARSER_EVENTS.each do |event|
    module_eval(<<-End, __FILE__, __LINE__ + 1)
      def on_#{event}(*args)
        s(:RAW_#{event}, *args)
      end
    End
  end

  SCANNER_EVENTS.each do |event|
    module_eval(<<-End, __FILE__, __LINE__ + 1)
      def on_#{event}(tok)
        s(:@#{event}, tok)
      end
    End
  end

  def on_error(mesg)
    @error = mesg
  end
  remove_method :on_parse_error
  alias on_parse_error on_error
  alias compile_error on_error

  def on_BEGIN(stmts)
    s(:preexe, collapse(stmts))
  end

  def on_END(stmts)
    s(:postexe, collapse(stmts))
  end

  def on_alias(name1, name2)
    s(:alias, name1, name2)
  end

  def on_aref(obj, args)
    args = arrayify(args)
    s(:send, obj, :[], *args)
  end

  def on_arg_paren(args)
    args || []
  end

  def on_array(elems)
    if elems
      elems = arrayify(elems)
      s(:array, *elems)
    else
      s(:array)
    end
  end

  def on_assign(lhs, rhs)
    if lhs.is_a?(Parser::AST::Node)
      s(lhs.type, *lhs.children, rhs)
    else
      s(:RAW_assign, lhs, rhs)
    end
  end

  def on_begin(stmts)
    s(:kwbegin, *stmts.compact)
  end

  def on_binary(lhs, op, rhs)
    case op
    when :and, :"&&"
      s(:and, lhs, rhs)
    when :or, :"||"
      s(:or, lhs, rhs)
    else
      s(:send, lhs, op, rhs)
    end
  end

  def on_bodystmt(body, rescue_cls, else_cl, ensure_cl)
    [*body, *rescue_cls, *[else_cl].compact, *[ensure_cl].compact]
  end

  def on_break(args)
    s(:break, *args)
  end

  def on_call(obj, op, meth)
    name = meth == :call ? meth : tokname(meth)
    if node_type(op) == :@op && tokname(op) == :"&."
      s(:csend, obj, name)
    else
      s(:send, obj, name)
    end
  end

  def on_case(cond, branches)
    s(:case, cond, *branches)
  end

  def on_class(path, superclass, body)
    s(:class, path, superclass, collapse(body))
  end

  def on_command(meth, args)
    s(:send, nil, tokname(meth), *args)
  end

  def on_command_call(obj, op, meth, args)
    if node_type(op) == :@op && tokname(op) == :"&."
      s(:csend, obj, tokname(meth), *args)
    else
      s(:send, obj, tokname(meth), *args)
    end
  end

  def on_const_path_ref(base, c)
    s(:const, base, tokname(c))
  end

  def on_const_ref(c)
    s(:const, nil, tokname(c))
  end

  def on_def(meth, params, stmts)
    s(:def, tokname(meth), s(:args, *params), collapse(stmts))
  end

  def on_defined(expr)
    s(:defined?, expr)
  end

  def on_dot2(lhs, rhs)
    s(:irange, lhs, rhs)
  end

  def on_dot3(lhs, rhs)
    s(:erange, lhs, rhs)
  end

  def on_else(stmts)
    # Dummy node
    s(:else, collapse(stmts))
  end

  def on_fcall(meth)
    s(:send, nil, tokname(meth))
  end

  def on_for(lhs, collection, body)
    s(:for, lhs, collection, collapse(body))
  end

  def on_if(cond, then_cl, else_cl)
    then_cl = collapse(then_cl)
    if node_type(else_cl) == :else
      else_cl = else_cl.children[0]
    end
    s(:if, cond, then_cl, else_cl)
  end

  def on_if_mod(cond, expr)
    s(:if, cond, expr, nil)
  end

  def on_ifop(cond, then_expr, else_expr)
    s(:if, cond, then_expr, else_expr)
  end

  def on_method_add_arg(call, args)
    if call.is_a?(Parser::AST::Node)
      s(call.type, *call.children, *args)
    else
      s(:RAW_method_add_arg, call, args)
    end
  end

  def on_module(path, body)
    s(:module, path, collapse(body))
  end

  def on_next(args)
    s(:next, *args)
  end

  def on_opassign(lhs, op, rhs)
    op_base = tokname(op).to_s.sub(/=$/, "").to_sym
    s(:op_asgn, lhs, op_base, rhs)
  end

  def on_params(pre, opt, rest, post, kw, kwrest, block)
    params = []
    (pre || []).each do |arg|
      params << s(:arg, tokname(arg))
    end
    (opt || []).each do |arg|
      params << s(:optarg, tokname(arg[0]), arg[1])
    end
    if rest
      if rest.children[0]
        params << s(:restarg, rest.children[0])
      else
        params << s(:restarg)
      end
    end
    (post || []).each do |arg|
      params << s(:arg, tokname(arg))
    end
    (kw || []).each do |arg|
      kwname = arg[0].children[0].sub(/:$/, "").to_sym
      if arg[1]
        params << s(:kwoptarg, kwname, arg[1])
      else
        params << s(:kwarg, kwname)
      end
    end
    if kwrest
      if kwrest.children[0]
        params << s(:kwrestarg, rest.children[0])
      else
        params << s(:kwrestarg)
      end
    end
    if block
      params << s(:blockarg, block)
    end
    params
  end

  def on_paren(stmts)
    s(:begin, *stmts.compact)
  end

  def on_program(stmts)
    collapse(arrayify(stmts))
  end

  def on_redo
    s(:redo)
  end

  def on_rescue(arg1, arg2, arg3, tail)
    tail ||= []
    tail.unshift(s(:RAW_rescue, arg1, arg2, arg3))
  end

  def on_retry
    s(:retry)
  end

  def on_return0
    s(:return)
  end

  def on_symbol(v)
    s(:sym, tokname(v))
  end

  def on_symbol_literal(sym)
    if node_type(sym) == :sym
      sym
    else
      s(:sym, tokname(sym))
    end
  end

  def on_unary(op, expr)
    case op
    when :not
      s(:send, expr, :!)
    else
      s(:send, expr, op)
    end
  end

  def on_undef(names)
    s(:undef, *names)
  end

  def on_unless_mod(cond, expr)
    s(:if, cond, nil, expr)
  end

  def on_until(cond, stmts)
    s(:until, cond, collapse(stmts))
  end

  def on_until_mod(cond, expr)
    s(:until, cond, expr)
  end

  def on_var_field(v)
    s(:lvasgn, tokname(v))
  end

  def on_var_ref(v)
    case node_type(v)
    when :@gvar
      s(:gvar, tokname(v))
    when :@cvar
      s(:cvar, tokname(v))
    when :@ivar
      s(:ivar, tokname(v))
    when :@const
      s(:const, nil, tokname(v))
    when :@ident
      s(:lvar, tokname(v))
    when :@kw
      case tokname(v)
      when :nil
        s(:nil)
      when :true
        s(:true)
      when :false
        s(:false)
      when :__FILE__
        s(:__FILE__)
      when :__LINE__
        s(:__LINE__)
      when :__ENCODING__
        s(:__ENCODING__)
      else
        s(:RAW_var_ref, v)
      end
    else
      s(:RAW_var_ref, v)
    end
  end

  def on_vcall(meth)
    s(:send, nil, tokname(meth))
  end

  def on_void_stmt
    nil
  end

  def on_when(conds, stmts, tail)
    if tail.nil?
      tail = [nil]
    elsif node_type(tail) == :else
      tail = [tail]
    end
    tail.unshift(s(:when, *conds, collapse(stmts)))
  end

  def on_while(cond, stmts)
    s(:while, cond, collapse(stmts))
  end

  def on_while_mod(cond, expr)
    s(:while, cond, expr)
  end

  def on_yield0
    s(:yield)
  end

  def on_args_new
    []
  end

  def on_args_add(list, item)
    list = arrayify(list)
    list.push(item)
  end

  def on_args_add_block(list, item)
    list = arrayify(list)
    if item
      list.push(item)
    else
      list
    end
  end

  def on_stmts_new
    []
  end

  def on_stmts_add(list, item)
    arrayify(list).push(item)
  end

  def on_backref(tok)
    case tok
    when /\A\$([1-9][0-9]*)\z/
      s(:nth_ref, $1.to_i)
    else
      s(:back_ref, tok.to_sym)
    end
  end

  def on_backtick(tok)
    s(:@op, tok.to_sym)
  end

  def on_cvar(tok)
    s(:@cvar, tok.to_sym)
  end

  def on_const(tok)
    s(:@const, tok.to_sym)
  end

  def on_gvar(tok)
    s(:@gvar, tok.to_sym)
  end

  def on_ident(tok)
    s(:@ident, tok.to_sym)
  end

  def on_int(tok)
    s(:int, tok.to_i)
  end

  def on_ivar(tok)
    s(:@ivar, tok.to_sym)
  end

  def on_kw(tok)
    s(:@kw, tok.to_sym)
  end

  def on_op(tok)
    s(:@op, tok.to_sym)
  end
end

case ARGV[0]
when "ripper"
  main :ripper
else
  main :pgem
end
