#!/usr/bin/env ruby

# TODO: WIP

# Add the language server into the load path
root = File.join(File.dirname(__FILE__),'..')

$LOAD_PATH.unshift(File.join(root,'lib'))
# #reject{ |o| !File.directory?(o)}
vendor_dir = File.join(root,'vendor')
Dir.new(vendor_dir)
  .reject{ |v| v == '.' || v == '..'}
  .map{ |v| File.join(vendor_dir,v) }
  .reject{ |v| !File.directory?(v)}
  .each do |vendor|
    $LOAD_PATH.unshift(File.join(vendor,'lib'))
end

puts "Tick tock..."
require 'puppet'
require 'pp'
puts "--- Time to rock-n-roll"

content = <<-EOT
plan mymodule::my_plan(
  TargetSpec $load_balancer,
  TargetSpec  $webservers,
) {

  # Extract the Target name from $webservers
  $webserver_names = get_targets($webservers).map |$n| { $n.name }

  # process webservers
  run_task('mymodule::lb_remove', $load_balancer, webservers => $webserver_names)
  run_task('mymodule::update_frontend_app', $webservers, version => '1.2.3')
  run_task('mymodule::lb_add', $load_balancer, webservers => $webserver_names)
 }
EOT

# TODO: array var assignments e.g. [$x, $y] = ['abc', 123]
# TODO: $facts['something']

# TODO: Resolve a variable given a nest of values and prefixes

EXPLICIT_PREFIX = 'explicit:'
SAME_AS_VARIABLE_PREFIX = 'var:'
FUNCTION_PREFIX = 'func:'

class VariableInference
  attr_accessor :name
  attr_accessor :qualified_name # TODO: Need to reverse engineer the qualified name
  attr_accessor :location # TODO Full document location
  attr_accessor :document_scope # TODO Maybe? Document to/from location that the variable exists in e.g.local block, class resource etc.
  attr_accessor :inference

  def initialize(name, location = nil, type_inference = nil)
    @name = name
    @inference = type_inference.nil? ? { :explicit => 'Any'} : type_inference
  end

  def to_s
    "[#{inference}] #{name}"
  end
end

class VariableInferenceEvaluator
  attr_reader :variables

  def initialize
    @inference_visitor ||= ::Puppet::Pops::Visitor.new(self, "infer", 0, 0)
    @variables = []
  end

  # PRIVATE
  def infer_type(o)
    @type_inference_eval ||= TypeInferenceEvaluator.new
    @type_inference_eval.infer(o)
  end

  def infer(ast)
    @inference_visitor.visit_this_0(self, ast)
  end

  def infer_Object(o) # rubocop:disable Naming/UncommunicativeMethodParamName
    # Ignore any other object types
    puts "VI DEFAULT #{o.class.to_s}"

    # Always traverse any child objects
    o._pcore_contents { |child| infer(child) }
  end

 def infer_Factory(o)
   infer(o.model)
 end

  def infer_Parameter(o) # rubocop:disable Naming/UncommunicativeMethodParamName
    variables << VariableInference.new(o.name, nil, infer_type(o.type_expr))
  end

  def infer_AssignmentExpression(o) # rubocop:disable Naming/UncommunicativeMethodParamName
    # Can only infer on assignments ($x = '123')
    return infer_Object(o) unless o.operator == '='
    case o.left_expr._pcore_type.name
    when 'Puppet::AST::VariableExpression'
      # Infer on single var assignments ($x = '123')
      variables << VariableInference.new(o.left_expr.expr.value, nil, infer_type(o.right_expr))

    # TODO: What about array based? Infer on single var assignments ([$x, $y] = ['abc', 123])

    else
      infer_Object(o)
    end
  end
end

class TypeInferenceEvaluator
  def initialize
    @inference_visitor ||= ::Puppet::Pops::Visitor.new(self, "infer", 0, 0)
  end

  def infer(ast)
    @inference_visitor.visit_this_0(self, ast)
  end

  def infer_Object(o) # rubocop:disable Naming/UncommunicativeMethodParamName
    # Ignore any other object types
    puts "TI DEFAULT #{o.class.to_s}"
    EXPLICIT_PREFIX + 'Any'
  end

  def infer_QualifiedReference(o) # rubocop:disable Naming/UncommunicativeMethodParamName
    EXPLICIT_PREFIX + ::Puppet::Pops::Adapters::SourcePosAdapter.adapt(o).extract_text
  end

  def infer_LiteralBoolean(o) # rubocop:disable Naming/UncommunicativeMethodParamName
    EXPLICIT_PREFIX + 'Boolean'
  end

  def infer_LiteralInteger(o) # rubocop:disable Naming/UncommunicativeMethodParamName
    EXPLICIT_PREFIX + 'Integer'
  end

  def infer_LiteralFloat(o) # rubocop:disable Naming/UncommunicativeMethodParamName
    EXPLICIT_PREFIX + 'Float'
  end

  def infer_LiteralList(o) # rubocop:disable Naming/UncommunicativeMethodParamName
    EXPLICIT_PREFIX + 'Array'
  end

  def infer_LiteralString(o) # rubocop:disable Naming/UncommunicativeMethodParamName
    EXPLICIT_PREFIX + 'String'
  end

  def infer_LiteralUndef(o) # rubocop:disable Naming/UncommunicativeMethodParamName
    EXPLICIT_PREFIX + 'Undef'
  end

  def infer_VariableExpression(o) # rubocop:disable Naming/UncommunicativeMethodParamName
    infer(o.expr)
  end

  def infer_QualifiedName(o) # rubocop:disable Naming/UncommunicativeMethodParamName
    # https://github.com/puppetlabs/puppet-specifications/blob/ca146e07160a06c6d1f740613fd05fc4d723c877/language/lexical_structure.md#lower-case-bare-words--name--qualified-name
    SAME_AS_VARIABLE_PREFIX + o.value
  end

  def infer_AccessExpression(o)
    # TODO the keys could contain variables ['abc', $foo]
    EXPLICIT_PREFIX + ::Puppet::Pops::Adapters::SourcePosAdapter.adapt(o).extract_text
  end

  def infer_CallNamedFunctionExpression(o)
    # TODO: What about multiple signatures with different return types?
    FUNCTION_PREFIX + o.functor_expr.value
  end

  # def infer_Object(o) # rubocop:disable Naming/UncommunicativeMethodParamName
  #   # Ignore any other object types
  #   puts "DEFAULT #{o.class.to_s}"

  #   # Always traverse any child objects
  #   o._pcore_contents { |child| infer(child) }
  # end

  # def infer_Factory(o)
  #   infer(o.model)
  # end

  # # def type_from_paramaeter(o)
  # #   param_type = 'Any'
  # #   return param_type if o.type_expr.nil?
  # #   # We can only deal with QualifiedReferences Right now
  # #   return 'Any?!?!?' unless o.type_expr._pcore_type.name == 'Puppet::AST::QualifiedReference'
  # #   o.type_expr.cased_value
  # # end

  # def infer_Parameter(o) # rubocop:disable Naming/UncommunicativeMethodParamName
  #   assert_inference(o.name, infer(o.type_expr))
  # end


  # def infer_AssignmentExpression(o) # rubocop:disable Naming/UncommunicativeMethodParamName
  #   # Can only infer on assignments ($x = '123')
  #   return infer_Object(o) unless o.operator == '='
  #   case o.left_expr._pcore_type.name
  #   when 'Puppet::AST::VariableExpression'
  #     # Infer on single var assignments ($x = '123')
  #     assert_inference(o.left_expr.expr.value, infer(o.right_expr))
  #   else
  #     infer_Object(o)
  #   end
  # end


end


  # def infer_Program(o) # rubocop:disable Naming/UncommunicativeMethodParamName
  #   infer(o.body)
  # end

  # def infer_HostClassDefinition(o) # rubocop:disable Naming/UncommunicativeMethodParamName
  #   infer(o.body)
  # end

  # def infer_BlockExpression(o) # rubocop:disable Naming/UncommunicativeMethodParamName
  #   infer(o.statements)
  # end


  # def infer_Array(o) # rubocop:disable Naming/UncommunicativeMethodParamName
  #   #infer(o.statements)
  # end

  # def infer_AccessExpression(o) # rubocop:disable Naming/UncommunicativeMethodParamName
  #   # Extract the raw text of the Access Expression
  #   ::Puppet::Pops::Adapters::SourcePosAdapter.adapt(o).extract_text
  # end

  # def infer_QualifiedReference(o) # rubocop:disable Naming/UncommunicativeMethodParamName
  #   # Extract the raw text of the Qualified Reference
  #   ::Puppet::Pops::Adapters::SourcePosAdapter.adapt(o).extract_text
  # end

  # # ----- The following methods are the same as the original infer_evaluator
  # def infer_Factory(o) # rubocop:disable Naming/UncommunicativeMethodParamName
  #   literal(o.model)
  # end

  # def infer_Program(o) # rubocop:disable Naming/UncommunicativeMethodParamName
  #   literal(o.body)
  # end

  # def infer_QualifiedName(o) # rubocop:disable Naming/UncommunicativeMethodParamName
  #   o.value
  # end

  # def infer_LiteralNumber(o) # rubocop:disable Naming/UncommunicativeMethodParamName
  #   o.value
  # end

  # def infer_LiteralBoolean(o) # rubocop:disable Naming/UncommunicativeMethodParamName
  #   o.value
  # end

  # def infer_LiteralUndef(o) # rubocop:disable Naming/UncommunicativeMethodParamName
  #   nil
  # end

  # def infer_LiteralDefault(o) # rubocop:disable Naming/UncommunicativeMethodParamName
  #   :default
  # end

  # def infer_LiteralRegularExpression(o) # rubocop:disable Naming/UncommunicativeMethodParamName
  #   o.value
  # end

  # def infer_ConcatenatedString(o) # rubocop:disable Naming/UncommunicativeMethodParamName
  #   # use double quoted string value if there is no interpolation
  #   throw :not_literal unless o.segments.size == 1 && o.segments[0].is_a?(Model::LiteralString)
  #   o.segments[0].value
  # end

  # def infer_LiteralList(o) # rubocop:disable Naming/UncommunicativeMethodParamName
  #   o.values.map {|v| literal(v) }
  # end

  # def infer_LiteralHash(o) # rubocop:disable Naming/UncommunicativeMethodParamName
  #   o.entries.reduce({}) do |result, entry|
  #     result[literal(entry.key)] = literal(entry.value)
  #     result
  #   end
  # end




def recurse_showast(item, depth = 0)
  output = "  " * depth
  output += "#{item.class.to_s} (#{item.object_id})"
  if item.respond_to?(:offset)
    output += " (Off-#{item.offset}:#{item.offset + item.length} Pos-#{item.line}:#{item.pos} Len-#{item.length}) ~#{item.locator.extract_text(item.offset, item.length).gsub("\n", "\\n")}~"
  end
  puts output
  item._pcore_contents do |child|
    recurse_showast(child, depth + 1)
  end
end


# Use Puppet to generate the AST
parser = Puppet::Pops::Parser::Parser.new

Puppet[:tasks] = true
result = parser.parse_string(content, '')

recurse_showast(result.model)
puts "---------------"
#require 'pry'; binding.pry
inferrer_eval = VariableInferenceEvaluator.new
inferrer_eval.infer(result)
puts "---------------"
puts inferrer_eval.variables
