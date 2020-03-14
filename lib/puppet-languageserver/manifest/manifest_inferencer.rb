# frozen_string_literal: true

require 'puppet/pops/types/type_factory'
require 'puppet/pops/types/type_parser'
require 'puppet-languageserver/manifest/manifest_function_inferencer'

module PuppetLanguageServer
  module Manifest
    module ManifestInferencer
      class BaseInference
        attr_accessor :name
        # attr_accessor :full_name  # The fully qualified name?  $foo::bar
        attr_accessor :range
        attr_accessor :scope
        attr_accessor :children
        # The object_id of the AST object that created the inference
        attr_accessor :ast_object_id

        def initialize(ast_source, name = nil, scope = nil)
          # We use object_ids to, hopefully, improve garbage collection etc. May not be true 🤷‍♂️
          @ast_object_id = ast_source.object_id.to_s
          @name = name
          @scope = scope
          @range = nil #TODO Build this from ast_source?
          @children = []
          scope.children << self unless scope.nil?
          puppet_type = Puppet::Pops::Types::TypeFactory.any if self.respond_to?(:puppet_type)
        end

        def to_lsp_symbol; end

        def to_s
          "[#{self.class}] #{name}"
        end
      end

      # Represents a confined scope of "something". Typically used to scope parameters or variables
      # @abstract
      class ScopeInference < BaseInference
      end

      class HostClassDefinitionInference < ScopeInference
        def to_lsp_symbol
          LSP::DocumentSymbol.new(
            'name'           => name,
            'kind'           => LSP::SymbolKind::CLASS,
            'detail'         => name,
            'range'          => LSP.create_range(*range),
            'selectionRange' => LSP.create_range(*range)
          ).tap do |symbol|
            symbol.children = children.map(&:to_lsp_symbol).compact
          end
        end
      end

      class PlanDefinitionInference < ScopeInference
        def to_lsp_symbol
          LSP::DocumentSymbol.new(
            'name'           => name,
            'kind'           => LSP::SymbolKind::CLASS,
            'detail'         => name,
            'range'          => LSP.create_range(*range),
            'selectionRange' => LSP.create_range(*range)
          ).tap do |symbol|
            symbol.children = children.map(&:to_lsp_symbol).compact
          end
        end
      end

      class VariableInference < BaseInference
        attr_accessor :puppet_type

        def to_s
          "[#{self.class}] #{name} is a #{puppet_type}"
        end
      end

      class ParameterInference < VariableInference
        attr_accessor :default_value

        def to_lsp_symbol
          LSP::DocumentSymbol.new(
            'name'           => '$' + name,
            'kind'           => LSP::SymbolKind::PROPERTY,
            'detail'         => '$' + name,
            'range'          => LSP.create_range(*range),
            'selectionRange' => LSP.create_range(*range),
            'children'       => []
          )
        end
      end

      class ResourceExpressionInference < ScopeInference
        def to_lsp_symbol
          LSP::DocumentSymbol.new(
            'name'           => name,
            'kind'           => LSP::SymbolKind::METHOD,
            'detail'         => name,
            'range'          => LSP.create_range(*range),
            'selectionRange' => LSP.create_range(*range),
            'children'       => []
          )
        end
      end

      class ManifestInferences
        attr_reader :inferences
        attr_accessor :debug

        def initialize(object_cache = nil)
          @debug = false #true
          @inferences = []
          @object_cache = object_cache
        end

        def find(name = nil, inference_class = BaseInference)
          inferences.find { |item| (name.nil? || item.name == name) && item.is_a?(inference_class) }
        end

        def select(name = nil, inference_class = BaseInference)
          inferences.select { |item| (name.nil? || item.name == name) && item.is_a?(inference_class) }
        end

        def infer(ast, root_name = '')
          puts "---- Content" if @debug
          ::Puppet::Pops::Adapters::SourcePosAdapter.adapt(ast.model.body).extract_text if @debug
          puts "----" if @debug

          # First pass, find all of the possible inference points
          pass_one = InferenceDetectorEvaluator.new
          #pass_one.debug = @debug
          pass_one.infer(ast, nil)
          @inferences = pass_one.inferences

          puts "---- Phase 1 inferences" if @debug
          @inferences.each { |i| puts "[#{i.class}] #{i.name}" } if @debug
          puts "----" if @debug

          pass_two = TypeDetectorEvaluator.new(@inferences, @object_cache)
          pass_two.debug = @debug
          pass_two.infer(ast)

          puts "---- Phase 2 inferences" if @debug
          @inferences.each { |i| puts "[#{i.class}] #{i.name} is a #{i.respond_to?(:puppet_type) ? i.puppet_type : 'N/A'}" } if @debug
          puts "----" if @debug

          nil
        end
      end

      # Scans an AST looking for inference points and populates as much detail as possible
      class InferenceDetectorEvaluator
        attr_reader :inferences
        attr_accessor :debug

        def initialize
          @debug = false
          @inferences = []
          @inference_visitor ||= ::Puppet::Pops::Visitor.new(self, "infer", 2, nil)
        end

        # Creates an array that we can splat into LSP::Range
        def create_range(offset, length, locator)
          start_line = locator.line_for_offset(offset) - 1
          start_char = locator.pos_on_line(offset) - 1
          end_line = locator.line_for_offset(offset + length) - 1
          end_char = locator.pos_on_line(offset + length) - 1

          [start_line, start_char, end_line, end_char]
        end

        def locator_text(offset, length, locator)
          locator.string.slice(offset, length)
        end

        def infer(ast, parent_scope = nil, parent_ast = nil)
          puts "INFERRING #{ast._pcore_type.name}" if @debug && ast.respond_to?(:_pcore_type)
          @inference_visitor.visit_this_2(self, ast, parent_scope, parent_ast)
          nil
        end

        # Plumbing object types
        def infer_Factory(item, _parent_scope, _parent_ast)
          # Reset the accumulator.  The Factory is the topmost object in the Puppet POPS Model
          @inferences = []
          infer(item.model, nil, item)
        end

        def infer_Program(item, _parent_scope, _parent_ast)
          # TODO: Do we need a root scope?
          #scope = ScopeInference.new(item, nil, nil)
          #scope.range = create_range(0, item.source_text.length, item.locator)
          #@inferences << scope

          infer(item.body, nil, item)
        end

        # Containing POPs
        def infer_HostClassDefinition(item, parent_scope, parent_ast)
          inf = HostClassDefinitionInference.new(item, item.name, parent_scope)
          inf.range = create_range(item.offset, item.length, item.locator)

          @inferences << inf

          infer_children_of(item, inf)
        end

        def infer_PlanDefinition(item, parent_scope, parent_ast)
          inf = PlanDefinitionInference.new(item, item.name, parent_scope)
          inf.range = create_range(item.offset, item.length, item.locator)

          @inferences << inf

          infer_children_of(item, inf)
        end

        def infer_ResourceExpression(item, parent_scope, parent_ast)
          inf = ResourceExpressionInference.new(item, item.type_name.value, parent_scope)
          inf.range = create_range(item.offset, item.length, item.locator)

          if item.bodies.count >= 1
            # Ick!   What about multi resource declarations?
            inf.name = item.type_name.value + ': ' +
                locator_text(item.bodies.first.title.offset, item.bodies.first.title.length, item.bodies.first.title.locator)
          end

          @inferences << inf
          nil
        end

        def infer_LambdaExpression(item, parent_scope, parent_ast)
          # Lambdas have no name (Duh!) but we still need the scope information
          scope = ScopeInference.new(item, nil, nil)
          scope.range = create_range(item.offset, item.length, item.locator)
          @inferences << scope

          infer_children_of(item, scope)
        end

        def infer_Parameter(item, parent_scope, parent_ast)
          inf = ParameterInference.new(item, item.name, parent_scope)
          inf.scope = parent_scope
          inf.range = create_range(item.offset, item.length, item.locator)
          unless item.value.nil?
            inf.default_value = locator_text(item.value.offset, item.value.length, item.value.locator)
          end

          @inferences << inf
          nil
        end

        def infer_VariableExpression(item, parent_scope, parent_ast)
          # Can only infer on assignments ($x = '123')
          # TODO: Infer variable usage
          # https://github.com/puppetlabs/puppet-specifications/blob/master/language/expressions.md#assignment-operator

          # # from array
          # [$a, $b] = [1, 2]  # assigns 1 to $a, and 2 to $b
          #
          # # from hash
          # [$a, $b] = { a => 10, b => 20, c => 30 }  # assigns 10 to $a, and 20 to $b
          #
          # # from class
          # class mymodule::someclass::example($x = 100) {
          #   $a = 10
          # }
          # include example
          # [$a, $x] = Class['mymodule::someclass::example']  # assigns 10 to $a, and 100 to $x

          # We need to have a parent
          return infer_children_of(item, parent_scope) if parent_ast.nil?
          # The parent needs to be an AssignmentExpression
          return infer_children_of(item, parent_scope) unless parent_ast._pcore_type.name == "Puppet::AST::AssignmentExpression"
          # We need to be the on the left hand-side of the assignment
          return infer_children_of(item, parent_scope) unless parent_ast.left_expr == item

          inf = VariableInference.new(item, item.expr.value, parent_scope)
          inf.range = create_range(item.offset, item.length, item.locator)

          @inferences << inf
          infer_children_of(item, parent_scope)
        end

        def infer_Object(item, parent_scope, parent_ast)
          # Always traverse any child objects?
          puts "WASIGNORED #{item._pcore_type.name}" if @debug && item.respond_to?(:_pcore_type)
          infer_children_of(item, parent_scope)
        end

        private

        def infer_children_of(item, parent_scope)
          item._pcore_contents { |child| infer(child, parent_scope, item) }
        end
      end

      # Scans an AST looking and tries to resolve all inferences with a Puppet type.
      # ref - https://github.com/puppetlabs/puppet/tree/master/lib/puppet/pops/types/type_factory.rb
      # ref - https://github.com/puppetlabs/puppet/tree/master/lib/puppet/pops/types/type_parser.rb
      class TypeDetectorEvaluator
        attr_reader :inferences
        attr_accessor :debug

        def default_type
          Puppet::Pops::Types::TypeFactory.any
        end

        def initialize(inferences, object_cache = nil)
          @debug = false
          @inferences = inferences
          @inference_visitor = ::Puppet::Pops::Visitor.new(self, "infer", 0, nil)
          @function_inferencer = ManifestFunctionInferencer.new(self, object_cache)
          @type_parser = Puppet::Pops::Types::TypeParser.new
          # @calc = Puppet::Pops::Types::TypeCalculator.new
        end

        def locator_text(offset, length, locator)
          locator.string.slice(offset, length)
        end

        def find_inference(ast_object)
          # TODO - This is quite a slow way to do this. Okay for small manifests but big ones?
          find_str = ast_object.object_id.to_s
          @inferences.find { |inf| inf.ast_object_id == find_str }
        end

        def find_variable_inference(ast_object)
          var_name = ast_object.expr.value
          # TODO - This is quite a slow way to do this. Okay for small manifests but big ones?
          @inferences.find { |inf| inf.is_a?(VariableInference) && inf.name == var_name }
        end

        def infer(ast)
          return default_type if ast.nil?
          puts "INFERRING #{ast._pcore_type.name}" if @debug && ast.respond_to?(:_pcore_type)
          @inference_visitor.visit_this_0(self, ast)
        rescue Puppet::ParseError => e
          puts "ERROR #{e}" if @debug
          default_type
        end

        # Plumbing object types
        def infer_Factory(item)
          infer(item.model)
          default_type
        end

        def infer_Program(item)
          infer(item.body)
          default_type
        end

        # Expressions
        def infer_BlockExpression(item)
          item._pcore_contents { |child| infer(child) }
          # For now we don't care about the return type of statements
          default_type
        end

        def infer_UnaryMinusExpression(item)
          result = infer(item.expr)
          return result unless result.is_a?(Puppet::Pops::Types::PNumericType)
          # If it's the default then you can't negate it
          return result if result.from.nil? && result.to.nil?
          # Change the from and to, to a minus
          from = result.from.nil? ? nil : -result.from
          to = result.to.nil? ? nil : -result.to
          # Flip the from and to so they're in the correct order if needs be
          if !from.nil? && !to.nil? && to > from
            temp = from
            from = to
            to = temp
          end
          # Create a new class
          result.class.new(from, to)
        end

        def infer_AssignmentExpression(item)
          # https://github.com/puppetlabs/puppet-specifications/blob/master/language/expressions.md#assignment-operator

          # Can only infer on assignments ($x = '123')
          return default_type unless item.operator == '='

          return_type = infer(item.right_expr)
          return_type = default_type if return_type.nil?

          # We can only infer on single var assignments ($x = '123'). Otherwise just return the result of the type on the right hand side.
          return return_type unless item.left_expr._pcore_type.name == 'Puppet::AST::VariableExpression'

          inf = find_inference(item.left_expr)
          # If we can't find the variable inference then return the type
          inf.puppet_type = return_type unless inf.nil?
          return_type
        end

        def infer_CallNamedFunctionExpression(item)
          if item.functor_expr._pcore_type.name == 'Puppet::AST::QualifiedReference'
            ref_name = infer(item.functor_expr)
            # Special case some types
            ref_name = 'Array[Any]' if ref_name == 'Array'
            ref_name = 'Hash[Any, Any]' if ref_name == 'Hash'
            return ref_name
          end

          if item.functor_expr.is_a?(Puppet::Pops::Model::QualifiedName)
            function_name = item.functor_expr.value
            parameter_types = item.arguments.map { |arg| infer(arg) }
            result = @function_inferencer.calculate_function_call_types(function_name, parameter_types, item.lambda)
            # Still need to process the children
            infer_children_of(item)
            return result
          end

          infer_children_of(item)
        end

        def infer_CallMethodExpression(item)
          return default_type unless item.functor_expr.is_a?(Puppet::Pops::Model::NamedAccessExpression)
          return default_type unless item.functor_expr.right_expr.is_a?(Puppet::Pops::Model::QualifiedName)

          function_name = item.functor_expr.right_expr.value
          parameter_types = [infer(item.functor_expr.left_expr)].concat(item.arguments.map { |arg| infer(arg) })

          item.arguments.map { |arg| infer(arg) }

          result = @function_inferencer.calculate_function_call_types(function_name, parameter_types, item.lambda)
          # Still need to process the children
          infer_children_of(item)
          return result
        end

        def infer_VariableExpression(item)
          inf = find_variable_inference(item)
          inf.nil? ? default_type : inf.puppet_type
        end

        def infer_ArithmeticExpression(item)
          type_left = infer(item.left_expr)
          type_right = infer(item.right_expr)

          if item.operator == '>>'
            return default_type unless type_left.instance_of?(Puppet::Pops::Types::PIntegerType)
            return default_type unless type_right.instance_of?(Puppet::Pops::Types::PIntegerType)
            return Puppet::Pops::Types::TypeFactory.integer
          end

          type_arithmetic_result(type_left, type_right)
        end

        def infer_InExpression(item)
          Puppet::Pops::Types::TypeFactory.boolean
        end

        def infer_ComparisonExpression(item)
          Puppet::Pops::Types::TypeFactory.boolean
        end

        def infer_NotExpression(item)
          expr_type = infer(item.expr)
          return Puppet::Pops::Types::TypeFactory.boolean unless expr_type.instance_of?(Puppet::Pops::Types::PBooleanType)
          return Puppet::Pops::Types::TypeFactory.boolean if expr_type.value.nil?
          # Actually do a binary NOT if we know the value
          Puppet::Pops::Types::TypeFactory.boolean(!expr_type.value)
        end

        def infer_AndExpression(item)
          type_left = infer(item.left_expr)
          type_right = infer(item.right_expr)
          return Puppet::Pops::Types::TypeFactory.boolean unless type_left.instance_of?(Puppet::Pops::Types::PBooleanType) && !type_left.value.nil?
          return Puppet::Pops::Types::TypeFactory.boolean unless type_right.instance_of?(Puppet::Pops::Types::PBooleanType) && !type_right.value.nil?
          Puppet::Pops::Types::TypeFactory.boolean(type_left.value && type_right.value)
        end

        def infer_OrExpression(item)
          type_left = infer(item.left_expr)
          type_right = infer(item.right_expr)
          return Puppet::Pops::Types::TypeFactory.boolean unless type_left.instance_of?(Puppet::Pops::Types::PBooleanType) && !type_left.value.nil?
          return Puppet::Pops::Types::TypeFactory.boolean unless type_right.instance_of?(Puppet::Pops::Types::PBooleanType) && !type_right.value.nil?
          Puppet::Pops::Types::TypeFactory.boolean(type_left.value || type_right.value)
        end

        def infer_MatchExpression(item)
          Puppet::Pops::Types::TypeFactory.boolean
        end

        def infer_AccessExpression(item)
          # Ref - https://github.com/puppetlabs/puppet-specifications/blob/4e54d7a8c1e8f16c1cdbc44d4b5537c27784e7cd/language/expressions.md#--access-operator
          left_type = infer(item.left_expr)

          # Strings
          if left_type.instance_of?(Puppet::Pops::Types::PStringType)
            # If it's 'String[..., ...] parse the type
            return @type_parser.parse(ast_text(item)) if item.left_expr.is_a?(Puppet::Pops::Model::QualifiedReference)
            # Otherwise it's just a plain string
            return Puppet::Pops::Types::TypeFactory.string
          end

          # Arrays
          if left_type.instance_of?(Puppet::Pops::Types::PArrayType)
            return default_type if item.keys.count < 1 || item.keys.count > 3
            type_text = 'Array[' + infer(item.keys[0]).to_s
            type_text += ", #{ast_text(item.keys[1])}" if item.keys.count >= 2
            type_text += ", #{ast_text(item.keys[2])}" if item.keys.count == 3
            type_text += ']'
            return @type_parser.parse(type_text)
          end

          # Classes are just a beast to deal with.  For now, any access expression
          # just returns a default class.
          return left_type if left_type.instance_of?(Puppet::Pops::Types::PClassType)

          # SemVer is always a SemVer
          return left_type if left_type.instance_of?(Puppet::Pops::Types::PSemVerType)

          # Somewhat catch all for:
          # Regular Expressions, Patterns, Enums, Hashes etc.
          # We can't really infer too much here as the arguments to the expression could be anything (Function, literal, expression etc.)
          # But we _always_ know the generic type, just not the specifics
          return infer(item.left_expr) if item.left_expr.is_a?(Puppet::Pops::Model::QualifiedReference)

          infer_children_of(item)
        end

        # POP Literal Types or obvious type assignment
        def infer_QualifiedReference(item)
         # A QualifiedReference (i.e. a capitalized qualified name such as Foo, or Foo::Bar) evaluates to a PTypeType
          # TODO  This may actually call REAL class loading... need to not use it
          @type_parser.parse(ast_text(item))
        end

        def infer_LiteralBoolean(item)
          Puppet::Pops::Types::TypeFactory.boolean(item.value)
        end

        def infer_LiteralInteger(item)
          Puppet::Pops::Types::TypeFactory.range(item.value, item.value)
        end

        def infer_LiteralFloat(item)
          Puppet::Pops::Types::TypeFactory.float_range(item.value, item.value)
        end

        def infer_LiteralList(item)
          value_types = item.values.map { |value| infer(value) }.compact
          value_types = squash_type_list(value_types)

          # An empty array ($foo = []). No idea what's going in there, so default Array
          return Puppet::Pops::Types::TypeFactory.array_of_any if value_types.nil? || value_types.empty?
          # If there are any values of type Any, all bets are off and just use 'Array[Any]'
          return Puppet::Pops::Types::TypeFactory.array_of_any if value_types.find { |i| i.name == 'Any' }
          # All the same types then use Array[<type>] else use any array of variant
          return Puppet::Pops::Types::TypeFactory.array_of(value_types.first) if value_types.count == 1

          Puppet::Pops::Types::TypeFactory.array_of(
            Puppet::Pops::Types::TypeFactory.variant(*value_types)
          )
        end

        def infer_LiteralHash(item)
          key_types = []
          value_types = []
          item.entries.each do |entry|
            key_types << infer(entry.key)
            value_types << infer(entry.value)
          end
          key_types = squash_type_list(key_types)
          value_types = squash_type_list(value_types)
          length = item.entries.count

          Puppet::Pops::Types::TypeFactory.hash_kv(
            key_types.count > 1 ? key_types[0] : Puppet::Pops::Types::TypeFactory.variant(*key_types),
            value_types.count > 1 ? value_types[0] : Puppet::Pops::Types::TypeFactory.variant(*value_types),
            Puppet::Pops::Types::TypeFactory.range(length, length)
          )
        end

        def infer_LiteralString(_)
          Puppet::Pops::Types::TypeFactory.string
        end

        def infer_ConcatenatedString(_)
          # A Concatenated string always returns a String type
          Puppet::Pops::Types::TypeFactory.string
        end

        def infer_LiteralUndef(_)
          Puppet::Pops::Types::TypeFactory.undef
        end

        def infer_Parameter(item)
          inf = find_inference(item)
          return infer_children_of(item) if inf.nil?

          new_param_type = infer(item.type_expr)
          if inf.puppet_type.nil? || inf.instance_of?(Puppet::Pops::Types::PAnyType)
            # Always update if the current type is not known or Any
            inf.puppet_type = new_param_type
          elsif !new_param_type.instance_of?(Puppet::Pops::Types::PAnyType)
            # We know we currently have a specific type, so we shouldn't update to Any.
            inf.puppet_type = new_param_type
          end

          inf.puppet_type
        end

        # Catch all
        def infer_Object(item)
          # Ignore any other object types
          puts "TYPES Ignornig #{item.class.to_s}" if @debug

          #infer_children_of(item)
          # Always traverse any child objects?
          #item._pcore_contents { |child| infer(child) }
          infer_children_of(item)
        end

        # Takes an array of types and reduces it down to the small list possible
        def squash_type_list(object_types)
          return [] if object_types.nil? | object_types.empty?
          return object_types if object_types.length == 1

          # TODO: Should we squish down Variant/s?
          # e.g. [ Variant[String, Integer], Variant[Undef] ] should be Variant[String, Integer, Undef]

          object_types.reduce({ result: [], found: []}) do |acc, object_type|
            type_name = object_type.to_s
            unless acc[:found].include?(type_name)
              acc[:result] << object_type
              acc[:found] << type_name
            end
            acc
          end[:result]
        end

        private

        def type_arithmetic_result(left, right)
          # Integer
          # Integer ∪ Integer               → Integer
          # Integer ∪ Float                 → Float
          # Integer ∪ Numeric               → Numeric
          if left.instance_of?(Puppet::Pops::Types::PIntegerType)
            return left if right.instance_of?(Puppet::Pops::Types::PIntegerType)
            return Puppet::Pops::Types::TypeFactory.float if right.is_a?(Puppet::Pops::Types::PFloatType)
            return Puppet::Pops::Types::TypeFactory.numeric if right.is_a?(Puppet::Pops::Types::PNumericType)
          end

          # Float
          # Float ∪ Float               → Float
          # Float ∪ Integer             → Float
          # Float ∪ Numeric             → Numeric
          if left.instance_of?(Puppet::Pops::Types::PFloatType)
            return left if right.is_a?(Puppet::Pops::Types::PNumericType)
          end

          # Numeric
          # Numeric ∪ Numeric           → Numeric
          # Numeric ∪ Integer           → Numeric
          # Numeric ∪ Float             → Float
          if left.instance_of?(Puppet::Pops::Types::PNumericType)
            return left if right.is_a?(Puppet::Pops::Types::PNumericType)
          end

          # Catch all. If we don't know, return the default type
          return default_type

          # #---- THIS IS ALL WRONG!!!
          # #
          # # Ref - https://github.com/puppetlabs/puppet-specifications/blob/3c0f46adf68000c08dcb04fa4a61e73560a6787b/language/types_values_variables.md

          # # Any Type
          # # Any ∪ Any        → Any
          # # Any ∪ (T != Any) → Any
          # return left if left.instance_of?(Puppet::Pops::Types::PAnyType)

          # # Undef Types
          # # Undef ∪ Undef          → Undef
          # # Undef ∪ (T ∉ Undef)    → Any
          # if left.instance_of?(Puppet::Pops::Types::PUndefType)
          #   return (right.instance_of?(Puppet::Pops::Types::PUndefType) ? right : Puppet::Pops::Types::TypeFactory.any)
          # end

          # # Data Type (Which is actually an Alias)
          # # Data ∪ Data                 → Data
          # # Data ∪ Numeric              → Data
          # # Data ∪ String               → Data
          # # Data ∪ Array[Data]          → Data
          # # Data ∪ Hash[String, Data]   → Data
          # # Data ∪ Undef                → Data
          # # Data ∪ (T ∉ Data)           → Any
          # if left.instance_of?(Puppet::Pops::Types::PTypeAliasType) && left.name == 'Data'
          #   result = left.assignable?(right) ? left : Puppet::Pops::Types::TypeFactory.any
          #   return result
          # end

          # # RichData ?

          # # ScalarData ?

          # # Scalar
          # # Scalar ∪ Scalar          → Scalar
          # # Scalar ∪ (T ∈ Scalar)    → Scalar
          # # Scalar ∪ (T ∉ Scalar)    → Any
          # if left.instance_of?(Puppet::Pops::Types::PScalarType)
          #   result = right.is_a?(Puppet::Pops::Types::PScalarType) ? Puppet::Pops::Types::TypeFactory.scalar : Puppet::Pops::Types::TypeFactory.any
          #   return result
          # end

          # # Numeric
          # # Numeric ∪ Numeric          → Numeric
          # # Numeric ∪ (T ∈ Numeric)    → Numeric
          # # Numeric ∪ (T ∈ Scalar)     → Scalar
          # # Numeric ∪ (T ∉ Scalar)     → Any
          # if left.instance_of?(Puppet::Pops::Types::PNumericType)
          #   return left if right.is_a?(Puppet::Pops::Types::PNumericType)
          #   result = right.is_a?(Puppet::Pops::Types::PScalarType) ? Puppet::Pops::Types::TypeFactory.scalar : Puppet::Pops::Types::TypeFactory.any
          #   return result
          # end

          # # Integer
          # # Integer ∪ Integer               → Integer
          # # Integer ∪ Float                 → Numeric
          # # Integer ∪ Numeric               → Numeric
          # # Integer ∪ (T ∈ Scalar)          → Scalar
          # # Integer ∪ (T ∉ Scalar)          → Any
          # # Integer[a, b] ∪ Integer[c, d]   → Integer[min(a, c), max(b,d)]
          # if left.instance_of?(Puppet::Pops::Types::PIntegerType)
          #   return left if right.instance_of?(Puppet::Pops::Types::PIntegerType)
          #   return Puppet::Pops::Types::TypeFactory.numeric if right.is_a?(Puppet::Pops::Types::PNumericType)
          #   result = right.is_a?(Puppet::Pops::Types::PScalarType) ? Puppet::Pops::Types::TypeFactory.scalar : Puppet::Pops::Types::TypeFactory.any
          #   return result
          # end

          # # Integer
          # # Float ∪ Float               → Float
          # # Float ∪ Integer             → Numeric
          # # Float ∪ Numeric             → Numeric
          # # Float ∪ (T ∈ Scalar)        → Scalar
          # # Float ∪ (T ∉ Scalar)        → Any
          # # Float[a, b] ∪ Float[c, d]   → Float[min(a, c), max(b,d)]
          # if left.instance_of?(Puppet::Pops::Types::PFloatType)
          #   return left if right.instance_of?(Puppet::Pops::Types::PFloatType)
          #   return Puppet::Pops::Types::TypeFactory.numeric if right.is_a?(Puppet::Pops::Types::PNumericType)
          #   result = right.is_a?(Puppet::Pops::Types::PScalarType) ? Puppet::Pops::Types::TypeFactory.scalar : Puppet::Pops::Types::TypeFactory.any
          #   return result
          # end

          # # Timespan
          # # Timespan ∪ Timespan            → Timespan
          # # Timespan ∪ Numeric             → Scalar
          # # Timespan ∪ (T ∈ Scalar)        → Scalar
          # # Timespan ∪ (T ∉ Scalar)        → Any
          # # Timespan[a, b] ∪ Timespan[c, d]   → Timespan[min(a, c), max(b,d)]
          # if left.instance_of?(Puppet::Pops::Types::PTimespanType)
          #   return left if right.instance_of?(Puppet::Pops::Types::PTimespanType)
          #   return Puppet::Pops::Types::TypeFactory.scalar if right.is_a?(Puppet::Pops::Types::PNumericType)
          #   result = right.is_a?(Puppet::Pops::Types::PScalarType) ? Puppet::Pops::Types::TypeFactory.scalar : Puppet::Pops::Types::TypeFactory.any
          #   return result
          # end

          # # Timestamp
          # # Timestamp ∪ Timestamp           → Timestamp
          # # Timestamp ∪ Numeric             → Scalar
          # # Timestamp ∪ (T ∈ Scalar)        → Scalar
          # # Timestamp ∪ (T ∉ Scalar)        → Any
          # # Timestamp[a, b] ∪ Timestamp[c, d]   → Timestamp[min(a, c), max(b,d)]
          # if left.instance_of?(Puppet::Pops::Types::PTimestampType)
          #   return left if right.instance_of?(Puppet::Pops::Types::PTimestampType)
          #   return Puppet::Pops::Types::TypeFactory.scalar if right.is_a?(Puppet::Pops::Types::PNumericType)
          #   result = right.is_a?(Puppet::Pops::Types::PScalarType) ? Puppet::Pops::Types::TypeFactory.scalar : Puppet::Pops::Types::TypeFactory.any
          #   return result
          # end

          # # String
          # # type_of([a,b,c])                 # => Array[String<a,b,c>]
          # # String<a,b,c> == String<b,c,a>   # => true
          # # typeof([String<a,b>, String<c>]) # => Array[Type[String<a,b,c>]]
          # #
          # # String    ∪ String     → String
          # # String<x> ∪ String<x>  → String<x>
          # # String<x> ∪ String<y>  → String<x,y>
          # # String    ∪ Enum       → String
          # # String<x> ∪ Enum[x]    → String<x>
          # # String    ∪ Pattern    → String
          # # String ∪ (T ∈ Scalar)  → Scalar
          # # String ∪ (T ∉ Scalar)  → Any
          # if left.instance_of?(Puppet::Pops::Types::PStringType)
          #   return left if right.instance_of?(Puppet::Pops::Types::PStringType) || right.instance_of?(Puppet::Pops::Types::PEnumType) || right.instance_of?(Puppet::Pops::Types::PPatternType)
          #   result = right.is_a?(Puppet::Pops::Types::PScalarType) ? Puppet::Pops::Types::TypeFactory.scalar : Puppet::Pops::Types::TypeFactory.any
          #   return result
          # end

          # # Enum
          # # type_of([Enum[a,b,c], Enum[x,b,c]] # => Array[Type[Enum[a,b,c,x]]
          # if left.instance_of?(Puppet::Pops::Types::PEnumType)
          #   result = right.instance_of?(Puppet::Pops::Types::PEnumType) ? Puppet::Pops::Types::TypeFactory.enum : Puppet::Pops::Types::TypeFactory.any
          #   return result
          # end

          # # Pattern
          # # type_of([Pattern[a], Pattern[b]]) # => Array[Type[Pattern[a,b]]]
          # if left.instance_of?(Puppet::Pops::Types::PPatternType)
          #   result = right.instance_of?(Puppet::Pops::Types::PPatternType) ? Puppet::Pops::Types::TypeFactory.pattern : Puppet::Pops::Types::TypeFactory.any
          #   return result
          # end

          # #------------------------
          # # UP TO HERE - https://github.com/puppetlabs/puppet-specifications/blob/3c0f46adf68000c08dcb04fa4a61e73560a6787b/language/types_values_variables.md#booleanboolean-value
          # #------------------------

          # default_type
          # # elsif left == 'Data'
          # #   # TODO: This comparison is dodgey
          # #   return 'Data' if ['Data', 'Numeric', 'String', 'Array[Data]', 'Hash[String, Data]', 'Undef'].include?(right)
          # #   return 'Any'
          # # elsif left == 'Numeric'
          # #   # TODO: This comparison is dodgey
          # #   return 'Numeric' if ['Numeric', 'Float', 'Integer'].include?(right)
          # #   return 'Scalar' if ['String', 'Pattern', 'Enum', 'Boolean', 'Regexp', 'TimeStamp', 'TimeSpan', 'SemVer'].include?(right)
          # #   return 'Any'
          # # end
          # #default_type
        end

        def ast_text(ast_object)
          ::Puppet::Pops::Adapters::SourcePosAdapter.adapt(ast_object).extract_text
        end

        def infer_children_of(item)
          result = item._pcore_contents { |child| infer(child) }
          return result if result.is_a?(Puppet::Pops::Types::PuppetObject)
          default_type
          # puts "----------"
          # puts item._pcore_type.name
          # puts result.class
          # puts result
          # require 'pry'; binding.pry if result.is_a?(Array)
          # puts "----------"
          # TODO: Do we massage the output here? e.g. nil vs empty
        end
      end
    end
  end
end
