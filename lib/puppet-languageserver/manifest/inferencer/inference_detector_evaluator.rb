# frozen_string_literal: true

require 'puppet-languageserver/manifest/inferencer/inferences'

# rubocop:disable Naming/MethodName
module PuppetLanguageServer
  module Manifest
    class Inferencer
      # Scans an AST looking for inference points and populates as much detail as possible
      class InferenceDetectorEvaluator
        attr_reader :inferences
        attr_accessor :debug

        def initialize
          @debug = false
          @inferences = []
          @inference_visitor ||= ::Puppet::Pops::Visitor.new(self, 'infer', 2, nil)
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
          # scope = ScopeInference.new(item, nil, nil)
          # scope.range = create_range(0, item.source_text.length, item.locator)
          # @inferences << scope

          infer(item.body, nil, item)
        end

        # Containing POPs
        def infer_HostClassDefinition(item, parent_scope, _parent_ast)
          inf = HostClassDefinitionInference.new(item, item.name, parent_scope)
          inf.range = create_range(item.offset, item.length, item.locator)

          @inferences << inf

          infer_children_of(item, inf)
        end

        def infer_PlanDefinition(item, parent_scope, _parent_ast)
          inf = PlanDefinitionInference.new(item, item.name, parent_scope)
          inf.range = create_range(item.offset, item.length, item.locator)

          @inferences << inf

          infer_children_of(item, inf)
        end

        def infer_ResourceExpression(item, parent_scope, _parent_ast)
          inf = ResourceExpressionInference.new(item, item.type_name.value, parent_scope)
          inf.range = create_range(item.offset, item.length, item.locator)

          if item.bodies.count >= 1
            # Ick!   What about multi resource declarations?
            inf.name = item.type_name.value + ': ' + locator_text(item.bodies.first.title.offset, item.bodies.first.title.length, item.bodies.first.title.locator)
          end

          @inferences << inf
          nil
        end

        def infer_LambdaExpression(item, _parent_scope, _parent_ast)
          # Lambdas have no name (Duh!) but we still need the scope information
          scope = ScopeInference.new(item, nil, nil)
          scope.range = create_range(item.offset, item.length, item.locator)
          @inferences << scope

          infer_children_of(item, scope)
        end

        def infer_Parameter(item, parent_scope, _parent_ast)
          inf = ParameterInference.new(item, item.name, parent_scope)
          inf.scope = parent_scope
          inf.range = create_range(item.offset, item.length, item.locator)
          inf.default_value = locator_text(item.value.offset, item.value.length, item.value.locator) unless item.value.nil?

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
          return infer_children_of(item, parent_scope) unless parent_ast._pcore_type.name == 'Puppet::AST::AssignmentExpression'
          # We need to be the on the left hand-side of the assignment
          return infer_children_of(item, parent_scope) unless parent_ast.left_expr == item

          inf = VariableInference.new(item, item.expr.value, parent_scope)
          inf.range = create_range(item.offset, item.length, item.locator)

          @inferences << inf
          infer_children_of(item, parent_scope)
        end

        def infer_Object(item, parent_scope, _parent_ast)
          # Always traverse any child objects?
          puts "WASIGNORED #{item._pcore_type.name}" if @debug && item.respond_to?(:_pcore_type)
          infer_children_of(item, parent_scope)
        end

        private

        def infer_children_of(item, parent_scope)
          item._pcore_contents { |child| infer(child, parent_scope, item) }
        end
      end
    end
  end
end
# rubocop:enable Naming/MethodName
