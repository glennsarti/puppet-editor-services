# frozen_string_literal: true

module PuppetLanguageServer
  module Manifest
    class Inferencer
      class BaseInference
        attr_accessor :name
        # attr_accessor :full_name  # The fully qualified name?  $foo::bar
        attr_accessor :range
        attr_accessor :scope
        attr_accessor :children
        # The object_id of the AST object that created the inference
        attr_accessor :ast_object_id

        def initialize(ast_source, name = nil, scope = nil)
          # We use object_ids to, hopefully, improve garbage collection etc. May not be true
          @ast_object_id = ast_source.object_id.to_s
          @name = name
          @scope = scope
          @range = nil # TODO: Build this from ast_source?
          @children = []
          scope.children << self unless scope.nil?
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

        def initialize(*_)
          super
          @puppet_type = Puppet::Pops::Types::TypeFactory.any
        end

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
    end
  end
end
