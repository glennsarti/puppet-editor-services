# frozen_string_literal: true

require 'puppet/pops/types/type_factory'
require 'puppet/pops/types/type_parser'

module PuppetLanguageServer
  module Manifest
    class Inferencer
      class FunctionInferencer
        def initialize(type_detector, object_cache = nil)
          @type_detector = type_detector
          @object_cache = object_cache
          @type_parser = Puppet::Pops::Types::TypeParser.new
        end

        def calculate_function_call_types(func_name, parameter_types, func_lambda)
          result = calculate_default_function_call_types(func_name, parameter_types, func_lambda)

          if result.nil? && !@object_cache.nil?
            result = calculate_object_cache_function_call(func_name, parameter_types, func_lambda)
          end

          # puts result
          # TODO return if no lambda

          # Setup lambda parameter type definitions
          # make sure we call infer(lambda) to do child inferences
          result
        end

        def calculate_object_cache_function_call(func_name, parameter_types, func_lambda)
          func = @object_cache.object_by_name(:function, func_name, fuzzy_match: true)
          return nil if func.nil? || func.signatures.empty?

          # We have a function, now find an appropriate signature
          # TODO: Also match type? For now just match the number of params
          func_sig = func.signatures.find { |sig| sig.parameters.count == parameter_types.count }
          return nil if func_sig.nil?
          return @type_detector.default_type if func_sig.return_types.nil? || func_sig.return_types.empty?

          type_text = func_sig.return_types.count == 1 ? func_sig.return_types[0] : 'Variant[' + func_sig.return_types.join(',') + ']'
          # I don't like this ... but ... best we've got.
          @type_parser.parse(type_text, nil)
        end

        def calculate_default_function_call_types(func_name, parameter_types, func_lambda)
          # https://puppet.com/docs/puppet/latest/function.html
          # C:\Source\puppet\lib\puppet\functions
          case func_name
          when 'abs'
            calculate_abs_function(parameter_types, func_lambda)
          when 'alert'
            Puppet::Pops::Types::TypeFactory.undef
          # all
          # annotate
          # any
          # assert_type
          when 'binary_file'
            Puppet::Pops::Types::TypeFactory.binary
          when 'break'
            Puppet::Pops::Types::TypeFactory.undef
          when 'call'
            @type_detector.default_type
          # camelcase
          # capitalize
          when 'ceiling'
            Puppet::Pops::Types::TypeFactory.integer
          # chomp
          # chop
          when 'compare'
            Puppet::Pops::Types::TypeFactory.range(-1, 1)
          # contain
          # convert_to
          # create_resources
          when 'crit'
            Puppet::Pops::Types::TypeFactory.undef
          when 'debug'
            Puppet::Pops::Types::TypeFactory.undef
          when 'defined'
            Puppet::Pops::Types::TypeFactory.boolean
          when 'dig'
            @type_detector.default_type
          when 'digest'
            Puppet::Pops::Types::TypeFactory.string
          # downcase
          # each
          when 'emerg'
            Puppet::Pops::Types::TypeFactory.undef
          when 'empty'
            Puppet::Pops::Types::TypeFactory.boolean
          when 'epp'
            Puppet::Pops::Types::TypeFactory.string
          when 'err'
            Puppet::Pops::Types::TypeFactory.undef
          when 'eyaml_lookup_key'
            @type_detector.default_type
          when 'fail'
            Puppet::Pops::Types::TypeFactory.undef
          when 'file'
            Puppet::Pops::Types::TypeFactory.string
          # filter
          when 'find_file'
            Puppet::Pops::Types::TypeFactory.variant(Puppet::Pops::Types::TypeFactory.string, Puppet::Pops::Types::TypeFactory.undef)
          # flatten
          when 'floor'
            Puppet::Pops::Types::TypeFactory.integer
          when 'fqdn_rand'
            Puppet::Pops::Types::TypeFactory.integer
          when 'generate'
            Puppet::Pops::Types::TypeFactory.string
          when 'get'
            @type_detector.default_type
          when 'getvar'
            @type_detector.default_type
          # group_by
          when 'hiera'
            @type_detector.default_type
          when 'hiera_array'
            Puppet::Pops::Types::TypeFactory.array_of_any
          when 'hiera_hash'
            Puppet::Pops::Types::TypeFactory.hash_of_any
          # hiera_include
          # hocon_data
          # import
          when 'include'
            # TODO: Should actually be Array[Type[Class]]
            Puppet::Pops::Types::TypeFactory.array_of_any
          when 'index'
            Puppet::Pops::Types::TypeFactory.integer
          when 'info'
            Puppet::Pops::Types::TypeFactory.undef
          when 'inline_epp'
            Puppet::Pops::Types::TypeFactory.string
          when 'inline_template'
            Puppet::Pops::Types::TypeFactory.string
          when 'join'
            Puppet::Pops::Types::TypeFactory.string
          # json_data
          # keys
          when 'length'
            Puppet::Pops::Types::TypeFactory.range(0, 'default')
          when 'lest'
            @type_detector.default_type
          when 'lookup'
            @type_detector.default_type
          # lstrip
          when 'map'
            calculate_map_function(parameter_types, func_lambda)
          # match
          when 'max'
            @type_detector.default_type
          when 'md5'
            Puppet::Pops::Types::TypeFactory.string
          when 'min'
            @type_detector.default_type
          when 'module_directory'
            Puppet::Pops::Types::TypeFactory.variant(Puppet::Pops::Types::TypeFactory.string, Puppet::Pops::Types::TypeFactory.undef)

          # new
          # next
          when 'notice'
            Puppet::Pops::Types::TypeFactory.undef
          when 'partition'
            Puppet::Pops::Types::TypeFactory.tuple(
              [
                Puppet::Pops::Types::TypeFactory.array_of_any,
                Puppet::Pops::Types::TypeFactory.array_of_any
              ]
            )
          when 'realize'
            Puppet::Pops::Types::TypeFactory.undef
          # reduce
          # regsubst
          when 'require'
            Puppet::Pops::Types::TypeFactory.undef
          # return
          # reverse_each
          when 'round'
            Puppet::Pops::Types::TypeFactory.integer
          # rstrip
          # scanf
          when 'sha1'
            Puppet::Pops::Types::TypeFactory.string
          when 'sha256'
            Puppet::Pops::Types::TypeFactory.string
          when 'shellquote'
            Puppet::Pops::Types::TypeFactory.string

          # size
          # slice
          # sort
          when 'split'
            Puppet::Pops::Types::TypeFactory.array_of(Puppet::Pops::Types::TypeFactory.string)
          when 'sprintf'
            Puppet::Pops::Types::TypeFactory.undef
          # step
          when 'strftime'
            Puppet::Pops::Types::TypeFactory.string
          # strip
          when 'tag'
            Puppet::Pops::Types::TypeFactory.undef
          when 'tagged'
            Puppet::Pops::Types::TypeFactory.boolean
          when 'template'
            Puppet::Pops::Types::TypeFactory.string
          when 'then'
            @type_detector.default_type
          when 'tree_each'
            @type_detector.default_type
          when 'type'
            # TODO: not strictly true! should just return the inferred type
            @type_detector.default_type
          # unique
          # unwrap
          # upcase
          # values
          when 'versioncmp'
            Puppet::Pops::Types::TypeFactory.range(-1, 1)
          when 'warning'
            Puppet::Pops::Types::TypeFactory.undef
          # with
          when 'yaml_data'
            @type_detector.default_type
          else
            nil
          end
        end

        def calculate_abs_function(parameter_types, func_lambda)
          return @type_detector.default_type unless parameter_types.count == 1
          return Puppet::Pops::Types::TypeFactory.numeric if parameter_types[0].is_a?(Puppet::Pops::Types::PStringType)

          param_type = parameter_types[0]
          if param_type.is_a?(Puppet::Pops::Types::PNumericType)
            new_from = param_type.from.nil? ? nil : param_type.from.abs
            new_to = param_type.to.nil? ? nil : param_type.to.abs
            return param_type.class.new(new_from, new_to)
          end

          @type_detector.default_type
        end

        def calculate_map_function(parameter_types, func_lambda)
          # https://puppet.com/docs/puppet/latest/function.html#map
          return_type = Puppet::Pops::Types::TypeFactory.array_of_any
          # If we have no parameters or there's no block, there's no need to continue
          return return_type if parameter_types.empty? || func_lambda.nil?
          lambda_inf = @type_detector.find_inference(func_lambda)
          return return_type if lambda_inf.nil?

          caller_type = parameter_types[0]

          set_param_types(lambda_inf) do |param_count|
            if caller_type.is_a?(Puppet::Pops::Types::PHashType)
              if param_count == 2
                [caller_type.key_type, caller_type.value_type]
              elsif param_count == 1
                tl = @type_detector.squash_type_list([caller_type.key_type, caller_type.value_type])
                [
                  Puppet::Pops::Types::TypeFactory.array_of(
                    Puppet::Pops::Types::TypeFactory.variant(*tl),
                    Puppet::Pops::Types::TypeFactory.range(2, 2)
                  )
                ]
              end
            elsif caller_type.is_a?(Puppet::Pops::Types::PArrayType)
              if param_count == 2
                [Puppet::Pops::Types::TypeFactory.integer, caller_type.element_type]
              elsif param_count == 1
                [caller_type.element_type]
              end
            end
          end

          return_type
        end

        def set_param_types(lambda_inf, &block)
          params = lambda_inf.children.select { |child| child.is_a?(ParameterInference) }
          new_param_types = yield params.count
          return if new_param_types.nil? || !new_param_types.is_a?(Array) || new_param_types.empty?

          new_param_types.each_with_index do |item, index|
            next if item.nil?
            params[index].puppet_type = item
          end
          nil
        end
      end
    end
  end
end
