# frozen_string_literal: true

module PuppetLanguageServer
  module Manifest
    class Inferencer
      attr_reader :inferences
      attr_accessor :debug

      def initialize(session_state = nil)
        @debug = false # true
        @inferences = []
        @object_cache = session_state.nil? ? nil : session_state.object_cache
      end

      def find(name = nil, inference_class = BaseInference)
        inferences.find { |item| (name.nil? || item.name == name) && item.is_a?(inference_class) }
      end

      def select(name = nil, inference_class = BaseInference)
        inferences.select { |item| (name.nil? || item.name == name) && item.is_a?(inference_class) }
      end

      def infer(ast, _root_name = '')
        puts '---- Content' if @debug
        ::Puppet::Pops::Adapters::SourcePosAdapter.adapt(ast.model.body).extract_text if @debug
        puts '----' if @debug

        # First pass, find all of the possible inference points
        pass_one = InferenceDetectorEvaluator.new
        # pass_one.debug = @debug
        pass_one.infer(ast, nil)
        @inferences = pass_one.inferences

        puts '---- Phase 1 inferences' if @debug
        @inferences.each { |i| puts "[#{i.class}] #{i.name}" } if @debug
        puts '----' if @debug

        pass_two = TypeDetectorEvaluator.new(@inferences, @object_cache)
        pass_two.debug = @debug
        pass_two.infer(ast)

        puts '---- Phase 2 inferences' if @debug
        @inferences.each { |i| puts "[#{i.class}] #{i.name} is a #{i.respond_to?(:puppet_type) ? i.puppet_type : 'N/A'}" } if @debug
        puts '----' if @debug

        nil
      end
    end
  end
end

require 'puppet-languageserver/manifest/inferencer/inferences'
require 'puppet-languageserver/manifest/inferencer/inference_detector_evaluator'
require 'puppet-languageserver/manifest/inferencer/type_detector_evaluator'
require 'puppet-languageserver/manifest/inferencer/function_inferencer'
