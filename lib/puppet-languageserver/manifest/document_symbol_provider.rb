# frozen_string_literal: true

module PuppetLanguageServer
  module Manifest
    module DocumentSymbolProvider
      def self.workspace_symbols(query, object_cache)
        query = '' if query.nil?
        result = []
        object_cache.all_objects do |key, item|
          key_string = key.to_s
          next unless query.empty? || key_string.include?(query)
          case item
          when PuppetLanguageServer::Sidecar::Protocol::PuppetType
            result << LSP::SymbolInformation.new(
              'name'     => key_string,
              'kind'     => LSP::SymbolKind::METHOD,
              'location' => {
                'uri'   => PuppetLanguageServer::UriHelper.build_file_uri(item.source),
                # Don't have char pos for types so just pick extreme values
                'range' => LSP.create_range(item.line, 0, item.line, 1024)
              }
            )

          when PuppetLanguageServer::Sidecar::Protocol::PuppetFunction
            result << LSP::SymbolInformation.new(
              'name'     => key_string,
              'kind'     => LSP::SymbolKind::FUNCTION,
              'location' => {
                'uri'   => PuppetLanguageServer::UriHelper.build_file_uri(item.source),
                # Don't have char pos for functions so just pick extreme values
                'range' => LSP.create_range(item.line, 0, item.line, 1024)
              }
            )

          when PuppetLanguageServer::Sidecar::Protocol::PuppetClass
            result << LSP::SymbolInformation.new(
              'name'     => key_string,
              'kind'     => LSP::SymbolKind::CLASS,
              'location' => {
                'uri'   => PuppetLanguageServer::UriHelper.build_file_uri(item.source),
                # Don't have char pos for classes so just pick extreme values
                'range' => LSP.create_range(item.line, 0, item.line, 1024)
              }
            )

          when PuppetLanguageServer::Sidecar::Protocol::PuppetDataType
            result << LSP::SymbolInformation.new(
              'name'     => key_string,
              'kind'     => LSP::SymbolKind::NAMESPACE,
              'location' => {
                'uri'   => PuppetLanguageServer::UriHelper.build_file_uri(item.source),
                # Don't have char pos for data types so just pick extreme values
                'range' => LSP.create_range(item.line, 0, item.line, 1024)
              }
            )

          when PuppetLanguageServer::Sidecar::Protocol::Fact
            # Do nothing

          else
            PuppetLanguageServer.log_message(:warn, "[Manifest::DocumentSymbolProvider] Unknown object type #{item.class}")
          end
        end
        result
      end

      def self.extract_document_symbols(content, options = {})
        options = {
          :tasks_mode => false
        }.merge(options)

# puts "-------------------------- CONTENT"
# puts content
# puts "--------------------------"

        parser = Puppet::Pops::Parser::Parser.new
        result = parser.singleton_parse_string(content, options[:tasks_mode], '')

        require 'puppet-languageserver/manifest/manifest_inferencer'
        inferrer = PuppetLanguageServer::Manifest::ManifestInferencer::ManifestInferences.new
        inferrer.infer(result)

        symbols = inferrer.inferences
                          .select { |i| i.scope.nil? }
                          .map(&:to_lsp_symbol)
                          .compact
        symbols
      end

      def self.create_range(offset, length, locator)
        start_line = locator.line_for_offset(offset) - 1
        start_char = locator.pos_on_line(offset) - 1
        end_line = locator.line_for_offset(offset + length) - 1
        end_char = locator.pos_on_line(offset + length) - 1

        LSP.create_range(start_line, start_char, end_line, end_char)
      end
    end
  end
end
