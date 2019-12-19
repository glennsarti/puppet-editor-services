# frozen_string_literal: true

require 'puppet-lint'
module PuppetLanguageServer
  module Manifest
    module ValidationProvider
      MAX_LINE_LENGTH = 1024

      # Similar to 'validate' this will run puppet-lint and returns
      # the manifest with any fixes applied
      #
      # Returns:
      #  [ <Int> Number of problems fixed,
      #    <String> New Content
      #  ]
      def self.fix_validate_errors(content)
        module_root = PuppetLanguageServer::DocumentStore.store_root_path
        linter_options = nil
        if module_root.nil?
          linter_options = PuppetLint::OptParser.build
        else
          Dir.chdir(module_root.to_s) { linter_options = PuppetLint::OptParser.build }
        end
        linter_options.parse!(['--fix'])

        linter = PuppetLint::Checks.new
        linter.load_data(nil, content)

        problems = linter.run(nil, content)
        problems_fixed = problems.nil? ? 0 : problems.count { |item| item[:kind] == :fixed }

        [problems_fixed, linter.manifest]
      end

      def self.validate(content, options = {})
        options = {
          :max_problems => 100,
          :tasks_mode   => false
        }.merge(options)

        result = []
        # TODO: Need to implement max_problems
        problems = 0

        module_root = PuppetLanguageServer::DocumentStore.store_root_path
        linter_options = nil
        if module_root.nil?
          linter_options = PuppetLint::OptParser.build
        else
          Dir.chdir(module_root.to_s) { linter_options = PuppetLint::OptParser.build }
        end
        linter_options.parse!([])

        begin
          linter = PuppetLint::Checks.new
          linter.load_data(nil, content)

          problems = linter.run(nil, content)
          unless problems.nil?
            problems.each do |problem|
              # Syntax errors are better handled by the puppet parser, not puppet lint
              next if problem[:kind] == :error && problem[:check] == :syntax
              # Ignore linting errors what were ignored by puppet-lint
              next if problem[:kind] == :ignored

              severity = case problem[:kind]
                         when :error
                           LSP::DiagnosticSeverity::ERROR
                         when :warning
                           LSP::DiagnosticSeverity::WARNING
                         else
                           LSP::DiagnosticSeverity::HINT
                         end

              endpos = problem[:column] - 1
              endpos = problem[:column] - 1 + problem[:token].to_manifest.length unless problem[:token].nil? || problem[:token].value.nil?

              result << LSP::Diagnostic.new('severity' => severity,
                                            'code'     => problem[:check].to_s,
                                            'range'    => LSP.create_range(problem[:line] - 1, problem[:column] - 1, problem[:line] - 1, endpos),
                                            'source'   => 'Puppet',
                                            'message'  => problem[:message])
            end
          end
        # rubocop:disable Lint/SuppressedException
        rescue StandardError
          # If anything catastrophic happens we resort to puppet parsing anyway
        end
        # rubocop:enable Lint/SuppressedException

        parser = Puppet::Pops::Parser::Parser.new
        begin
          options = {
            :tasks_mode => false
          }.merge(options)
          parser.singleton_parse_string(content, options[:tasks_mode], '')
        rescue Puppet::ParseErrorWithIssue => e
          # If we have no position information assume it's line 0, character 0
          ex_line = e.line.nil? ? 0 : e.line - 1 # Line numbers from puppet exceptions are base 1
          ex_pos = e.pos.nil? ? 0 : e.pos - 1 # Pos numbers from puppet are base 1
          # ParseErrorWithIssue doesn't give us a length parameter so we have to guess. The best
          # option is it's on this one and only character, so a length of 1. If we no positional information
          # then assume it's on the entire line (MAX_LINE_LENGTH)
          ex_length = e.pos.nil? ? MAX_LINE_LENGTH : 1
          message = (e.basic_message.nil? || e.basic_message.empty?) ? e.issue_code.to_s : e.basic_message

          result << LSP::Diagnostic.new('severity' => LSP::DiagnosticSeverity::ERROR,
                                        'range'    => LSP.create_range(ex_line, ex_pos, ex_line, ex_pos + ex_length),
                                        'source'   => 'Puppet',
                                        'message'  => message)
        rescue StandardError => e
          # Sometimes the error is in the cause not the root object itself
          e = e.cause if !e.respond_to?(:line) && e.respond_to?(:cause)
          ex_line = e.respond_to?(:line) && !e.line.nil? ? e.line - 1 : nil # Line numbers from puppet exceptions are base 1
          ex_pos = e.respond_to?(:pos) && !e.pos.nil? ? e.pos : nil # Pos numbers from puppet are base 1

          message = e.respond_to?(:message) ? e.message : nil
          message = e.basic_message if message.nil? && e.respond_to?(:basic_message)

          unless ex_line.nil? || ex_pos.nil? || message.nil?
            result << LSP::Diagnostic.new('severity' => LSP::DiagnosticSeverity::ERROR,
                                          'range'    => LSP.create_range(ex_line, ex_pos, ex_line, ex_pos + 1),
                                          'source'   => 'Puppet',
                                          'message'  => message)
          end
        end

        # Output any Lexer warnings too
        parser.lexer.lexer_warnings.each do |warning_hash|
          message = (warning_hash[:message].nil? || warning_hash[:message].empty?) ? warning_hash[:issue].issue_code.to_s : warning_hash[:message]
          ex_line, ex_start_pos, ex_end_pos = extract_lexer_warning_location(content, warning_hash)

          result << LSP::Diagnostic.new('severity' => LSP::DiagnosticSeverity::WARNING,
            'range'    => LSP.create_range(ex_line, ex_start_pos, ex_line, ex_end_pos),
            'source'   => 'Puppet',
            'message'  => message)
        end
        result
      end

      def self.extract_lexer_warning_location(content, warning_hash)
        # If there's no positional information just assume the first line
        return [0, 0, MAX_LINE_LENGTH] if warning_hash[:pos].nil? || warning_hash[:line].nil?

        # Default result is the single character that caused the issue
        result = [warning_hash[:line] - 1, warning_hash[:pos] - 1, warning_hash[:pos]] # Line and Pos numbers from puppet exceptions are base 1
        if warning_hash[:issue].issue_code == :UNRECOGNIZED_ESCAPE
          # We need the line_text and :ch arg to do this, return default if they don't exist
          return result if warning_hash[:line_text].nil? || warning_hash[:args].nil? || warning_hash[:args][:ch].nil?
          # Try and find the unrecognized code in the text
          # Trim down the seach text to UP to the error.  Lex warnings report the END of the token, not the beginning
          search_text = warning_hash[:line_text].slice(0, warning_hash[:pos] - 1)
          # So now search backwards for the bad character
          start_pos = search_text.rindex('\\' + warning_hash[:args][:ch])
          # Return the default if we couldn't find it
          return result if start_pos.nil?
          # Modify the positional information to cover the unrecognised code, to the end of the token
          result[2] = result[1]
          result[1] = start_pos
        end
        result
      end
      private_class_method :extract_lexer_warning_location
    end
  end
end
