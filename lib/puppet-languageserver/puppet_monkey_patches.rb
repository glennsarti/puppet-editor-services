# frozen_string_literal: true

# Monkey Patch the Puppet language parser so we can globally lock any changes to the
# global setting Puppet[:tasks].  We need to manage this so we can switch between
# parsing modes.  Unfortunately we can't do this as method parameter, only via the
# global Puppet settings which is not thread safe
$PuppetParserMutex = Mutex.new # rubocop:disable Style/GlobalVars
module Puppet
  module Pops
    module Parser
      class Parser
        def singleton_parse_string(code, task_mode = false, path = nil)
          $PuppetParserMutex.synchronize do # rubocop:disable Style/GlobalVars
            begin
              original_taskmode = Puppet[:tasks] if Puppet.tasks_supported?
              Puppet[:tasks] = task_mode if Puppet.tasks_supported?
              return parse_string(code, path)
            ensure
              Puppet[:tasks] = original_taskmode if Puppet.tasks_supported?
            end
          end
        end
      end
    end
  end
end

module Puppet
  # Tasks first appeared in Puppet 5.4.0
  def self.tasks_supported?
    Gem::Version.new(Puppet.version) >= Gem::Version.new('5.4.0')
  end
end

module Puppet::Pops
  module Parser
    class Lexer2
      attr_reader :lexer_warnings

      def line_text(pos = nil)
        line_num = line(pos)
        return nil if line_num.nil?
        line_num -= 1 # line(pos) returns base 1 line numbers, but line_index is a base 0 array so need to subtract one
        line_start_offset = @locator.line_index[line_num]
        # Get the offset of the next (if possible), otherwise just read to the end of the document
        line_end_offset = line_num >= @locator.line_index.count ? @locator.string.count - 1 : @locator.line_index[line_num + 1]
        # This may or may not be a good thing. e.g. Does it handle double-byte characters?
        @locator.string.slice(line_start_offset, line_end_offset - line_start_offset)
      end

      alias_method :original_lex_warning, :lex_warning
      def lex_warning(issue, args = {}, pos=nil)
        @lexer_warnings = [] if @lexer_warnings.nil?

        @lexer_warnings << {
          message: issue.format(args),
          line: line(pos),
          pos: position(pos),
          line_text: line_text(pos),
          issue: issue.dup,
          args: args.dup
        }
        original_lex_warning(issue, args, pos)
      end

    end
  end
end

# MUST BE LAST!!!!!!
# Suppress any warning messages to STDOUT.  It can pollute stdout when running in STDIO mode
Puppet::Util::Log.newdesttype :null_logger do
  def handle(msg)
    #require 'pry'; binding.pry
    PuppetLanguageServer.log_message(:debug, "[PUPPET LOG] [#{msg.level}] #{msg.message}")
  end
end
