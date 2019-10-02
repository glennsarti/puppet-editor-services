# frozen_string_literal: true

module PuppetEditorServices
  module Protocol
    class Base
      attr_reader :connection
      attr_reader :handler

      def initialize(connection)
        @connection = connection
        @handler = connection.server.handler_options[:class].new(self)
      end

      # @abstract
      def receive_data(data); end
    end
  end
end
