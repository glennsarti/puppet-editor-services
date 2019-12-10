# frozen_string_literal: true

require 'puppet-languageserver/session_state/document_store'
require 'puppet-languageserver/session_state/language_client'
require 'puppet-languageserver/session_state/object_cache'

module PuppetLanguageServer
  class ClientSessionState
    attr_reader :documents

    attr_reader :language_client

    attr_reader :object_cache

    attr_reader :connection_id

    def initialize(message_handler, options = {})
      @documents       = options[:documents].nil? ? PuppetLanguageServer::SessionState::DocumentStore.new : options[:documents]
      @language_client = options[:language_client].nil? ? PuppetLanguageServer::SessionState::LanguageClient.new(message_handler) : options[:language_client]
      @object_cache    = options[:object_cache].nil? ? PuppetLanguageServer::SessionState::ObjectCache.new : options[:object_cache]
      @connection_id   = options[:connection_id].nil? ? message_handler.protocol.connection.id : options[:connection_id]
    end

    # Helper methods to know the state of the object cache
    def default_classes_loaded?
      object_cache.section_in_origin_exist?(:class, :default)
    end

    def default_datatypes_loaded?
      object_cache.section_in_origin_exist?(:datatype, :default)
    end

    def default_functions_loaded?
      object_cache.section_in_origin_exist?(:function, :default)
    end

    def default_types_loaded?
      object_cache.section_in_origin_exist?(:type, :default)
    end

    def static_data_loaded?
      object_cache.origin_exist?(:bolt)
    end

    # Loaders for object cache information
    def load_default_data!(async = true)
      if PuppetLanguageServer.featureflag?('puppetstrings')
        PuppetLanguageServer.log_message(:info, "Loading Default Data via aggregate #{'(Async)' if async}...")
        if async
          sidecar_queue.enqueue('default_aggregate', [], false, connection_id)
        else
          sidecar_queue.execute('default_aggregate', [], false, connection_id)
        end
      else
        # Order is (somewhat) important here. Try to get the most common information first
        [
          { :name => 'Puppet Types', :action => 'default_types' },
          { :name => 'Functions',    :action => 'default_functions' },
          { :name => 'Classes',      :action => 'default_classes' },
          { :name => 'DataTypes',    :action => 'default_datatypes' }
        ].each do |item|
          PuppetLanguageServer.log_message(:info, "Loading #{item[:name]} #{'(Async)' if async}...")
          if async
            sidecar_queue.enqueue(item[:action], [], false, connection_id)
          else
            sidecar_queue.execute(item[:action], [], false, connection_id)
          end
        end
      end
      PuppetLanguageServer.log_message(:info, 'Loading Facter (Async)...')
      PuppetLanguageServer::FacterHelper.load_facts_async
      true
    end

    def load_static_data!(async = true)
      if async
        Thread.new do
          PuppetLanguageServer.log_message(:info, 'Loading static data (Async)...')
          load_static_data_impl
        end
      else
        PuppetLanguageServer.log_message(:info, 'Loading static data...')
        load_static_data_impl
      end
      true
    end

    def load_workspace_data!(async = true)
      return true if documents.store_root_path.nil?
      action_args = ['--local-workspace', documents.store_root_path]
      if PuppetLanguageServer.featureflag?('puppetstrings')
        PuppetLanguageServer.log_message(:info, "Loading Workspace Data via aggregate #{'(Async)' if async}...")
        if async
          sidecar_queue.enqueue('workspace_aggregate', action_args, false, connection_id)
        else
          sidecar_queue.execute('workspace_aggregate', action_args, false, connection_id)
        end
      else
        # Order is (somewhat) important here. Try to get the most common information first
        [
          { :name => 'Puppet Types', :action => 'workspace_types' },
          { :name => 'Functions',    :action => 'workspace_functions' },
          { :name => 'Classes',      :action => 'workspace_classes' },
          { :name => 'DataTypes',    :action => 'workspace_datatypes' }
        ].each do |item|
          PuppetLanguageServer.log_message(:info, "Loading #{item[:name]} #{'(Async)' if async}...")
          if async
            sidecar_queue.enqueue(item[:action], action_args, false, connection_id)
          else
            sidecar_queue.execute(item[:action], action_args, false, connection_id)
          end
        end
      end
    end

    def purge_workspace_data!
      object_cache.remove_origin!(:workspace)
    end

    private

    def sidecar_queue
      PuppetLanguageServer::GlobalQueues.sidecar_queue
    end

    def load_static_data_impl
      bolt_static_data = PuppetLanguageServer::Sidecar::Protocol::AggregateMetadata.new
      Dir.glob(File.join(PuppetLanguageServer.static_data_dir, 'bolt-*.json')) do |path|
        PuppetLanguageServer.log_message(:debug, "Importing static data file #{path}...")
        # No need to catch errors here. As this is static data and is tested in rspec
        # Sure, we could have corrupt/missing files on disk, but then we have bigger issues
        data = PuppetLanguageServer::Sidecar::Protocol::AggregateMetadata.new.from_json!(File.open(path, 'rb:UTF-8') { |f| f.read })
        data.each_list { |_, list| bolt_static_data.concat!(list) }
      end

      object_cache.import_sidecar_list!(bolt_static_data.classes,   :class,    :bolt)
      object_cache.import_sidecar_list!(bolt_static_data.datatypes, :datatype, :bolt)
      object_cache.import_sidecar_list!(bolt_static_data.functions, :function, :bolt)
      object_cache.import_sidecar_list!(bolt_static_data.types,     :type,     :bolt)

      bolt_static_data.each_list do |k, v|
        if v.nil?
          PuppetLanguageServer.log_message(:debug, "Static bolt data returned no #{k}")
        else
          PuppetLanguageServer.log_message(:debug, "Static bolt data returned #{v.count} #{k}")
        end
      end
    end
  end
end
