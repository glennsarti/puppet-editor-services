require 'spec_helper'

def number_of_completion_item_with_type(completion_list, typename)
  (completion_list.items.select { |item| item.data['type'] == typename}).length
end

def completion_item_with_type_and_name(completion_list, typename, name)
  completion_list.items.find { |item| item.data['type'] == typename && item.data['name'] == name }
end

def retrieve_completion_response(label, kind)
  value = @completion_response.items.find do |item|
    item.label == label && item.kind == kind
  end
  raise RuntimeError, "#{label} could not be found" if value.nil?
  value
end

RSpec::Matchers.define :be_completion_item_with_type do |value|
  value = [value] unless value.is_a?(Array)

  match { |actual| value.include?(actual.data['type']) }

  description do
    "be a Completion Item with a data type in the list of #{value}"
  end
end

RSpec::Matchers.define :include_completion_item do |item_kind, item_name|
  match do |actual|
    actual.items.any? { |item| item.kind == item_kind && item.label == item_name.to_s }
  end

  description do
    "include a Completion Item with kind #{item_kind} and name #{item_name}"
  end
end

# Custom matcher which compares JSON
# representation of objects
RSpec::Matchers.define :be_json_like do |expected|
  match do |actual|
    actual.to_json == expected.to_json
  end

  failure_message do |actual|
    "expected that #{actual.to_json} would be #{expected.to_json}"
  end
end

def create_mock_type(parameters = [], properties = [])
  object = PuppetLanguageServer::Sidecar::Protocol::PuppetType.new
  object.doc = 'mock documentation'
  object.attributes = {}
  parameters.each { |name| object.attributes[name] = {
    :type        => :param,
    :doc         => 'mock parameter doc',
    :required? => nil,
    :isnamevar?  => nil
  }}
  properties.each { |name| object.attributes[name] = {
    :type        => :property,
    :doc         => 'mock parameter doc',
    :required? => nil,
    :isnamevar?  => nil
  }}

  object
end

def create_ensurable_property
  {
    :type        => :property,
    :doc         => 'mock ensure doc',
    :required? => nil,
    :isnamevar?  => nil
  }
end

describe 'completion_provider' do
  let(:session_state) { PuppetLanguageServer::ClientSessionState.new(nil, :connection_id => 'mock') }
  let(:subject) { PuppetLanguageServer::Manifest::CompletionProvider }
  let(:completion_options) { {} }

  let(:no_param_function) do
    object = random_sidecar_puppet_function(:no_param_function)
    object.function_version = 4

    sig = PuppetLanguageServer::Sidecar::Protocol::PuppetFunctionSignature.new
    sig.key = object.key.to_s
    sig.doc = object.doc
    sig.return_types = ['Any']
    sig.parameters = []

    object.signatures = [sig]
    object
  end

  let(:one_param_function) do
    object = random_sidecar_puppet_function(:one_param_function)
    object.function_version = 4

    sig = PuppetLanguageServer::Sidecar::Protocol::PuppetFunctionSignature.new
    sig.key = object.key.to_s
    sig.doc = object.doc
    sig.return_types = ['Any']
    sig.parameters = [random_sidecar_puppet_function_signature_parameter]

    object.signatures = [sig]
    object
  end

  let(:spec_data_type_attr_name) { 'attr1' }
  let(:spec_data_type) do
    object = random_sidecar_puppet_datatype
    object.key = :SpecDataType
    object.is_type_alias = false
    object.alias_of = nil

    type_attr = random_sidecar_puppet_datatype_attribute
    type_attr.key = spec_data_type_attr_name

    object.attributes = [type_attr]
    object
  end

  before(:each) do
    populate_cache(session_state.object_cache)
    # Prepopulate the Object Cache with workspace objects
    # Classes / Defined Types
    list = PuppetLanguageServer::Sidecar::Protocol::PuppetClassList.new
    obj = random_sidecar_puppet_class
    obj.key = :mock_workspace_class
    list << obj
    session_state.object_cache.import_sidecar_list!(list, :class, :workspace)
    # Functions
    list = PuppetLanguageServer::Sidecar::Protocol::PuppetFunctionList.new
    list << no_param_function
    list << one_param_function
    session_state.object_cache.import_sidecar_list!(list, :function, :workspace)
    # DataTypes
    list = PuppetLanguageServer::Sidecar::Protocol::PuppetDataTypeList.new
    list << spec_data_type
    session_state.object_cache.import_sidecar_list!(list, :datatype, :workspace)
  end

  describe '#complete' do
    describe "Given an incomplete manifest which has syntax errors" do
      it "should raise an error" do
        expect{subject.complete(session_state, 'user { "Bob"', 0, 1, completion_options)}.to raise_error(RuntimeError)
      end
    end

    context 'Given a Puppet Plan', :if => Puppet.tasks_supported? do
      let(:completion_options) { { tasks_mode: true } }
      let(:content) { <<-EOT
        plan mymodule::my_plan(
        ) {
          # Needed
        }
        EOT
      }

      it "should not raise an error" do
        result = subject.complete(session_state, content, 0, 1, completion_options)
      end

      it 'should suggest Bolt functions' do
        expect(session_state.static_data_loaded?).to eq(true)
        result = subject.complete(session_state, content, 2, 1, completion_options)

        expect(completion_item_with_type_and_name(result, 'function', 'run_task')).to_not be_nil
      end
    end

    context "When completing after typing a period" do
      subject(:result) { PuppetLanguageServer::Manifest::CompletionProvider.complete(session_state, content, line_num, char_num, completion_options) }

      let(:completion_options) do
        {
          context: LSP::CompletionContext.new({ 'triggerKind' => LSP::CompletionTriggerKind::TRIGGERCHARACTER, 'triggerCharacter' => '.' })
        }
      end

      context 'with a manifest using a core datatype with no attributes or functions' do
        let(:content) do
<<-EOT
$var1 = [1, 2, 3]

$var1.

EOT
        end
        let(:line_num) { 2 }
        let(:char_num) { 6 }
        let(:expected_types) { ['function'] }
        let(:unexpected_types) { ['keyword', 'resource_type', 'resource_class'] }

        it 'should only return functions' do
          result.items.each do |item|
            expect(item).to be_completion_item_with_type(expected_types)
          end

          expected_types.each do |typename|
            expect(number_of_completion_item_with_type(result, typename)).to be > 0
          end

          result.items.each do |item|
            expect(item).to_not be_completion_item_with_type(unexpected_types)
          end
        end

        it 'should return functions with at least one parameter' do
          expect(result).to include_completion_item(LSP::CompletionItemKind::FUNCTION, one_param_function.key)

          expect(result).to_not include_completion_item(LSP::CompletionItemKind::FUNCTION, no_param_function.key)
        end
      end

      context 'with a manifest using a custom datatype with attributes and functions' do
        let(:content) do
<<-EOT
$var1 = SpecDataType[0]

$var1.

EOT
        end
        let(:line_num) { 2 }
        let(:char_num) { 6 }
        let(:expected_types) { ['function', 'datatype_attribute'] }
        let(:unexpected_types) { ['keyword', 'resource_type', 'resource_class'] }

        it 'should only return functions and datatype_attributes' do
          result.items.each do |item|
            expect(item).to be_completion_item_with_type(expected_types)
          end

          expect(result).to include_completion_item(LSP::CompletionItemKind::PROPERTY, spec_data_type_attr_name)
        end

        # TODO: Test for data type functions
      end

      RSpec.shared_examples 'a function only completer' do |name, manifest|
        context "after a #{name}" do
          let(:content) { '$var = ' + manifest + '.' }
          let(:line_num) { 0 }
          let(:char_num) { 8 + manifest.length }
          let(:expected_types) { ['function'] }

          it 'should only return functions' do
            #require 'pry'; binding.pry
            result.items.each do |item|
              expect(item).to be_completion_item_with_type(expected_types)
            end
          end
        end
      end

      include_examples 'a function only completer', 'literal hash', '{ "1" => "2" }'
      include_examples 'a function only completer', 'literal array', '[1, 2, 3]'
      include_examples 'a function only completer', 'number', '1'
      include_examples 'a function only completer', 'string', '"abc123"'
      include_examples 'a function only completer', 'lambda', '[1,2,3].map |$items| { $items * 10 }'
      include_examples 'a function only completer', 'class', 'class test () {}'
      include_examples 'a function only completer', 'core type', 'Integer[0]'
    end

    context "Given a simple valid manifest" do
      let(:content) { <<-EOT

class Alice {

  user { 'Bob':
    ensure => 'present',
    name   => 'name',
  }
}

  # Needed

user { 'Charlie':

  ensure => 'present',
  name   => 'name',
}

define delta (
) {

}
EOT
      }

      describe "When inside the root of the manifest" do
        let(:char_num) { 0 }
        let(:expected_types) { ['keyword','resource_type','function','resource_class'] }

        [0, 9].each do |line_num|
          it "should return a list of keyword, resource_type, function, resource_class regardless of cursor location (Testing line #{line_num})" do
            result = subject.complete(session_state, content, line_num, char_num, completion_options)

            result.items.each do |item|
              expect(item).to be_completion_item_with_type(expected_types)
            end

            expected_types.each do |typename|
              expect(number_of_completion_item_with_type(result, typename)).to be > 0
            end
          end
        end
      end

      [
        { :name => 'class', :line_num => 1 },
        { :name => 'defined type', :line_num => 19 },
      ].each do |testcase|
        describe "When inside the root of a #{testcase[:name]}" do
          let(:char_num) { 0 }
          let(:expected_types) { ['keyword','resource_type','resource_class'] }

          it 'should return a list of keyword, resource_type, resource_class' do
            result = subject.complete(session_state, content, testcase[:line_num], char_num, completion_options)

            result.items.each do |item|
              expect(item).to be_completion_item_with_type(expected_types)
            end

            expected_types.each do |typename|
              expect(number_of_completion_item_with_type(result,typename)).to be > 0
            end
          end
        end
      end

      describe "When inside the root of a resource" do
        let(:line_num) { 12 }
        let(:char_num) { 0 }
        let(:expected_types) { ['resource_parameter','resource_property'] }

        it 'should return a list of resource_parameter, resource_property' do
          result = subject.complete(session_state, content, line_num, char_num, completion_options)

          result.items.each do |item|
            expect(item).to be_completion_item_with_type(expected_types)
          end

          expected_types.each do |typename|
            expect(number_of_completion_item_with_type(result,typename)).to be > 0
          end
        end
      end
    end

    context "Given a simple manifest mid-typing" do
      let(:content_empty) { <<-EOT
c
EOT
      }

      let(:content_simple) { <<-EOT
user { 'Charlie':

  ensure => 'present',
  name   => 'name',
}

r
EOT
      }

      describe "When typing inside the root of an empty manifest" do
        let(:line_num) { 0 }
        let(:char_num) { 1 }
        let(:expected_types) { ['keyword','resource_type','function','resource_class'] }

        it "should return a list of keyword, resource_type, function, resource_class" do
          result = subject.complete(session_state, content_empty, line_num, char_num, completion_options)

          result.items.each do |item|
            expect(item).to be_completion_item_with_type(expected_types)
          end

          expected_types.each do |typename|
            expect(number_of_completion_item_with_type(result,typename)).to be > 0
          end
        end
      end

      describe "When typing inside the root of a non-empty manifest" do
        let(:line_num) { 6 }
        let(:char_num) { 1 }
        let(:expected_types) { ['keyword','resource_type','function','resource_class'] }

        it "should return a list of keyword, resource_type, function, resource_class" do
          result = subject.complete(session_state, content_simple, line_num, char_num, completion_options)

          result.items.each do |item|
            expect(item).to be_completion_item_with_type(expected_types)
          end

          expected_types.each do |typename|
            expect(number_of_completion_item_with_type(result,typename)).to be > 0
          end
        end
      end
    end

    context '$facts variable' do
      describe "With newlines at the beginning of the document and inside the brackets of $facts" do
        let(:content) { <<-EOT

# Newlines are need above to test if parsing is ok.
$test1 = $::operatingsystem
$test2 = $operatingsystem
$test3 = $facts[]
EOT
        }
        let(:line_num) { 4 }
        let(:char_num) { 16 }

        it 'should return a list of facts' do
          result = subject.complete(session_state, content, line_num, char_num, completion_options)

          result.items.each do |item|
            expect(item).to be_completion_item_with_type('variable_expr_fact')
          end
        end
      end

      describe "When inside the brackets of $facts" do
        let(:content) { <<-EOT
$test1 = $::operatingsystem
$test2 = $operatingsystem
$test3 = $facts[]
EOT
        }
        let(:line_num) { 2 }
        let(:char_num) { 16 }

        it 'should return a list of facts' do
          result = subject.complete(session_state, content, line_num, char_num, completion_options)

          result.items.each do |item|
            expect(item).to be_completion_item_with_type('variable_expr_fact')
          end
        end
      end

      describe "When inside the start brackets of $facts" do
        let(:content) { <<-EOT
$test1 = $::operatingsystem
$test2 = $operatingsystem
$test3 = $facts[
EOT
        }
        let(:line_num) { 2 }
        let(:char_num) { 16 }

        it 'should return a list of facts' do
          result = subject.complete(session_state, content, line_num, char_num, completion_options)

          result.items.each do |item|
            expect(item).to be_completion_item_with_type('variable_expr_fact')
          end
        end
      end
    end
  end

  describe '#resolve' do
    it 'should return the original request if it is not understood' do
      resolve_request = LSP::CompletionItem.new(
        'label'  => 'spec-test-label',
        'kind'   => LSP::CompletionItemKind::TEXT,
        'detail' => 'spec-test-detail',
        'data'   => { 'type' => 'unknown_type' }
      )

      result = subject.resolve(session_state, resolve_request)
      expect(result).to be_json_like(resolve_request)
    end

    context 'when resolving a variable_expr_fact request' do
      let(:content) { <<-EOT
  $test = $facts[
EOT
      }
      let(:line_num) { 0 }
      let(:char_num) { 17 }

      before(:each) do
        # Generate the resolution request based on a completion response
        @completion_response = subject.complete(session_state, content, line_num, char_num)
      end

      context 'for a well known fact (operatingsystem)' do
        before(:each) do
          @resolve_request = retrieve_completion_response('operatingsystem', LSP::CompletionItemKind::VARIABLE)
        end

        it 'should return the fact value' do
          result = subject.resolve(session_state, @resolve_request)
          expect(result.documentation).to eq('windows')
        end
      end

      context 'for a fact that does not exist' do
        it 'should return empty string' do
          resolve_request = LSP::CompletionItem.new(
            'label'  => 'spec-test-label',
            'kind'   => LSP::CompletionItemKind::TEXT,
            'detail' => 'spec-test-detail',
            'data'   => { 'type' => 'variable_expr_fact', 'expr' => 'I_dont_exist'}
          )

          result = subject.resolve(session_state, resolve_request)

          expect(result.documentation).to eq('')
        end
      end
    end

    context 'when resolving a keyword request' do
      let(:content) { <<-EOT
        class Alice {
        }
      EOT
      }
      let(:line_num) { 0 }
      let(:char_num) { 0 }

      before(:each) do
        # Generate the resolution request based on a completion response
        @completion_response = subject.complete(session_state, content, line_num, char_num)
      end

      context 'for an unknown keyword' do
        before(:each) do
          @resolve_request = retrieve_completion_response('class', LSP::CompletionItemKind::KEYWORD)
        end

        it 'should return the original request' do
          @resolve_request.data['name'] = 'keyword_not_found'
          result = subject.resolve(session_state, @resolve_request)
          expect(result).to be_json_like(@resolve_request)
        end
      end

      %w[class define].each do |testcase|
        context "for #{testcase}" do
          before(:each) do
            @resolve_request = retrieve_completion_response(testcase, LSP::CompletionItemKind::KEYWORD)
          end

          it 'should return the documentation' do
            result = subject.resolve(session_state, @resolve_request)
            expect(result.documentation).to match(/.+/)
          end

          it 'should return a text snippet' do
            result = subject.resolve(session_state, @resolve_request)
            expect(result.insertText).to match(/.+/)
            expect(result.insertTextFormat).to eq(LSP::InsertTextFormat::SNIPPET)
          end
        end
      end

      %w[application site].each do |testcase|
        context "for #{testcase}" do
          before(:each) do
            @resolve_request = retrieve_completion_response(testcase, LSP::CompletionItemKind::KEYWORD)
          end

          it 'should return the documentation' do
            result = subject.resolve(session_state, @resolve_request)
            expect(result.documentation).to match(/.+/)
          end

          it 'should return Orchestrator detail' do
            result = subject.resolve(session_state, @resolve_request)
            expect(result.detail).to eq('Orchestrator')
          end

          it 'should return a text snippet' do
            result = subject.resolve(session_state, @resolve_request)
            expect(result.insertText).to match(/.+/)
            expect(result.insertTextFormat).to eq(LSP::InsertTextFormat::SNIPPET)
          end
        end
      end
    end

    context 'when resolving a function request' do
      let(:content) { <<-EOT
        class Alice {
        }
      EOT
      }
      let(:line_num) { 0 }
      let(:char_num) { 0 }

      before(:each) do
        # Generate the resolution request based on a completion response
        @completion_response = subject.complete(session_state, content, line_num, char_num)
        @resolve_request = retrieve_completion_response('alert', LSP::CompletionItemKind::FUNCTION)
      end

      context 'for an unknown function' do
        it 'should return the original request' do
          @resolve_request.data['name'] = 'function_not_found'
          result = subject.resolve(session_state, @resolve_request)
          expect(result).to be_json_like(@resolve_request)
        end
      end

      context 'for a well known function (alert)' do
        it 'should return the documentation' do
          result = subject.resolve(session_state, @resolve_request)
          expect(result.documentation).to match(/.+/)
        end

        it 'should return plain text' do
          result = subject.resolve(session_state, @resolve_request)
          expect(result.insertText).to match(/.+/)
          expect(result.insertTextFormat).to eq(LSP::InsertTextFormat::PLAINTEXT)
        end
      end

      context 'for a Bolt function (run_task)' do
        it 'should return the documentation' do
          @resolve_request.data['name'] = 'run_task'
          result = subject.resolve(session_state, @resolve_request)
          expect(result.documentation).to match(/.+/)
        end

        it 'should return plain text' do
          @resolve_request.data['name'] = 'run_task'
          result = subject.resolve(session_state, @resolve_request)
          expect(result.insertText).to match(/.+/)
          expect(result.insertTextFormat).to eq(LSP::InsertTextFormat::PLAINTEXT)
        end
      end
    end

    context 'when resolving a resource_type request' do
      let(:content) { <<-EOT
        class Alice {
        }
      EOT
      }
      let(:line_num) { 0 }
      let(:char_num) { 0 }
      let(:mock_resource) { create_mock_type([:param1, :param2], [:prop1, :prop2]) }

      before(:each) do
        # Generate the resolution request based on a completion response
        @completion_response = subject.complete(session_state, content, line_num, char_num)
        @resolve_request = retrieve_completion_response('user', LSP::CompletionItemKind::MODULE)
      end

      context 'for an unknown puppet type' do
        it 'should return the original request' do
          expect(PuppetLanguageServer::PuppetHelper).to receive(:get_type).and_return(nil)
          result = subject.resolve(session_state, @resolve_request)
          expect(result).to be_json_like(@resolve_request)
        end
      end

      context 'for a well known puppet type (user)' do
        it 'should return the documentation' do
          result = subject.resolve(session_state, @resolve_request)
          expect(result.documentation).to match(/.+/)
        end

        it 'should return a text snippet' do
          result = subject.resolve(session_state, @resolve_request)
          expect(result.insertText).to match(/.+/)
          expect(result.insertTextFormat).to eq(LSP::InsertTextFormat::SNIPPET)
        end
      end

      context 'for a non-ensurable puppet type with no required attributes' do
        it 'should not return any parameters or properties in the snippet' do
          expect(PuppetLanguageServer::PuppetHelper).to receive(:get_type).and_return(mock_resource)
          result = subject.resolve(session_state, @resolve_request)
          expect(result.insertText).to_not match(/param1/)
          expect(result.insertText).to_not match(/param2/)
          expect(result.insertText).to_not match(/prop1/)
          expect(result.insertText).to_not match(/prop2/)
          expect(result.insertText).to_not match(/ensure/)
          expect(result.insertTextFormat).to eq(LSP::InsertTextFormat::SNIPPET)
        end
      end

      context 'for an ensurable puppet type with no required attributes' do
        it 'should only return the ensure property' do
          mock_resource.attributes[:ensure] = create_ensurable_property
          expect(PuppetLanguageServer::PuppetHelper).to receive(:get_type).and_return(mock_resource)

          result = subject.resolve(session_state, @resolve_request)
          expect(result.insertText).to_not match(/param1/)
          expect(result.insertText).to_not match(/param2/)
          expect(result.insertText).to_not match(/prop1/)
          expect(result.insertText).to_not match(/prop2/)
          expect(result.insertText).to match(/ensure/)
          expect(result.insertTextFormat).to eq(LSP::InsertTextFormat::SNIPPET)
        end
      end

      context 'for an ensurable puppet type with required attributes, and namevars' do
        it 'should only the ensure property' do
          mock_resource.attributes[:ensure] = create_ensurable_property
          mock_resource.attributes[:param1][:required?] = true
          mock_resource.attributes[:param1][:isnamevar?] = true
          mock_resource.attributes[:param2][:required?] = true
          mock_resource.attributes[:prop1][:required?] = true
          mock_resource.attributes[:prop2][:required?] = true
          mock_resource.attributes[:prop2][:isnamevar?] = true
          expect(PuppetLanguageServer::PuppetHelper).to receive(:get_type).and_return(mock_resource)

          result = subject.resolve(session_state, @resolve_request)
          expect(result.insertText).to_not match(/param1/)
          expect(result.insertText).to match(/param2/)
          expect(result.insertText).to match(/prop1/)
          expect(result.insertText).to_not match(/prop2/)
          expect(result.insertText).to match(/ensure/)
          expect(result.insertTextFormat).to eq(LSP::InsertTextFormat::SNIPPET)
        end
      end
    end

    context 'when resolving a resource_parameter request' do
      let(:content) { <<-EOT
        user { 'Alice':

        }
      EOT
      }
      let(:line_num) { 1 }
      let(:char_num) { 0 }

      before(:each) do
        # Generate the resolution request based on a completion response
        @completion_response = subject.complete(session_state, content, line_num, char_num)
        @resolve_request = retrieve_completion_response('name', LSP::CompletionItemKind::PROPERTY)
      end

      context 'for an unknown type' do
        it 'should return the original request' do
          @resolve_request.data['resource_type'] = 'resource_not_found'
          result = subject.resolve(session_state, @resolve_request)
          expect(result).to be_json_like(@resolve_request)
        end
      end

      context 'for an unknown parameter' do
        it 'should return the original request' do
          @resolve_request.data['param'] = 'param_not_found'
          result = subject.resolve(session_state, @resolve_request)
          expect(result).to be_json_like(@resolve_request)
        end
      end

      context 'for the name parameter of a well known puppet type (user)' do
        it 'should return the documentation' do
          result = subject.resolve(session_state, @resolve_request)
          expect(result.documentation).to match(/.+/)
        end

        it 'should return a text literal with the parameter defintion' do
          result = subject.resolve(session_state, @resolve_request)
          expect(result.insertText).to match(/.+ => /)
          expect(result.insertTextFormat).to be_nil
        end
      end
    end

    context 'when resolving a resource_property request' do
      let(:content) { <<-EOT
        user { 'Alice':

        }
      EOT
      }
      let(:line_num) { 1 }
      let(:char_num) { 0 }

      before(:each) do
        # Generate the resolution request based on a completion response
        @completion_response = subject.complete(session_state, content, line_num, char_num)
        @resolve_request = retrieve_completion_response('ensure', LSP::CompletionItemKind::PROPERTY)
      end

      context 'for an unknown type' do
        it 'should return the original request' do
          @resolve_request.data['resource_type'] = 'resource_not_found'
          result = subject.resolve(session_state, @resolve_request)
          expect(result).to be_json_like(@resolve_request)
        end
      end

      context 'for an unknown property' do
        it 'should return the original request' do
          @resolve_request.data['prop'] = 'prop_not_found'
          result = subject.resolve(session_state, @resolve_request)
          expect(result).to be_json_like(@resolve_request)
        end
      end

      context 'for the ensure property of a well known puppet type (user)' do
        it 'should return the documentation' do
          result = subject.resolve(session_state, @resolve_request)
          expect(result.documentation).to match(/.+/)
        end

        it 'should return a text literal with the property defintion' do
          result = subject.resolve(session_state, @resolve_request)
          expect(result.insertText).to match(/.+ => /)
          expect(result.insertTextFormat).to be_nil
        end
      end
    end

    context 'when resolving a resource_class request' do
      let(:content) { <<-EOT
        user { 'Alice':

        }
      EOT
      }
      let(:line_num) { 0 }
      let(:char_num) { 0 }

      before(:each) do
        # Generate the resolution request based on a completion response
        @completion_response = subject.complete(session_state, content, line_num, char_num)
        @resolve_request = @completion_response.items.find do |item|
          item.label == 'mock_workspace_class' && item.kind == LSP::CompletionItemKind::MODULE
        end
        raise RuntimeError, "mock_workspace_class class could not be found" if @resolve_request.nil?
      end

      context 'for an unknown class' do
        it 'should return the original request' do
          @resolve_request.data['name'] = 'class_not_found'
          result = subject.resolve(session_state, @resolve_request)
          expect(result).to be_json_like(@resolve_request)
        end
      end

      context 'for a known class' do
        it 'should return a text snippet' do
          result = subject.resolve(session_state, @resolve_request)
          expect(result.insertText).to match(/.+/)
          expect(result.insertTextFormat).to eq(LSP::InsertTextFormat::SNIPPET)
        end
      end
    end

    context 'when resolving a datatype_attribute request' do
      let(:content) { "$var1 = SpecDataType[0]\n$var1." }
      let(:completion_options) do
        {
          context: LSP::CompletionContext.new({ 'triggerKind' => LSP::CompletionTriggerKind::TRIGGERCHARACTER, 'triggerCharacter' => '.' })
        }
      end
      let(:line_num) { 1 }
      let(:char_num) { 6 }

      before(:each) do
        # Generate the resolution request based on a completion response
        completion_response = subject.complete(session_state, content, line_num, char_num, completion_options)
        @resolve_request = completion_response.items.find do |item|
          item.label == spec_data_type_attr_name && item.kind == LSP::CompletionItemKind::PROPERTY
        end
        raise RuntimeError, "#{spec_data_type_attr_name} property could not be found" if @resolve_request.nil?
      end

      context 'for an unknown attribute' do
        it 'should return the original request' do
          @resolve_request.data['datatype'] = 'type_not_found'
          result = subject.resolve(session_state, @resolve_request)
          expect(result).to be_json_like(@resolve_request)
        end
      end

      context 'for a known attribute' do
        it 'should return a text literal with the attribute defintion' do
          result = subject.resolve(session_state, @resolve_request)
          expect(result.insertText).to match(/.+/)
          expect(result.insertTextFormat).to be_nil
        end

        it 'should add the return type and documentation' do
          result = subject.resolve(session_state, @resolve_request)
          expect(result.documentation).to match(/#{spec_data_type.attributes[0].types}/)
          expect(result.documentation).to match(/#{spec_data_type.attributes[0].doc}/)
          expect(result.insertTextFormat).to be_nil
        end
      end
    end
  end
end
