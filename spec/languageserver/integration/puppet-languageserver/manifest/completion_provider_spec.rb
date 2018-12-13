require 'spec_helper'

def number_of_completion_item_with_type(completion_list, typename)
  (completion_list['items'].select { |item| item['data']['type'] == typename}).length
end

def create_mock_resource(parameters = [], properties = [])
  object = PuppetLanguageServer::PuppetHelper::PuppetType.new
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
  let(:subject) { PuppetLanguageServer::Manifest::CompletionProvider }
  let(:nil_response) { LanguageServer::CompletionList.create_nil_response }

  before(:all) do
    wait_for_puppet_loading
  end

  before(:each) do
    # Prepopulate the Object Cache with workspace objects
    # Classes / Defined Types
    list = PuppetLanguageServer::Sidecar::Protocol::PuppetClassList.new
    obj = random_sidecar_puppet_class
    obj.key = :mock_workspace_class
    list << obj
    PuppetLanguageServer::PuppetHelper.cache.import_sidecar_list!(list, :class, :workspace)
    # Functions
    list = PuppetLanguageServer::Sidecar::Protocol::PuppetFunctionList.new
    list << random_sidecar_puppet_function
    PuppetLanguageServer::PuppetHelper.cache.import_sidecar_list!(list, :function, :workspace)
    # Types
    list = PuppetLanguageServer::Sidecar::Protocol::PuppetTypeList.new
    list << random_sidecar_puppet_type
    PuppetLanguageServer::PuppetHelper.cache.import_sidecar_list!(list, :type, :workspace)
  end

  after(:each) do
    # Clear out the Object Cache of workspace objects
    PuppetLanguageServer::PuppetHelper.cache.import_sidecar_list!([], :class, :workspace)
    PuppetLanguageServer::PuppetHelper.cache.import_sidecar_list!([], :function, :workspace)
    PuppetLanguageServer::PuppetHelper.cache.import_sidecar_list!([], :type, :workspace)
  end

  describe '#complete' do
    describe "Given an incomplete manifest which has syntax errors" do
      it "should raise an error" do
        expect{subject.complete('user { "Bob"', 0, 1)}.to raise_error(RuntimeError)
      end
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
EOT
      }

      describe "When inside the root of the manifest" do
        let(:char_num) { 0 }
        let(:expected_types) { ['keyword','resource_type','function','resource_class'] }

        [0, 8, 9].each do |line_num|
          it "should return a list of keyword, resource_type, function, resource_class regardless of cursor location (Testing line #{line_num})" do
            result = subject.complete(content, line_num, char_num)

            result['items'].each do |item|
              expect(item).to be_completion_item_with_type(expected_types)
            end

            expected_types.each do |typename|
              expect(number_of_completion_item_with_type(result,typename)).to be > 0
            end
          end
        end
      end

      describe "When inside the root of a class" do
        let(:line_num) { 1 }
        let(:char_num) { 0 }
        let(:expected_types) { ['keyword','resource_type','resource_class'] }

        it 'should return a list of keyword, resource_type, resource_class' do
          result = subject.complete(content, line_num, char_num)

          result['items'].each do |item|
            expect(item).to be_completion_item_with_type(expected_types)
          end

          expected_types.each do |typename|
            expect(number_of_completion_item_with_type(result,typename)).to be > 0
          end
        end
      end

      describe "When inside the root of a resource" do
        let(:line_num) { 11 }
        let(:char_num) { 0 }
        let(:expected_types) { ['resource_parameter','resource_property'] }

        it 'should return a list of resource_parameter, resource_property' do
          result = subject.complete(content, line_num, char_num)

          result['items'].each do |item|
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
          result = subject.complete(content_empty, line_num, char_num)

          result['items'].each do |item|
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
          result = subject.complete(content_simple, line_num, char_num)

          result['items'].each do |item|
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
          result = subject.complete(content, line_num, char_num)

          result['items'].each do |item|
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
          result = subject.complete(content, line_num, char_num)

          result['items'].each do |item|
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
          result = subject.complete(content, line_num, char_num)

          result['items'].each do |item|
            expect(item).to be_completion_item_with_type('variable_expr_fact')
          end
        end
      end
    end
  end

  describe '#resolve' do
    it 'should return the original request if it is not understood' do
      resolve_request = {
        'label'  => 'spec-test-label',
        'kind'   => LanguageServer::COMPLETIONITEMKIND_TEXT,
        'detail' => 'spec-test-detail',
        'data'   => { 'type' => 'unknown_type' }
      }

      result = subject.resolve(resolve_request)
      expect(result).to eq(resolve_request)
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
        @completion_response = subject.complete(content, line_num, char_num)
      end

      context 'for a well known fact (operatingsystem)' do
        before(:each) do
          @resolve_request = @completion_response["items"].find do |item|
            item["label"] == 'operatingsystem' && item["kind"] == LanguageServer::COMPLETIONITEMKIND_VARIABLE
          end
          raise RuntimeError, "operatingsystem fact could not be found" if @resolve_request.nil?
        end

        it 'should return the fact value' do
          result = subject.resolve(@resolve_request)
          expect(result['documentation']).to eq(Facter.fact('operatingsystem').value)
        end
      end

      context 'for a fact that does not exist' do
        it 'should return empty string' do
          resolve_request = {
            'label'  => 'spec-test-label',
            'kind'   => LanguageServer::COMPLETIONITEMKIND_TEXT,
            'detail' => 'spec-test-detail',
            'data'   => { 'type' => 'variable_expr_fact', 'expr' => 'I_dont_exist'}
          }

          result = subject.resolve(resolve_request)

          expect(result['documentation']).to eq('')
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
        @completion_response = subject.complete(content, line_num, char_num)
      end

      context 'for an unknown keyword' do
        before(:each) do
          @resolve_request = @completion_response["items"].find do |item|
            item["label"] == 'class' && item["kind"] == LanguageServer::COMPLETIONITEMKIND_KEYWORD
          end
          raise RuntimeError, "The keyword class response could not be found" if @resolve_request.nil?
        end

        it 'should return the original request' do
          @resolve_request['data']['name'] = 'keyword_not_found'
          result = subject.resolve(@resolve_request)
          expect(result).to eq(@resolve_request)
        end
      end

      %w[class define].each do |testcase|
        context "for #{testcase}" do
          before(:each) do
            @resolve_request = @completion_response["items"].find do |item|
              item["label"] == testcase && item["kind"] == LanguageServer::COMPLETIONITEMKIND_KEYWORD
            end
            raise RuntimeError, "A #{testcase} keyword response could not be found" if @resolve_request.nil?
          end

          it 'should return the documentation' do
            result = subject.resolve(@resolve_request)
            expect(result['documentation']).to match(/.+/)
          end

          it 'should return a text snippet' do
            result = subject.resolve(@resolve_request)
            expect(result['insertText']).to match(/.+/)
            expect(result['insertTextFormat']).to eq(LanguageServer::INSERTTEXTFORMAT_SNIPPET)
          end
        end
      end

      %w[application site].each do |testcase|
        context "for #{testcase}" do
          before(:each) do
            @resolve_request = @completion_response["items"].find do |item|
              item["label"] == testcase && item["kind"] == LanguageServer::COMPLETIONITEMKIND_KEYWORD
            end
            raise RuntimeError, "A #{testcase} keyword response could not be found" if @resolve_request.nil?
          end

          it 'should return the documentation' do
            result = subject.resolve(@resolve_request)
            expect(result['documentation']).to match(/.+/)
          end

          it 'should return Orchestrator detail' do
            result = subject.resolve(@resolve_request)
            expect(result['detail']).to eq('Orchestrator')
          end

          it 'should return a text snippet' do
            result = subject.resolve(@resolve_request)
            expect(result['insertText']).to match(/.+/)
            expect(result['insertTextFormat']).to eq(LanguageServer::INSERTTEXTFORMAT_SNIPPET)
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
        @completion_response = subject.complete(content, line_num, char_num)

        @resolve_request = @completion_response["items"].find do |item|
          item["label"] == 'alert' && item["kind"] == LanguageServer::COMPLETIONITEMKIND_FUNCTION
        end
        raise RuntimeError, "alert function could not be found" if @resolve_request.nil?
      end

      context 'for an unknown function' do
        it 'should return the original request' do
          @resolve_request['data']['name'] = 'function_not_found'
          result = subject.resolve(@resolve_request)
          expect(result).to eq(@resolve_request)
        end
      end

      context 'for a well known function (alert)' do
        it 'should return the documentation' do
          result = subject.resolve(@resolve_request)
          expect(result['documentation']).to match(/.+/)
        end

        it 'should return a text snippet' do
          result = subject.resolve(@resolve_request)
          expect(result['insertText']).to match(/.+/)
          expect(result['insertTextFormat']).to eq(LanguageServer::INSERTTEXTFORMAT_SNIPPET)
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
      let(:mock_resource) { create_mock_resource([:param1, :param2], [:prop1, :prop2]) }

      before(:each) do
        # Generate the resolution request based on a completion response
        @completion_response = subject.complete(content, line_num, char_num)

        @resolve_request = @completion_response["items"].find do |item|
          item["label"] == 'user' && item["kind"] == LanguageServer::COMPLETIONITEMKIND_MODULE
        end
        raise RuntimeError, "user type could not be found" if @resolve_request.nil?
      end

      context 'for an unknown puppet type' do
        it 'should return the original request' do
          expect(PuppetLanguageServer::PuppetHelper).to receive(:get_type).and_return(nil)
          result = subject.resolve(@resolve_request)
          expect(result).to eq(@resolve_request)
        end
      end

      context 'for a well known puppet type (user)' do
        it 'should return the documentation' do
          result = subject.resolve(@resolve_request)
          expect(result['documentation']).to match(/.+/)
        end

        it 'should return a text snippet' do
          result = subject.resolve(@resolve_request)
          expect(result['insertText']).to match(/.+/)
          expect(result['insertTextFormat']).to eq(LanguageServer::INSERTTEXTFORMAT_SNIPPET)
        end
      end

      context 'for a non-ensurable puppet type with no required attributes' do
        it 'should not return any parameters or properties in the snippet' do
          expect(PuppetLanguageServer::PuppetHelper).to receive(:get_type).and_return(mock_resource)
          result = subject.resolve(@resolve_request)
          expect(result['insertText']).to_not match(/param1/)
          expect(result['insertText']).to_not match(/param2/)
          expect(result['insertText']).to_not match(/prop1/)
          expect(result['insertText']).to_not match(/prop2/)
          expect(result['insertText']).to_not match(/ensure/)
          expect(result['insertTextFormat']).to eq(LanguageServer::INSERTTEXTFORMAT_SNIPPET)
        end
      end

      context 'for an ensurable puppet type with no required attributes' do
        it 'should only return the ensure property' do
          mock_resource.attributes[:ensure] = create_ensurable_property
          expect(PuppetLanguageServer::PuppetHelper).to receive(:get_type).and_return(mock_resource)

          result = subject.resolve(@resolve_request)
          expect(result['insertText']).to_not match(/param1/)
          expect(result['insertText']).to_not match(/param2/)
          expect(result['insertText']).to_not match(/prop1/)
          expect(result['insertText']).to_not match(/prop2/)
          expect(result['insertText']).to match(/ensure/)
          expect(result['insertTextFormat']).to eq(LanguageServer::INSERTTEXTFORMAT_SNIPPET)
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

          result = subject.resolve(@resolve_request)
          expect(result['insertText']).to_not match(/param1/)
          expect(result['insertText']).to match(/param2/)
          expect(result['insertText']).to match(/prop1/)
          expect(result['insertText']).to_not match(/prop2/)
          expect(result['insertText']).to match(/ensure/)
          expect(result['insertTextFormat']).to eq(LanguageServer::INSERTTEXTFORMAT_SNIPPET)
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
        @completion_response = subject.complete(content, line_num, char_num)
        @resolve_request = @completion_response["items"].find do |item|
          item["label"] == 'name' && item["kind"] == LanguageServer::COMPLETIONITEMKIND_PROPERTY
        end
        raise RuntimeError, "name parameter could not be found" if @resolve_request.nil?
      end

      context 'for an unknown type' do
        it 'should return the original request' do
          @resolve_request['data']['resource_type'] = 'resource_not_found'
          result = subject.resolve(@resolve_request)
          expect(result).to eq(@resolve_request)
        end
      end

      context 'for an unknown parameter' do
        it 'should return the original request' do
          @resolve_request['data']['param'] = 'param_not_found'
          result = subject.resolve(@resolve_request)
          expect(result).to eq(@resolve_request)
        end
      end

      context 'for the name parameter of a well known puppet type (user)' do
        it 'should return the documentation' do
          result = subject.resolve(@resolve_request)
          expect(result['documentation']).to match(/.+/)
        end

        it 'should return a text literal with the parameter defintion' do
          result = subject.resolve(@resolve_request)
          expect(result['insertText']).to match(/.+ => /)
          expect(result['insertTextFormat']).to be_nil
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
        @completion_response = subject.complete(content, line_num, char_num)
        @resolve_request = @completion_response["items"].find do |item|
          item["label"] == 'ensure' && item["kind"] == LanguageServer::COMPLETIONITEMKIND_PROPERTY
        end
        raise RuntimeError, "ensure property could not be found" if @resolve_request.nil?
      end

      context 'for an unknown type' do
        it 'should return the original request' do
          @resolve_request['data']['resource_type'] = 'resource_not_found'
          result = subject.resolve(@resolve_request)
          expect(result).to eq(@resolve_request)
        end
      end

      context 'for an unknown property' do
        it 'should return the original request' do
          @resolve_request['data']['prop'] = 'prop_not_found'
          result = subject.resolve(@resolve_request)
          expect(result).to eq(@resolve_request)
        end
      end

      context 'for the ensure property of a well known puppet type (user)' do
        it 'should return the documentation' do
          result = subject.resolve(@resolve_request)
          expect(result['documentation']).to match(/.+/)
        end

        it 'should return a text literal with the property defintion' do
          result = subject.resolve(@resolve_request)
          expect(result['insertText']).to match(/.+ => /)
          expect(result['insertTextFormat']).to be_nil
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
        @completion_response = subject.complete(content, line_num, char_num)
        @resolve_request = @completion_response["items"].find do |item|
          item["label"] == 'mock_workspace_class' && item["kind"] == LanguageServer::COMPLETIONITEMKIND_MODULE
        end
        raise RuntimeError, "mock_workspace_class class could not be found" if @resolve_request.nil?
      end

      context 'for an unknown class' do
        it 'should return the original request' do
          @resolve_request['data']['name'] = 'class_not_found'
          result = subject.resolve(@resolve_request)
          expect(result).to eq(@resolve_request)
        end
      end

      context 'for a known class' do
        it 'should return a text snippet' do
          result = subject.resolve(@resolve_request)
          expect(result['insertText']).to match(/.+/)
          expect(result['insertTextFormat']).to eq(LanguageServer::INSERTTEXTFORMAT_SNIPPET)
        end
      end
    end
  end
end
