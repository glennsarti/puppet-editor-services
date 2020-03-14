require 'spec_helper'
require 'puppet-languageserver/manifest/manifest_inferencer'

# Monkey Patch
require 'puppet/pal/pal_api'
require 'puppet/pal/compiler'
class Puppet::Pal::Compiler
  def public_top_scope
    topscope
  end
end
# End Monkey patch

RSpec::Matchers.define :be_document_symbol do |name, kind, start_line, start_char, end_line, end_char|
  match do |actual|
    actual.name == name &&
    actual.kind == kind &&
    actual.range.start.line == start_line &&
    actual.range.start.character == start_char &&
    actual.range.end.line == end_line &&
    actual.range.end.character == end_char
  end

  failure_message do |actual|
    "expected that symbol called '#{actual['name']}' of type '#{actual['kind']}' located at " +
      "(#{actual['range']['start']['line']}, #{actual['range']['start']['character']}, " +
      "#{actual['range']['end']['line']}, #{actual['range']['end']['character']}) would be " +
      "a document symbol called '#{name}', of type '#{kind}' located at (#{start_line}, #{start_char}, #{end_line}, #{end_char})"
  end

  description do
    "be a document symbol called '#{name}' of type #{kind} located at #{start_line}, #{start_char}, #{end_line}, #{end_char}"
  end
end

RSpec::Matchers.define :be_an_inference do |name = nil, klass = nil|
  match do |actual|
    return false unless actual.is_a?(PuppetLanguageServer::Manifest::ManifestInferencer::BaseInference)

    (name.nil? || actual.name == name) && (klass.nil? || actual.is_a?(klass))
  end

  failure_message do |actual|
    message_name = name.nil? ? 'anything' : "'#{name}'"
    "expected that #{actual.nil? ? 'nil' : actual.to_s} would be an inference called #{message_name}, of type #{klass.nil? ? PuppetLanguageServer::Manifest::ManifestInferencer::BaseInference : klass}"
  end

  description do
    "be an inference called '#{name}' of type #{klass}"
  end
end

RSpec::Matchers.define :be_inference_type do |puppet_typename|
  match do |actual|
    return false unless actual.respond_to?(:puppet_type)
    return false unless actual.puppet_type.is_a?(Puppet::Pops::Types::PuppetObject)

    actual.puppet_type.to_s == puppet_typename
  end

  failure_message do |actual|
    if !actual.respond_to?(:puppet_type)
      "expected that #{actual} would respond to 'puppet_type'"
    elsif !actual.is_a?(Puppet::Pops::Types::PuppetObject)
      "expected that #{actual.class} would be a 'Puppet::Pops::Types::PuppetObject'"
    else
      "expected that #{actual.puppet_type.to_s} would be #{puppet_typename}"
    end
  end

  description do
    "be an inference with type #{puppet_typename}"
  end
end

RSpec.shared_examples 'a type detector' do |var_name|
  it "detects variable called #{var_name} the same as a real Puppet compiler" do
    inferencer, varmap = inference_and_actual_result(content, true)
    item = inferencer.find(var_name, PuppetLanguageServer::Manifest::ManifestInferencer::VariableInference)
    expect(item).to be_an_inference

    expect(item.puppet_type).to be_a(Puppet::Pops::Types::PuppetObject)
    expect(item.puppet_type.name).to eq(varmap[var_name].name)
  end
end

RSpec.shared_examples 'an exact type detector' do |var_name|
  it "detects variable called #{var_name} the same as a real Puppet compiler" do
    inferencer, varmap = inference_and_actual_result(content, true)
    item = inferencer.find(var_name, PuppetLanguageServer::Manifest::ManifestInferencer::VariableInference)
    expect(item).to be_an_inference

    expect(item.puppet_type).to be_a(Puppet::Pops::Types::PuppetObject)
    expect(item.puppet_type.to_s).to eq(varmap[var_name].to_s)
  end
end

RSpec.shared_examples 'a type inferrer' do |var_name, puppet_typename|
  it "detects variable called #{var_name} of type #{puppet_typename}" do
    result = inference_result(content)
    item = result.find(var_name, PuppetLanguageServer::Manifest::ManifestInferencer::VariableInference)
    expect(item).to be_an_inference

    expect(item.puppet_type).to be_a(Puppet::Pops::Types::PuppetObject)
    expect(item.puppet_type.name).to eq(puppet_typename)
  end
end

RSpec.shared_examples 'an exact type inferrer' do |var_name, puppet_typename|
  it "detects variable called #{var_name} of type #{puppet_typename}" do
    inferencer = inference_result(content)
    item = inferencer.find(var_name, PuppetLanguageServer::Manifest::ManifestInferencer::VariableInference)
    expect(item).to be_an_inference

    expect(item.puppet_type).to be_a(Puppet::Pops::Types::PuppetObject)
    expect(item.puppet_type.to_s).to eq(puppet_typename)
  end
end

describe 'PuppetLanguageServer::Manifest::ManifestInferencer' do
  describe 'PuppetLanguageServer::Manifest::ManifestInferencer::ManifestInferences' do
    def inference_result(content, tasks_mode = true)
      parser = Puppet::Pops::Parser::Parser.new
      ast = parser.singleton_parse_string(content, tasks_mode, '')

      inferencer = PuppetLanguageServer::Manifest::ManifestInferencer::ManifestInferences.new
      inferencer.infer(ast)
      inferencer
    end

    def inference_and_actual_result(content, tasks_mode = true)
      parser = Puppet::Pops::Parser::Parser.new
      ast = parser.singleton_parse_string(content, tasks_mode, '')

      inferencer = PuppetLanguageServer::Manifest::ManifestInferencer::ManifestInferences.new
      inferencer.infer(ast)

      Puppet.initialize_settings unless Puppet.settings.global_defaults_initialized?
      topscope = Puppet::Pal.in_tmp_environment('pal_env',
        modulepath: [],
        facts: {}
        ) do |pal|
          pal.with_script_compiler { |c|
            c.evaluate(ast)
            c.public_top_scope
          }
        end
      tc = Puppet::Pops::Types::TypeCalculator.new

      # Munge the topscope
      varmap = {}
      topscope.to_hash
              .reject { |k, _| k == 'trusted' || k == 'facts' || k == 'server_facts' }
              .each { |k, v| varmap[k] = v.is_a?(Puppet::Pops::Types::PAnyType) ? v : tc.infer(v) }

      [inferencer, varmap]
    end

    context 'given a trivial class' do
      let(:content) { "class foo {\n}" }

      it 'should find a class in the document root' do
        result = inference_result(content)
        expect(result.inferences.count).to eq(1)
        item = result.inferences[0]
        expect(item).to be_an_inference('foo', PuppetLanguageServer::Manifest::ManifestInferencer::HostClassDefinitionInference)
        expect(item.children).to be_empty
        expect(item.scope).to be_nil
      end
    end

    context 'given a trivial class with parameters' do
      let(:content) { "class foo(String $var1 = 'value1', String $var2 = 'value2') {\n}" }

      it 'should find all inferences' do
        result = inference_result(content)
        expect(result.inferences.count).to eq(3)
      end

      it 'should find a class in the document root' do
        result = inference_result(content)
        item = result.find('foo', PuppetLanguageServer::Manifest::ManifestInferencer::HostClassDefinitionInference)
        expect(item).to be_an_inference
        expect(item.children).to_not be_empty
        expect(item.scope).to be_nil
      end

      it 'should find the class parameters' do
        result = inference_result(content)
        items = result.select(nil, PuppetLanguageServer::Manifest::ManifestInferencer::ParameterInference)
        expect(items.count).to eq(2)

        item = result.find('var1', PuppetLanguageServer::Manifest::ManifestInferencer::ParameterInference)
        expect(item).to be_an_inference
        expect(item.children).to be_empty
        expect(item).to be_inference_type('String')
        expect(item.default_value).to eq("'value1'")
        expect(item.scope).to be_an_inference('foo', PuppetLanguageServer::Manifest::ManifestInferencer::HostClassDefinitionInference)

        item = result.find('var2', PuppetLanguageServer::Manifest::ManifestInferencer::ParameterInference)
        expect(item).to be_an_inference
        expect(item).to be_inference_type('String')
        expect(item.children).to be_empty
        expect(item.default_value).to eq("'value2'")
        expect(item.scope).to be_an_inference('foo', PuppetLanguageServer::Manifest::ManifestInferencer::HostClassDefinitionInference)
      end
    end

    context 'given plain variable assignment' do
      context 'with booleans' do
        let(:content) do
          <<-EOT
          $bool1 = true
          $bool2 = false
          EOT
        end

        it_behaves_like 'an exact type detector', 'bool1'
        it_behaves_like 'an exact type detector', 'bool2'
      end

      context 'with numbers' do
        let(:content) do
          <<-EOT
          $num1 = 1
          $num2 = 077
          $num3 = -1.0e2
          $num4 = 1.0
          EOT
        end

        it_behaves_like 'an exact type detector', 'num1'
        it_behaves_like 'an exact type detector', 'num2'
        it_behaves_like 'an exact type detector', 'num3'
        it_behaves_like 'an exact type detector', 'num4'
      end

      context 'with strings' do
        let(:content) do
          <<-EOT
          $str1 = 'A String'
          $str2 = "A String"
          $str3 = @(END)
          Text until the given end marker
          is in the resulting string.
          END
          $max_beers = 2
          $str4 = "I can not drink more than $max_beers beers"
          EOT
        end

        it_behaves_like 'an exact type detector', 'str1'
        it_behaves_like 'an exact type detector', 'str2'
        it_behaves_like 'an exact type detector', 'str3'
        it_behaves_like 'an exact type detector', 'str4'
      end

      context 'with undefined' do
        let(:content) do
          <<-EOT
          $var1 = Undef
          EOT
        end

        it_behaves_like 'an exact type detector', 'var1'
      end

      context 'with arrrays' do
        let(:content) do
          <<-EOT
          $arr1 = [1, 2, 3]
          $arr2 = ['a', 'b', 'c']
          $arr3 = []
          $arr4 = [1, 2, 3, 'a', 'b', 'c']
          $arr5 = [1, 2, 3, Any, 'a', 'b', 'c']
          EOT
        end

        # TODO: Should be able to detect these not infer them (maybe>?)
        it_behaves_like 'an exact type inferrer', 'arr1', 'Array[Variant[Integer[1, 1], Integer[2, 2], Integer[3, 3]]]'
        it_behaves_like 'an exact type inferrer', 'arr2', 'Array[String]'
        it_behaves_like 'an exact type inferrer', 'arr3', 'Array'
        it_behaves_like 'an exact type inferrer', 'arr4', 'Array[Variant[Integer[1, 1], Integer[2, 2], Integer[3, 3], String]]'
        it_behaves_like 'an exact type inferrer', 'arr5', 'Array'
      end

      context 'with hashes' do
        let(:content) do
          <<-EOT
          $hash1 = { "a" => 2 }
          $hash2 = { "a" => "b" }
          $hash3 = { 1 => 1.0 }
          $hash4 = { "a" => 2, "c" => "b", 1 => 1.0 }
          EOT
        end

        it_behaves_like 'an exact type detector', 'hash1'
        it_behaves_like 'an exact type detector', 'hash2'
        it_behaves_like 'an exact type detector', 'hash3'
        it_behaves_like 'a type detector', 'hash4'
      end
    end

    context 'given an invalid type assignment' do
      let(:content) do
        <<-EOT
        $str1 = 'value'
        $arr1 = Array[Foo]
        $num2 = 1
        EOT
      end

      it_behaves_like 'a type inferrer', 'str1', 'String'
      it_behaves_like 'a type inferrer', 'arr1', 'Any'
      it_behaves_like 'a type inferrer', 'num2', 'Integer'
    end

    context 'given chained assignment' do
      let(:content) do
        <<-EOT
        $str1 = $str2 = $str3 = 'value'
        $num1 = $num2 = $num3 = 123
        EOT
      end

      it_behaves_like 'a type detector', 'str1'
      it_behaves_like 'a type detector', 'str2'
      it_behaves_like 'a type detector', 'str3'
      it_behaves_like 'a type detector', 'num1'
      it_behaves_like 'a type detector', 'num2'
      it_behaves_like 'a type detector', 'num3'
    end

    context 'given explicit type assignment' do
      let(:content) do
        <<-EOT
        $var1 = String('value')
        $var2 = Array('value', false)
        $something = 'value'
        $var3 = Hash([$something, 'abc'])
        EOT
      end

      it_behaves_like 'a type detector', 'var1', 'String'
      it_behaves_like 'a type detector', 'var2', 'Array'
      it_behaves_like 'a type detector', 'var3', 'Hash'
    end

    context 'given simple transitive type assignment' do
      let(:content) do
        <<-EOT
        $var1 = 'value'
        $var2 = $var1
        EOT
      end

      it_behaves_like 'a type detector', 'var1', 'String'
      it_behaves_like 'a type detector', 'var2', 'String'
    end

    context 'given simple invalid transitive type assignment' do
      let(:content) do
        <<-EOT
        $var3 = $var1 # Technically is not allowed as $var1 has not be defined
        $var1 = 'value'
        $var2 = $var1
        EOT
      end

      it_behaves_like 'a type inferrer', 'var1', 'String'
      it_behaves_like 'a type inferrer', 'var2', 'String'
      it_behaves_like 'a type inferrer', 'var3', 'Any' # Processing is top-botttom
    end

    context 'using the map function' do
      let(:content) do
        <<-EOT
        $var1 = ["a", "b"]
        $var2 = $var1.map |$n| { 2 }
        EOT
      end

      it_behaves_like 'a type detector', 'var1', 'String'
      # puppet compiler won't see $n, but we do
      it_behaves_like 'a type inferrer', 'n', 'String'
      # We cant ever _really_ know the the result of .map so assume an Array[Any]
      it_behaves_like 'a type inferrer', 'var2', 'Array'
    end

    context 'given mathematic expressions' do
      let(:integer) { 'Integer(1)' }
      let(:float) { 'Float(1.0)' }
      let(:int_as_str) { "'1'" }
      let(:float_as_str) { "'1.0'" }

      context 'plus operator' do
        # ref - https://github.com/puppetlabs/puppet-specifications/blob/4e54d7a8c1e8f16c1cdbc44d4b5537c27784e7cd/language/expressions.md#-operator
        #   + operator
        #   PlusExpression : Expression<R> '+' Expression<R> ;
        #  Performs a concatenate/merge if the LHS is an Array, Hash, or URI
        #  Adds LHS and RHS numerically otherwise
        #  Operation fails if LHS or RHS are not numeric
        #  Is not commutative for non numeric/string operands ( [1,2,3] + 3 is not the same as 3 + [1,2,3], and [1,2,3] + [4,5,6] is not the same as [4,5,6] + [1,2,3] )
        #
        # T = Timestamp, D = Timespan (Duration), I = Integer (seconds), and F = Float (seconds with fraction)

        # T + T is illegal
        # T + D = T
        # T + I = T
        # T + F = T
        # D + T = T
        # D + D = D
        # D + I = D
        # D + F = D
        # I + T = T
        # F + T = T
        # I + D = D
        # F + D = D

        # LHS must evaluate to an Array or a Hash (or it is a form of arithmetic expression)
        # [1,2,3] + [4,5,6]               # => [1,2,3,4,5,6]
        # [1,2,3] + 4                     # => [1,2,3,4]
        # [1,2,3] + {a => 10, b => 20}    # => [1,2,3, [a, 10], [b, 20]]

        # {a => 10, b => 20} + {b => 30}  # => {a => 10, b => 30}
        # {a => 10, b => 20} + {c => 30}  # => {a => 10, b => 30, c => 30}
        # {a => 10, b => 20} + 30         # => error
        # {a => 10, b => 20} + [30]       # => error
        # {a => 10, b => 20} + [c, 30]    # => {a => 10, b => 20, c => 30}

        # URI('http://example.com/a/b/') + URI('/c/d') # => URI('http://example.com/c/d')
        # URI('http://example.com/a/b/') + URI('c/d')  # => URI('http://example.com/a/b/c/d')
        # URI('http://example.com/a/b' ) + URI('c/d')  # => URI('http://example.com/a/c/d')
        context 'with assignments that would cause parsing errors' do
          # These should all return Any because they cause parsing issues
          let(:content) do
            <<-EOT
            $bad1 = #{integer} + #{int_as_str}
            $bad2 = #{integer} + #{float_as_str}
            $bad3 = #{float} + #{int_as_str}
            $bad4 = #{float} + #{float_as_str}
            EOT
          end

          it_behaves_like 'a type inferrer', 'bad1', 'Any'
          it_behaves_like 'a type inferrer', 'bad2', 'Any'
          it_behaves_like 'a type inferrer', 'bad3', 'Any'
          it_behaves_like 'a type inferrer', 'bad4', 'Any'
        end

        context 'with Integer' do
          let(:content) do
            <<-EOT
            $integer1 = #{integer} + #{integer}
            $integer2 = #{integer} + #{float}
            $integer3 = #{integer} + Numeric(#{int_as_str})
            $integer4 = #{integer} + Numeric(#{float_as_str})
            EOT
          end

          it_behaves_like 'a type detector', 'integer1'
          it_behaves_like 'a type detector', 'integer2'
          it_behaves_like 'a type inferrer', 'integer3', 'Numeric'
          it_behaves_like 'a type inferrer', 'integer4', 'Numeric'
        end

        context 'with Float' do
          let(:content) do
            <<-EOT
            $float1 = #{float} + #{integer}
            $float2 = #{float} + #{float}
            $float3 = #{float} + Numeric(#{int_as_str})
            $float4 = #{float} + Numeric(#{float_as_str})
            EOT
          end

          it_behaves_like 'a type detector', 'float1'
          it_behaves_like 'a type detector', 'float2'
          it_behaves_like 'a type detector', 'float3'
          it_behaves_like 'a type detector', 'float4'
        end

        context 'with Numeric' do
          # Numerics can't really be created in a manfiest, as they're
          # a meta-type. So we can't use a real puppet compiler result
          let(:content) do
            <<-EOT
            $numeric1 = Numeric(#{float_as_str}) + #{integer}
            $numeric2 = Numeric(#{float_as_str}) + #{float}
            $numeric3 = Numeric(#{float_as_str}) + Numeric(#{int_as_str})
            EOT
          end

          it_behaves_like 'a type inferrer', 'numeric1', 'Numeric'
          it_behaves_like 'a type inferrer', 'numeric2', 'Numeric'
          it_behaves_like 'a type inferrer', 'numeric3', 'Numeric'
        end
      end


      context 'minus operator' do
        # ref - https://github.com/puppetlabs/puppet-specifications/blob/master/language/expressions.md#--operator

        # Performs a delete if the LHS is an Array or Hash
        # Subtracts RHS from LHS otherwise
        # LHS and RHS are converted to Numeric (see the section on Numeric Conversions in Conversions and Promotions)
        # Operation fails if LHS or RHS are not numeric or conversion failed
        #
        # T = Timestamp, D = Timespan (Duration), I = Integer (seconds), and F = Float (seconds with fraction)
        # T - T = D
        # T - D = T
        # T - I = T
        # T - F = T
        # D - D = D
        # D - I = D
        # D - F = D
        # I - D = D
        # F - D = D
        # Illegal operations:
        # D - T
        # I - T
        # F - T
        #
        # LHS must evaluate to an Array or a Hash (or it is a form of arithmetic expression)
        # [1,2,3,4,5,6] - [4,5,6]         # => [1,2,3]
        # [1,2,3] - 3                     # => [1,2]
        # [1,2,b] - {a => 1, b => 20}     # => [2]

        # {a => 10, b => 20} - {b => 30}  # => {a => 10}
        # {a => 10, b => 20} - a          # => {b => 20}
        # {a => 10, b => 20} - [a,c]      # => {b => 20}
      end

      context 'mulitply operator' do
        # ref - https://github.com/puppetlabs/puppet-specifications/blob/master/language/expressions.md#-operator-1
        #
        # Multiplies LHS and RHS
        # LHS and RHS are converted to Numeric (see the section on Numeric Conversions in Conversions and Promotions)
        # Operation fails if LHS or RHS are not numeric or if conversion failed
        #
        # T = Timestamp, D = Timespan (Duration), I = Integer, and F = Float
        #
        # D * I = D
        # D * F = D
        # I * D = D
        # F * D = D
        # Illegal operations:
        # T * <any>
        # <any> * T
        # D * D
      end

      context 'divide operator' do
        # ref - https://github.com/puppetlabs/puppet-specifications/blob/master/language/expressions.md#-operator-2
        #
        # LHS and RHS are converted to Numeric (see the section on Numeric Conversions in Conversions and Promotions)
        # Operation fails if LHS or RHS are not numeric or conversion failed
        # Division by 0 is an error
        # Division of integer values produces an integer result (without rounding). If one of the operands is a Float the result is also a Float.
        #
        # T = Timestamp, D = Timespan (Duration), I = Integer, and F = Float
        # D / D = F
        # D / I = F
        # D / F = F
        # Illegal operations:
        # T / <any>
        # <any> / T
        # I / D
        # F / D
      end

      context 'modulo operator' do
        # ref - https://github.com/puppetlabs/puppet-specifications/blob/master/language/expressions.md#-modulo-operator
        #
        # LHS and RHS are converted to Numeric (see the section on Numeric Conversions in Conversions and Promotions)
        # Operation fails if LHS or RHS are not Integer or if conversion failed
        # Modulo by 0 is an error
        # T = Timestamp, D = Timespan (Duration), I = Integer, and F = Float
        # D % I = I (seconds)
        # I % D = F (seconds and fractions of second)
        # Illegal operations:
        # T % <any>
        # <any> % T
        # D % D
        # F % D
        # D % F
      end

      context 'left shift operator' do
        # ref - https://github.com/puppetlabs/puppet-specifications/blob/master/language/expressions.md#-operator-3
        #
        # erforms an append if the LHS is an Array
        # Performs binary left shift of the LHS by the RHS count of shift steps otherwise
        # LHS and RHS are converted to Numeric (see the section on Numeric Conversions in Conversions and Promotions)
        # Operation fails if LHS or RHS are not Integer or if conversion failed
        # A left shift of a negative count reverses the shift direction
        # 1 << 1  # 2
        # 2 << 2  # 8
        # 8 << -1 # 4
        # [1,2,3] << 4       # [1,2,3,4]
        # [1,2,3] << [4]     # [1,2,3,[4]]
        # [1,2,3] << {a=>10} # [1,2,3,{a=>10}]
      end

      context 'right shift operator' do
        # ref - https://github.com/puppetlabs/puppet-specifications/blob/master/language/expressions.md#-operator-4
        # Only Integers
        # 1 >> 1   # 0
        # 8 >> 2   # 2
        # 2 >> -1  # 4
        let(:content) do
          <<-EOT
          $var1 = 8 >> 2
          EOT
        end

        it_behaves_like 'a type detector', 'var1'

        context 'with content that would cause parsing errors' do
          # These should all return Any because they cause parsing issues
          let(:content) do
            <<-EOT
            $bad1 = #{integer} >> #{int_as_str}
            $bad2 = #{integer} >>  #{float_as_str}
            $bad3 = #{float} >> #{integer}
            EOT
          end

          it_behaves_like 'a type inferrer', 'bad1', 'Any'
          it_behaves_like 'a type inferrer', 'bad2', 'Any'
          it_behaves_like 'a type inferrer', 'bad3', 'Any'
        end
      end

      context 'binary result operators' do
        # ref - https://github.com/puppetlabs/puppet-specifications/blob/master/language/expressions.md#and-or--logical-operators
        # ref - https://github.com/puppetlabs/puppet-specifications/blob/master/language/expressions.md#equality
        # ref - https://github.com/puppetlabs/puppet-specifications/blob/master/language/expressions.md#-operator-6
        # ref - https://github.com/puppetlabs/puppet-specifications/blob/master/language/expressions.md#-match-operator
        # ref - https://github.com/puppetlabs/puppet-specifications/blob/master/language/expressions.md#-match-operator-1
        # ref - https://github.com/puppetlabs/puppet-specifications/blob/master/language/expressions.md#----comparison-operators
        # ref - https://github.com/puppetlabs/puppet-specifications/blob/master/language/expressions.md#----comparison-operators
        let(:content) do
          <<-EOT
          $var1 = !(1 > 2)
          $var2 = 2 < 1
          $var3 = 4 >= 3
          $var4 = 1.0 <= 99.0
          $var5 = 1.0 <= 99.0
          $var6 = abc =~ /(a)b(c)/
          $var7 = [1,2,3] =~ Array[Integer]
          $var8 = abc !~ /(a)b(c)/
          $var9 = [1,2,3] !~ Array[Integer]
          $var10 = 'abc' == 'def'
          $var11 = true and 1
          $var12 = true or ''
          $var13 = true and !undef
          $var14 = 'eat' in ['eat', 'ate', 'eating']
          $var15 = Integer[100, 199] in [1, 2, 125]

          $bool1 = !false
          $bool2 = !!true
          $bool3 = true and false
          $bool4 = true or false
          $bool5 = true and !false
          EOT
        end

        it_behaves_like 'a type detector', 'var1'
        it_behaves_like 'a type detector', 'var2'
        it_behaves_like 'a type detector', 'var3'
        it_behaves_like 'a type detector', 'var4'
        it_behaves_like 'a type detector', 'var5'
        it_behaves_like 'a type detector', 'var6'
        it_behaves_like 'a type detector', 'var7'
        it_behaves_like 'a type detector', 'var8'
        it_behaves_like 'a type detector', 'var9'
        it_behaves_like 'a type detector', 'var10'
        it_behaves_like 'a type detector', 'var11'
        it_behaves_like 'a type detector', 'var12'
        it_behaves_like 'a type detector', 'var13'
        it_behaves_like 'a type detector', 'var14'
        it_behaves_like 'a type detector', 'var15'

        it_behaves_like 'an exact type detector', 'bool1'
        it_behaves_like 'an exact type detector', 'bool2'
        it_behaves_like 'an exact type detector', 'bool3'
        it_behaves_like 'an exact type detector', 'bool4'
        it_behaves_like 'an exact type detector', 'bool5'
      end
    end

    context 'given named function type expressions' do
      context 'with Numerics' do
        let(:content) do
          <<-EOT
          $strval = "1"

          $int1 = Integer("0xFF", 16)
          $int2 = Integer("010")
          $int3 = Integer("010", 10)
          $int4 = Integer(true)
          $int5 = Integer(-38, 10, true)
          $int6 = Integer($strval, 10, true)

          $num1 = Numeric($strval, true)
          $num2 = Numeric(true)
          $num3 = Numeric("0xFF")
          $num4 = Numeric("010")
          $num5 = Numeric("3.14")
          $num6 = Numeric(-42.3, true)
          $num7 = Numeric(-42, true)

          $float1 = Float($strval, true)
          EOT
        end

        it_behaves_like 'a type detector', 'int1'
        it_behaves_like 'a type detector', 'int2'
        it_behaves_like 'a type detector', 'int3'
        it_behaves_like 'a type detector', 'int4'
        it_behaves_like 'a type detector', 'int5'
        it_behaves_like 'a type detector', 'int6'

        # Can't automatically convert these
        it_behaves_like 'an exact type inferrer', 'num1', 'Numeric'
        it_behaves_like 'an exact type inferrer', 'num2', 'Numeric'
        it_behaves_like 'an exact type inferrer', 'num3', 'Numeric'
        it_behaves_like 'an exact type inferrer', 'num4', 'Numeric'
        it_behaves_like 'an exact type inferrer', 'num5', 'Numeric'
        it_behaves_like 'an exact type inferrer', 'num6', 'Numeric'
        it_behaves_like 'an exact type inferrer', 'num7', 'Numeric'

        it_behaves_like 'a type detector', 'float1'
      end

# $duration = Timespan(13.5)       # 13 seconds and 500 milliseconds
# $duration = Timespan({days=>4})  # 4 days
# $duration = Timespan(4, 0, 0, 2) # 4 days and 2 seconds
# $duration = Timespan('13:20')    # 13 hours and 20 minutes (using default pattern)
# $duration = Timespan('10:03.5', '%M:%S.%L') # 10 minutes, 3 seconds, and 5 milli-seconds
# $duration = Timespan('10:03.5', '%M:%S.%N') # 10 minutes, 3 seconds, and 5 nano-seconds

# $ts = Timestamp(1473150899)                              # 2016-09-06 08:34:59 UTC
# $ts = Timestamp({string=>'2015', format=>'%Y'})          # 2015-01-01 00:00:00.000 UTC
# $ts = Timestamp('Wed Aug 24 12:13:14 2016', '%c')        # 2016-08-24 12:13:14 UTC
# $ts = Timestamp('Wed Aug 24 12:13:14 2016 PDT', '%c %Z') # 2016-08-24 19:13:14.000 UTC
# $ts = Timestamp('2016-08-24 12:13:14', '%F %T', 'PST')   # 2016-08-24 20:13:14.000 UTC
# $ts = Timestamp('2016-08-24T12:13:14', default, 'PST')   # 2016-08-24 20:13:14.000 UTC

#$a = Binary('YWJj')

# # The following declaration
# $x = Init[Integer].new('128')
# # is exactly the same as
# $x = Integer.new('128')
# or, with base 16 and using implicit new
#
# # The following declaration
# $x = Init[Integer,16]('80')
# # is exactly the same as
# $x = Integer('80', 16)
# $fmt = Init[String,'%#x']
# notice($fmt(256)) # will notice '0x100'
    end

    context 'given access expressions' do
      # Ref https://github.com/puppetlabs/puppet-specifications/tree/master/language/types_values_variables.md
      let(:content) do
        <<-EOT
        $str1 = "Hello World"[6]
        $str2 = String[6]
        $str3 = String[6, 12]

        $regex1 = Regexp['(f)(o)(o)']

        $pat1 = Pattern[red, blue, green]

        $enum1 = Enum[red, blue, green]

        $hash1 = Hash[String, Integer]
        $hash2 = Hash[Scalar, String, 1, 10]
        # hash3 = Hash([2, 'abc'])
        # hash4 = { 'abc' => 2}

        $emptyArray = Array
        $arr1 = Array[String]
        $arr2 = $emptyArray[Integer]
        $arr3 = $emptyArray[Numeric, 1,      5]
        $arr4 = Array[Data, 1]
        $arr5 = Array[Data, 2, 4]

        $struct1 = Struct[{mode=>Enum[read, write, update], path=>String[1]}]

        $coll1 = Collection
        $coll2 = Collection[1]
        $coll3 = Collection[1, 3]

        $class1 = Class
        $class2 = Class[apache]
        $apache_string = 'apache'
        $class3 = Class[$apache_string]
        $class4 = Class[apache, nginx]
        $class5 = $class1[varname]  # TODO: Should be the type of the parameter called varname. TBH, just do ANY!

        $int1 = Integer[2]
        $int2 = Integer[1, 3]

        $float1 = Float[2]
        $float2 = Float[2.0]
        $float3 = Float[1, 3.2]
        $float4 = Float[1.5, 3.2]
        $float5 = Float[-1.0,1.0]

        $timespan1 = Timespan[2]
        $timespan2 = Timespan[77.3]
        $timespan3 = Timespan[{hour => 1}, {hour => 2}]
        $timespan4 = Timespan['1-00:00:00', '2-00:00:00']
        $timespan5 = Timespan[Timespan('11', '%H'), Timespan('12', '%H')]
        $timespan6 = Timespan['1-00:00:00', Timespan({days => 2, nanoseconds => -1})]

        $timestamp1 = Timestamp['2000-01-01T00:00:00.000', default]
        $timestamp2 = Timestamp['2012-10-10']
        $timestamp3 = Timestamp[default, 1433116800]
        $timestamp4 = Timestamp['2010-01-01', '2015-12-31T23:59:59.999999999']

        $optional1 = Optional[String]

        $variant1 = Variant[String, Numeric]

        $type1 = Type[String]

        $semver1 = SemVer[SemVerRange('>=1.0.0 <2.0.0'), SemVerRange('>=3.0.0 <4.0.0')]
        EOT
      end

      context 'with String' do
        it_behaves_like 'an exact type detector', 'str1'
        it_behaves_like 'an exact type detector', 'str2'
        it_behaves_like 'an exact type detector', 'str3'
      end

      context 'with RegExp' do
        it_behaves_like 'a type detector', 'regex1'
      end

      context 'with Pattern' do
        it_behaves_like 'a type detector', 'pat1'
      end

      context 'with Enum' do
        it_behaves_like 'a type detector', 'enum1'
      end

      context 'with Hash' do
        it_behaves_like 'a type detector', 'hash1'
        it_behaves_like 'a type detector', 'hash2'
      end

      context 'with Array' do
        it_behaves_like 'an exact type detector', 'arr1'
        it_behaves_like 'an exact type detector', 'arr2'
        it_behaves_like 'an exact type detector', 'arr3'
        it_behaves_like 'an exact type detector', 'arr4'
        it_behaves_like 'an exact type detector', 'arr5'
      end

      context 'with Struct' do
        it_behaves_like 'a type detector', 'struct1'
      end

      context 'with Collection' do
        it_behaves_like 'a type detector', 'coll1'
        it_behaves_like 'a type detector', 'coll2'
        it_behaves_like 'a type detector', 'coll3'
      end

      context 'with Class' do
        # Classes are just a beast to deal with. For now, anything with a Class is, just a Class
        # To test for real, change to 'an exact type detector'
        it_behaves_like 'an exact type inferrer', 'class1', 'Class'
        it_behaves_like 'an exact type inferrer', 'class2', 'Class'
        it_behaves_like 'an exact type inferrer', 'class3', 'Class'
        it_behaves_like 'an exact type inferrer', 'class4', 'Class'
        it_behaves_like 'an exact type inferrer', 'class5', 'Class'
      end

      context 'with Resource' do
        let(:content) do
          <<-EOT
          $res1 = Resource['file']
          EOT
        end

        it_behaves_like 'an exact type detector', 'res1' # TODO: Can this be solved staticly?
      end

      context 'with a fake Resource' do
        let(:content) do
          <<-EOT
          $res1 = Resource['zaphod']
          EOT
        end

        # Can't use 'a type detector' here because the resources don't actually exist
        it_behaves_like 'an exact type inferrer', 'res1', 'Resource'
      end

      context 'with Integer' do
        it_behaves_like 'a type detector', 'int1'
        it_behaves_like 'a type detector', 'int2'
      end

      context 'with Float' do
        it_behaves_like 'a type detector', 'float1'
        it_behaves_like 'a type detector', 'float2'
        it_behaves_like 'a type detector', 'float3'
        it_behaves_like 'a type detector', 'float4'
        it_behaves_like 'a type detector', 'float5'
      end

      context 'with Timespan' do
        it_behaves_like 'a type detector', 'timespan1'
        it_behaves_like 'a type detector', 'timespan2'
        it_behaves_like 'a type detector', 'timespan3'
        it_behaves_like 'a type detector', 'timespan4'
        it_behaves_like 'a type detector', 'timespan5'
        it_behaves_like 'a type detector', 'timespan6'
      end

      context 'with Timestamp' do
        it_behaves_like 'a type detector', 'timestamp1'
        it_behaves_like 'a type detector', 'timestamp2'
        it_behaves_like 'a type detector', 'timestamp3'
        it_behaves_like 'a type detector', 'timestamp4'
      end

      context 'with Optional' do
        it_behaves_like 'a type detector', 'optional1'
      end

      context 'with Variant' do
        it_behaves_like 'a type detector', 'variant1'
      end

      context 'with Type' do
        it_behaves_like 'a type detector', 'type1'
      end

      context 'with Semver' do
        it_behaves_like 'a type detector', 'semver1'
      end
    end

    # Not implemented yet
    context 'given complex assignments', :skip => true do
      let(:content) do
        <<-EOT
        # from array
        [$a, $b] = [1, 'abc']  # assigns 1 to $a, and 2 to $b

        # from hash
        [$d, $e] = { d => 10, e => 'value', f => 30 }  # assigns 10 to $d, and 'value' to $e

        # # from class
        # class mymodule::someclass::example($x = 100) {
        #   $a = 10
        # }
        # [$a, $x] = Class['mymodule::someclass::example']  # assigns 10 to $a, and 100 to $x
        EOT
      end

      it_behaves_like 'a type inferrer', 'a', 'Integer'
      it_behaves_like 'a type inferrer', 'b', 'String'

      it_behaves_like 'a type inferrer', 'd', 'Integer'
      it_behaves_like 'a type inferrer', 'e', 'String'
    end
  end
end








    # This is all broken
    # context 'given mathematic expressions' do
    #   let(:content) do
    #     <<-EOT
    #     $any1 = Any + Any
    #     $any2 = Any + Scalar

    #     # $undef1 = Undef + Undef
    #     # $undef2 = Undef + Any

    #     # $data1 = Data + Data
    #     # $data2 = Data + Numeric
    #     # $data3 = Data + String
    #     # $data4 = Data + Array[Data]
    #     # $data5 = Data + Hash[String, Data]
    #     # $data6 = Data + Undef
    #     # $data7 = Data + TimeSpan

    #     # $scalar1 = Scalar + Scalar
    #     # $scalar2 = Scalar + String
    #     # $scalar3 = Scalar + Array[Data]

    #     # $numeric1 = Numeric + Numeric
    #     # $numeric2 = Numeric + Integer
    #     # $numeric3 = Numeric + Float
    #     # $numeric4 = Numeric + String
    #     # $numeric5 = Numeric + Data

    #     # $integer1 = Integer + Integer
    #     # $integer2 = Integer + Float
    #     # $integer3 = Integer + Numeric
    #     # $integer4 = Integer + String
    #     # $integer5 = Integer + Data
    #     # $integer6 = Integer[1,2] + Integer[3,4]

    #     # $float1 = Float + Float
    #     # $float2 = Float + Integer
    #     # $float3 = Float + Numeric
    #     # $float4 = Float + String
    #     # $float5 = Float + Data
    #     # $float6 = Float[1,2] + Float[3,4]

    #     # $timespan1 = Timespan + Timespan
    #     # $timespan2 = Timespan + Numeric
    #     # $timespan3 = Timespan + String
    #     # $timespan4 = Timespan + Data
    #     # $timespan5 = Timespan[1,2] + Timespan[3,4]

    #     # $timestamp1 = Timestamp + Timestamp
    #     # $timestamp2 = Timestamp + Numeric
    #     # $timestamp3 = Timestamp + String
    #     # $timestamp4 = Timestamp + Data
    #     # $timestamp5 = Timestamp[1,2] + Timestamp[3,4]

    #     # $str1 = String + String
    #     # $str2 = String + Enum
    #     # $str3 = String + Pattern
    #     # $str4 = String + Integer
    #     # $str5 = String + Data

    #     # $enum1 = Enum + Enum
    #     # $enum2 = Enum + String

    #     # $pat1 = Pattern + Pattern
    #     # $pat2 = Pattern + String
    #     EOT
    #   end

      # it_behaves_like 'a type detector', 'any1', 'Any'
      # it_behaves_like 'a type detector', 'any2', 'Any'

      # it_behaves_like 'a type detector', 'undef1', 'Undef'
      # it_behaves_like 'a type detector', 'undef2', 'Any'

      # it_behaves_like 'a type detector', 'data1', 'Data'
      # it_behaves_like 'a type detector', 'data2', 'Data'
      # it_behaves_like 'a type detector', 'data3', 'Data'
      # it_behaves_like 'a type detector', 'data4', 'Data'
      # it_behaves_like 'a type detector', 'data5', 'Data'
      # it_behaves_like 'a type detector', 'data6', 'Data'
      # it_behaves_like 'a type detector', 'data7', 'Any'

      # it_behaves_like 'a type detector', 'scalar1', 'Scalar'
      # it_behaves_like 'a type detector', 'scalar2', 'Scalar'
      # it_behaves_like 'a type detector', 'scalar3', 'Any'

      # it_behaves_like 'a type detector', 'numeric1', 'Numeric'
      # it_behaves_like 'a type detector', 'numeric2', 'Numeric'
      # it_behaves_like 'a type detector', 'numeric3', 'Numeric'
      # it_behaves_like 'a type detector', 'numeric4', 'Scalar'
      # it_behaves_like 'a type detector', 'numeric5', 'Any'

      # it_behaves_like 'a type detector', 'integer1', 'Integer'
      # it_behaves_like 'a type detector', 'integer2', 'Numeric'
      # it_behaves_like 'a type detector', 'integer3', 'Numeric'
      # it_behaves_like 'a type detector', 'integer4', 'Scalar'
      # it_behaves_like 'a type detector', 'integer5', 'Any'
      # it_behaves_like 'a type detector', 'integer6', 'Integer'

      # it_behaves_like 'a type detector', 'float1', 'Float'
      # it_behaves_like 'a type detector', 'float2', 'Numeric'
      # it_behaves_like 'a type detector', 'float3', 'Numeric'
      # it_behaves_like 'a type detector', 'float4', 'Scalar'
      # it_behaves_like 'a type detector', 'float5', 'Any'
      # it_behaves_like 'a type detector', 'float6', 'Float'

      # it_behaves_like 'a type detector', 'timespan1', 'Timespan'
      # it_behaves_like 'a type detector', 'timespan2', 'Scalar'
      # it_behaves_like 'a type detector', 'timespan3', 'Scalar'
      # it_behaves_like 'a type detector', 'timespan4', 'Any'
      # it_behaves_like 'a type detector', 'timespan5', 'Timespan'

      # it_behaves_like 'a type detector', 'timestamp1', 'Timestamp'
      # it_behaves_like 'a type detector', 'timestamp2', 'Scalar'
      # it_behaves_like 'a type detector', 'timestamp3', 'Scalar'
      # it_behaves_like 'a type detector', 'timestamp4', 'Any'
      # it_behaves_like 'a type detector', 'timestamp5', 'Timestamp'

      # it_behaves_like 'a type detector', 'str1', 'String'
      # it_behaves_like 'a type detector', 'str2', 'String'
      # it_behaves_like 'a type detector', 'str3', 'String'
      # it_behaves_like 'a type detector', 'str4', 'Scalar'
      # it_behaves_like 'a type detector', 'str5', 'Any'

      # it_behaves_like 'a type detector', 'enum1', 'Enum'
      # it_behaves_like 'a type detector', 'enum2', 'Any'

      # it_behaves_like 'a type detector', 'pat1', 'Pattern'
      # it_behaves_like 'a type detector', 'pat2', 'Any'
    # end
