require 'spec_helper'
require 'puppet-languageserver/manifest/inferencer'

# Monkey Patch
require 'puppet/pal/pal_api'
require 'puppet/pal/compiler'
class Puppet::Pal::Compiler
  def public_top_scope
    topscope
  end
end
# End Monkey patch

RSpec::Matchers.define :be_an_inference do |name = nil, klass = nil|
  match do |actual|
    return false unless actual.is_a?(PuppetLanguageServer::Manifest::Inferencer::BaseInference)

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
    item = inferencer.find(var_name, PuppetLanguageServer::Manifest::Inferencer::VariableInference)
    expect(item).to be_an_inference

    expect(item.puppet_type).to be_a(Puppet::Pops::Types::PuppetObject)
    expect(item.puppet_type.name).to eq(varmap[var_name].name)
  end
end

RSpec.shared_examples 'an exact type detector' do |var_name|
  it "detects variable called #{var_name} the same as a real Puppet compiler" do
    inferencer, varmap = inference_and_actual_result(content, true)
    item = inferencer.find(var_name, PuppetLanguageServer::Manifest::Inferencer::VariableInference)
    expect(item).to be_an_inference

    expect(item.puppet_type).to be_a(Puppet::Pops::Types::PuppetObject)
    expect(item.puppet_type.to_s).to eq(varmap[var_name].to_s)
  end
end

RSpec.shared_examples 'a type inferrer' do |var_name, puppet_typename|
  it "detects variable called #{var_name} of type #{puppet_typename}" do
    result = inference_result(content)
    item = result.find(var_name, PuppetLanguageServer::Manifest::Inferencer::VariableInference)
    expect(item).to be_an_inference

    expect(item.puppet_type).to be_a(Puppet::Pops::Types::PuppetObject)
    expect(item.puppet_type.name).to eq(puppet_typename)
  end
end

RSpec.shared_examples 'an exact type inferrer' do |var_name, puppet_typename|
  it "detects variable called #{var_name} of type #{puppet_typename}" do
    inferencer = inference_result(content)
    item = inferencer.find(var_name, PuppetLanguageServer::Manifest::Inferencer::VariableInference)
    expect(item).to be_an_inference

    expect(item.puppet_type).to be_a(Puppet::Pops::Types::PuppetObject)
    expect(item.puppet_type.to_s).to eq(puppet_typename)
  end
end

describe 'PuppetLanguageServer::Manifest::Inferencer' do
  describe 'PuppetLanguageServer::Manifest::Inferencer::FunctionInferencer' do
    def inference_result(content, tasks_mode = true)
      parser = Puppet::Pops::Parser::Parser.new
      ast = parser.singleton_parse_string(content, tasks_mode, '')

      inferencer = PuppetLanguageServer::Manifest::Inferencer.new
      inferencer.infer(ast)
      inferencer
    end

    def inference_and_actual_result(content, tasks_mode = true)
      parser = Puppet::Pops::Parser::Parser.new
      ast = parser.singleton_parse_string(content, tasks_mode, '')

      inferencer = PuppetLanguageServer::Manifest::Inferencer.new
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

    context 'using functions which can not be inferred' do
      # These functions are just too hard to make sensible decisions on so they always return a known variation on Any
      # e.g. Any, Array[Any], Hash[Any, Any] etc.
      let(:content) do
        <<-EOT
        $result1 = dig({}, 'a', 'b', 1, 'x')
        $result2 = {}.dig('a', 'b', 1, 'x')
        $result3 = eyaml_lookup_key('key', {})
        $result4 = get($facts, 'os.family')
        $result5 = $facts.get('os.family')
        $result6 = $facts.get('os.family') |$p1| { }
        $result7 = getvar('facts')
        $result8 = getvar('facts') |$p2| { }
        $result9 = hiera('users', undef)
        $result10 = hiera_array('users', undef)
        $result11 = hiera_hash('users', undef)
        $result12 = lest($x) || { do_things() }
        $result13 = [].lest || { }
        $result14 = lookup('users')
        $result15 = lookup(['users'])
        $result16 = lookup('users') |$p3| { }
        $result17 = max("1", 2)
        $result18 = [1,2,3].max
        $result19 = [1,2,3].max |$p4, $p5| { compare($a, $b) }
        $result20 = min("1", 2)
        $result21 = [1,2,3].min
        $result22 = [1,2,3].min |$p6, $p7| { compare($a, $b) }
        $result23 = ['', b, c].partition |$p8| { }
        $result24 = partition(['', b, c]) |$p9| { }
        $result25 = then($x) || { do_things() }
        $result26 = [].then || { }
        $result27 = [].tree_each |$p10, $p11| {}
        $result28 = tree_each([]) |$p12, $p13| {}
        $result29 = yaml_data('users')
        EOT
      end

      it_behaves_like 'an exact type inferrer', 'result1', 'Any'
      it_behaves_like 'an exact type inferrer', 'result2', 'Any'
      it_behaves_like 'an exact type inferrer', 'result3', 'Any'
      it_behaves_like 'an exact type inferrer', 'result4', 'Any'
      it_behaves_like 'an exact type inferrer', 'result5', 'Any'
      it_behaves_like 'an exact type inferrer', 'result6', 'Any'
      it_behaves_like 'an exact type inferrer', 'result7', 'Any'
      it_behaves_like 'an exact type inferrer', 'result8', 'Any'
      it_behaves_like 'an exact type inferrer', 'result9', 'Any'
      it_behaves_like 'an exact type inferrer', 'result10', 'Array'
      it_behaves_like 'an exact type inferrer', 'result11', 'Hash'
      it_behaves_like 'an exact type inferrer', 'result12', 'Any'
      it_behaves_like 'an exact type inferrer', 'result13', 'Any'
      it_behaves_like 'an exact type inferrer', 'result14', 'Any'
      it_behaves_like 'an exact type inferrer', 'result15', 'Any'
      it_behaves_like 'an exact type inferrer', 'result16', 'Any'
      it_behaves_like 'an exact type inferrer', 'result17', 'Any'
      it_behaves_like 'an exact type inferrer', 'result18', 'Any'
      it_behaves_like 'an exact type inferrer', 'result19', 'Any'
      it_behaves_like 'an exact type inferrer', 'result20', 'Any'
      it_behaves_like 'an exact type inferrer', 'result21', 'Any'
      it_behaves_like 'an exact type inferrer', 'result22', 'Any'
      it_behaves_like 'an exact type inferrer', 'result24', 'Tuple[Array, Array]'
      it_behaves_like 'an exact type inferrer', 'result25', 'Any'
      it_behaves_like 'an exact type inferrer', 'result26', 'Any'
      it_behaves_like 'an exact type inferrer', 'result27', 'Any'
      it_behaves_like 'an exact type inferrer', 'result28', 'Any'
      it_behaves_like 'an exact type inferrer', 'result29', 'Any'
      it_behaves_like 'an exact type inferrer', 'p1', 'Any'
      it_behaves_like 'an exact type inferrer', 'p2', 'Any'
      it_behaves_like 'an exact type inferrer', 'p3', 'Any'
      it_behaves_like 'an exact type inferrer', 'p4', 'Any'
      it_behaves_like 'an exact type inferrer', 'p5', 'Any'
      it_behaves_like 'an exact type inferrer', 'p6', 'Any'
      it_behaves_like 'an exact type inferrer', 'p7', 'Any'
      it_behaves_like 'an exact type inferrer', 'p8', 'Any'
      it_behaves_like 'an exact type inferrer', 'p9', 'Any'
      it_behaves_like 'an exact type inferrer', 'p10', 'Any'
      it_behaves_like 'an exact type inferrer', 'p11', 'Any'
      it_behaves_like 'an exact type inferrer', 'p12', 'Any'
      it_behaves_like 'an exact type inferrer', 'p13', 'Any'
    end

    context 'using returnless functions' do
      let(:content) do
        <<-EOT
        $result1 = alert("something")
        $result2 = crit("something")
        $result3 = debug("something")
        $result4 = emerg("something")
        $result5 = err("something")
        $result6 = fail("something")
        $result7 = info("something")
        $result8 = notice("something")
        $result9 = realize("something")
        $result10 = require("something")
        $result11 = sprintf("something")
        $result12 = tag("something")
        $result13 = warning("something")
        $result14 = break()

        EOT
      end

      # Returnless functions won't really compile
      it_behaves_like 'a type inferrer', 'result1', 'Undef'
      it_behaves_like 'a type inferrer', 'result2', 'Undef'
      it_behaves_like 'a type inferrer', 'result3', 'Undef'
      it_behaves_like 'a type inferrer', 'result4', 'Undef'
      it_behaves_like 'a type inferrer', 'result5', 'Undef'
      it_behaves_like 'a type inferrer', 'result6', 'Undef'
      it_behaves_like 'a type inferrer', 'result7', 'Undef'
      it_behaves_like 'a type inferrer', 'result8', 'Undef'
      it_behaves_like 'a type inferrer', 'result9', 'Undef'
      it_behaves_like 'a type inferrer', 'result10', 'Undef'
      it_behaves_like 'a type inferrer', 'result11', 'Undef'
      it_behaves_like 'a type inferrer', 'result12', 'Undef'
      it_behaves_like 'a type inferrer', 'result13', 'Undef'
      it_behaves_like 'a type inferrer', 'result14', 'Undef'
    end

    context 'using string functions' do
      let(:content) do
        <<-EOT
        $result1 = digest('abc')
        # $result2 = epp('abc')
        # $result3 = file('abc')
        # $result4 = generate('abc')
        # $result5 = inline_epp('abc')
        # $result6 = inline_template('abc')
        $result2 = [1,2,3].join
        $result3 = join([1,2,3])
        $result4 = md5('abc')
        $result5 = sha1('abc')
        $result6 = sha256('abc')
        $result7 = shellquote(['abc', '123'])
        $result8 = strftime('abc')
        # $result13 = template('abc')
        EOT
      end

      it_behaves_like 'a type detector', 'result1'
      it_behaves_like 'a type detector', 'result2'
      it_behaves_like 'a type detector', 'result3'
      it_behaves_like 'a type detector', 'result4'
      it_behaves_like 'a type detector', 'result5'
      it_behaves_like 'a type detector', 'result6'
      it_behaves_like 'a type detector', 'result7'
      it_behaves_like 'a type detector', 'result8'

      context 'with a manifest that puppet can\'t compile' do
        let(:content) do
          <<-EOT
          $result1 = epp('abc')
          $result2 = file('abc')
          $result3 = generate('abc')
          $result4 = inline_epp('abc')
          $result5 = inline_template('abc')
          $result6 = template('abc')
          EOT
        end

        it_behaves_like 'a type inferrer', 'result1', 'String'
        it_behaves_like 'a type inferrer', 'result2', 'String'
        it_behaves_like 'a type inferrer', 'result3', 'String'
        it_behaves_like 'a type inferrer', 'result4', 'String'
        it_behaves_like 'a type inferrer', 'result5', 'String'
        it_behaves_like 'a type inferrer', 'result6', 'String'
      end

    end

    context 'using binary functions' do
      let(:content) do
        <<-EOT
        $result1 = binary_file('abc')
        EOT
      end

      it_behaves_like 'a type inferrer', 'result1', 'Binary'
    end

    context 'using integer functions' do
      let(:content) do
        <<-EOT
        $result1 = ceiling(1.2)
        $result2 = compare(1, 1.2)
        $result3 = floor(1.2)
        $result4 = ['a', 'b'].index |$value| { $value == 'a' }
        $result5 = index(['a', 'b']) |$value| { $value == 'a' }
        $result6 = round(1.2)
        $result7 = versioncmp('1.0', '2.0')
        EOT
      end

      it_behaves_like 'a type detector', 'result1'
      it_behaves_like 'a type detector', 'result2'
      it_behaves_like 'a type detector', 'result3'
      it_behaves_like 'a type detector', 'result4'
      it_behaves_like 'a type detector', 'result5'
      it_behaves_like 'a type detector', 'result6'
      it_behaves_like 'a type detector', 'result7'

      context 'with a manifest that puppet can\'t compile' do
        let(:content) do
          <<-EOT
          $result1 = fqdn_rand(1)
          EOT
        end

        it_behaves_like 'a type inferrer', 'result1', 'Integer'
      end
    end

    context 'using the abs function' do
      let(:content) do
        <<-EOT
        $var1 = abs(-1.0)
        $var2 = abs(1.0)
        $var3 = abs(-1)
        $var4 = abs(1)
        $var5 = abs("-1.0")
        EOT
      end

      it_behaves_like 'an exact type detector', 'var1'
      it_behaves_like 'an exact type detector', 'var2'
      it_behaves_like 'an exact type detector', 'var3'
      it_behaves_like 'an exact type detector', 'var4'
      # Can't resolve strings but they are at least numerics
      it_behaves_like 'a type inferrer', 'var5', 'Numeric'
    end

    context 'using the map function' do
      context 'with arrays' do
        let(:content) do
          <<-EOT
          $var1 = ["a", "b"]
          $var2 = $var1.map |$p1| { 2 }
          $var3 = map($var1) |$p2| { 2 }
          $var4 = $var1.map |$p3, $p4| { 2 }
          EOT
        end

        it_behaves_like 'a type detector', 'var1'
        # puppet compiler won't see $n, but we do
        it_behaves_like 'a type inferrer', 'p1', 'String'
        it_behaves_like 'a type inferrer', 'p2', 'String'
        it_behaves_like 'a type inferrer', 'p3', 'Integer'
        it_behaves_like 'a type inferrer', 'p4', 'String'
        # We cant ever _really_ know the the result of .map so assume an Array[Any]
        it_behaves_like 'a type inferrer', 'var2', 'Array'
        it_behaves_like 'a type inferrer', 'var3', 'Array'
      end

      context 'with hashes' do
        let(:content) do
          <<-EOT
          $var1 = { "a" => 2 }
          $var2 = $var1.map |$p1| { 2 }
          $var3 = map($var1) |$p2| { 2 }
          $var4 = $var1.map |$p3, $p4| { 2 }
          EOT
        end

        it_behaves_like 'an exact type detector', 'var1'
        # puppet compiler won't see $n, but we do
        it_behaves_like 'a type inferrer', 'p1', 'Array'
        it_behaves_like 'a type inferrer', 'p2', 'Array'
        it_behaves_like 'a type inferrer', 'p3', 'String'
        it_behaves_like 'a type inferrer', 'p4', 'Integer'
        # We cant ever _really_ know the the result of .map so assume an Array[Any]
        it_behaves_like 'a type inferrer', 'var2', 'Array'
        it_behaves_like 'a type inferrer', 'var3', 'Array'
      end

      context 'with an explicit type' do
        let(:content) do
          <<-EOT
          $var1 = [1, 2, 3]
          $var2 = $var1.map |Numeric $n| { 2 }
          EOT
        end

        it_behaves_like 'a type detector', 'var1'
        # puppet compiler won't see $n, but we do
        it_behaves_like 'a type inferrer', 'n', 'Numeric'
        # We cant ever _really_ know the the result of .map so assume an Array[Any]
        it_behaves_like 'a type inferrer', 'var2', 'Array'
      end
    end
  end
end
