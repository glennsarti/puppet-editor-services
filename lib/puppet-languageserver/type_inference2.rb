#!/usr/bin/env ruby

# TODO: WIP

# Add the language server into the load path
root = File.join(File.dirname(__FILE__),'..','..')

$LOAD_PATH.unshift(File.join(root,'lib'))
# #reject{ |o| !File.directory?(o)}
vendor_dir = File.join(root,'vendor')
Dir.new(vendor_dir)
  .reject{ |v| v == '.' || v == '..'}
  .map{ |v| File.join(vendor_dir,v) }
  .reject{ |v| !File.directory?(v)}
  .each do |vendor|
    $LOAD_PATH.unshift(File.join(vendor,'lib'))
end

puts "Tick tock..."
require 'puppet'
require 'pp'
puts "--- Time to rock-n-roll"

puts "loading bolt data..."
require 'puppet_languageserver'
%w[
  validation_queue
  sidecar_protocol
  sidecar_queue
  puppet_parser_helper
  puppet_helper
  facter_helper
  uri_helper
  puppet_monkey_patches
  providers
].each do |lib|
  begin
    require "puppet-languageserver/#{lib}"
  rescue LoadError
    require File.expand_path(File.join(File.dirname(__FILE__), 'puppet-languageserver', lib))
  end
end
PuppetLanguageServer::PuppetHelper.initialize_helper
PuppetLanguageServer::PuppetHelper.load_static_data


def recurse_showast(item, depth = 0)
  output = "  " * depth
  output += "#{item.class.to_s} (#{item.object_id})"
  if item.respond_to?(:offset)
    output += " (Off-#{item.offset}:#{item.offset + item.length} Pos-#{item.line}:#{item.pos} Len-#{item.length}) ~#{item.locator.extract_text(item.offset, item.length).gsub("\n", "\\n")}~"
  end
  puts output
  item._pcore_contents do |child|
    recurse_showast(child, depth + 1)
  end
end


# Monkey Patch
require 'puppet/pal/pal_api'
require 'puppet/pal/compiler'
class Puppet::Pal::Compiler
  def public_top_scope
    topscope
  end
end

# $parser_scopes = []
# class Puppet::Parser::Scope
#   alias_method :original_initialize, :initialize
#   def initialize(*args)
#     $parser_scopes << self
#     original_initialize(*args)
#   end

#   def self.reset_scopelist
#     $parser_scopes = []
#   end
# end


# End Monkey patch

def evaluate_ast_to_vars(ast)
  Puppet.initialize_settings unless Puppet.settings.global_defaults_initialized?

  # Puppet::Parser::Scope.reset_scopelist
  # Puppet[:tasks] = true
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
  varmap
end

content = <<-EOT
plan mymodule::my_plan(
  TargetSpec $load_balancer,
  TargetSpec  $webservers,
) {

  # Extract the Target name from $webservers
  $webserver_names = get_targets($webservers).map |$n| { $n.name }

  # process webservers
  run_task('mymodule::lb_remove', $load_balancer, webservers => $webserver_names)
  run_task('mymodule::update_frontend_app', $webservers, version => '1.2.3')
  run_task('mymodule::lb_add', $load_balancer, webservers => $webserver_names)
 }
EOT

content = <<-EOT
  $load_balancer = TargetSpec['abc']
  $webservers = TargetSpec['abc']

  # Extract the Target name from $webservers
  $xxxx = get_targets($webservers)
  $webserver_names = get_targets($webservers).map |$n| { $n.name }

  # process webservers
  run_task('mymodule::lb_remove', $load_balancer, webservers => $webserver_names)
EOT

# content = '$result2 = fqdn_rand(30)'
content = <<-EOT
$var1 = ["a", "b"]
$var2 = $var1.map |$p1| { 2 }
$var3 = map($var1) |$p2| { 2 }
$var4 = $var1.map |$p3, $p4| { 2 }
EOT

# Use Puppet to generate the AST
parser = Puppet::Pops::Parser::Parser.new

Puppet[:tasks] = true
ast = parser.parse_string(content, '')

recurse_showast(ast.model)
puts "---------------"

require 'puppet-languageserver/manifest/manifest_inferencer'
inferrer = PuppetLanguageServer::Manifest::ManifestInferencer::ManifestInferences.new(PuppetLanguageServer::PuppetHelper.cache)
inferrer.debug = true
inferrer.infer(ast)
puts "---- INFERRED"
#require 'pry'; binding.pry
puts inferrer.inferences

puts "---- ACTUAL"
begin
  vars = evaluate_ast_to_vars(ast)
  vars.each { |k, v| puts "#{k} is a #{v.name}  (#{v})" }
rescue Puppet::PreformattedError => e
  puts e.message
end

# plan something::setup
# (
#   STRING[1] $deployment_name,
#   STRING[1] $master_host,
#   STRING[1] $console_password,
#   ARRAY[STRING[1]] $dns_alt_names,
#   STRING[1] $version,
#   OPTIONAL[STRING] $ssh_user = undef,
#   OPTIONAL[STRING] $inventory_file = "${deployment_name}.yaml",
#   OPTIONAL[STRING] $puppetdb_database_host = undef,
#   OPTIONAL[STRING] $puppetdb_database_replica_host = undef,
#   OPTIONAL[STRING] $master_replica_host = undef,
#   OPTIONAL[ARRAY[STRING]] $compiler_hosts = undef,
#   OPTIONAL[STRING] $compiler_pool_address = undef,
# )
# {
#   # Target self
#   $target = get_target('local://localhost')
#   # Create the bolt inventory file to use with the deployment
#   # If no file is specified, then the default will be used: ~/.puppetlabs/bolt/inventory.yaml
#   out::message('Building Bolt Inventory File')
#   run_task('something::bolt_inventory_add', $target,
#     group_name                     => $deployment_name,
#     inventory_file                 => $inventory_file,
#     ssh_user                       => $ssh_user,
#     master_host                    => $master_host,
#     master_replica_host            => $master_replica_host,
#     puppetdb_database_host         => $puppetdb_database_host,
#     puppetdb_database_replica_host => $puppetdb_database_replica_host,
#     compiler_hosts                 => $compiler_hosts
#   )
#   # Create the params needed for the peadm plan
#   out::message('Building PEAdm Params File')
#   run_task('something::peadm_params', $target,
#     deployment_name                 => $deployment_name,
#     master_host                     => $master_host,
#     master_replica_host             => $master_replica_host,
#     puppetdb_database_host          => $puppetdb_database_host,
#     puppetdb_database_replica_host  => $puppetdb_database_host,
#     console_password                => $console_password,
#     dns_alt_names                   => $dns_alt_names,
#     version                         => $version,
#     ssh_user                        => $ssh_user,
#     compiler_hosts                  => $compiler_hosts,
#     compiler_pool_address           => $compiler_pool_address
#   )
#   # Build a list os servers in this deployment
#   $temp_list = unique(concat(
#     [$master_host],
#     $compiler_hosts,
#     [$puppetdb_database_host],
#     [$puppetdb_database_replica_host],
#     [strip($master_replica_host)]
#   ))
#   $server_list = $temp_list.filter |$item| {
#     unless empty($item) {
#       $item =~ NotUndef
#     }
#   }
#   notice("Server list is: ${join($server_list,', ')}")
#   notice("Server list length is: ${length($server_list)}")
#   # Manage name resolution via /etc/hosts
#   # This may not be needed in the final version, but for now...
#   out::message('Updating host file name resolution.')
#   $server_list.each |$server| {
#     $output = run_command('hostname -I', $server)
#     $ip = strip($output.first['stdout'])
#     notice("IP for ${server} is ${ip}")
#     notice("Server list is: ${join($server_list,', ')}")
#     run_task('something::update_hosts',
#       $server_list,
#       'ip'             => $ip,
#       'fqdn'           => $server,
#     )
#   }
