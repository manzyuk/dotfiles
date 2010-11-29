begin
  require 'wirble'
  Wirble.init :skip_prompt => true 
rescue LoadError => err
  $stderr.puts "Couldn't load Wirble: #{err}"
end
