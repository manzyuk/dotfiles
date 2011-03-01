#!/usr/bin/env ruby

# Usage: ruby translate.rb text
#
# Translate text from English to Russian using Google Translate.

require 'open-uri'
require 'json'

# Retrieve contents of URL as UTF-8 encoded string.  Google Translate
# won't allow us to make a request unless we send a "User-Agent"
# header it recognizes, e.g. "Mozilla/4.0".
def page(url)
  open(url, "User-Agent" => "Mozilla/4.0") {|response| response.read.encode("UTF-8")}
end

def translate(text, sl="en", tl="ru")
  baseURL     = "http://translate.google.com/translate_a/t"
  parameters  = [["client", "t"], ["text", text], ["sl", sl], ["tl", tl]]
  requestURL  = baseURL + "?" + parameters.map {|p| "#{p[0]}=#{URI.escape(p[1])}"}.join("&")
  # Deal with invalid (obfuscated?) JSON that Google sends us back.
  contents    = page(requestURL).gsub(/,(?=[\],])/, ",null")
  json        = JSON.parse(contents)
  dictionary  = json[1]
  translation = json[0][0][0]
  if dictionary
    dictionary.each do |item|
      puts item[0]
      item[1].each_with_index do |translation, index|
        printf("%2d. %s\n", index+1, translation)
      end
      puts
    end
  else
    puts translation
  end
end

if ARGV.size == 0
  ARGF.each {|line| translate(line)}
elsif ARGV.size == 1
  translate(ARGV[0])
else
  puts "Usage: translate TEXT"
end
