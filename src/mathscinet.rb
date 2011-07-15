#!/usr/bin/env ruby

# Usage: ruby mathscinet.rb name1 name2 ...
#
# Retrieve BibTeX items for all publications of authors matching
# name1, name2, ...  from the MathSciNet database.

require 'open-uri'

FORM  = /<form name="batchDownload" action="\/mathscinet\/search\/publications\.html">(.*?)<\/form>/m
INPUT = /<input.*?\/>/m
NAME  = /name=\"(.*?)\"/
VALUE = /value=\"(.*?)\"/
PRE   = /<pre>.*?<\/pre>/m
BIB   = /<pre>(.*?)<\/pre>/m

# Retrieve contents of URL as string.
def page(url)
  open(url) {|response| response.read}
end

ARGV.each do |name|
  pubURL = "http://www.ams.org/mathscinet/search/publications.html?" \
  + "co4=AND&co5=AND&co6=AND&co7=AND&dr=all&pg4=AUCN&pg5=TI&pg6=PC&" \
  + "pg7=ALLF&pg8=ET&review_format=html&s4=#{name.downcase}&s8=All&" \
  + "vfpref=html&yrop=eq&r=1&extend=1"
  if page(pubURL) =~ FORM
    inputs = $1.scan(INPUT).select {|match| match =~ NAME && match =~ VALUE}
    params = inputs.map {|tag| [tag.match(NAME)[1], tag.match(VALUE)[1]]}
    params << ['fmt', 'bibtex']
    bibURL = 'http://www.ams.org/mathscinet/search/publications.html?' \
    + params.map {|p| "#{URI.escape(p[0])}=#{URI.escape(p[1])}"}.join('&')
    page(bibURL).scan(PRE).each {|tag| print tag.match(BIB)[1].strip, "\n\n"}
  end
end
