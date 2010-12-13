#!/usr/bin/env ruby

# Remove duplicate entries  (with the same ID) in a  .bib file.  It is
# assumed that each entry begins with @ in the zero column followed by
# the entity  name, an opening  curly brace (possibly padded  by white
# space),  and the entry's  ID followed  by comma;  and ends  with the
# closing curly brace in the zero column alone on a separate line.

seen = Hash.new(false)

while line = gets do
  if line =~ /^@[a-zA-Z]+\s*{\s*([a-zA-Z0-9]+),/
    if seen[$1]
      line = gets until line =~ /^}\s*$/
    else
      seen[$1] = true
      puts line
      puts (line = gets) until line =~ /^}\s*$/
      puts
    end
  end
end
