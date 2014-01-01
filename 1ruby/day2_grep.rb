abort("USAGE: filename phrase") if ARGV.length < 2

fn = ARGV[0]
phrase = Regexp.new(ARGV.drop(1).join(" "))

# Open the file and print each line that matches the phrase
line_num = 1
File.open(fn, 'r').each do |line|
	puts line_num.to_s + "-- " + line if phrase.match(line)
	line_num += 1
end