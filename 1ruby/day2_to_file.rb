# Array of 16 numbers
a = (1..16).to_a

# Print 4 at a time
(1..a.length).each do |i|
	print "#{a[i]} " unless (i + 1) % 4 == 0
	print "#{a[i]}\n" if (i + 1) % 4 == 0
end