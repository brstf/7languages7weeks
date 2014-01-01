hidden_number = rand(10)

puts "Guess a number 0..9:\n"
guess = gets.to_i
until guess == hidden_number
	puts "Too low" if guess < hidden_number
	puts "Too high" if guess > hidden_number
	guess = gets.to_i
end
puts "Correct! The hidden number was #{hidden_number}"