// 8 - The ubiquitous number guessing game

secret := (Random value(99) + 1) floor

// Create a handle for standard in to get guesses from
stdin := File standardInput

// Give the user ten guesses
10 repeat(
	"Guess a number: " print
	guess := stdin readLine asNumber;

	// Check if the guess is correct
	if(guess == secret, break, "Wrong" println);

	// Otherwise, give a hint
	if(hasSlot("prevGuess"), 
		if((guess - secret) abs < (prevGuess - secret) abs,
			"Hotter" println, 
			"Colder" println
		)
	)
	prevGuess := guess
);

// Check the result of the game
if(guess == secret, 
    "You guessed the secret Number!" println, 
    ("That's too bad! The secret number was " .. secret) println
)