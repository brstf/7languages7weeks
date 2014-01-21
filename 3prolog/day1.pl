% Represent some books and authors
author('Leviathan Wakes', 'James S. A. Corey').
author('Calibans War', 'James S. A. Corey').
author('Abaddons Gate', 'James S. A. Corey').
author('Casino Royale', 'Ian Fleming').
author('One Second After', 'William R. Forstchen').
author('Cooking For Geeks', 'Jeff Potter').
author('Design Patterns', 'Erich Gamma').
author('Design Patterns', 'Richard Helm').
author('Design Patterns', 'Ralph Johnson').
author('Design Patterns', 'John Vlissides').

% Find all books by a certain author:
% | ? - author(Book, 'James S. A. Corey').

% Make a knowledge base representing musicians and instruments
musician('John Mitchell', guitar).
musician('Jem Godfrey', keyboard).
musician('Nathan King', bass).
musician('Craig Blundell', drums).
musician('Steven Wilson', guitar).
musician('Steven Wilson', keyboard).
musician('Gavin Harrison', drums).
musician('Matthew Bellamy', guitar).
musician('Christopher Wolstenhome', bass).
musician('Dominic Howard', drums).
musician('Morgan Nicholis', keyboard).
musician('Morgan Nicholis', guitar).
genre('Jem Godfrey', progressive).
genre('Matthew Bellamy', rock).
genre('Steven Wilson', progressive).
genre('Steven Wilson', rock).

% Find all musicians that play the guitar
% | ? - musician(Who, guitar).
