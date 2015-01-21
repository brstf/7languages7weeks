-module(day2).
-export([get_value/2]).
-export([total_price/1]).

% Given a list of keyword-value tuples and a keyword, return the corresponding
% value in the list
get_value([], _)      -> error;
get_value([H | T], Keyword) -> 
    case H of 
        {Keyword, Value} -> Value;
        _ -> get_value(T, Keyword)
    end.

% Given a shopping list of the form
% [ {item, quantity, price}, ... ]
% write a list comprehension that returns a list of items in the form
% [ {item, total_price}, ... ]
total_price(L) -> [{Item, Price * Quantity} || {Item, Quantity, Price} <- L].