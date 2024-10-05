:- module(message, [message/2, show_message/1]).

message(Sender, Content).

show_message(message(Sender, Content)) :-
    format('Sender: ~w~n', [Sender]),
    format('Content: ~w~n', [Content]).
