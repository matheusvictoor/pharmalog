:- module(client, [client/6, show_client/1]).

client(Name, Age, CPF, Address, Phone, Sales).

show_client(client(Name, Age, CPF, Address, Phone, Sales)) :-
    format('Client Name: ~w~n', [Name]),
    format('Age: ~d~n', [Age]),
    format('CPF: ~w~n', [CPF]),
    format('Address: ~w~n', [Address]),
    format('Phone: ~w~n', [Phone]),
    format('Sales: ~w~n', [Sales]).
