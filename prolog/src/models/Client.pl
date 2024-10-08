:- dynamic client/6.

save_client :-
    tell('../data/customerDB.pl'),
    listing(client),
    told.

create_client(Name, Age, CPF, Address, Phone, Sales). :-
    assertz(client(Name, Age, CPF, Address, Phone, Sales)),
    save_client.

client_exists_name(Name) :-
    list_all_clients(Clients),
    member(client(Name, _, _, _, _, _), Clients).

client_exists_cpf(CPF) :-
    list_all_clients(Clients),
    member(product(_, _, CPF, _, _,_), Clients).

get_product_by_cpf(CPF, Client) :-
    list_all_clients(Clients),
    member(Client, Clients),
    Client = client(_, _, CPF, _, _, _).

delete_product(CPF) :-
    list_all_clients(Clients),
    member(Client, Clients),
    Client = client(_, _, CPF, _, _, _),
    retract(Client),
    save_client.

update_client(Name, Age, CPF, Address, Phone, Sales) :-
    list_all_clients(Clients),
    member(Client, Clients),
    OldClient = client(_, _, CPF, _, _, _),
    retract(OldClient),
    assertz(product(_, _, CPF, _, _, _)),
    save_client.

list_all_clients(Clients) :-
    findall(client(Name, Age, CPF, Address, Phone, Sales),
            client(Name, Age, CPF, Address, Phone, Sales),
            Clients).

print_clients([]).
print_clients([client(Name, Age, CPF, Address, Phone, Sales) | T]) :-
    format('Nome: ~w\nIdade: ~w\nEndereço: ~w\nTelefone: ~w\nVendas: ~d\n----------------------------------------\n',
           [Name, Age, CPF, Address, Phone, Sales]),
    print_clients(T).
