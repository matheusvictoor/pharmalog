:- module(client_service, [
    create_client/0, 
    get_all_clients/1, 
    get_client_by_cpf/2, 
    update_client/1, 
    delete_client/1, 
    add_sale_to_client/2, 
    view_client_info/1, 
    menu_client/0
]).

:- use_module(library(readutil)).
:- use_module(library(lists)).
:- use_module(client).  
:- use_module(product).  
:- use_module(sale).

create_client :-
    write('Nome: '), flush_output, read_line_to_string(user_input, Name),
    write('Idade: '), flush_output, read_line_to_string(user_input, AgeStr), number_string(Age, AgeStr),
    write('Endereço: '), flush_output, read_line_to_string(user_input, Address),
    write('CPF: '), flush_output, read_line_to_string(user_input, CPF),
    write('Telefone: '), flush_output, read_line_to_string(user_input, Phone),
    Client = client(Name, Age, CPF, Address, Phone, []),
    open('customerDB.pl', append, Stream),
    write(Stream, Client), write(Stream, '.'), nl(Stream),
    close(Stream),
    write('** Cliente cadastrado com sucesso! **'), nl.

get_all_clients(Clients) :-
    open('customerDB.pl', read, Stream),
    read_terms(Stream, Clients),
    close(Stream).

get_client_by_cpf(CPF, Client) :-
    get_all_clients(Clients),
    member(Client, Clients),
    client_cpf(Client, CPF).

view_client_info(CPF) :-
    get_client_by_cpf(CPF, Client),
    format('Informações do Cliente: ~w~n', [Client]).

update_client(CPF) :-
    get_all_clients(Clients),
    maplist(update_if_found(CPF), Clients, UpdatedClients),
    open('customerDB.pl', write, Stream),
    write_terms(Stream, UpdatedClients),
    close(Stream),
    write('** Cliente atualizado com sucesso! **'), nl.

update_if_found(CPF, Client, UpdatedClient) :-
    client_cpf(Client, CPF), !,
    write('Novo Nome: '), flush_output, read_line_to_string(user_input, NewName),
    write('Nova Idade: '), flush_output, read_line_to_string(user_input, NewAgeStr), number_string(NewAge, NewAgeStr),
    write('Novo Endereço: '), flush_output, read_line_to_string(user_input, NewAddress),
    write('Novo Telefone: '), flush_output, read_line_to_string(user_input, NewPhone),
    UpdatedClient = client(NewName, NewAge, CPF, NewAddress, NewPhone, Sales).
update_if_found(_, Client, Client).

delete_client(CPF) :-
    get_all_clients(Clients),
    exclude(client_cpf_match(CPF), Clients, FilteredClients),
    open('customerDB.pl', write, Stream),
    write_terms(Stream, FilteredClients),
    close(Stream),
    write('** Cliente deletado com sucesso! **'), nl.

client_cpf_match(CPF, client(_, _, CPF, _, _, _)).

add_sale_to_client(CPF, NewSale) :-
    get_all_clients(Clients),
    maplist(add_sale(CPF, NewSale), Clients, UpdatedClients),
    open('customerDB.pl', write, Stream),
    write_terms(Stream, UpdatedClients),
    close(Stream),
    write('** Venda adicionada ao cliente com sucesso! **'), nl.

add_sale(CPF, NewSale, client(Name, Age, CPF, Address, Phone, Sales), client(Name, Age, CPF, Address, Phone, [NewSale|Sales])).
add_sale(_, _, Client, Client).

n_sales_client(Client, N) :-
    client_sales(Client, Sales),
    length(Sales, N).

level_client(Client, Level) :-
    n_sales_client(Client, N),
    (N >= 50 -> Level = 'Cliente nível ouro!';
     N >= 25 -> Level = 'Cliente nível prata!';
     Level = 'Cliente nível bronze!').

relatory_product(product(Name, Description, Category, DateManufacture, ExpirationDate, Price, Stock), Report) :-
    format(atom(Report), '~w: Descrição = ~w, Categoria = ~w, Data de Produção = ~w, Data de Validade = ~w, Preço = ~2f, Estoque = ~d', 
           [Name, Description, Category, DateManufacture, ExpirationDate, Price, Stock]).

relatory_product_client(Client, Report) :-
    client_sales(Client, Sales),
    maplist(relatory_product, Sales, Reports),
    atomic_list_concat(Reports, '\n', Report).

menu_client :-
    write('\nSelecione uma opção:\n1.  Cadastrar um novo cliente\n2.  Buscar um cliente por CPF\n3.  Buscar todos os clientes\n4.  Atualizar um cliente\n5.  Deletar um cliente\n0 <- Voltar\n'),
    write('\nOpção -> '), flush_output,
    read_line_to_string(user_input, Option),
    menu_option(Option).

menu_option("1") :- create_client.
menu_option("2") :- write('CPF: '), flush_output, read_line_to_string(user_input, CPF), view_client_info(CPF).
menu_option("3") :- get_all_clients(Clients), maplist(write, Clients).
menu_option("4") :- write('CPF: '), flush_output, read_line_to_string(user_input, CPF), update_client(CPF).
menu_option("5") :- write('CPF: '), flush_output, read_line_to_string(user_input, CPF), delete_client(CPF).
menu_option("0") :- write('\n<---\n').
menu_option(_) :- write('Opção inválida. Tente novamente.\n'), menu_client.
