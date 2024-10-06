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

:- use_module('../models/Client').  
:- use_module(library(readutil)).
:- use_module(library(lists)).

valid_cpf(CPF) :-
    string_length(CPF, 11),
    string_chars(CPF, Digits),
    maplist(is_digit, Digits).

is_digit(Char) :- char_type(Char, digit).

create_client :-
    write('Nome: '), flush_output, read_line_to_string(user_input, Name),
    write('Idade: '), flush_output, read_line_to_string(user_input, AgeStr), number_string(Age, AgeStr),
    write('Endereço: '), flush_output, read_line_to_string(user_input, Address),
    write('CPF (somente números, 11 dígitos): '), flush_output, read_line_to_string(user_input, CPF),
    (   \+ valid_cpf(CPF) -> 
        writeln('** CPF inválido! O CPF deve ter 11 dígitos e conter apenas números. **'),
        create_client
    ;   client_exists(CPF) -> 
        writeln('** CPF já cadastrado! **'),
        create_client
    ;   write('Telefone: '), flush_output, read_line_to_string(user_input, Phone),
        Client = client(Name, Age, CPF, Address, Phone, []),
        open('customerDB.pl', append, Stream),
        write(Stream, Client), write(Stream, '.'), nl(Stream),
        close(Stream),
        write('** Cliente cadastrado com sucesso! **'), nl
    ).

client_exists(CPF) :-
    get_all_clients(Clients),
    member(client(_, _, CPF, _, _, _), Clients).

get_all_clients(Clients) :-
    open('customerDB.pl', read, Stream),
    read_terms(Stream, Clients),
    close(Stream).

get_client_by_cpf(CPF, Client) :-
    (   valid_cpf(CPF)
    ->  get_all_clients(Clients),
        member(Client, Clients),
        client_cpf(Client, CPF)
    ;   writeln('** CPF inválido! **')
    ).

view_client_info(CPF) :-
    (   valid_cpf(CPF)
    ->  (   get_client_by_cpf(CPF, Client)
        ->  format('Informações do Cliente: ~w~n', [Client])
        ;   writeln('** Cliente não encontrado! **')
        )
    ;   writeln('** CPF inválido! **')
    ).

update_client(CPF) :-
    (   valid_cpf(CPF)
    ->  (   get_client_by_cpf(CPF, Client)
        ->  get_all_clients(Clients),
            maplist(update_if_found(CPF), Clients, UpdatedClients),
            open('customerDB.pl', write, Stream),
            write_terms(Stream, UpdatedClients),
            close(Stream),
            write('** Cliente atualizado com sucesso! **'), nl
        ;   writeln('** Cliente não encontrado! **')
        )
    ;   writeln('** CPF inválido! **')
    ).

update_if_found(CPF, client(_, _, CPF, _, _, Sales), UpdatedClient) :-
    write('Novo Nome: '), flush_output, read_line_to_string(user_input, NewName),
    write('Nova Idade: '), flush_output, read_line_to_string(user_input, NewAgeStr), number_string(NewAge, NewAgeStr),
    write('Novo Endereço: '), flush_output, read_line_to_string(user_input, NewAddress),
    write('Novo Telefone: '), flush_output, read_line_to_string(user_input, NewPhone),
    UpdatedClient = client(NewName, NewAge, CPF, NewAddress, NewPhone, Sales).
update_if_found(_, Client, Client).

delete_client(CPF) :-
    (   valid_cpf(CPF)
    ->  get_all_clients(Clients),
        exclude(client_cpf_match(CPF), Clients, FilteredClients),
        open('customerDB.pl', write, Stream),
        write_terms(Stream, FilteredClients),
        close(Stream),
        write('** Cliente deletado com sucesso! **'), nl
    ;   writeln('** CPF inválido! **')
    ).

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

menu_client :-
    writeln("\nMenu de Clientes"),
    writeln("1. Cadastrar um novo cliente"),
    writeln("2. Buscar um cliente por CPF"),
    writeln("3. Listar todos os clientes"),
    writeln("4. Atualizar um cliente"),
    writeln("5. Deletar um cliente"),
    writeln("0. Voltar"),
    write("\nOpção -> "), flush_output,
    read_line_to_string(user_input, Option),
    handle_client_menu_option(Option).

handle_client_menu_option("1") :- create_client.
handle_client_menu_option("2") :- write('CPF: '), flush_output, read_line_to_string(user_input, CPF), view_client_info(CPF).
handle_client_menu_option("3") :- get_all_clients(Clients), maplist(writeln, Clients).
handle_client_menu_option("4") :- write('CPF: '), flush_output, read_line_to_string(user_input, CPF), update_client(CPF).
handle_client_menu_option("5") :- write('CPF: '), flush_output, read_line_to_string(user_input, CPF), delete_client(CPF).
handle_client_menu_option("0") :- writeln('\n<--- Voltando ao Menu Principal').
handle_client_menu_option(_) :- writeln('Opção inválida. Tente novamente.\n'), menu_client.
