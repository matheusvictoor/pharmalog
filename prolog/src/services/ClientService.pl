:- module(client_service, [
    create_client/0, 
    get_all_clients/0, 
    get_client_by_cpf/0, 
    update_client/0, 
    delete_client/0, 
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
    get_char(_),  
    write('Nome: '), flush_output, read_line_to_string(user_input, Name),
    write('Idade: '), flush_output, read_line_to_string(user_input, AgeStr), 
    ( catch(number_string(Age, AgeStr), _, fail) -> true ; writeln('** Idade inválida! **'), fail),
    write('Endereço: '), flush_output, read_line_to_string(user_input, Address),
    write('CPF (somente números, 11 dígitos): '), flush_output, read_line_to_string(user_input, CPF),
    (   \+ valid_cpf(CPF) -> 
        writeln('** CPF inválido! O CPF deve ter 11 dígitos e conter apenas números. **'), fail
    ;   client_exists(CPF) -> 
        writeln('** CPF já cadastrado! **'), fail
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

get_all_clients :-
    open('customerDB.pl', read, Stream),
    read_terms(Stream, Clients),
    close(Stream),
    print_clients(Clients).

get_client_by_cpf :-
    write('Digite o CPF do Cliente: '), flush_output, read_line_to_string(user_input, CPF),
    (   valid_cpf(CPF)
    ->  get_all_clients(Clients),
        (   member(client(_, Name, Age, CPF, Address, Phone, _), Clients)
        ->  format('Nome: ~w | Idade: ~w | Endereço: ~w | Telefone: ~w~n', [Name, Age, Address, Phone])
        ;   writeln('** Cliente não encontrado! **')
        )
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

update_client :-
    write('Digite o CPF do Cliente para atualizar: '), flush_output, read_line_to_string(user_input, CPF),
    (   valid_cpf(CPF)
    ->  (   client_exists(CPF)
        ->  get_all_clients(Clients),
            maplist(update_if_found(CPF), Clients, UpdatedClients),
            open('customerDB.pl', write, Stream),
            write_terms(Stream, UpdatedClients),
            close(Stream),
            writeln('** Cliente atualizado com sucesso! **')
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

delete_client :-
    write('Digite o CPF do Cliente para deletar: '), flush_output, read_line_to_string(user_input, CPF),
    (   valid_cpf(CPF)
    ->  get_all_clients(Clients),
        exclude(client_cpf_match(CPF), Clients, FilteredClients),
        open('customerDB.pl', write, Stream),
        write_terms(Stream, FilteredClients),
        close(Stream),
        writeln('** Cliente deletado com sucesso! **')
    ;   writeln('** CPF inválido! **')
    ).

client_cpf_match(CPF, client(_, _, CPF, _, _, _)).

add_sale_to_client(CPF, NewSale) :-
    get_all_clients(Clients),
    maplist(add_sale(CPF, NewSale), Clients, UpdatedClients),
    open('customerDB.pl', write, Stream),
    write_terms(Stream, UpdatedClients),
    close(Stream),
    writeln('** Venda adicionada ao cliente com sucesso! **').

add_sale(CPF, NewSale, client(Name, Age, CPF, Address, Phone, Sales), client(Name, Age, CPF, Address, Phone, [NewSale|Sales])).
add_sale(_, _, Client, Client).

menu_client :- 
    client_layout, 
    nl,
    write("Escolha uma opção: "), flush_output,
    read(Option),
    handle_client_option(Option).

handle_client_option(1) :-
    create_client,
    menu_client.

handle_client_option(2) :-
    get_client_by_cpf,
    menu_client.

handle_client_option(3) :-
    get_all_clients,
    menu_client.

handle_client_option(4) :-
    update_client,
    menu_client.

handle_client_option(5) :-
    delete_client,
    menu_client.

handle_client_option(0) :-
    writeln("\nVoltando ao menu principal...").

handle_client_option(_) :-
    writeln("\n** Opção inválida. Tente novamente! **"),
    menu_client.

print_clients([]) :- writeln('Nenhum cliente cadastrado.').
print_clients([client(ID, Name, Age, CPF, Address, Phone, _) | Rest]) :-
    format('ID: ~w | Nome: ~w | Idade: ~w | CPF: ~w | Endereço: ~w | Telefone: ~w~n', [ID, Name, Age, CPF, Address, Phone]),
    print_clients(Rest).
