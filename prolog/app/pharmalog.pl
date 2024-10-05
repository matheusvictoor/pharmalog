:- consult('controllers/menu_controller.pl').
:- use_module(chat_service).
:- use_module('ClientService').
:- use_module('ProductService').
:- use_module('SaleService').
:- use_module('RelatorioProduct').
:- use_module('SellerService').
:- use_module('user_service').
:- use_module(library(readutil)).
:- use_module(library(threads)).

main :-
    setup_files(['userDB.pl', 'productDB.pl', 'customerDB.pl', 'saleDB.pl', 'chatDB.pl']),
    message_channel(ChatChannel),
    program_loop(ChatChannel).


setup_files([]).
setup_files([File | Rest]) :-
    (exists_file(File) -> true ; open(File, write, Stream), close(Stream)),
    setup_files(Rest).


message_channel(ChatChannel) :-
    message_queue_create(ChatChannel),
    thread_create(read_messages(ChatChannel), _, []).


read_messages(ChatChannel) :-
    repeat,
    thread_get_message(ChatChannel, Message),
    save_message(Message).


save_message(message(Sender, Content)) :-
    open('chatDB.pl', append, Stream),
    format(Stream, '~w: ~w~n', [Sender, Content]),
    close(Stream).


program_loop(ChatChannel) :-
    menu(Option),
    handle_option(Option, ChatChannel).

handle_option(1, ChatChannel) :- menu_user, continue(ChatChannel).
handle_option(2, ChatChannel) :- menu_product, continue(ChatChannel).
handle_option(3, ChatChannel) :- menu_sale, continue(ChatChannel).
handle_option(4, ChatChannel) :- menu_client, continue(ChatChannel).
handle_option(5, ChatChannel) :- start_chat(ChatChannel), continue(ChatChannel).
handle_option(6, ChatChannel) :- menu_relatorio, continue(ChatChannel).
handle_option(0, _) :- writeln('Encerrando o programa...').
handle_option(_, ChatChannel) :-
    writeln('Opção inválida. Tente novamente.'),
    continue(ChatChannel).

continue(ChatChannel) :- program_loop(ChatChannel).

