:- module(chat_service, [start_chat/1]).

:- use_module(library(threads)).
:- use_module(library(readutil)).
:- use_module(message).  

start_chat(ChatChannel) :-
    write('\n**************************** Bem-vindo ao Chat da Pharmalog **********************************\n'),
    write('\nDigite seu nome, cliente: '), flush_output,
    read_line_to_string(user_input, ClientName),
    write('\nCliente, digite "sair" para encerrar o chat!\n'),
    write('\n*************************** Chat iniciado, digite sua mensagem ********************************\n'),
    simule_chat(ClientName, ChatChannel).

simule_chat(ClientName, ChatChannel) :-
    thread_self(ThreadId),
    atom_string(ThreadId, ClientId),
    chat_loop(ClientName, ClientId, ChatChannel).

chat_loop(ClientName, ClientId, ChatChannel) :-
    format('~w: ', [ClientName]), flush_output,
    read_line_to_string(user_input, UserInput),
    (   UserInput = "sair" ->
        write('\n****************************** Chat encerrado pelo cliente! ***********************************\n')
    ;   Message = message(ClientName, UserInput),
        thread_send_message(ChatChannel, Message),
        write('\n                                                          Atendente: '), flush_output,
        read_line_to_string(user_input, AttendantResponse),
        AttendantMessage = message("Atendente", AttendantResponse),
        thread_send_message(ChatChannel, AttendantMessage),
        chat_loop(ClientName, ClientId, ChatChannel)
    ).
