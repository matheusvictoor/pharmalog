:- consult('../models/message.pl').
:- consult('../assets/chat_layout.pl').
:- consult('user_service.pl').
:- dynamic message/3.
:- use_module(library(random)).

start_chat :- 
  chat_layout,
  nl,
  simulate_chat.

simulate_chat :-
  new_message_id(ID),
  read_line_to_string(user_input, _),
  chat_loop(ID).

new_message_id(ID) :-
  random_between(1, 10000, ID).

chat_loop(ID) :-
  write('\n* Cliente: '),
  flush_output,
  read_line_to_string(user_input, ClientMessage),
  
  ( ClientMessage == "sair" -> 
      exibir_mensagem_formatada('Chat encerrado pelo cliente!'),
      display_messages(ID)
    ;
      assertz(message('Cliente', ClientMessage, ID)),
      save_chat([message('Cliente', ClientMessage, ID)]),
      nl,
      write('Atendente: '),
      flush_output,
      read_line_to_string(user_input, AttendantResponse),
      assertz(message('Atendente', AttendantResponse, ID)),
      save_chat([message('Atendente', AttendantResponse, ID)]),
      chat_loop(ID)
  ).

display_messages(ID) :-
  format('\n------------------------------------------------------------------ Histórico de Mensagens (ID: ~w) -------------------------------------------------------------------\n', [ID]), nl,
  forall(message(Sender, Content, ID),
    (format('~w: ~s~n', [Sender, Content]))),
  aguardar_enter.

save_chat(NewMessages) :-
  open('../data/chatDB.pl', append, Stream),
  forall(member(message(Sender, Content, ID), NewMessages),
          format(Stream, 'message(~q, ~q, ~q).~n', [Sender, Content, ID])),
  close(Stream).

load_chat :-
  exists_file('../data/chatDB.pl'),
  consult('../data/chatDB.pl').
