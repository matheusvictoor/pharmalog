:- consult('../services/user_service.pl').
:- consult('../services/chat_service.pl').
:- consult('../services/ProductService.pl').
:- consult('../services/product_service.pl').
:- consult('../assets/menu_layout.pl').
:- consult('../assets/user_layout.pl').
:- consult('../assets/product_layout.pl').
:- consult('../assets/sales_layout.pl').
:- consult('../assets/client_layout.pl').
:- consult('../assets/report_layout.pl').
:- consult('../assets/chat_layout.pl').

:- load_chat.

menu :-
  menu_layout,
  nl,
  write("Escolha uma opção: "),
  read(Option),
  handle_menu_option(Option).

handle_menu_option(1) :- 
  user_layout,
  menu_user,
  menu.

handle_menu_option(2) :-
  product_layout,
  menu_product,
  menu.

handle_menu_option(3) :-
  sales_layout,
  menu_sale,
  menu.

handle_menu_option(4) :-
  client_layout,
   menu_client,
   menu.

handle_menu_option(5) :-
  chat_layout,
  start_chat,
  menu.

handle_menu_option(0) :- 
  writeln("Encerrando o programa..."), halt.

handle_menu_option(_) :- 
  writeln("Opção inválida. Tente novamente."),
  menu.