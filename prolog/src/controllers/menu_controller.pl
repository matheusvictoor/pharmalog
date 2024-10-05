:- consult('../services/user_service.pl').
:- consult('../services/chat_service.pl').
:- consult('../services/ProductService.pl'). 
:- consult('../assets/menu_layout.pl').
:- load_chat.

menu :-
  menu_layout,
  nl,
  write("Escolha uma opção: "),
  read_line_to_string(user_input, Option),  % Usando read_line_to_string para consistência
  handle_menu_option(Option).

handle_menu_option("1") :- 
  menu_user,  % Chamando o menu de usuários
  menu.

handle_menu_option("2") :- 
  menu_product,  % Chamando o menu de produtos
  menu.

handle_menu_option("5") :-
  start_chat,  % Chamando o chat
  menu.

handle_menu_option("0") :- 
  writeln("Encerrando o programa..."), halt.

handle_menu_option(_) :- 
  writeln("Opção inválida. Tente novamente."),
  menu.
