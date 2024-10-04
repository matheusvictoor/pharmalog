:- consult('../services/user_service.pl').

menu :-
  writeln("\nPharmalog - Seu sistema de controle de farmácia (v1.0)"),
  writeln("******************************************************"),
  writeln("********************** Bem-Vindo *********************\n"),
  writeln("1. Usuários"),
  writeln("2. Produtos"),
  writeln("3. Vendas"),
  writeln("4. Clientes"),
  writeln("5. Chat"),
  writeln("6. Relatórios de produtos"),
  writeln("0 <- Sair\n"),
  write("Escolha uma opção: "),
  read(Option),
  handle_menu_option(Option).

handle_menu_option(1) :- 
  menu_user,
  menu.

handle_menu_option(0) :- 
  writeln("Encerrando o programa...").

handle_menu_option(_) :- 
  writeln("Opção inválida. Tente novamente."),
  menu.
