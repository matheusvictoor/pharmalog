:- consult('../services/user_service.pl').

menu :-
  writeln("\n******************************************************"),
  writeln("*                                                    *"),
  writeln("*           Pharmalog - Controle de Farmácia         *"),
  writeln("*                   Versão 1.0 (PROLOG)              *"),
  writeln("*                                                    *"),
  writeln("******************************************************"),
  writeln("********************** Bem-Vindo *********************"),
  writeln("*                                                    *"),
  writeln("*                 🌟 1. Usuários                     *"),
  writeln("*                 📦 2. Produtos                     *"),
  writeln("*                 💰 3. Vendas                       *"),
  writeln("*                 👥 4. Clientes                     *"),
  writeln("*                 💬 5. Chat                         *"),
  writeln("*                 📊 6. Relatórios de Produtos       *"),
  writeln("*                                                    *"),
  writeln("*                 ❌ 0 <- Sair                       *"),
  writeln("*                                                    *"),
  writeln("******************************************************"),
  nl,
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
