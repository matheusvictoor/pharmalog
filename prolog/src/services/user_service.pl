:- consult('../models/user.pl').

menu_user :-
  writeln("\n******************************************************"),
  writeln("*                                                    *"),
  writeln("*                Selecione uma opção                 *"),
  writeln("*                                                    *"),
  writeln("******************************************************"),
  writeln("*                                                    *"),
  writeln("*                1 - Cadastrar um novo usuário       *"),
  writeln("*                2 - Buscar um usuário por CPF       *"),
  writeln("*                3 - Listar todos os usuários        *"),
  writeln("*                4 - Atualizar um usuário            *"),
  writeln("*                5 - Deletar um usuário              *"),
  writeln("*                                                    *"),
  writeln("*                0 - Voltar                          *"),
  writeln("*                                                    *"),
  writeln("******************************************************"),
  nl,
  write("Escolha uma opção: "),
  read(Option), nl,
  handle_user_option(Option).

handle_user_option(1) :-
  writeln("\nCPF: "), read(CPF),
  ( user_exists_cpf(CPF) -> 
    writeln("\n** CPF já cadastrado! **"), menu_user
  ;
    writeln("Nome: "), read(Name),
    writeln("Senha: "), read(Password),
    writeln("Função (administrador | gerente | vendedor): "), read(Role),
    create_user(Name, CPF, Password, Role),
    write("\n*********** Usuário cadastrado com sucesso! **********"), nl, nl,
    menu_user
  ).

handle_user_option(2) :-
  writeln("CPF do usuário para buscar: "), read(CPF),
  ( user_exists_cpf(CPF) ->
    get_user_by_cpf(CPF, User), nl,
    writeln('*************** Informações do Usuário ***************'), nl,
    print_users(User)
  ;
    writeln("\n************** Usuário não encontrado! **************"), nl, nl
  ),
  menu_user.
  
handle_user_option(3) :-
  list_users,
  menu_user.

handle_user_option(4) :-
  writeln("CPF do usuário para atualizar: "), read(CPF),
  ( user_exists_cpf(CPF) ->
    writeln("Nova senha: "), read(NewPassword),
    writeln("Novo cargo (administrador | gerente | vendedor): "), read(NewRole),
    update_user(CPF, NewPassword, NewRole), nl,
    writeln('********** Usuário atualizado com sucesso! **********'), nl, nl
  ;
    writeln("\n************** Usuário não encontrado! **************")
  ),
  menu_user.

handle_user_option(5) :-
  writeln("CPF do usuário para deletar: "), read(CPF),
  ( user_exists_cpf(CPF) ->
    delete_user(CPF),
    writeln('*********** Usuário excluído com sucesso! ***********'), nl, nl
  ;
    writeln("\n************** Usuário não encontrado! **************"), nl, nl
  ),
  menu_user.

handle_user_option(0) :-
  writeln("\n<--- Voltando ao Menu Principal").

handle_user_option(_) :-
  writeln("Opção inválida. Tente novamente."),
  menu_user.