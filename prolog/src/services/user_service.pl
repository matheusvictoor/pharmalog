:- consult('../models/user.pl').

menu_user :-
  writeln("\nSelecione uma opção:"),
  writeln("1 - Cadastrar um novo usuário"),
  writeln("2 - Buscar um usuário por CPF"),
  writeln("3 - Listar todos os usuários"),
  writeln("4 - Atualizar um usuário"),
  writeln("5 - Deletar um usuário"),
  writeln("0 <- Voltar"),
  write("Opção: "),
  read(Option),
  handle_user_option(Option).

handle_user_option(1) :-
  writeln("CPF: "), read(CPF),
  ( user_exists_cpf(CPF) -> 
    writeln("\n** CPF já cadastrado! **"), menu_user
  ;
    writeln("Nome: "), read(Name),
    writeln("Senha: "), read(Password),
    writeln("Função (administrador | gerente | vendedor): "), read(Role),
    create_user(Name, CPF, Password, Role),  % Corrigido a ordem dos argumentos
    menu_user
  ).

handle_user_option(2) :-
  writeln("CPF do usuário para buscar: "), read(CPF),
  ( user_exists_cpf(CPF) ->
    get_user_by_cpf(CPF, User),
    writeln(User)
  ;
    writeln("\n** Usuário não encontrado! **")
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
    update_user(CPF, NewPassword, NewRole),
    writeln('** Usuário atualizado com sucesso! **')
  ;
    writeln("\n** Usuário não encontrado! **")
  ),
  menu_user.

handle_user_option(5) :-
  writeln("CPF do usuário para deletar: "), read(CPF),
  ( user_exists_cpf(CPF) ->
    delete_user(CPF),
    writeln('** Usuário excluído com sucesso! **')
  ;
    writeln("\n** Usuário não encontrado! **")
  ),
  menu_user.

handle_user_option(0) :-
  writeln("\n<--- Voltando ao Menu Principal").

handle_user_option(_) :-
  writeln("Opção inválida. Tente novamente."),
  menu_user.