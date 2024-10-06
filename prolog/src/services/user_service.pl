:- consult('../models/user.pl').
:- consult('../assets/user_layout.pl').

menu_user :-
  user_layout,
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
    % write("\n*********** Usuário cadastrado com sucesso! **********"), nl, nl,
    exibir_mensagem_formatada('Usuário cadastrado com sucesso!'), nl, nl,
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

exibir_mensagem_formatada(Mensagem) :-
    nl,
    format('~`-t~168|~n'),
    string_length(Mensagem, TamMsg),
    EspacoTotal is 168 - TamMsg - 2,
    EspacoEsquerda is EspacoTotal // 2,
    EspacoDireita is EspacoTotal - EspacoEsquerda,
    format('~` t~*|~w~` t~*|~n', [EspacoEsquerda, Mensagem, EspacoDireita]),
    format('~`-t~168|~n'),
    nl. % Pula uma linha após a mensagem
