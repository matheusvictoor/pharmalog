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
  ( validar_cpf(CPF) ->
    ( user_exists_cpf(CPF) -> 
      exibir_mensagem_formatada("CPF já cadastrado!"),
      aguardar_enter,
      menu_user
    ;
      writeln("Nome: "), read(Name),
      writeln("Senha: "), read(Password),
      writeln("Função (administrador | gerente | vendedor): "), read(Role),
      create_user(Name, CPF, Password, Role),
      exibir_mensagem_formatada('✓ Usuário cadastrado com sucesso!'),
      aguardar_enter,
      menu_user
    )
;
  exibir_mensagem_formatada('✗ CPF inválido! Deve conter apenas números e ter 11 dígitos.'),
  aguardar_enter,
  menu_user
  ).

handle_user_option(2) :-
  writeln("CPF do usuário para buscar: "), read(CPF),
  ( user_exists_cpf(CPF) ->
    get_user_by_cpf(CPF, User), nl,
    exibir_mensagem_formatada('Informações do Usuário'), nl,
    print_users(User),
    aguardar_enter
  ;
    exibir_mensagem_formatada('✗ Usuário não encontrado!'), nl, nl,
    aguardar_enter
  ),
  menu_user.
  
handle_user_option(3) :-
  exibir_mensagem_formatada('Lista de usuarios'), nl, nl,
  list_users,
  aguardar_enter,
  menu_user.

handle_user_option(4) :-
  writeln("CPF do usuário para atualizar: "), read(CPF),
  ( user_exists_cpf(CPF) ->
    writeln("Nova senha: "), read(NewPassword),
    writeln("Novo cargo (administrador | gerente | vendedor): "), read(NewRole),
    update_user(CPF, NewPassword, NewRole), nl,
    exibir_mensagem_formatada('✓ Usuário atualizado com sucesso!'),
    aguardar_enter
  ;
    exibir_mensagem_formatada('✗ Usuário não encontrado!'),
    aguardar_enter
  ),
  menu_user.

handle_user_option(5) :-
  writeln("CPF do usuário para deletar: "), read(CPF),
  ( user_exists_cpf(CPF) ->
    delete_user(CPF),
    exibir_mensagem_formatada('✓ Usuário excluído com sucesso!'),
    aguardar_enter
  ;
    exibir_mensagem_formatada('✗ Usuário não encontrado!'), nl, nl,
    aguardar_enter
  ),
  menu_user.

handle_user_option(0) :-
  writeln("\n<--- Voltando ao Menu Principal").

handle_user_option(_) :-
  exibir_mensagem_formatada("✗ Opção inválida. Tente novamente."),
  aguardar_enter,
  menu_user.

exibir_mensagem_formatada(Mensagem) :-
    nl,
    format('~`-t~168|~n'),
    string_length(Mensagem, TamMsg),
    EspacoTotal is 168 - TamMsg - 2,
    EspacoEsquerda is EspacoTotal // 2,
    EspacoDireita is EspacoTotal - EspacoEsquerda,
    format('~` t~*|~w~` t~*|~n', [EspacoEsquerda, Mensagem, EspacoDireita]),
    format('~`-t~168|~n').

aguardar_enter :-
    writeln("\nPressione ENTER para continuar..."),
    get_char(_),
    get_char(_).

validar_cpf(CPF) :-
    atom_length(CPF, 11),
    atom_chars(CPF, Chars),
    maplist(is_digit_char, Chars).

is_digit_char(Char) :-
    char_type(Char, digit).

