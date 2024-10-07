:- consult('../models/Client.pl').
:- consult('../assets/client_layout.pl').

menu_client :-
  client_layout,
  nl,
  write("Escolha uma opção: "),
  read(Option), nl,
  handle_user_option(Option).


handle_client_option(1) :-
  writeln("\nNome: "), read(Name),
  writeln("\nIdade: "), read(Age),
  writeln("\nEndereço: "), read(Address),
  writeln("\Telefone: "), read(Phone),
  writeln("\nCPF: "), read(CPF),
  ( validar_cpf(CPF) ->  % Valida se o CPF é válido
    ( client_exists_cpf(CPF) ->
      exibir_mensagem_formatada("CPF já cadastrado!"),
      aguardar_enter,
      menu_client
    ;
      create_client(Name, Age, Address, CPF, Phone),
      exibir_mensagem_formatada('✓ Usuário cadastrado com sucesso!'),
      aguardar_enter,
      menu_client
    )
;
  exibir_mensagem_formatada('✗ CPF inválido! Deve conter apenas números e ter 11 dígitos.'),
  aguardar_enter,
  menu_client
  ).

handle_client_option(2) :-
  writeln("CPF do usuário para buscar: "), read(CPF),
  ( client_exists_cpf(CPF) ->
    get_client_by_cpf(CPF, Client), nl,
    exibir_mensagem_formatada('Informações do Cliente'), nl,
    print_clients(Client),
    aguardar_enter
  ;
    exibir_mensagem_formatada('✗ Cliente não encontrado!'), nl, nl,
    aguardar_enter
  ),
  menu_client.

handle_client_option(3) :-
  exibir_mensagem_formatada('Lista de Clients:'), nl, nl,
  list_all_clients,
  aguardar_enter,
  menu_client.

handle_client_option(4) :-
  writeln("CPF do cliente para atualizar: "), read(CPF),
  ( client_exists_cpf(CPF) ->
    writeln("Novo Endereço: "), read(NewAddress),
    writeln("Novo Telefone: "), read(NewPhone),
    update_client(Name, Age, CPF, NewAddress, NewPhone, Sales), nl,
    exibir_mensagem_formatada('✓ Cliente atualizado com sucesso!'),
    aguardar_enter
  ;
    exibir_mensagem_formatada('✗ Cliente não encontrado!'),
    aguardar_enter
  ),
  menu_client.


handle_client_option(5) :-
  writeln("CPF do cliente para deletar: "), read(CPF),
  ( client_exists_cpf(CPF) ->
    delete_client(CPF),
    exibir_mensagem_formatada('✓ Cliente excluído com sucesso!'),
    aguardar_enter
  ;
    exibir_mensagem_formatada('✗ Cliente não encontrado!'), nl, nl,
    aguardar_enter
  ),
  menu_client.

handle_user_option(0) :-
  writeln("\n<--- Voltando ao Menu Principal").


handle_user_option(_) :-
  exibir_mensagem_formatada("✗ Opção inválida. Tente novamente."),
  aguardar_enter,
  menu_client.


exibir_mensagem_formatada(Mensagem) :-
    nl,
    format('~`-t~168|~n'),
    string_length(Mensagem, TamMsg),
    EspacoTotal is 168 - TamMsg - 2,
    EspacoEsquerda is EspacoTotal // 2,
    EspacoDireita is EspacoTotal - EspacoEsquerda,
    format('~` t~*|~w~` t~*|~n', [EspacoEsquerda, Mensagem, EspacoDireita]),
    format('~`-t~168|~n'),
    nl.


aguardar_enter :-
    writeln("\nPressione ENTER para continuar..."),
    get_char(_),
    get_char(_).

validar_cpf(CPF) :-
    atom_length(CPF, 11),    % Verifica se o CPF tem 11 dígitos
    atom_chars(CPF, Chars),  % Transforma o átomo CPF em uma lista de caracteres
    maplist(is_digit, Chars). % Verifica se todos os caracteres são dígitos

is_digit(Char) :-
    char_type(Char, digit). % Verifica se um caractere é um dígito

