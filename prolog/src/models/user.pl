:- dynamic user/4.

role(administrador).
role(gerente).
role(vendedor).

load_users :-
  exists_file('data/userDB.pl'),
  consult('data/userDB.pl'), !;
  open('data/userDB.pl', write, Stream), close(Stream).

save_user:-
  tell('data/userDB.pl'),
  listing(user),
  told.

create_user(Name, CPF, Password, Role) :-
  \+ user(_, CPF, _, _),
  assertz(user(Name, CPF, Password, Role)),
  save_user,
  write('** Usuario cadastrado com sucesso! **').

user_exists_cpf(CPF) :-
  user(_, CPF, _, _).

get_user_by_cpf(CPF, User) :-
  user(Name, CPF, Password, Role),
  User = user(Name, CPF, Password, Role).

update_user(CPF, NewPassword, NewRole) :-
  retract(user(Name, CPF, _, _)),
  assertz(user(Name, CPF, NewPassword, NewRole)),
  save_user,
  write('** Informações do usuário atualizadas com sucesso! **').

delete_user(CPF) :-
  retract(user(_, CPF, _, _)),
  save_user,
  write('** Usuário excluído com sucesso! **').

list_users :-
  findall(user(Name, CPF, _, Role), user(Name, CPF, _, Role), Users),
  write('** Lista de usuarios **'),
  print_users(Users).

print_users([]).
print_users([user(Name, CPF, _, Role) | T]) :-
  format('Nome: ~w, CPF: ~w, Cargo: ~w~n', [Name, CPF, Role]),
  print_users(T).