:- dynamic user/4.

role(administrador).
role(gerente).
role(vendedor).

load_users :-
  exists_file('../data/userDB.pl'),
  consult('../data/userDB.pl'), !;
  open('../data/userDB.pl', write, Stream), close(Stream).

save_user:-
  tell('../data/userDB.pl'),
  listing(user),
  told.

create_user(Name, CPF, Password, Role) :-
  ( user(_, CPF, _, _) -> 
    write("\n** CPF já cadastrado! **"), nl
  ; 
    assertz(user(Name, CPF, Password, Role)),
    save_user    
  ).

user_exists_cpf(CPF) :-
  user(_, CPF, _, _).

get_user_by_cpf(CPF, [user(Name, CPF, Password, Role)]) :-
  user(Name, CPF, Password, Role).

update_user(CPF, NewPassword, NewRole) :-
  retract(user(Name, CPF, _, _)),
  assertz(user(Name, CPF, NewPassword, NewRole)),
  save_user.

delete_user(CPF) :-
  retract(user(_, CPF, _, _)),
  save_user.

list_users :-
  findall(user(Name, CPF, _, Role), user(Name, CPF, _, Role), Users),
  write('***************** Lista de usuarios ******************'), nl, nl,
  print_users(Users).

print_users([]).
print_users([user(Name, CPF, _, Role) | T]) :-
  write('Nome: '), write(Name), nl,
  write('CPF: '), write(CPF), nl,
  write('Cargo: '), write(Role), nl, nl,
  print_users(T).