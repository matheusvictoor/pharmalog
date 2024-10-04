:- module(product_service, [
    create_product/0, 
    get_product_by_id/0, 
    delete_product/0, 
    update_product/0, 
    get_all_products/1, 
    show_all_products/0, 
    alert_low_stock_products/0, 
    alert_expiring_products/0, 
    menu_product/0
]).

:- use_module(library(readutil)).
:- use_module(library(lists)).
:- use_module(library(time)).

:- use_module(product).  

create_product :-
    write('Nome: '), flush_output, read_line_to_string(user_input, ProductName),
    product_exist_name(ProductName, Exists),
    (   Exists -> 
        write('\n** Produto já existe **'), nl
    ;   open('_productDB.dat', append, Stream),
        write('Descrição: '), flush_output, read_line_to_string(user_input, Description),
        write('Categoria: '), flush_output, read_line_to_string(user_input, Category),
        write('Data de Fabricação (YYYY-MM-DD): '), flush_output, read_line_to_string(user_input, ManufactureDateStr),
        parse_date(ManufactureDateStr, ManufactureDate),
        write('Data de Expiração (YYYY-MM-DD): '), flush_output, read_line_to_string(user_input, ExpirationDateStr),
        parse_date(ExpirationDateStr, ExpirationDate),
        write('Preço: '), flush_output, read_line_to_string(user_input, PriceStr), number_string(Price, PriceStr),
        write('Estoque: '), flush_output, read_line_to_string(user_input, StockStr), number_string(Stock, StockStr),
        write(Stream, product(ProductName, Description, Category, ManufactureDate, ExpirationDate, Price, Stock)), write(Stream, '.'), nl(Stream),
        close(Stream),
        write('** Produto cadastrado com sucesso! **'), nl).

get_product_by_id :-
    write('ID do produto para buscar: '), flush_output, read_line_to_string(user_input, IdStr), number_string(ProductId, IdStr),
    read_products(Products),
    (   find_product_by_id(ProductId, Products, Product) -> 
        print_product(Product)
    ;   write('Produto não encontrado.'), nl).

delete_product :-
    write('ID do produto a ser deletado: '), flush_output, read_line_to_string(user_input, IdStr), number_string(ProductId, IdStr),
    read_products(Products),
    exclude(product_id_match(ProductId), Products, FilteredProducts),
    write_products(FilteredProducts),
    write('** Produto deletado com sucesso! **'), nl.

update_product :-
    write('ID do produto para atualizar: '), flush_output, read_line_to_string(user_input, IdStr), number_string(ProductId, IdStr),
    read_products(Products),
    (   find_product_by_id(ProductId, Products, Product) -> 
        update_product_details(ProductId, UpdatedProduct),
        update_product_in_list(Products, ProductId, UpdatedProduct, UpdatedProducts),
        write_products(UpdatedProducts),
        write('Produto atualizado com sucesso!'), nl
    ;   write('Produto não encontrado.'), nl).

get_all_products(Products) :-
    read_products(Products).

show_all_products :-
    read_products(Products),
    write('\nTodos os produtos cadastrados no sistema\n'), nl,
    maplist(print_product, Products).

alert_low_stock_products :-
    write('Digite o limite de estoque para alerta: '), flush_output, read_line_to_string(user_input, LimitStr), number_string(Limit, LimitStr),
    read_products(Products),
    maplist(alert_product_stock(Limit), Products).

alert_expiring_products :-
    write('Digite a quantidade de dias para alerta de vencimento: '), flush_output, read_line_to_string(user_input, DaysStr), number_string(DaysBefore, DaysStr),
    get_time(CurrentTime),
    read_products(Products),
    maplist(check_product_expiration(CurrentTime, DaysBefore), Products).

menu_product :-
    write('\nSelecione uma opção:\n1.  Cadastrar um novo produto\n2.  Buscar um produto por ID\n3.  Buscar todos os produtos\n4.  Atualizar um produto\n5.  Remover um produto\n6.  Alertar sobre Baixo Estoque\n7.  Alertar sobre Produtos Perto de Vencer\n0 <- Voltar\n'),
    write('\nOpção -> '), flush_output, read_line_to_string(user_input, Option),
    menu_option(Option).

menu_option("1") :- create_product.
menu_option("2") :- get_product_by_id.
menu_option("3") :- show_all_products.
menu_option("4") :- update_product.
menu_option("5") :- delete_product.
menu_option("6") :- alert_low_stock_products.
menu_option("7") :- alert_expiring_products.
menu_option("0") :- write('\n<---\n').
menu_option(_) :- write('Opção inválida. Tente novamente.\n'), menu_product.



product_exist_name(Name, Exists) :-
    read_products(Products),
    member(product(Name, _, _, _, _, _, _), Products), !,
    Exists = true.
product_exist_name(_, false).

find_product_by_id(Id, Products, Product) :-
    nth1(Id, Products, Product).

product_id_match(Id, Product) :-
    nth1(Id, Product).

alert_product_stock(Limit, product(Name, _, _, _, _, _, Stock)) :-
    (   Stock < Limit -> 
        format('Alerta! Produto: ~w está com estoque baixo. Estoque atual: ~w~n', [Name, Stock])
    ;   format('Produto: ~w está com estoque suficiente. Estoque atual: ~w~n', [Name, Stock])).

check_product_expiration(CurrentTime, DaysBefore, product(Name, _, _, _, ExpirationDate, _, _)) :-
    DaysBeforeSeconds is DaysBefore * 86400,
    ExpiringDate is CurrentTime + DaysBeforeSeconds,
    (   ExpirationDate =< ExpiringDate -> 
        format('Alerta! O produto "~w" está perto de vencer ou já venceu. Data de Expiração: ~w~n', [Name, ExpirationDate])
    ;   format('O produto "~w" está ok. Data de Expiração: ~w~n', [Name, ExpirationDate])).

parse_date(DateStr, Date) :-
    parse_time(DateStr, '%Y-%m-%d', Date).

read_products(Products) :-
    open('_productDB.dat', read, Stream),
    read_terms(Stream, Products),
    close(Stream).

write_products(Products) :-
    open('_productDB.dat', write, Stream),
    maplist(write_term(Stream), Products),
    close(Stream).

print_product(product(Name, Description, Category, ManufactureDate, ExpirationDate, Price, Stock)) :-
    format('Nome: ~w\nDescrição: ~w\nCategoria: ~w\nData de Fabricação: ~w\nData de Expiração: ~w\nPreço: ~2f\nEstoque: ~d\n----------------------------------------\n', 
           [Name, Description, Category, ManufactureDate, ExpirationDate, Price, Stock]).
