:- consult('../models/product.pl').
:- consult('../assets/product_layout.pl').

menu_product :-
    product_layout,
    nl,
    write("Escolha uma opção: "),
    read(Option), nl,
    handle_product_option(Option).

handle_product_option(1) :-
    create_product,
    menu_product.

handle_product_option(2) :-
    get_product_by_id,
    menu_product.

handle_product_option(3) :-
    get_product_by_name,
    menu_product.

handle_product_option(4) :-
    show_all_products,
    menu_product.

handle_product_option(5) :-
    update_product,
    menu_product.

handle_product_option(6) :-
    delete_product,
    menu_product.

handle_product_option(7) :-
    alert_low_stock_products,
    menu_product.

handle_product_option(8) :-
    alert_expiring_products,
    menu_product.

handle_product_option(0) :-
    writeln("\n<--- Voltando ao Menu Principal").

handle_product_option(_) :-
    writeln("Opção inválida. Tente novamente."),
    menu_product.

create_product :-
    writeln('Nome: '), read(Name),
    writeln('Descrição: '), read(Description),
    writeln('Categoria: '), read(Category),
    writeln('Fabricante: '), read(Manufacture),
    writeln('Data de Fabricação (YYYY-MM-DD): '), read(ManufactureDate),
    writeln('Data de Expiração (YYYY-MM-DD): '), read(ExpirationDate),
    writeln('Preço: '), read(Price),
    writeln('Estoque: '), read(Stock),
    (   product(_, Name, _, _, Manufacture, _, _, _, _) -> % Verifica duplicidade pelo Nome e Fabricante
        writeln("\n** Produto já cadastrado com o mesmo nome e fabricante! **"), nl
    ;   generate_new_id(ID),
        assertz(product(ID, Name, Description, Category, Manufacture, ManufactureDate, ExpirationDate, Price, Stock)), % Cria o produto com 9 argumentos
        format("\n** Produto cadastrado com sucesso! ID: ~w **\n", [ID]), nl
    ).

get_product_by_id :-
    writeln('ID do produto para buscar: '), read(Id),
    ( product_exists_id(Id) ->
        get_product_by_id(Id, Product),
        show_product(Product)
    ;
        writeln("\n** Produto não encontrado! **"), nl
    ).

get_product_by_name :-
    writeln('Nome do produto para buscar: '), read(Name),
    list_all_products(Products),
    (
        member(product(_, Name, Description, Category, Manufacture, ManufactureDate, ExpirationDate, Price, Stock), Products) -> % Inclui o Fabricante
        show_product(product(_, Name, Description, Category, Manufacture, ManufactureDate, ExpirationDate, Price, Stock))
    ;
        writeln("\n** Produto não encontrado! **"), nl
    ).

delete_product :-
    writeln('ID do produto para deletar: '), read(Id),
    ( product_exists_id(Id) ->
        delete_product(Id),
        writeln("\n** Produto deletado com sucesso! **"), nl
    ;
        writeln("\n** Produto não encontrado! **"), nl
    ).

update_product :-
    writeln('ID do produto para atualizar: '), read(Id),
    ( product_exists_id(Id) ->
        writeln('Novo nome: '), read(NewName),
        writeln('Nova descrição: '), read(NewDescription),
        writeln('Nova categoria: '), read(NewCategory),
        writeln('Novo fabricante: '), read(NewManufacture),
        writeln('Nova data de fabricação (YYYY-MM-DD): '), read(NewManufactureDate),
        writeln('Nova data de expiração (YYYY-MM-DD): '), read(NewExpirationDate),
        writeln('Novo preço: '), read(NewPrice),
        writeln('Novo estoque: '), read(NewStock),
        update_product(Id, NewName, NewDescription, NewCategory, NewManufacture, NewManufactureDate, NewExpirationDate, NewPrice, NewStock), % Agora inclui o Fabricante
        writeln("\n** Produto atualizado com sucesso! **"), nl
    ;
        writeln("\n** Produto não encontrado! **"), nl
    ).

show_all_products :-
    list_all_products(Products),
    writeln('\n***************** Lista de Produtos ******************'),
    print_products(Products).

alert_low_stock_products :-
    writeln('Limite de estoque para alerta: '), read(Limit),
    alert_low_stock(Limit).

alert_expiring_products :-
    writeln('Número de dias para alerta de vencimento: '), read(Days),
    alert_products_near_expiration(Days).

generate_new_id(NewID) :-
    findall(ID, product(ID, _, _, _, _, _, _, _, _), IDs),
    ( IDs = [] -> NewID = 1; max_list(IDs, MaxID), NewID is MaxID + 1).