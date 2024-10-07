:- consult('../models/product.pl').
:- consult('../assets/product_layout.pl').
:- consult('user_service.pl').

menu_product :-
    product_layout,
    nl,
    write("Escolha uma opção: "),
    read(Option), nl,
    handle_product_option(Option).

handle_product_option(1) :-
    create_product,
    aguardar_enter,
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
    get_char(_),  % Limpa qualquer caractere residual
    writeln('Nome: '), read_line_to_string(user_input, Name),
    ( Name == "" -> 
        exibir_mensagem_formatada('✗ Erro: O nome do produto é obrigatório.'), aguardar_enter, fail
    ; true),
    
    writeln('Descrição: '), read_line_to_string(user_input, Description),
    writeln('Categoria: '), read_line_to_string(user_input, Category),
    writeln('Fabricante: '), read_line_to_string(user_input, Manufacture),
    ( Manufacture == "" ->
        exibir_mensagem_formatada('✗ Erro: O fabricante do produto é obrigatório.'), aguardar_enter, fail
    ; true),
    
    writeln('Data de Fabricação (YYYY-MM-DD): '), read_line_to_string(user_input, ManufactureDate),
    ( \+ valid_date(ManufactureDate) -> 
        exibir_mensagem_formatada('✗ Erro: Data de fabricação inválida. Use o formato YYYY-MM-DD.'), aguardar_enter, fail
    ; true),
    
    writeln('Data de Expiração (YYYY-MM-DD): '), read_line_to_string(user_input, ExpirationDate),
    ( \+ valid_date(ExpirationDate) -> 
        exibir_mensagem_formatada('✗ Erro: Data de expiração inválida. Use o formato YYYY-MM-DD.'), aguardar_enter, fail
    ; true),

    ( \+ data_fabricacao_antes_expiracao(ManufactureDate, ExpirationDate) -> 
        exibir_mensagem_formatada('✗ Erro: A data de expiração não pode ser anterior à data de fabricação.'), aguardar_enter, fail
    ; true),
    
    writeln('Preço: '), read_line_to_string(user_input, PriceStr), 
    ( catch(number_string(Price, PriceStr), _, fail) -> 
        true
    ; writeln('✗ Erro: Preço inválido. Insira um número válido. **'), aguardar_enter, fail),
    
    writeln('Estoque: '), read_line_to_string(user_input, StockStr),
    ( catch(number_string(Stock, StockStr), _, fail) -> 
        true
    ; writeln('✗ Erro: Estoque inválido. Insira um número válido. **'), aguardar_enter, fail),
    
    (   product(_, Name, _, _, Manufacture, _, _, _, _) -> 
        exibir_mensagem_formatada("✗ Erro: Produto já cadastrado com o mesmo nome e fabricante!"),
        aguardar_enter
    ;   generate_new_id(ID),
        create_product(ID, Name, Description, Category, Manufacture, ManufactureDate, ExpirationDate, Price, Stock),
        format(atom(Msg), "✓ Produto cadastrado com sucesso! ID: ~w", [ID]),
        exibir_mensagem_formatada(Msg)
    ).

valid_date(DateStr) :-
    split_string(DateStr, "-", "", [Year, Month, Day]),
    string_length(Year, 4), string_length(Month, 2), string_length(Day, 2),
    atom_number(Year, YearNum),
    atom_number(Month, MonthNum),
    atom_number(Day, DayNum),
    MonthNum >= 1, MonthNum =< 12,
    DayNum >= 1, DayNum =< 31,
    YearNum >= 1900.

get_product_by_id :-
    writeln('ID do produto para buscar: '), read(Id),
    ( product_exists_id(Id) ->
        get_product_by_id(Id, Product), nl,
        exibir_mensagem_formatada('Informações do Produto'),
        print_products(Product),
        aguardar_enter
    ;
        exibir_mensagem_formatada("✗ Erro: Produto não encontrado!"), aguardar_enter
    ).

get_product_by_name :-
    writeln('Nome do produto para buscar: '), read(Name),
    list_all_products(Products),
    (
        member(product(_, Name, Description, Category, Manufacture, ManufactureDate, ExpirationDate, Price, Stock), Products) -> % Inclui o Fabricante
        show_product(product(_, Name, Description, Category, Manufacture, ManufactureDate, ExpirationDate, Price, Stock))
    ;
        exibir_mensagem_formatada("✗ Erro: Produto não encontrado!"), aguardar_enter
    ).

delete_product :-
    writeln('ID do produto para deletar: '), read(Id),
    ( product_exists_id(Id) ->
        delete_product(Id),
        exibir_mensagem_formatada("✓ Produto deletado com sucesso!"), aguardar_enter
    ;
        exibir_mensagem_formatada("✗ Erro: Produto não encontrado!"), aguardar_enter
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
        update_product(Id, NewName, NewDescription, NewCategory, NewManufacture, NewManufactureDate, NewExpirationDate, NewPrice, NewStock),
        exibir_mensagem_formatada("✓ Produto atualizado com sucesso!"), aguardar_enter
    ;
        exibir_mensagem_formatada("✗ Erro: Produto não encontrado!"), aguardar_enter
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

data_fabricacao_antes_expiracao(ManufactureDate, ExpirationDate) :-
    split_string(ManufactureDate, "-", "", [Y1, M1, D1]),
    split_string(ExpirationDate, "-", "", [Y2, M2, D2]),
    atom_number(Y1, Year1), atom_number(M1, Month1), atom_number(D1, Day1),
    atom_number(Y2, Year2), atom_number(M2, Month2), atom_number(D2, Day2),
    ( Year1 < Year2 ;
      (Year1 =:= Year2, Month1 < Month2) ;
      (Year1 =:= Year2, Month1 =:= Month2, Day1 =< Day2) ).