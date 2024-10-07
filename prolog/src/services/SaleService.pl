:- consult('product_service.pl').
:- dynamic sale/6.

menu_sale :- 
    sales_layout,  
    nl,
    write("Escolha uma opção: "), flush_output,
    read(Option),
    handle_sale_option(Option).

handle_sale_option(1) :-
    create_sale,
    menu_sale.

handle_sale_option(2) :-
    get_sale_by_client_cpf,
    menu_sale.

handle_sale_option(3) :-
    get_all_sales,
    menu_sale.

handle_sale_option(0) :-
    writeln("\nVoltando ao menu principal...").

handle_sale_option(_) :- 
    writeln("\n** Opção inválida. Tente novamente! **"),
    menu_sale.

:- dynamic sale/6.

create_sale :- 
    writeln('CPF do Cliente: '), read(CPF),
    writeln('ID do Vendedor: '), read(SellerId),
    writeln('Data da Venda (YYYY-MM-DD): '), read(DateSale),
    writeln('Valor da Venda (9.99): '), read(TotalSale),
    generate_new_sale_id(SaleId),  
    assertz(sale(SaleId, CPF, SellerId, DateSale, TotalSale, [])),
    format("\n** Venda cadastrada com sucesso! ID da Venda: ~w **\n", [SaleId]).

get_sale_by_client_cpf :- 
    writeln('Digite o CPF do Cliente: '), read(CPF),
    findall(_, sale(_, CPF, _, _, _, _), Sales),
    ( Sales = [] ->
        writeln('Nenhuma venda encontrada para este CPF.')
    ;
        writeln('Vendas encontradas:'),
        print_sales(Sales)
    ).

get_all_sales :- 
    writeln('Todas as vendas cadastradas no sistema:'),
    findall(_, sale(_, _, _, _, _, _), Sales),
    ( Sales = [] ->
        writeln('Nenhuma venda cadastrada.')
    ;
        print_sales(Sales)
    ).

generate_new_sale_id(NewID) :-
    findall(ID, sale(ID, _, _, _, _, _), IDs),
    ( IDs = [] -> NewID = 1; max_list(IDs, MaxID), NewID is MaxID + 1 ).

print_sales([]) :- writeln('--- Fim da lista ---').
print_sales([sale(ID, CPF, SellerId, DateSale, TotalSale, _) | Rest]) :-
    format('ID: ~w | CPF Cliente: ~w | Vendedor: ~w | Data: ~w | Valor: R$~2f\n', 
        [ID, CPF, SellerId, DateSale, TotalSale]),
    print_sales(Rest).