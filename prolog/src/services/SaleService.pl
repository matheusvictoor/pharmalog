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
    ( \+ valid_cpf(CPF) -> 
        writeln('** Erro: CPF inválido. O CPF deve conter 11 dígitos numéricos. **'), fail
    ; true),
    
   
    writeln('ID do Vendedor: '), read(SellerId),
    ( \+ integer(SellerId) -> 
        writeln('** Erro: O ID do vendedor deve ser um número inteiro. **'), fail
    ; true),


    writeln('Data da Venda (YYYY-MM-DD): '), read(DateSale),
    ( \+ valid_date(DateSale) -> 
        writeln('** Erro: Data da venda inválida. Use o formato YYYY-MM-DD. **'), fail
    ; true),

 
    writeln('Valor da Venda (9.99): '), read(TotalSale),
    ( \+ valid_price(TotalSale) -> 
        writeln('** Erro: Valor da venda inválido. Insira um valor numérico. **'), fail
    ; true),
    
    
    generate_new_sale_id(SaleId),  % Função para gerar ID único
    assertz(sale(SaleId, CPF, SellerId, DateSale, TotalSale, [])),  
    format("\n** Venda cadastrada com sucesso! ID da Venda: ~w **\n", [SaleId]).


valid_cpf(CPF) :-
    atom_chars(CPF, Digits),
    length(Digits, 11),  % O CPF deve ter exatamente 11 dígitos
    maplist(char_type, Digits, digit).  % Verifica se todos os caracteres são dígitos


valid_date(DateStr) :- 
    split_string(DateStr, "-", "", [Year, Month, Day]),
    string_length(Year, 4), string_length(Month, 2), string_length(Day, 2),
    catch(number_string(_, Year), _, fail),
    catch(number_string(_, Month), _, fail),
    catch(number_string(_, Day), _, fail).


valid_price(Price) :- 
    number(Price), 
    Price > 0.


generate_new_sale_id(NewID) :-
    findall(Id, sale(Id, _, _, _, _, _), IDs),
    ( IDs = [] -> NewID = 1 ; max_list(IDs, MaxID), NewID is MaxID + 1).



get_sale_by_client_cpf :- 
    writeln('Digite o CPF do Cliente: '), read(CPF),
    findall(Sale, sale(_, CPF, _, _, _, _), Sales),
    ( Sales = [] ->
        writeln('Nenhuma venda encontrada para este CPF.')
    ;
        writeln('Vendas encontradas:'),
        print_sales(Sales)
    ).

get_all_sales :- 
    writeln('Todas as vendas cadastradas no sistema:'),
    findall(Sale, sale(_, _, _, _, _, _), Sales),
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
