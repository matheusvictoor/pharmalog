:- module(sale_service, [
    create_sale/0, 
    get_all_sales/0, 
    get_sale_by_client_cpf/0, 
    update_sale/1, 
    delete_sale/1, 
    menu_sale/0
]).

:- use_module(library(lists)).
:- use_module(library(time)).
:- use_module(library(readutil)).

:- use_module(sale).  % Módulo Sale.


create_sale :-
    read_file_to_terms('saleDB.pl', Sales, []),
    length(Sales, SaleId),
    write('CPF do Cliente: '), flush_output, read_line_to_string(user_input, ClientCpfStr), number_string(ClientCpf, ClientCpfStr),
    write('ID do Vendedor: '), flush_output, read_line_to_string(user_input, SellerIdStr), number_string(SellerId, SellerIdStr),
    write('Data da Venda (YYYY-MM-DD): '), flush_output, read_line_to_string(user_input, DateSaleStr), parse_date(DateSaleStr, DateSale),
    write('Valor da Venda (9.99): '), flush_output, read_line_to_string(user_input, TotalSaleStr), number_string(TotalSale, TotalSaleStr),
    Sale = sale(ClientCpf, SellerId, DateSale, TotalSale, []),
    open('saleDB.pl', append, Stream),
    write(Stream, sale(SaleId, Sale)), write(Stream, '.'), nl(Stream),
    close(Stream),
    write('** Venda registrada com sucesso! **'), nl.


get_all_sales :-
    read_file_to_terms('saleDB.pl', Sales, []),
    maplist(print_sale, Sales).


get_sale_by_client_cpf :-
    write('CPF do Cliente: '), flush_output, read_line_to_string(user_input, ClientCpfStr), number_string(ClientCpf, ClientCpfStr),
    read_file_to_terms('saleDB.pl', Sales, []),
    (   member(sale(_, Sale), Sales),
        Sale = sale(ClientCpf, _, _, _, _) -> 
        print_sale(sale(ClientCpf, Sale))
    ;   write('Venda não encontrada.'), nl).

update_sale(ClientCpf) :-
    read_file_to_terms('saleDB.pl', Sales, []),
    maplist(update_if_found(ClientCpf), Sales, UpdatedSales),
    open('saleDB.pl', write, Stream),
    maplist(write_term(Stream, [fullstop(true)]), UpdatedSales),
    close(Stream),
    write('** Venda atualizada com sucesso! **'), nl.

update_if_found(ClientCpf, sale(Index, Sale), sale(Index, UpdatedSale)) :-
    Sale = sale(ClientCpf, SellerId, DateSale, TotalSale, Products),
    !, 
    UpdatedSale = sale(ClientCpf, SellerId, DateSale, TotalSale, Products).
update_if_found(_, Sale, Sale).

delete_sale(ClientCpf) :-
    read_file_to_terms('saleDB.pl', Sales, []),
    exclude(sale_cpf_match(ClientCpf), Sales, FilteredSales),
    open('saleDB.pl', write, Stream),
    maplist(write_term(Stream, [fullstop(true)]), FilteredSales),
    close(Stream),
    write('** Venda deletada com sucesso! **'), nl.

sale_cpf_match(ClientCpf, sale(_, sale(ClientCpf, _, _, _, _))).


print_sale(sale(Index, sale(ClientId, SellerId, DateSale, TotalSale, Products))) :-
    format('ID da Venda: ~w\nCPF do Cliente: ~w\nID do Vendedor: ~w\nData da Venda: ~w\nValor Total: ~2f\nProdutos: ~w\n----------------------------------------\n', 
           [Index, ClientId, SellerId, DateSale, TotalSale, Products]).


parse_date(DateStr, Date) :-
    parse_time(DateStr, '%Y-%m-%d', Date).


menu_sale :-
    write('\nSelecione uma opção:\n1. Cadastrar um nova venda\n2. Buscar um cliente por CPF\n3. Buscar todas as vendas\n0 <- Voltar\n'),
    write('\nOpção -> '), flush_output, read_line_to_string(user_input, Option),
    menu_opcao(Option).

menu_opcao("1") :- create_sale.
menu_opcao("2") :- get_sale_by_client_cpf.
menu_opcao("3") :- get_all_sales.
menu_opcao("0") :- write('\n<---\n').
menu_opcao(_)   :- write('Opção inválida. Tente novamente.\n'), menu_sale.