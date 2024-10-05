:- module(relatorio_product, [
    relatorio_por_preco/0, 
    relatorio_por_categoria/0, 
    relatorio_por_estoque/0, 
    exibir_produtos/1, 
    menu_relatorio/0
]).

:- use_module(library(lists)).
:- use_module(library(time)).
:- use_module(product_service).

relatorio_por_preco :-
    write('Insira o preço mínimo: '), flush_output, read_line_to_string(user_input, PrecoMinStr), number_string(PrecoMin, PrecoMinStr),
    write('Insira o preço máximo: '), flush_output, read_line_to_string(user_input, PrecoMaxStr), number_string(PrecoMax, PrecoMaxStr),
    get_all_products(Produtos),
    include(filtra_por_preco(PrecoMin, PrecoMax), Produtos, Filtrados),
    exibir_produtos(Filtrados).

relatorio_por_categoria :-
    write('Insira a categoria: '), flush_output, read_line_to_string(user_input, Categoria),
    get_all_products(Produtos),
    include(filtra_por_categoria(Categoria), Produtos, Filtrados),
    exibir_produtos(Filtrados).

relatorio_por_estoque :-
    write('Insira o estoque mínimo: '), flush_output, read_line_to_string(user_input, EstoqueMinStr), number_string(EstoqueMin, EstoqueMinStr),
    write('Insira o estoque máximo: '), flush_output, read_line_to_string(user_input, EstoqueMaxStr), number_string(EstoqueMax, EstoqueMaxStr),
    get_all_products(Produtos),
    include(filtra_por_estoque(EstoqueMin, EstoqueMax), Produtos, Filtrados),
    exibir_produtos(Filtrados).

exibir_produtos([]) :-
    write('\nNenhum produto encontrado :('), nl.
exibir_produtos([Produto|Produtos]) :-
    format_produto(Produto, FormattedProduct),
    write(FormattedProduct), nl,
    exibir_produtos(Produtos).

menu_relatorio :-
    write('\nSelecione uma opção:\n1. Relatório por faixa de preço\n2. Relatório por categoria\n3. Relatório por faixa de estoque\n0 <- Voltar\n'),
    write('\nOpção -> '), flush_output, read_line_to_string(user_input, Option),
    menu_opcao(Option).

menu_opcao("1") :- relatorio_por_preco.
menu_opcao("2") :- relatorio_por_categoria.
menu_opcao("3") :- relatorio_por_estoque.
menu_opcao("0") :- write('\n<---\n').
menu_opcao(_)   :- write('\nOpção inválida\n'), menu_relatorio.


filtra_por_preco(PrecoMin, PrecoMax, product(_, _, _, _, _, Price, _)) :-
    Price >= PrecoMin, Price =< PrecoMax.

filtra_por_categoria(Categoria, product(_, _, Cat, _, _, _, _)) :-
    Cat = Categoria.

filtra_por_estoque(EstoqueMin, EstoqueMax, product(_, _, _, _, _, _, Stock)) :-
    Stock >= EstoqueMin, Stock =< EstoqueMax.


format_produto(product(Name, Description, Category, ManufactureDate, ExpirationDate, Price, Stock), FormattedProduct) :-
    format(atom(FormattedProduct), 
           'Nome: ~w\nDescrição: ~w\nCategoria: ~w\nData de fabricação: ~w\nData de vencimento: ~w\nPreço: ~2f\nEstoque: ~d\n----------------------------------------',
           [Name, Description, Category, ManufactureDate, ExpirationDate, Price, Stock]).
