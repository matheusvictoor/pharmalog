:- module(relatorio_product, [ 
    relatorio_por_preco/0,
    relatorio_por_categoria/0,
    relatorio_por_estoque/0,
    exibir_produtos/1,
    menu_relatorio/0
]).

:- use_module(product_service).
:- use_module(library(readutil)).
:- use_module(library(lists)).
:- use_module(library(time)).

relatorio_por_preco :-
    write('Insira o preço mínimo: '), flush_output,
    read_line_to_string(user_input, PrecoMinStr),
    number_string(PrecoMin, PrecoMinStr),
    
    write('Insira o preço máximo: '), flush_output,
    read_line_to_string(user_input, PrecoMaxStr),
    number_string(PrecoMax, PrecoMaxStr),
    
    product_service:list_all_products(Products),
    filtrar_por_preco(PrecoMin, PrecoMax, Products, ProdutosFiltrados),
    exibir_produtos(ProdutosFiltrados).

relatorio_por_categoria :-
    write('Insira a categoria: '), flush_output,
    read_line_to_string(user_input, Categoria),
    
    product_service:list_all_products(Products),
    filtrar_por_categoria(Categoria, Products, ProdutosFiltrados),
    exibir_produtos(ProdutosFiltrados).

relatorio_por_estoque :-
    write('Insira o estoque mínimo: '), flush_output,
    read_line_to_string(user_input, EstoqueMinStr),
    number_string(EstoqueMin, EstoqueMinStr),
    
    write('Insira o estoque máximo: '), flush_output,
    read_line_to_string(user_input, EstoqueMaxStr),
    number_string(EstoqueMax, EstoqueMaxStr),
    
    product_service:list_all_products(Products),
    filtrar_por_estoque(EstoqueMin, EstoqueMax, Products, ProdutosFiltrados),
    exibir_produtos(ProdutosFiltrados).

exibir_produtos([]) :-
    write('\nNenhum produto encontrado :('), nl.
exibir_produtos(Produtos) :-
    write('\nProdutos Encontrados:\n'), nl,
    product_service:print_products(Produtos).

menu_relatorio :-
    repeat,
    write('\nSelecione uma opção de relatório:\n'),
    write('1. Relatório por faixa de preço\n'),
    write('2. Relatório por categoria\n'),
    write('3. Relatório por faixa de estoque\n'),
    write('0. Voltar\n'),
    write('\nOpção -> '), flush_output,
    
    read_line_to_string(user_input, Opção),
    tratar_opção_relatorio(Opção),
    (Opção = "0" -> ! ; fail).

tratar_opção_relatorio("1") :- relatorio_por_preco, !.
tratar_opção_relatorio("2") :- relatorio_por_categoria, !.
tratar_opção_relatorio("3") :- relatorio_por_estoque, !.
tratar_opção_relatorio("0") :- write('Voltando ao menu anterior...\n'), !.
tratar_opção_relatorio(_) :-
    write('Opção inválida. Tente novamente.'), nl,
    fail.

filtrar_por_preco(Min, Max, Produtos, Filtrados) :-
    include(produto_na_faixa_de_preco(Min, Max), Produtos, Filtrados).

produto_na_faixa_de_preco(Min, Max, product(_, _, _, _, _, _, _, Preco, _)) :-
    Preco >= Min,
    Preco =< Max.

filtrar_por_categoria(Categoria, Produtos, Filtrados) :-
    include(produto_da_categoria(Categoria), Produtos, Filtrados).

produto_da_categoria(Categoria, product(_, _, Categoria, _, _, _, _, _, _)).

filtrar_por_estoque(Min, Max, Produtos, Filtrados) :-
    include(produto_na_faixa_de_estoque(Min, Max), Produtos, Filtrados).

produto_na_faixa_de_estoque(Min, Max, product(_, _, _, _, _, _, _, _, Estoque)) :-
    Estoque >= Min,
    Estoque =< Max.
