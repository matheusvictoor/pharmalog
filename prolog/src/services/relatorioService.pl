:- consult('../models/product.pl').
:- consult('../services/product_service.pl').
:- consult('user_service').

menu_relatorio_service :-
    nl,
    write("Escolha uma opção de relatório: "),
    read(Option), nl,
    handle_relatorio_option(Option).

handle_relatorio_option(1) :-
    relatorio_por_preco,
    aguardar_enter,
    menu_relatorio_service.

handle_relatorio_option(2) :-
    relatorio_por_categoria,
    aguardar_enter,
    menu_relatorio_service.

handle_relatorio_option(3) :-
    relatorio_por_estoque,
    aguardar_enter,
    menu_relatorio_service.

handle_relatorio_option(0) :-
    writeln("\n<--- Voltando ao Menu Principal").

handle_relatorio_option(_) :-
    writeln("Opção inválida. Tente novamente."),
    aguardar_enter,
    menu_relatorio_service.

relatorio_por_preco :-
    write('Insira o preço mínimo: '), flush_output,
    read_number(PrecoMin),
    
    write('Insira o preço máximo: '), flush_output,
    read_number(PrecoMax),
    
    product_service:list_all_products(Products),
    filtrar_por_preco(PrecoMin, PrecoMax, Products, ProdutosFiltrados),
    exibir_produtos(ProdutosFiltrados).

read_number(Number) :-
    read_line_to_string(user_input, Input),
    (   number_string(Number, Input)
    ->  true
    ;   writeln('Entrada inválida. Por favor, insira um número.'),
        read_number(Number)
    ).

relatorio_por_categoria :-
    write('Insira a categoria: '), flush_output,
    read_categoria(Categoria),
    
    product_service:list_all_products(Products),
    filtrar_por_categoria(Categoria, Products, ProdutosFiltrados),
    exibir_produtos(ProdutosFiltrados).

read_categoria(Categoria) :-
    read_line_to_string(user_input, CategoriaStr),
    (   CategoriaStr \= ""
    ->  Categoria = CategoriaStr
    ;   writeln('Entrada inválida. A categoria não pode ser vazia.'),
        read_categoria(Categoria)
    ).

relatorio_por_estoque :-
    write('Insira o estoque mínimo: '), flush_output,
    read_number(EstoqueMin),
    
    write('Insira o estoque máximo: '), flush_output,
    read_number(EstoqueMax),
    
    product_service:list_all_products(Products),
    filtrar_por_estoque(EstoqueMin, EstoqueMax, Products, ProdutosFiltrados),
    exibir_produtos(ProdutosFiltrados).

exibir_produtos([]) :-
    write('\nNenhum produto encontrado :('), nl.
exibir_produtos(Produtos) :-
    write('\nProdutos Encontrados:\n'), nl,
    product_service:print_products(Produtos).

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
