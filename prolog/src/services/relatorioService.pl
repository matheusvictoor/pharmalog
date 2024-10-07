:- module(relatorioService, [
    menu_relatorio_service/0
]).

:- consult('../models/product.pl').
:- consult('../assets/product_layout.pl').
:- consult('user_service.pl').
:- use_module(relatorio_product).

menu_relatorio_service :-
    relatorio_layout,
    nl,
    write("Escolha uma opção de relatório: "),
    read(Option), nl,
    handle_relatorio_option(Option).

handle_relatorio_option(1) :-
    relatorio_product:relatorio_por_preco,
    aguardar_enter,
    menu_relatorio_service.

handle_relatorio_option(2) :-
    relatorio_product:relatorio_por_categoria,
    aguardar_enter,
    menu_relatorio_service.

handle_relatorio_option(3) :-
    relatorio_product:relatorio_por_estoque,
    aguardar_enter,
    menu_relatorio_service.

handle_relatorio_option(0) :-
    writeln("\n<--- Voltando ao Menu Principal").

handle_relatorio_option(_) :-
    writeln("Opção inválida. Tente novamente."),
    menu_relatorio_service.

relatorio_layout :-
    writeln('=============================='),
    writeln('       MENU DE RELATÓRIOS      '),
    writeln('=============================='),
    writeln('1. Relatório por Faixa de Preço'),
    writeln('2. Relatório por Categoria'),
    writeln('3. Relatório por Faixa de Estoque'),
    writeln('0. Voltar ao Menu Principal'),
    writeln('==============================').

start_relatorio_service :-
    repeat,
    menu_relatorio_service,
    !.
