:- module(seller_service, [
    create_vendedor/3, 
    get_all_vendedores/2, 
    get_vendedor_by_id/3, 
    update_vendedor/3, 
    delete_vendedor/3
]).

:- use_module(library(lists)).

create_vendedor(NovoVendedor, Vendedores, [NovoVendedor|Vendedores]).

get_all_vendedores(Vendedores, Vendedores).

get_vendedor_by_id(SearchId, Vendedores, Vendedor) :-
    member(Vendedor, Vendedores),
    vendedor_id(Vendedor, SearchId).

update_vendedor(SearchId, UpdatedVendedor, Vendedores, UpdatedVendedores) :-
    maplist(update_if_found(SearchId, UpdatedVendedor), Vendedores, UpdatedVendedores).

update_if_found(SearchId, UpdatedVendedor, Vendedor, UpdatedVendedor) :-
    vendedor_id(Vendedor, SearchId), !.
update_if_found(_, _, Vendedor, Vendedor).

delete_vendedor(SearchId, Vendedores, UpdatedVendedores) :-
    exclude(vendedor_id_match(SearchId), Vendedores, UpdatedVendedores).

vendedor_id_match(SearchId, Vendedor) :-
    vendedor_id(Vendedor, SearchId).

vendedor_id(vendedor(Id, _, _, _), Id).
