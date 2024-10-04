:- module(sale, [sale/5, show_sale/1]).

sale(ClientId, SellerId, DateSale, TotalSale, Products).

show_sale(sale(ClientId, SellerId, DateSale, TotalSale, Products)) :-
    format('Client ID: ~d~n', [ClientId]),
    format('Seller ID: ~d~n', [SellerId]),
    format('Date of Sale: ~w~n', [DateSale]),
    format('Total Sale: ~2f~n', [TotalSale]),
    format('Products: ~w~n', [Products]).
