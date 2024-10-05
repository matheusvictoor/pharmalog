:- module(product, [product/7, show_product/1]).

product(NameProduct, Description, Category, DateManufacture, ExpirationDate, Price, Stock).

show_product(product(NameProduct, Description, Category, DateManufacture, ExpirationDate, Price, Stock)) :-
    format('Product Name: ~w~n', [NameProduct]),
    format('Description: ~w~n', [Description]),
    format('Category: ~w~n', [Category]),
    format('Date of Manufacture: ~w~n', [DateManufacture]),
    format('Expiration Date: ~w~n', [ExpirationDate]),
    format('Price: ~2f~n', [Price]),
    format('Stock: ~d~n', [Stock]).
