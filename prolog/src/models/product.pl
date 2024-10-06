:- dynamic product/9.

load_products :-
    exists_file('../data/productDB.pl'),
    consult('../data/productDB.pl'), !;
    open('../data/productDB.pl', write, Stream), close(Stream).

save_product :-
    tell('../data/productDB.pl'),
    listing(product),
    told.

create_product(ID, Name, Description, Category, Manufacture, ManufactureDate, ExpirationDate, Price, Stock) :-
    assertz(product(ID, Name, Description, Category, Manufacture, ManufactureDate, ExpirationDate, Price, Stock)),
    save_product.

product_exists_name(Name) :-
    list_all_products(Products),
    member(product(_, Name, _, _, _, _, _, _, _), Products).

product_exists_id(Id) :-
    list_all_products(Products),
    member(product(Id, _, _, _, _, _, _, _, _), Products).

get_product_by_id(Id, Product) :-
    list_all_products(Products),
    member(Product, Products),
    Product = product(Id, _, _, _, _, _, _, _, _).

delete_product(Id) :-
    list_all_products(Products),
    member(Product, Products),
    Product = product(Id, _, _, _, _, _, _, _, _),
    retract(Product),
    save_product.

update_product(Id, NewName, NewDescription, NewCategory, NewManufacture, NewManufactureDate, NewExpirationDate, NewPrice, NewStock) :-
    list_all_products(Products),
    member(OldProduct, Products),
    OldProduct = product(Id, _, _, _, _, _, _, _, _),
    retract(OldProduct),
    assertz(product(Id, NewName, NewDescription, NewCategory, NewManufacture, NewManufactureDate, NewExpirationDate, NewPrice, NewStock)),
    save_product.

list_all_products(Products) :-
    findall(product(ID, Name, Description, Category, Manufacture, ManufactureDate, ExpirationDate, Price, Stock), 
            product(ID, Name, Description, Category, Manufacture, ManufactureDate, ExpirationDate, Price, Stock), 
            Products).

print_products([]).
print_products([product(Name, Description, Category, Manufacture, ManufactureDate, ExpirationDate, Price, Stock) | T]) :-
    format('Nome: ~w\nDescrição: ~w\nCategoria: ~w\nFabricante: ~w\nData de Fabricação: ~w\nData de Expiração: ~w\nPreço: ~2f\nEstoque: ~d\n----------------------------------------\n', 
           [Name, Description, Category, Manufacture, ManufactureDate, ExpirationDate, Price, Stock]),
    print_products(T).

alert_low_stock(Limit) :-
    findall(product(_, Name, _, _, _, _, _, _, Stock), 
            (product(_, Name, _, _, _, _, _, _, Stock), Stock < Limit), 
            LowStockProducts),
    print_products(LowStockProducts).

alert_products_near_expiration(Days) :-
    get_time(CurrentTime),
    findall(product(_, Name, _, _, _, _, ExpirationDate, _, _), 
        (product(_, Name, _, _, _, _, ExpirationDate, _, _), 
         ExpirationDate =< CurrentTime + Days * 86400), 
        ExpiringProducts),
    print_products(ExpiringProducts).
