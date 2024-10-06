module Services.RelatorioProduct (relatorioPorPreco, relatorioPorCategoria, relatorioPorEstoque, exibirProdutos, menuRelatorio) where

import Models.Product
import Services.ProductService (getAllProducts)
import System.IO (hFlush, stdout)
import Data.Time.Format (formatTime, defaultTimeLocale)

relatorioPorPreco :: IO ()
relatorioPorPreco = do
    putStrLn "Insira o preço mínimo:"
    precoMin <- readLn
    putStrLn "Insira o preço máximo:"
    precoMax <- readLn
    produtos <- getAllProducts
    let filtrados = filter (\p -> price p >= precoMin && price p <= precoMax) produtos
    exibirProdutos filtrados

relatorioPorCategoria :: IO ()
relatorioPorCategoria = do
    putStrLn "Insira a categoria:"
    cat <- getLine
    produtos <- getAllProducts
    let filtrados = filter (\p -> category p == cat) produtos
    exibirProdutos filtrados

relatorioPorEstoque :: IO ()
relatorioPorEstoque = do
    putStrLn "Insira o estoque mínimo:"
    estoqueMin <- readLn
    putStrLn "Insira o estoque máximo:"
    estoqueMax <- readLn
    produtos <- getAllProducts
    let filtrados = filter (\p -> stock p >= estoqueMin && stock p <= estoqueMax) produtos
    exibirProdutos filtrados

exibirProdutos :: [Product] -> IO ()
exibirProdutos [] = putStrLn "\nNenhum produto encontrado :("
exibirProdutos ps = mapM_ (putStrLn . formatProduct) ps
  where
    formatProduct p = unlines [
        "Nome: " ++ nameProduct p,
        "Descrição: " ++ description p,
        "Categoria: " ++ category p,
        "Data de fabricação: " ++ formatTime defaultTimeLocale "%Y-%m-%d" (dateManufacture p),
        "Data de vencimento: " ++ formatTime defaultTimeLocale "%Y-%m-%d" (expirationDate p),
        "Preço: " ++ show (price p),
        "Estoque: " ++ show (stock p)
      ]

menuRelatorio :: IO ()
menuRelatorio = do
    putStrLn "\nSelecione uma opção:"
    putStrLn "1. Relatório por faixa de preço"
    putStrLn "2. Relatório por categoria"
    putStrLn "3. Relatório por faixa de estoque"
    putStrLn "0 <- Voltar"

    putStr "\nOpção -> "
    hFlush stdout

    option <- getLine

    case option of
        "1" -> relatorioPorPreco
        "2" -> relatorioPorCategoria
        "3" -> relatorioPorEstoque
<<<<<<< HEAD:src/Services/RelatorioProduct.hs
        "0" -> putStrLn "Saindo..."
        _   -> putStrLn "Opção inválida" >> menuRelatorio
=======
        "0" -> putStrLn "\n<---"
        _   -> putStrLn "\nOpção inválida" >> menuRelatorio
>>>>>>> 8da5a02b4db93b0254cfe0cca2688f2382631277:haskell/src/Services/RelatorioProduct.hs
    putStrLn ""
