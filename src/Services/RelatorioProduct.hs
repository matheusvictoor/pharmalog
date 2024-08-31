module Services.RelatorioProduct(relatorioPorPreco, relatorioPorCategoria, relatorioPorEstoque, exibirProdutos, menuRelatorio) where

import Models.Product
import Services.ProductService (getAllProducts)
import Data.Maybe (fromMaybe)

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
exibirProdutos [] = putStrLn "Nenhum produto encontrado."
exibirProdutos ps = mapM_ (putStrLn . show) ps

menuRelatorio :: IO ()
menuRelatorio = do
    putStrLn "Escolha uma opção de relatório:"
    putStrLn "1. Relatório por faixa de preço"
    putStrLn "2. Relatório por categoria"
    putStrLn "3. Relatório por faixa de estoque"
    putStrLn "0. Sair"
    opcao <- getLine
    case opcao of
        "1" -> relatorioPorPreco
        "2" -> relatorioPorCategoria
        "3" -> relatorioPorEstoque
        "0" -> putStrLn "Saindo..."
        _   -> putStrLn "Opção inválida" >> menuRelatorio
    putStrLn ""
