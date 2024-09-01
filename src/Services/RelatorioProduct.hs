module Services.RelatorioProduct(relatorioPorPreco, relatorioPorCategoria, relatorioPorEstoque, exibirProdutos, menuRelatorio) where

import Models.Product
import Services.ProductService (getAllProducts)
import System.IO (hFlush, stdout)

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
exibirProdutos ps = mapM_ (putStrLn . show) ps

-- Faltando adicionar a integração com o menuController
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
        "0" -> putStrLn "\n<---"
        _   -> putStrLn "\nOpção inválida" >> menuRelatorio
    putStrLn ""