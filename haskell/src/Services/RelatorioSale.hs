module Services.RelatorioSale(vendaDentroDoIntervalo, relatorioVendasPorProduto, relatorioVendasPorReceita, menuRelatorioVendas) where

import Models.Sale
import Services.SaleService (getAllSales)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Control.Monad (filterM)

vendaDentroDoIntervalo :: UTCTime -> UTCTime -> Sale -> Bool
vendaDentroDoIntervalo dataInicial dataFinal venda =
  let dataVenda = dateSale venda
  in dataVenda >= dataInicial && dataVenda <= dataFinal

relatorioVendasPorProduto :: IO ()
relatorioVendasPorProduto = do
  putStrLn "Data inicial (YYYY-MM-DD):"
  dataInicialStr <- getLine
  dataInicial <- parseDate dataInicialStr

  putStrLn "Data final (YYYY-MM-DD):"
  dataFinalStr <- getLine
  dataFinal <- parseDate dataFinalStr

  vendas <- getAllSales
  let vendasNoIntervalo = filter (vendaDentroDoIntervalo dataInicial dataFinal) vendas
  let produtosVendidos = concatMap products vendasNoIntervalo

  putStrLn "\nProdutos vendidos no período:"
  mapM_ (putStrLn . show) produtosVendidos

  if null produtosVendidos
    then putStrLn "Nenhum produto vendido nesse período."
    else putStrLn "\n** Relatório gerado com sucesso! **"

relatorioVendasPorReceita :: IO ()
relatorioVendasPorReceita = do
  putStrLn "Data inicial (YYYY-MM-DD):"
  dataInicialStr <- getLine
  dataInicial <- parseDate dataInicialStr

  putStrLn "Data final (YYYY-MM-DD):"
  dataFinalStr <- getLine
  dataFinal <- parseDate dataFinalStr

  vendas <- getAllSales
  let vendasNoIntervalo = filter (vendaDentroDoIntervalo dataInicial dataFinal) vendas
  let receitaTotal = sum (map totalSale vendasNoIntervalo)

  putStrLn $ "\nReceita total no período: R$ " ++ show receitaTotal
  putStrLn "\n** Relatório gerado com sucesso! **"

--Faltando integração com o menu principal
menuRelatorioVendas :: IO ()
menuRelatorioVendas = do
  putStrLn "Escolha o tipo de relatório:"
  putStrLn "1. Relatório por Produtos Vendidos"
  putStrLn "2. Relatório por Receita Gerada"
  putStrLn "0. Sair"
  putStrLn "Digite o número da sua escolha:"

  escolha <- getLine

  case escolha of
    "1" -> relatorioVendasPorProduto
    "2" -> relatorioVendasPorReceita
    "0" -> putStrLn "Saindo..."
    _   -> putStrLn "Opção inválida, tente novamente."
