# PharmaLog

## Descrição

Este projeto é um sistema de farmácia desenvolvido como parte da disciplina de Paradigmas de Linguagens de Programação. O objetivo é implementar um backend funcional utilizando Haskell e, posteriormente, Prolog, para aprender e aplicar conceitos de programação funcional e lógica.

## Tecnologias Utilizadas

- **Haskell**: Linguagem de programação funcional que será utilizada para a implementação inicial do backend.
- **Prolog**: Linguagem de programação lógica que será utilizada na segunda fase do desenvolvimento.

## Funcionalidades

| ID | Funcionalidade | Descrição |
|----|----------------|-----------|
| 1  | **CRUD de Usuário: Administrador** | <ul><li>Pode criar novos usuários (administradores, gerentes, vendedores) no sistema.</li><li>Define as permissões e credenciais de cada novo usuário.</li><li>Pode visualizar todas as informações de todos os usuários.</li><li>Acesso completo aos dados do sistema.</li><li>Pode modificar as informações de qualquer usuário (dados pessoais, permissões, etc.).</li><li>Pode redefinir senhas e atualizar níveis de acesso.</li><li>Pode remover qualquer usuário do sistema.</li><li>Gerencia a exclusão de contas inativas ou desnecessárias.</li></ul> |
| 2  | **CRUD de Usuário: Gerente** | <ul><li>Pode criar novos usuários vendedores.</li><li>Define as permissões e credenciais dos vendedores criados.</li><li>Pode visualizar informações dos vendedores que supervisiona.</li><li>Acesso a relatórios e dados de vendas relacionados à sua equipe.</li><li>Pode modificar informações dos vendedores que supervisiona.</li><li>Pode atualizar metas e informações de desempenho.</li><li>Pode remover vendedores da sua equipe.</li><li>Gerencia a exclusão de contas de vendedores inativos.</li></ul> |
| 3  | **CRUD de Usuário: Vendedor** | <ul><li>Altera o status de disponibilidade do produto.</li><li>Permissão para alterar a quantidade disponível no estoque do produto.</li><li>Gera relatórios de venda.</li><li>Modifica informações de promoções e descontos aplicados ao produto.</li><li>Remove produtos descontinuados ou que não estão mais à venda.</li></ul> |
| 4  | **CRUD de Usuário: Cliente** | <ul><li>Guarda informações do cliente (Nome, idade, endereço).</li><li>Permite visualizar informações pessoais do cliente.</li><li>Atualiza informações pessoais do cliente.</li><li>Remove informações de clientes (se aplicável).</li></ul> |
| 5  | **CRUD de Produto** | <ul><li>Insere informações detalhadas do produto, como nome, descrição, preço, categoria, quantidade em estoque, e outras especificações relevantes.</li><li>Define status de disponibilidade (disponível, indisponível, em promoção, etc.).</li><li>Visualiza detalhes específicos de um produto, como descrições, preços, estoque disponível, e especificações técnicas.</li><li>Edita detalhes do produto, como nome, descrição, preço, categoria, e quantidade em estoque.</li></ul> |
| 6  | **Gerar Relatório com Filtros** | <ul><li>Vendas: Seleção de produtos vendidos dentro de um intervalo de tempo específico.</li><li>Possibilidade de filtrar por produto, categoria, região de venda, e cliente.</li><li>Inclusão de métricas como total de vendas, quantidade vendida, receita gerada, e margens de lucro.</li><li>Compras: Seleção de produtos comprados dentro de um intervalo de tempo específico.</li><li>Possibilidade de filtrar por fornecedor, produto, categoria, e região de compra.</li><li>Inclusão de métricas como total de compras, quantidade comprada, custos, e variação de preços.</li><li>Categoria de Produtos: Filtragem de produtos por categorias predefinidas.</li><li>Data: Seletor de data que permite escolher um intervalo de datas específicas. Possibilidade de gerar relatórios diários, semanais, mensais, trimestrais, e anuais.</li></ul> |
| 7  | **Histórico de Compra por Cliente** | <ul><li>ID da compra, data, valor total, método de pagamento.</li><li>Detalhes dos Itens Comprados: Nome do produto, quantidade, preço unitário, total do item.</li><li>Filtros de Pesquisa: Intervalo de datas, categoria de produto, valor da compra.</li><li>Detalhes do Pedido: Status do pedido, informações de entrega, notas do cliente.</li></ul> |
| 8  | **Alerta de Estoque Baixo** | <ul><li>Monitora e recebe notificações quando o estoque de produtos atinge níveis críticos.</li><li>Ajuda a evitar rupturas de estoque e a garantir que os produtos estejam sempre disponíveis para os clientes.</li></ul> |
| 9  | **Chat com Farmacêutico/Vendedor** | <ul><li>Interação em tempo real com profissionais da farmácia para obter assistência personalizada.</li><li>Consultas sobre preço, categoria, estoque de produtos, informações sobre o horário de funcionamento da farmácia, etc.</li></ul> |
| 10 | **Consultas por Cliente, Fabricante, Produto** | <ul><li>Consultas por Cliente: Critérios de pesquisa (nome, ID, histórico de compras), resultados detalhados (perfil, histórico de compras, total gasto).</li><li>Consultas por Fabricante: Critérios de pesquisa (nome, ID, tipo de produto), resultados detalhados (informações do fabricante, lista de produtos, histórico de transações).</li><li>Consultas por Produto: Critérios de pesquisa (nome, categoria), resultados detalhados (detalhes do produto, quantidade em estoque, vendas totais).</li></ul> |

## Como Executar o Projeto

Para executar este projeto, siga os passos abaixo:

1. **Instale o Haskell e o Stack:**
   - Certifique-se de que você tenha o Haskell e o Stack instalados na sua máquina.
   - Você pode instalar o Stack seguindo as instruções em: [Instalação do Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

2. **Clone o Repositório:**
   ```bash
   git clone https://github.com/matheusvictoor/pharmalog.git
   cd pharmalog
   ```

3. **Compile o Projeto:**
   - Dentro do diretório do projeto, execute o comando abaixo para compilar:
   ```bash
   stack build
   ```

4. **Execute o Projeto:**
   - Após a compilação, você pode executar o projeto usando:
   ```bash
   stack exec pharmalog
   ```

Pronto! Agora você pode utilizar o sistema Pharmalog.

## Estrutura do Projeto

A estrutura do projeto é organizada da seguinte forma:

```plaintext
/pharmalog
├── /app
|   └── Main.hs
├── /src
|   └── /Controllers
|   |   └── MenuController.hs
|   └── /Models
|   |   ├── Client.hs
|   |   ├── Message.hs
|   |   ├── Product.hs
|   |   ├── Sale.hs
|   |   ├── User.hs
|   |   └── Chat.hs
|   └── /Services
|   |   ├── ClientService.hs
|   |   ├── ProductService.hs
|   |   ├── SaleService.hs
|   |   ├── UserService.hs
|   |   ├── SelleService.hs
|   |   ├── RelatorioService.hs
|   |   └── ChatService.hs
├── _chatDB.dat
├── _customerDB.dat
├── _productDB.dat
├── _saleDB.dat
├── _userDB.dat
├── pharmlog.cabal
├── Setup.hs
├── README.ms
└── stack.yaml

Autores
 Arthur Fernandes Falcão de Araújo - ArthurFernandes8
 Dimas Gabriel Sales Diniz - DimasGSD
 Hiago Emanuel Aires Diniz - hiagoaires
 Matheus Victor Pereira - matheusvictoor
 Rute Pereira da Silva - rutesilvva