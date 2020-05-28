setwd("D:/CientistadeDados/Projetos/RecomendacaoParaRedeVarejo")

# Orders (ENCOMENDAS FEITAS)
  #order_id: ID do Pedido
  #user_id: ID do Usuário
  #tipo_df: Tipo do Dado(Treino, Test, Pedidos Anteriores)
  #order_number: quantidade de compras efetuadas sequencial (1 = first, n = nth)
  #dia_semana_pedido: Dia da Semana do Pedido 
  #hora_pedido: Hora do dia do Pedido 
  #dias_ultimo_pedido: Dias Do Ultimo Pedido até 30 (com NAs para order_number = 1) 

# products (Cadastro dos Produtos)
  #product_id: ID do Produto
  #descricao_prod: Descrição do Produto
  #corredor_id: Corredor - foreign key
  #department_id: Departamento - foreign key

# aisles (Corredor)
  #corredor_id: ID do Corredor 
  #descricao: the Descrição do Corredor

# deptartments (Departamentos)
  #department_id: ID do Departamento
  #descricao: Descriçao do Departamento

# order_products__SET (Produtos Comprados):
  #order_id: ID da Compra - foreign key
  #product_id: ID do Produto - foreign key
  #ordem_prod_car: Ordem que adicionou ao carrinho 
  #historico_comp: 1 se este produto foi encomendado por este usuário no passado, 0 caso contrário

  #where SET is one of the four following evaluation sets (eval_set in orders):
  
# "prior": pedidos anteriores ao pedido mais recente do usuário (~ 3,2 milhões de pedidos)
# "train": dados de treinamento fornecidos aos participantes (~ 131 mil pedidos)
# "test": dados de teste reservados para competições de aprendizado de máquina (~ 75 mil pedidos)

library(OpenImageR)
library(dplyr)
library(data.table)
library(sqldf)
library(tidyr)
library(arules)
library(arulesViz)
library(purrr)
library(comprehenr)
library(RColorBrewer)
library(ggplot2)
library(knitr)
library(stringr)
library(DT)
library(plotly)
library(visNetwork)
library(igraph)
library(kableExtra)


save.image() 

imageShow("RelacionamentoTabelas.jpg")

col_pedidos <- c("order_id", 
                 "user_id", 
                 "tipo_df", 
                 "order_number", 
                 "dia_semana_pedido", 
                 "hora_pedido", 
                 "dias_ultimo_pedido" )

type_pedidos <- c(order_id = "integer", 
                  user_id = "integer", 
                  eval_set = "factor", 
                  order_number = "integer", 
                  order_dow = "factor", 
                  order_hour_of_day = "factor",
                  days_since_prior_order = "double")

col_produtos <- c("product_id",
                  "descricao", 
                  "corredor_id", 
                  "department_id")

type_produtos <- c(product_id = "integer",
                   product_name = "character", 
                   aisle_id = "integer", 
                   department_id = "integer")

col_corredor <- c("corredor_id", "descricao")

type_corredor <- c(aisle_id = "integer", 
                   aisle = "character")

col_departamento <- c("department_id", "descricao")

type_departamento <- c(department_id = "integer",
                       department = "character")


Col_pedido_prod <- c("order_id",
                     "product_id", 
                     "ordem_prod_car", 
                     "historico_comp" )

type_pedido_prod <- c(order_id = "integer",
                      product_id = "integer", 
                      add_to_cart_order = "factor", 
                      reordered = "factor" )

pedidos <- fread("./instacart_2017_05_01/orders.csv", 
                    header = TRUE,
                    sep = ",", 
                    dec = ".",
                    colClasses = type_pedidos,
                    col.names = col_pedidos,
                    fill = FALSE)

produtos <- fread("./instacart_2017_05_01/products.csv", 
                    header = TRUE,
                    sep = ",", 
                    dec = ".",
                    col.names = col_produtos,
                    colClasses = type_produtos)

corredor <- fread("./instacart_2017_05_01/aisles.csv", 
                     header = TRUE,
                     sep = ",", 
                     dec = ".",
                     colClasses = type_corredor,
                     col.names = col_corredor)

departamento <- fread("./instacart_2017_05_01/departments.csv", 
                     header = TRUE,
                     sep = ",", 
                     dec = ".",
                     colClasses = type_departamento,
                     col.names = col_departamento)

pedido_prod_prior <- fread("./instacart_2017_05_01/order_products__prior.csv", 
                     header = TRUE,
                     sep = ",", 
                     dec = ".",
                     colClasses = type_pedido_prod,
                     col.names = Col_pedido_prod)

pedido_prod_treino <- fread("./instacart_2017_05_01/order_products__train.csv", 
                     header = TRUE,
                     sep = ",", 
                     dec = ".",
                     colClasses = type_pedido_prod,
                     col.names = Col_pedido_prod)

pedido_treino <- pedidos %>% 
                 filter(tipo_df == "train")

pedido_teste <- pedidos %>%
                filter(tipo_df == "test")

pedido_prior <- pedidos %>%
                filter( tipo_df == "prior")

# Gera um DF com Prior e Produtos

pedido_produto_prior_v2 <- pedido_prior %>% 
                           inner_join(pedido_prod_prior, by = "order_id")

pedido_produto_prior <- pedido_produto_prior_v2 %>% 
                        inner_join(produtos, by = "product_id")

# Compras por dia da Semana 
# Temos domingo como o dia de maior compra seguido da segunda-feira e sexta-feira

query_consulta <- "select pp.dia_semana_pedido, 
                          count(pp.dia_semana_pedido) as total
                   from pedido_produto_prior pp
                   group by pp.dia_semana_pedido
                   order by total desc"

sqldf(query_consulta)

# Produto que vendeu mais
# O produto mais vendido: BANANA e suas derivações como: Banana Orgânica

query_consulta <- "select p.descricao,
                          count(pp.product_id) as total
                   from pedido_produto_prior pp, produtos p
                   where pp.product_id = p.product_id
                   group by pp.product_id
                   order by total DESC"

sqldf(query_consulta)

# Dia da Semana por produto que vendeu mais 
# Comprovando o que foi visto anteriormente Domingo e Segundo são os dias que mais venderam e o produto: BANANA

query_consulta <- "select pp.dia_semana_pedido,
                          p.descricao,
                          count(pp.product_id) as total
                   from pedido_produto_prior pp, produtos p
                   where pp.product_id = p.product_id
                   group by pp.dia_semana_pedido, pp.product_id
                   order by total DESC"

sqldf(query_consulta)

# Hora que Vendeu Mais 
# Os horário que tiveram maior movimento foram 10:00, 11:00 e 14:00

query_consulta <- "select pp.hora_pedido,
                          count(pp.hora_pedido) as total
                   from pedido_produto_prior pp
                   group by pp.hora_pedido
                   order by total DESC"

sqldf(query_consulta)

# Produto que mais foi Recomprado 
# Esta consulta que banana é o produto mais comprado

query_consulta <- "select p.descricao,
                          count(pp.historico_comp) as total
                   from pedido_produto_prior pp, produtos p
                   where pp.product_id = p.product_id and 
                         pp.historico_comp = 1
                   group by pp.product_id
                   order by total DESC"


sqldf(query_consulta)

# Produto que menos foi Recomprado 
# Pelo menos uma vez, todos os produtos da loja foram adquiridos

query_consulta <- "select p.descricao,
                          count(pp.historico_comp) as total
                   from pedido_produto_prior pp, produtos p
                   where pp.product_id = p.product_id and 
                         pp.historico_comp = 1
                   group by pp.product_id
                   order by total"


sqldf(query_consulta)

# produto colocado no carrinho com prioridade até 10
# Banana é o primeiro produto a ser colocado no carrinho comprovando a adoração dos americanos por Bananas

query_consulta <- "select pp.ordem_prod_car,
                          p.descricao,
                          count(pp.ordem_prod_car) as total
                   from pedido_produto_prior pp, produtos p
                   where pp.product_id = p.product_id and 
                         pp.ordem_prod_car <= 10
                   group by pp.ordem_prod_car, pp.product_id
                   order by total DESC"


sqldf(query_consulta)

# produto colocado no carrinho com prioridade > 100 por order_id

query_consulta <- "select pp.ordem_prod_car,
                          pp.order_id,
                          count(pp.ordem_prod_car) as total
                   from pedido_produto_prior pp
                   where pp.ordem_prod_car >= 100
                   group by pp.order_id, pp.ordem_prod_car
                   order by pp.ordem_prod_car"


sqldf(query_consulta)

# Total produtos comprados por order_id

query_consulta <- "SELECT
                       p.user_id,
                       p.order_id,
                       p.order_number,
                       p.product_id,
                  (SELECT
                       COUNT(pp.order_id) as grupo
                   FROM
                       pedido_produto_prior pp
                   WHERE
                       pp.order_id = p.order_id
                   group by 
                       pp.order_number) AS Total
                   FROM
                       pedido_produto_prior P
                   where 
                       user_id = 1
                   order by
                       p.order_number"

sqldf(query_consulta)

# Incluir a coluna fidelidade - Quantas vezes o usuário retornou pra cmprar
# Todos os clientes retornaram pra comprar novamente na base PRIOR

pedido_produto_fidelidade <- data.frame(pedido_produto_prior %>% 
                             group_by(user_id) %>%
                             mutate(fidelidade =  max(order_number)))

# Substitui os NAs da coluna dias_ultimo_pedido por 1

pedido_produto_fidelidade <- pedido_produto_fidelidade  %>% 
                             mutate_at("dias_ultimo_pedido", replace_na, 1)

# Substituir zeros na coluna dias_ultimo_pedido por 1

pedido_produto_fidelidade$dias_ultimo_pedido[pedido_produto_fidelidade$dias_ultimo_pedido == 0] <- 1

# Gerar dataframe de produtos comprados por ORDER_ID

cesta_prod = pedido_produto_prior_v2 %>% 
             inner_join(produtos, by="product_id") %>% 
             group_by(order_id) %>%
             summarise(cesta = as.vector(list(descricao)))

# Transactions no BANCO

transacoes <- as(cesta_prod$cesta, "transactions")
inspect(transacoes[20])

# Implementando o algoritmo Apriori
linhas <- apriori(transacoes, parameter = list(support = 0.004, confidence = 0.25, minlen = 2, maxlen = 3))

basket_rules <- sort(linhas, by = 'confidence', decreasing = TRUE)
inspect(basket_rules[1:20])

# Remover regra redundante    
linhas <- linhas[!is.redundant(linhas)]
linhas_dt <- data.table( lhs = labels( lhs(linhas) ), 
                        rhs = labels( rhs(linhas) ), 
                        quality(linhas) )[ order(-lift), ]
head(linhas_dt,20)

# Gráfico de Frequencia 

arules::itemFrequencyPlot(transacoes,
                          topN=20,
                          col=brewer.pal(8,'Pastel2'),
                          main='Item Frequencia Relativa Plot',
                          type="relative",
                          ylab="Item Frequencia (Relative)") 

# Gráfico de Plotagem

plotly_arules(linhas, method = "scatterplot", measure = c("support", "confidence"), 
              shading = "lift", colors = c("blue", "red"))

sel <- plot(linhas_dt, measure=c("support", "lift"), 
            shading = "confidence",
            interactive = TRUE)

sel <- plot(linhas, measure=c("support", "lift"), 
            shading = "confidence",
            interactive = TRUE)

# Grafico de Rede de Correlação

subrules2 <- head(sort(linhas, by="confidence"),100)
ig <- plot( subrules2, method="graph", control=list(type="items") )

# Grafico de Rede de Correlação - Interativo

ig_df <- toVisNetworkData(ig, idToLabel = FALSE)

visNetwork(ig_df$nodes, ig_df$edges) %>%
  visNodes(size = 10) %>%
  visLegend() %>%
  visEdges(smooth = FALSE) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visInteraction(navigationButtons = TRUE) %>%
  visEdges(arrows = 'from') %>%
  visPhysics(
    solver = "barnesHut",
    maxVelocity = 35,
    forceAtlas2Based = list(gravitationalConstant = -6000))

# Dataframe com as Regras

regras_df <- as(linhas_dt, "data.frame")
View(regras_df)



