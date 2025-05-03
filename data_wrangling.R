#### PACOTES MAIS UTILIZADOS ####

# Teste

# Carrega pacotes - [inicio] ----

library("DBI")
library("odbc")
library("rstudioapi")
library("sqldf")
library("readxl")
library("writexl")
library("openxlsx")
library("lubridate")
library("tidyverse")
library("janitor")
library("stringi")
library("psych")

# Carrega pacotes - [fim] ----

#### CONSULTAS BIG ####

# Busca campos de uma tabela - [inicio] ----

# Le arquivo de conexao

PASTA_DADOS <- "U:/"
NOME_ARQUIVO <- "conexao.csv"
CAMINHO_ARQUIVO <- file.path(PASTA_DADOS, NOME_ARQUIVO)
LOGIN <- read_delim(CAMINHO_ARQUIVO, delim = ";", col_types = cols(.default = "c"))
MATRICULA <- LOGIN$UID
SENHA <- LOGIN$PWD

# Cria a conexao com o banco de dados
CONEXAODB <- DBI::dbConnect(
  drv = odbc::odbc(),
  dsn = "BIG64",
  UID = MATRICULA,
  PWD = SENHA
)

# Executa a busca (alterar o nome da tabela desejada)
CAMPOS_TABELA <- DBI::dbGetQuery(CONEXAODB, "SELECT column_name, data_type, data_length, data_precision, data_scale FROM all_tab_columns WHERE table_name = 'EQUIFAX_CLIENTE' AND owner = 'BIG'")

# Encerra a conexao com o banco de dados
DBI::dbDisconnect(CONEXAODB)

view(CAMPOS_TABELA)

# Busca campos de uma tabela - [fim] ----

# Busca distribuicao de frequencias de uma var em uma tabela na BIG - ver obs - [inicio] ----

## Para uma variavel

QRY_ESPECIE_EQFX_CLT <- " SELECT COD_ESPECIE
                      COUNT(*) as frequency
                      FROM BIG.EQUIFAX_CLIENTE
                      GROUP BY COD_ESPECIE
                      ORDER BY COD_ESPECIE, COUNT(*) DESC "

# Executa query da distribuicao de frequencias
PASTA_DADOS <- "U:/"
NOME_ARQUIVO <- "conexao.csv"
CAMINHO_ARQUIVO <- file.path(PASTA_DADOS, NOME_ARQUIVO)
LOGIN <- read_delim(CAMINHO_ARQUIVO, delim = ";", col_types = cols(.default = "c"))
MATRICULA <- LOGIN$UID
SENHA <- LOGIN$PWD

# Cria a conexao com o banco de dados
CONEXAODB <- DBI::dbConnect(
  drv = odbc::odbc(),
  dsn = "BIG64",
  UID = MATRICULA,
  PWD = SENHA
)

# Executa consulta
system.time({
  
  ESPECIE_EQFX_CLT <- DBI::dbGetQuery(CONEXAODB, QRY_ESPECIE_EQFX_CLT)
  
})

# Encerra a conexao com o banco de dados
DBI::dbDisconnect(CONEXAODB)

view(ESPECIE_EQFX_CLT)

## Para duas variaveis agrupadas hierarquicamente, ex. contrato e sub-contrato

QRY_ESP_SUBGPO_EQFX_CLT <- " SELECT COD_ESPECIE, COD_SUBGRUPO,
                      COUNT(*) as frequency
                      FROM BIG.EQUIFAX_CLIENTE
                      GROUP BY COD_ESPECIE, COD_SUBGRUPO
                      ORDER BY COD_ESPECIE, COUNT(*) DESC "

# Executa query da distribuicao de frequencias
PASTA_DADOS <- "U:/"
NOME_ARQUIVO <- "conexao.csv"
CAMINHO_ARQUIVO <- file.path(PASTA_DADOS, NOME_ARQUIVO)
LOGIN <- read_delim(CAMINHO_ARQUIVO, delim = ";", col_types = cols(.default = "c"))
MATRICULA <- LOGIN$UID
SENHA <- LOGIN$PWD

# Cria a conexao com o banco de dados
CONEXAODB <- DBI::dbConnect(
  drv = odbc::odbc(),
  dsn = "BIG64",
  UID = MATRICULA,
  PWD = SENHA
)

# Executa consulta
system.time({
  
  ESP_SUBGPO_EQFX_CLT <- DBI::dbGetQuery(CONEXAODB, QRY_ESP_SUBGPO_EQFX_CLT)
  
})

# Encerra a conexao com o banco de dados
DBI::dbDisconnect(CONEXAODB)

view(ESP_SUBGPO_EQFX_CLT)

# Busca distribuicao de frequencias de uma var em uma tabela na BIG - ver obs - [fim] ----

# Consultar na BIG ultima data ref - [inicio] ----

# Seleciona apenas a var COD_CONTRATO

CODS_DF <- DF %>%
  select(COD_CONTRATO) %>%
  distinct()

# Cria vetor a partir da var COD_CONTRATO

LISTA_DF <- CODS_DF$COD_CONTRATO

# Aspas simples sao adicionadas diretamente na funcao paste

LISTA_DF <- sapply(LISTA_DF, function(x) paste0("'", x, "'"))

# Divide a lista em SUBLISTAS de ateh 1000 elementos cada, pois esse eh o
# limite para buscar codigos no banco de dados
SUBLISTAS <- split(LISTA_DF, ceiling(seq_along(LISTA_DF) / 1000))

# Inicializa um dataframe vazio para armazenar os resultados

RESULT_DF <- data.frame()

# Loop atraves das SUBLISTAS para realizar as consultas
for(i in seq_along(SUBLISTAS)) {
  # Le arquivo de conexao
  PASTA_DADOS <- "U:/"
  NOME_ARQUIVO <- "conexao.csv"
  CAMINHO_ARQUIVO <- file.path(PASTA_DADOS, NOME_ARQUIVO)
  LOGIN <- read_delim(CAMINHO_ARQUIVO, delim = ";", col_types = cols(.default = "c"))
  MATRICULA <- LOGIN$UID
  SENHA <- LOGIN$PWD
  
  # Cria a conexao com o banco de dados
  CONEXAODB <- DBI::dbConnect(
    drv = odbc::odbc(),
    dsn = "BIG64",
    UID = MATRICULA,
    PWD = SENHA
  )
  
  CODS_QUERY <- paste(SUBLISTAS[[i]], collapse = ", ")
  QRY_DF <- sprintf("SELECT * FROM BIG.POSICAO_CLI_DIA_BBJ WHERE COD_CONTRATO IN (%s)
                                      AND DATA_REFERENCIA = (SELECT MAX (DATA_REFERENCIA) FROM BIG.POSICAO_CLI_DIA_BBJ)", CODS_QUERY)
  
  cat("Executando consulta:\n", QRY_DF, "\n\n")
  
  temp_result <- tryCatch({
    DBI::dbGetQuery(CONEXAODB, QRY_DF)
  }, error = function(e) {
    cat("Erro ao executar a consulta:", e$message, "\n")
    return(data.frame())
  })
  
  RESULT_DF <- rbind(RESULT_DF, temp_result)
  
  # Encerra a conexao com o banco de dados
  DBI::dbDisconnect(CONEXAODB)
  
  # Pausa para evitar sobrecarga (opcional)
  Sys.sleep(10)  # Ajuste conforme necess?rio para dar um intervalo entre as consultas
}

RESULT_DF <- RESULT_DF %>%
  arrange(COD_CONTRATO) %>%
  distinct()

# Consultar na BIG ultima data ref - [fim] ----

# Consultar na BIG com data dinamica - [inicio] ----

# Cria datas minima e maxima para busca na clidia bkq

# Obtem data do sistema
MIN_BKQ_REF_DATE <- Sys.Date()

# Subtrai um mes
MIN_BKQ_REF_DATE <- MIN_BKQ_REF_DATE %m-% months(1)

# Altera data para primeiro dia do mes
MIN_BKQ_REF_DATE <- floor_date(MIN_BKQ_REF_DATE, "month")

# Altera para tipo character
MIN_BKQ_REF_DATE <- as.character(MIN_BKQ_REF_DATE)

# Remove hifens
MIN_BKQ_REF_DATE <- gsub("-", "", MIN_BKQ_REF_DATE)

# Obtem data do sistema
MAX_BKQ_REF_DATE <- Sys.Date()

# Subtrai um mes
MAX_BKQ_REF_DATE <- MAX_BKQ_REF_DATE %m-% months(1)

# Altera data para primeiro dia do mes subsequente
MAX_BKQ_REF_DATE <- ceiling_date(MAX_BKQ_REF_DATE, "month")

# Remove um dia. O dia resultante sera o ultimo dia do mes de referencia
MAX_BKQ_REF_DATE <- MAX_BKQ_REF_DATE - 1

# Altera para tipo character
MAX_BKQ_REF_DATE <- as.character(MAX_BKQ_REF_DATE)

# Remove hifens
MAX_BKQ_REF_DATE <- gsub("-", "", MAX_BKQ_REF_DATE) 

# Cria e executa query ops agro bkq acima de 1MM liberadas no mes anterior

# Cria query

QRY_CDIABKQ <- paste0(
  "SELECT T0.*, T1.NOME_CONTA FROM BIG.POSICAO_CLI_DIA_BKQ T0, BIG.CONTA T1
       WHERE T0.COD_CONTA_BIB = T1.COD_CONTA 
       AND (T0.DATA_REFERENCIA >= '", MIN_BKQ_REF_DATE,"')
       AND (T0.DATA_CONTRATACAO >= '", MIN_BKQ_REF_DATE, "' AND T0.DATA_CONTRATACAO <= '", MAX_BKQ_REF_DATE, "')
       AND (
        (
          SUBSTR(T1.COD_PRODUTO_GESTAO_BIB, 1,3) IN ('113') 
          OR SUBSTR(T1.COD_PRODUTO_GESTAO_BIB, 1, 5) IN ('12002', '12003')
        )
       )
       AND (T0.COD_CLIENTE >= 1000000) 
       ")


# Executa query

system.time({
  
  CDIABKQ_DF <- DBI::dbGetQuery(CONEXAODB, QRY_CDIABKQ)
  
})

# Consultar na BIG com data dinamica - [fim] ----

# Consultar na BIG CDIABBJ com data dinamica em padrao timestamp - ver OBS - [inicio] ----

# OBSERVACAO: esse formato eh um pouco mais complicado porque tem que usar a estrutura 'TO_TIMESTAMP()'
# para consultar. Isso eh facilmente resolvido com a conversao da data para character e removendo os
# hifens. Nao sei em que contexto essa estrutura de codigos pode servir, mas vou guardar para caso
# precise um dia.

# Obtem a data do sistema e a armazena na variavel chamada data_atual

DATA_INIC_PRG <- Sys.Date()

# Verifica se o dia armazenado na variavel data_atual e o primeiro dia do mes

DATA_INIC_PRG <- floor_date(DATA_INIC_PRG, "month")

# Subtrai 37 meses da data_atual ajustada pelo codigo iterativo anterior

DATA_INIC_PRG <- DATA_INIC_PRG %m-% months(37)

## Data final ##

# Obtem a data do sistema e a armazena na variavel chamada data_atual.

DATA_FIM_PRG <- Sys.Date()

# Verifica se o dia armazenado na variavel data_atual e o primeiro dia do mes.

DATA_FIM_PRG <- floor_date(DATA_FIM_PRG, "month")

# Retira um dia para obter o ultimo dia do mes passado

DATA_FIM_PRG <- DATA_FIM_PRG - 1

# Cria e executa query da posicao cli dia bbj

# Comentario:

# Essa busca na CLI_DIA vai retornar uma parte da base de onde vamos obter 
# os dados dos clientes que possuem tres ou mais prorrogacoes. A outra parte 
# da base vira da consulta a HIST_COMPLEMENTO_RURAL_DIA. A data minima da 
# data_ref da pos_cli_dia_bbj eh 2029-09-10, confirmei buscando todas as 
# observacoes da tabela e usei o comando min(pos_cli_dia_bbj$DATA_REFERENCIA). 
# Faz sentido pq o sistema bbj existe de 2019 em diante.

# Cria query cdiabbj

QRY_CDIABBJ <- paste0("SELECT DATA_REFERENCIA, DATA_CONTRATACAO, DATA_LIBERACAO, COD_CLIENTE,
  COD_CONTRATO, COD_BASE_LEGAL_RENEGOCIACAO, IND_PRORROGACAO, NUMERO_REFERENCIA_BACEN 
  FROM BIG.POSICAO_CLI_DIA_BBJ
  WHERE (DATA_REFERENCIA >= TO_TIMESTAMP('", DATA_INIC_PRG, "00:00:01', 'YYYY-MM-DD HH24:MI:SS')
                        AND DATA_REFERENCIA <= TO_TIMESTAMP('", DATA_FIM_PRG, "00:00:01', 'YYYY-MM-DD HH24:MI:SS'))")

# Executa query cdiabbj

system.time({
  
  CDIABBJ_DF <- DBI::dbGetQuery(CONEXAODB, QRY_CDIABBJ)
  
})

# Consultar na BIG CDIABBJ com data dinamica em padrao timestamp - ver OBS - [fim] ----

# Consultar BIG CDIABBJ sem padrao timestamp de data (acho mais simples) - [inicio] ----

# Cria datas minima e maxima para busca na clidia bkq

# Obtem data do sistema
DATA_INIC_PRG <- Sys.Date()

# Subtrai um mes
DATA_INIC_PRG <- DATA_INIC_PRG %m-% months(2)

# Altera data para primeiro dia do mes
DATA_INIC_PRG <- floor_date(DATA_INIC_PRG, "month")

# Altera para tipo character
DATA_INIC_PRG <- as.character(DATA_INIC_PRG)

# Remove hifens
DATA_INIC_PRG <- gsub("-", "", DATA_INIC_PRG)

# Obtem data do sistema
DATA_FIM_PRG <- Sys.Date()

# Subtrai um mes
DATA_FIM_PRG <- DATA_FIM_PRG %m-% months(1)

# Altera data para primeiro dia do mes subsequente
DATA_FIM_PRG <- ceiling_date(DATA_FIM_PRG, "month")

# Remove um dia. O dia resultante sera o ultimo dia do mes de referencia
DATA_FIM_PRG <- DATA_FIM_PRG - 1

# Altera para tipo character
DATA_FIM_PRG <- as.character(DATA_FIM_PRG)

# Remove hifens
DATA_FIM_PRG <- gsub("-", "", DATA_FIM_PRG) 

# Cria query

QRY_CDIABBJ <- paste0("SELECT DATA_REFERENCIA, DATA_CONTRATACAO, DATA_LIBERACAO, COD_CLIENTE,
  COD_CONTRATO, COD_BASE_LEGAL_RENEGOCIACAO, IND_PRORROGACAO, NUMERO_REFERENCIA_BACEN 
  FROM BIG.POSICAO_CLI_DIA_BBJ
  WHERE (DATA_REFERENCIA >= '", MIN_BKQ_REF_DATE,"'
                        AND DATA_REFERENCIA <= '", MAX_BKQ_REF_DATE, "')")

# Executa query

# Le arquivo de conexao
PASTA_DADOS <- "U:/"
NOME_ARQUIVO <- "conexao.csv"
CAMINHO_ARQUIVO <- file.path(PASTA_DADOS, NOME_ARQUIVO)
LOGIN <- read_delim(CAMINHO_ARQUIVO, delim = ";", col_types = cols(.default = "c"))
MATRICULA <- LOGIN$UID
SENHA <- LOGIN$PWD

# Cria a conexao com o banco de dados
CONEXAODB <- DBI::dbConnect(
  drv = odbc::odbc(),
  dsn = "BIG64",
  UID = MATRICULA,
  PWD = SENHA
)

system.time({
  
  CDIABBJ <- DBI::dbGetQuery(CONEXAODB, QRY_CDIABBJ)
  
})


# Consultar BIG CDIABBJ sem padrao timestamp de data (acho mais simples) - [fim] ----

# Consultar vetores maiores que 1000 nas bases - opcao 1 - [inicio] ----

# Criar dataframe com refbacen das opera??es sem comprova??o financeira do m?s
Refbacen_ConFin <- bkq3702_CompFin_Nao_SemMaq_Group %>%
  select(ref_bacen)

# Criar lista para usar no Query SQL utilizando DBI::dbQuoteString
# ? necess?rio ter a conexao aberta para usar dbQuoteString
# Portanto, mover essa parte ap?s a conexao

# Criar df vazio para armazenar resultados
CliDia_BKQ_Temp <- data.frame()

# Definir caminho e ler credenciais
PASTA_DADOS <- "U:/"
NOME_ARQUIVO <- "conexao.csv"
CAMINHO_ARQUIVO <- file.path(PASTA_DADOS, NOME_ARQUIVO)
LOGIN <- read_delim(CAMINHO_ARQUIVO, delim = ";", col_types = cols(.default = "c"))
MATRICULA <- LOGIN$UID
SENHA <- LOGIN$PWD

# Criar a conexao com o banco de dados
CONEXAODB <- DBI::dbConnect(
  drv = odbc::odbc(),
  dsn = "BIG64",
  UID = MATRICULA,
  PWD = SENHA
)

# Remover credenciais da mem?ria
rm(LOGIN)

# Escapar corretamente os valores usando dbQuoteString
refbacen_list <- dbQuoteString(CONEXAODB, Refbacen_ConFin$ref_bacen)
refbacen_list <- as.character(refbacen_list)

# Gera sublistas de 1000 (limite do Oracle para IN clause)
sublist_refbacen <- split(refbacen_list, ceiling(seq_along(refbacen_list) / 1000))

# Remover poss?veis sublistas vazias
sublist_refbacen <- sublist_refbacen[sapply(sublist_refbacen, length) > 0]

# Monitorar o tempo de execu??o
start_time <- Sys.time()

# Loop para executar consultas em sublistas
for (i in seq_along(sublist_refbacen)){
  cat("Processando sublista ", i, " de ", length(sublist_refbacen), "\n")
  
  refbacen_temp <- paste(sublist_refbacen[[i]], collapse = ", ")
  
  # Verificar se a sublista n?o est? vazia
  if(nchar(refbacen_temp) == 0){
    warning("Sublista ", i, " est? vazia. Pulando.")
    next
  }
  
  # Construir a consulta SQL usando glue
  query_CLI_DIA_BKQ <- glue("
    SELECT MAX(DATA_REFERENCIA), COD_CLIENTE, NUMERO_REFERENCIA_BACEN, COD_CONTRATO, 
                                        DATA_CONTRATACAO, DATA_LIBERACAO, DATA_VENCIMENTO_CONTRATO, IND_PESSOA_FISCAL, 
                                        VLR_FINANCIADO, COD_AGENCIA, COD_MUNICIPIO_BACEN, EMPREENDIMENTO, COD_TIPO_SEGURO
    FROM BIG.POSICAO_CLI_DIA_BKQ
    WHERE NUMERO_REFERENCIA_BACEN IN ({refbacen_temp})
    GROUP BY COD_CLIENTE, NUMERO_REFERENCIA_BACEN, COD_CONTRATO, DATA_CONTRATACAO, DATA_LIBERACAO, 
                              DATA_VENCIMENTO_CONTRATO, IND_PESSOA_FISCAL, VLR_FINANCIADO, COD_AGENCIA, COD_MUNICIPIO_BACEN, 
                              EMPREENDIMENTO, COD_TIPO_SEGURO
  ")
  
  # Imprimir parte da consulta para verifica??o
  cat("Executando consulta:\n", substr(query_CLI_DIA_BKQ, 1, 100), "...\n\n")
  
  # Executar a consulta com tratamento de erros
  temp_result <- tryCatch({
    DBI::dbGetQuery(CONEXAODB, query_CLI_DIA_BKQ)
  }, error = function(e){
    cat("Erro ao executar consulta:", e$message, "\n")
    return(NULL)
  })
  
  # Adicionar o resultado ao dataframe final se n?o for nulo
  if(!is.null(temp_result)){
    CliDia_BKQ_Temp <- rbind(CliDia_BKQ_Temp, temp_result)
  }
}

# Fechar a conexao com o banco de dados
DBI::dbDisconnect(CONEXAODB)

# Monitorar o tempo de execu??o
end_time <- Sys.time()
cat("Tempo total de execu??o: ", end_time - start_time, "\n")


# Encerra a conexao com o banco de dados
DBI::dbDisconnect(CONEXAODB)

# Consultar vetores maiores que 1000 nas bases - opcao 1 - [fim] ----

# Consultar vetores maiores que 1000 nas bases - opcao 2 - [inicio] ----

# Seleciona apenas a var COD_CONTRATO

DF_AUX <- DF_PRINCIPAL %>%
  select(NUM_ID)

# Cria vetor a partir da var NUM_ID

DF_AUX <- DF_AUX$NUM_ID

# Aspas simples sao adicionadas diretamente na funcao paste

DF_AUX <- sapply(DF_AUX, function(x) paste0("'", x, "'"))

# Divide a lista em SUBLISTAS de ateh 1000 elementos cada, pois esse eh o
# limite para buscar codigos no banco de dados
SUBLISTAS <- split(DF_AUX, ceiling(seq_along(DF_AUX) / 1000))

# Inicializa um dataframe vazio para armazenar os resultados

EMPS_FATO_BBJ <- data.frame()

# Loop atraves das SUBLISTAS para realizar as consultas
for(i in seq_along(SUBLISTAS)) {
  # Le arquivo de conexao
  PASTA_DADOS <- "U:/"
  NOME_ARQUIVO <- "conexao.csv"
  CAMINHO_ARQUIVO <- file.path(PASTA_DADOS, NOME_ARQUIVO)
  LOGIN <- read_delim(CAMINHO_ARQUIVO, delim = ";", col_types = cols(.default = "c"))
  MATRICULA <- LOGIN$UID
  SENHA <- LOGIN$PWD
  
  # Cria a conexao com o banco de dados
  CONEXAODB <- DBI::dbConnect(
    drv = odbc::odbc(),
    dsn = "BIG64",
    UID = MATRICULA,
    PWD = SENHA
  )
  
  CODS_QUERY <- paste(SUBLISTAS[[i]], collapse = ", ")
  QRY_DF_AUX <- sprintf("SELECT * FROM BIG.POSICAO_CLI_DIA_BBJ WHERE COD_CONTRATO IN (%s)", CODS_QUERY)
  
  cat("Executando consulta:\n", QRY_DF_AUX, "\n\n")
  
  temp_result <- tryCatch({
    DBI::dbGetQuery(conexaoDB, QRY_DF_AUX)
  }, error = function(e) {
    cat("Erro ao executar a consulta:", e$message, "\n")
    return(data.frame())
  })
  
  EMPS_FATO_BBJ <- rbind(EMPS_FATO_BBJ, temp_result)
  
  # Encerra a conexao com o banco de dados
  DBI::dbDisconnect(CONEXAODB)
  
  # Pausa para evitar sobrecarga (opcional)
  Sys.sleep(10)  # Ajuste conforme necess?rio para dar um intervalo entre as consultas
}

# Ordena o df
EMPS_FATO_BBJ <- EMPS_FATO_BBJ %>%
  arrange(NUM_ID, EMPREENDIMENTO)

EMPS_FATO_BBJ <- EMPS_FATO_BBJ %>%
  distinct()

# Consultar vetores maiores que 1000 nas bases - opcao 2 - [fim] ----

# [Comentario] sobre como o RStudio interpreta vetores, matrizes e listas - explica em parte codigo acima [inicio] ----

# Na linha do codigo 

# CODS_FATO_BBJ <- CODS_FATO_BBJ %>% 
#   select(COD_CONTRATO)

# o objeto CODS_FATO_BBJ continua a ser um dataframe, apesar de possuir apenas
# uma variavel.

# Eh possivel confirmar isso usando o comando print(class(CODS_FATO_BBJ)), o
# qual ira retornar o seguinte:

# [1] "tbl_df"     "tbl"        "data.frame"

# O que significa que eh um dataframe do tipo 'tibble', o qual possui mais
# propriedades do que um df de base do R.

# Porem, para que uma consulta possa ser realizada atraves de uma query, eh
# preciso que o objeto seja um vetor.

# Por isso, o comando CODS_FATO_BBJ <- CODS_FATO_BBJ$COD_CONTRATO eh necessario,
# pois ele transforma o df CODS_FATO_BBJ em um vetor composto dos elementos da
# variavel COD_CONTRATO.

# Eh possivel ver que o objeto se transformou em um vetor porque, quando executamos
# o comando print(class(CODS_FATO_BBJ)) apos a transformacao anterior, o console
# retorna:

# [1] "character"

# Ok, e qual a diferenca entre uma variavel contida em um dataframe que, quando
# queremos saber sua classe, executamos o comando print((class(CODS_FATO_BBJ$COD_CONTRATO)))
# retornando [1] "character", e um objeto que tamb?m retorna [1] "character"
# quando executamos o comando print(class(CODS_FATO_BBJ))?

# A resposta eh: nenhuma!

# Em ambos casos, os objetos sao vetores. 

# A diferenca eh que a variavel COD_CONTRATO (que eh um vetor do tipo 'character') 
# esta contida em um dataframe, enquanto o objeto CODS_FATO_BBJ eh um vetor
# do tipo character nao contido em um df.

# Exemplo de lista:

OBJETO_TESTE <- sapply(1:3, function(x) 1:x)

# O objeto 'OBJETO_TESTE' eh um exemplo de objeto do tipo 'lista', o qual
# consiste em, como o nome diz, uma lista em que armazena elementos podem 
# dos mais variados tipos, como numeros, texto, vetores, matrizes e ate
# mesmo outras listas.

# No caso acima, o 'OBJETO_TESTE' eh uma lista que armazena vetores de di-
# ferentes tamanhos. Ao executarmos o comando 

# print(OBJETO_TESTE)

# obtemos o seguinte resultado:

# [[1]]
# [1] 1
# 
# [[2]]
# [1] 1 2
# 
# [[3]]
# [1] 1 2 3

# Que consiste em uma lista onde o primeiro vetor (dado por [[1]]) eh um 
# numero 1, o segundo vetor (dado por [[2]]) eh uma sequencia de 1 a 2 e o 
# terceiro vetor (dado por [[3]]) eh uma sequencia de 1 a 3.

# [Comentario] sobre como o RStudio interpreta vetores, matrizes e listas - explica em parte codigo acima [fim] ----

# Busca infos CLIENTES por cod cliente - [inicio] ----

# Busca infos de cadastro do cliente
# Seleciona apenas a var COD_CONTRATO

CODS_CLTS <- CUSTEIOS_DF %>%
  select(COD_CLIENTE) %>%
  distinct() %>%
  filter(!is.na(COD_CLIENTE))

# Cria vetor a partir da var COD_CONTRATO

CODS_CLTS <- CODS_CLTS$COD_CLIENTE

# Aspas simples sao adicionadas diretamente na funcao paste

CODS_CLTS <- sapply(CODS_CLTS, function(x) paste0("'", x, "'"))

# Divide a lista em SUBLISTAS de ateh 1000 elementos cada, pois esse eh o
# limite para buscar codigos no banco de dados
SUBLISTAS <- split(CODS_CLTS, ceiling(seq_along(CODS_CLTS) / 1000))

# Inicializa um dataframe vazio para armazenar os resultados

CLTS_DF <- data.frame()

# Loop atraves das SUBLISTAS para realizar as consultas
for(i in seq_along(SUBLISTAS)) {
  # Le arquivo de conexao
  PASTA_DADOS <- "U:/"
  NOME_ARQUIVO <- "conexao.csv"
  CAMINHO_ARQUIVO <- file.path(PASTA_DADOS, NOME_ARQUIVO)
  LOGIN <- read_delim(CAMINHO_ARQUIVO, delim = ";", col_types = cols(.default = "c"))
  MATRICULA <- LOGIN$UID
  SENHA <- LOGIN$PWD
  
  # Cria a conexao com o banco de dados
  CONEXAODB <- DBI::dbConnect(
    drv = odbc::odbc(),
    dsn = "BIG64",
    UID = MATRICULA,
    PWD = SENHA
  )
  
  CODS_QUERY <- paste(SUBLISTAS[[i]], collapse = ", ")
  QRY_CLTS <- sprintf("SELECT NOME_CLIENTE, COD_CLIENTE, TELEFONE, IDADE, 
                              GRAU_INSTRUCAO, AGENCIA_CADASTRO, AGENCIA_RELAC, ENDERECO, 
                              NUMERO, BAIRRO, CIDADE, UF, CEP, CIC_CONJUGE, DATA_OBITO, 
                              IND_IMPEDIDO, IND_IMP_PROTESTO, DT_ABERTURA_CADASTRO, 
                              DATA_ULT_LIBERACAO, VLR_CL, VLR_RESP_BANRISUL, 
                              VLR_RESP_SISBACEN 
                              FROM BIG.CLIENTE WHERE COD_CLIENTE IN (%s)", CODS_QUERY)
  
  cat("Executando consulta:\n", QRY_CLTS, "\n\n")
  
  temp_result <- tryCatch({
    DBI::dbGetQuery(CONEXAODB, QRY_CLTS)
  }, error = function(e) {
    cat("Erro ao executar a consulta:", e$message, "\n")
    return(data.frame())
  })
  
  CLTS_DF <- rbind(CLTS_DF, temp_result)
  
  # Encerra a conexao com o banco de dados
  DBI::dbDisconnect(CONEXAODB)
  
  # Pausa para evitar sobrecarga (opcional)
  Sys.sleep(10)  # Ajuste conforme necess?rio para dar um intervalo entre as consultas
}

CLTS_DF <- CLTS_DF %>%
  select(COD_CLIENTE, NOME_CLIENTE, everything()) %>%
  arrange(COD_CLIENTE)

CLTS_DF <- CLTS_DF %>%
  filter(!NOME_CLIENTE == 'SERVICO_CLI_MES SEM CADASTRO NO BAL')

# Busca infos CLIENTES por cod cliente - [fim] ----

# Query dist freq por cod especie, cod natureza e cod subgrupo - util em tabelas muito grandes - [inicio] ----

# Busca base cadastro impedimento equifax

base_folder <- "I:/Agronegocios-GPIA/GPIA/Fiscalizacao/Script"
file_name <- "Dist_freq_cod_especie_e_subgrupo_equifax_cliente.csv"
file_path <- file.path(base_folder, file_name)
cad_imped_eqfx <- readr::read_delim(file_path, delim = ";", col_types = readr::cols(.default = "c"))

# Isola somente a var COD_ESPECIE no df

cod_especie <- cad_imped_eqfx %>%
  select(COD_ESPECIE)

# Mantem apenas COD_ESPECIE unicos, nao duplicados no df

cod_especie <- cod_especie %>%
  distinct(COD_ESPECIE)

# Converte o df para uma lista

cod_especie <- cod_especie$COD_ESPECIE

# Aspas simples s?o adicionadas diretamente na fun??o paste
cod_especie <- sapply(cod_especie, function(x) paste0("'", x, "'"))

# Inicializa um dataframe vazio para armazenar os resultados

dist_freq_esp_nat_sub <- data.frame()

for(i in seq_along(cod_especie)) {
  
  # Le arquivo de conexao
  PASTA_DADOS <- "U:/"
  NOME_ARQUIVO <- "conexao.csv"
  CAMINHO_ARQUIVO <- file.path(PASTA_DADOS, NOME_ARQUIVO)
  LOGIN <- read_delim(CAMINHO_ARQUIVO, delim = ";", col_types = cols(.default = "c"))
  MATRICULA <- LOGIN$UID
  SENHA <- LOGIN$PWD
  
  # Cria a conexao com o banco de dados
  CONEXAODB <- DBI::dbConnect(
    drv = odbc::odbc(),
    dsn = "BIG64",
    UID = MATRICULA,
    PWD = SENHA
  )
  
  cods_query <- cod_especie[[i]] # J? que cada itera??o pega um ?nico c?digo de esp?cie
  qry_dist_freq_esp_nat_sub <- sprintf("SELECT COD_ESPECIE, COD_NATUREZA, COD_SUBGRUPO,
                                        COUNT(*) AS count
                                        FROM BIG.V_IMPEDIMENTO_CLIENTE
                                        WHERE COD_ESPECIE = %s
                                        GROUP BY COD_ESPECIE, COD_NATUREZA, COD_SUBGRUPO
                                        ORDER BY COD_ESPECIE, COD_NATUREZA, COUNT(*) DESC", cods_query)
  
  cat("Executando consulta:\n", qry_dist_freq_esp_nat_sub, "\n\n")
  
  temp_result <- tryCatch({
    DBI::dbGetQuery(CONEXAODB, qry_dist_freq_esp_nat_sub)
  }, error = function(e) {
    cat("Erro ao executar a consulta:", e$message, "\n")
    return(data.frame())
  })
  
  dist_freq_esp_nat_sub <- rbind(dist_freq_esp_nat_sub, temp_result)
  
  # Encerra a conexao com o banco de dados
  DBI::dbDisconnect(CONEXAODB)
  
  # Pausa para evitar sobrecarga (opcional)
  Sys.sleep(10)  # Ajuste conforme necess?rio para dar um intervalo entre as consultas 
}

# Query dist freq por cod especie, cod natureza e cod subgrupo - util em tabelas muito grandes - [fim] ----

# Busca informacoes em bases muito pesadas - Ex. Cli dia geral - [inicio] ----

# Busca cods bpw das operacoes
BASE_OPS_BPW <- BASE_OPS %>%
  mutate(CONTRATO_BBJ = as.character(CONTRATO_BBJ)) %>%
  mutate(COD_CONTRATO_BIG = if_else(OP_MIGRADAS != 0, COD_CONTRATO, CONTRATO_BBJ)) %>%
  filter(SISTEMA == "BPW") %>%
  select(COD_CONTRATO_BIG, COD_CONTRATO, CONTRATO_BBJ) %>%
  distinct()

# Adiciona zeros para os cods bpw ficarem com 10 caracteres

SHORT_IDS <- which(nchar(BASE_OPS_BPW$COD_CONTRATO_BIG) < 9)
BASE_OPS_BPW$COD_CONTRATO_BIG[SHORT_IDS] <- str_pad(BASE_OPS_BPW$COD_CONTRATO_BIG[SHORT_IDS], width = 9, side = "left", pad = "0")

# Busca cods bpw das operacoes
BASE_OPS_NAO_BPW <- BASE_OPS %>%
  mutate(CONTRATO_BBJ = as.character(CONTRATO_BBJ)) %>%
  mutate(COD_CONTRATO_BIG = if_else(OP_MIGRADAS != 0, COD_CONTRATO, CONTRATO_BBJ)) %>%
  filter(SISTEMA != "BPW") %>%
  select(COD_CONTRATO_BIG, COD_CONTRATO, CONTRATO_BBJ) %>%
  distinct()

# Adiciona zeros para os cods bpw ficarem com 15 caracteres

SHORT_IDS <- which(nchar(BASE_OPS_NAO_BPW$COD_CONTRATO_BIG) < 15)
BASE_OPS_NAO_BPW$COD_CONTRATO_BIG[SHORT_IDS] <- str_pad(BASE_OPS_NAO_BPW$COD_CONTRATO_BIG[SHORT_IDS], width = 15, side = "left", pad = "0")

# Mantem somente cods de contrato big no df bpw
BASE_OPS_BPW <- BASE_OPS_BPW %>%
  select(COD_CONTRATO_BIG)

# Mantem somente cods de contrato big no df diferente de bpw
BASE_OPS_NAO_BPW <- BASE_OPS_NAO_BPW %>%
  select(COD_CONTRATO_BIG)

# Empilha cods de consulta na cli dia geral
CODS_OPS_CDIA <- BASE_OPS_NAO_BPW %>%
  bind_rows(BASE_OPS_BPW)

# Cria vetor a partir da var COD_CONTRATO_BIG
# Supondo que CODS_OPS_CDIA ? um dataframe com a coluna COD_CONTRATO_BIG
CODS_OPS_CDIA <- CODS_OPS_CDIA$COD_CONTRATO_BIG

# Adiciona aspas simples em torno de cada COD_CONTRATO
CODS_OPS_CDIA <- sapply(CODS_OPS_CDIA, function(x) paste0("'", x, "'"))

# Divide a lista em SUBLISTAS de at? 1000 elementos cada
SUBLISTAS <- split(CODS_OPS_CDIA, ceiling(seq_along(CODS_OPS_CDIA) / 1000))

# Inicializa um dataframe vazio para armazenar os resultados
CLI_DIA <- data.frame()

# Cria uma sequ?ncia de datas no formato AAAAMMDD
data_inicio <- as.Date("2024-01-01")
data_fim <- as.Date("2024-01-07")
# data_fim <- Sys.Date() - 1
sequencia_datas <- seq.Date(from = data_inicio, to = data_fim, by = "day")
sequencia_datas_formatted <- format(sequencia_datas, "%Y%m%d")

# Loop atrav?s das datas
for(data_atual in sequencia_datas_formatted) {
  
  cat("Processando a data:", data_atual, "\n")
  
  # Loop atrav?s das sublistas de COD_CONTRATO
  for(i in seq_along(SUBLISTAS)) {
    
    # Define o caminho do arquivo de conexao
    PASTA_DADOS <- "U:/"
    NOME_ARQUIVO <- "conexao.csv"
    CAMINHO_ARQUIVO <- file.path(PASTA_DADOS, NOME_ARQUIVO)
    
    # L? as informa??es de login
    LOGIN <- tryCatch({
      read_delim(CAMINHO_ARQUIVO, delim = ";", col_types = cols(.default = "c"))
    }, error = function(e) {
      cat("Erro ao ler o arquivo de login:", e$message, "\n")
      return(NULL)
    })
    
    # Verifica se o login foi lido corretamente
    if(is.null(LOGIN)) {
      next  # Pula para a pr?xima itera??o se houver erro no login
    }
    
    MATRICULA <- LOGIN$UID
    SENHA <- LOGIN$PWD
    
    # Cria a conexao com o banco de dados
    CONEXAODB <- tryCatch({
      dbConnect(
        drv = odbc::odbc(),
        dsn = "BIG64",
        UID = MATRICULA,
        PWD = SENHA
      )
    }, error = function(e) {
      cat("Erro ao conectar ao banco de dados:", e$message, "\n")
      return(NULL)
    })
    
    # Verifica se a conexao foi estabelecida
    if(is.null(CONEXAODB)) {
      next  # Pula para a pr?xima itera??o se houver erro na conexao
    }
    
    # Prepara a lista de contratos para a consulta
    CODS_QUERY <- paste(SUBLISTAS[[i]], collapse = ", ")
    
    # Monta a consulta SQL usando INNER JOIN com subconsulta
    QRY_CODS <- sprintf("
      SELECT COD_CLIENTE, COD_CONTRATO, COD_STATUS_CONTRATO, DATA_REFERENCIA, DATA_TRANSF_CL, 
                    DATA_VENCIMENTO_CONTRATO, DATA_VENCTO_PROXIMA_PARCELA, 
                    IND_PESSOA_FISCAL, IND_RENEGOCIACAO, MOTIVO_CANC_CONTRATO, PRAZO_OPERACAO, 
                    QTDE_DIAS_ATRASO, QTDE_DIAS_A_VENCER, QTDE_TOTAL_PARCELAS, SALDO_ATUAL, 
                    SALDO_CONTABIL, SALDO_CREDOR, SALDO_DEVEDOR, SISTEMA_ORIGEM, SITUACAO_OPER, 
                    VALOR_A_VENCER, VALOR_CONTRATACAO, VALOR_LIQUIDO_CONCESSAO, VALOR_TRANSFERIDO_CL
      FROM (
          SELECT COD_CLIENTE, COD_CONTRATO, COD_STATUS_CONTRATO, DATA_REFERENCIA, DATA_TRANSF_CL, 
                    DATA_VENCIMENTO_CONTRATO, DATA_VENCTO_PROXIMA_PARCELA, 
                    IND_PESSOA_FISCAL, IND_RENEGOCIACAO, MOTIVO_CANC_CONTRATO, PRAZO_OPERACAO, 
                    QTDE_DIAS_ATRASO, QTDE_DIAS_A_VENCER, QTDE_TOTAL_PARCELAS, SALDO_ATUAL, 
                    SALDO_CONTABIL, SALDO_CREDOR, SALDO_DEVEDOR, SISTEMA_ORIGEM, SITUACAO_OPER, 
                    VALOR_A_VENCER, VALOR_CONTRATACAO, VALOR_LIQUIDO_CONCESSAO, VALOR_TRANSFERIDO_CL,
                 ROW_NUMBER() OVER (PARTITION BY COD_CONTRATO ORDER BY DATA_REFERENCIA DESC) AS rn
          FROM BIG.POSICAO_CLI_DIA
          WHERE DATA_REFERENCIA = '%s'
            AND COD_CONTRATO IN (%s)
      ) sub
      WHERE rn = 1
    ", data_atual, CODS_QUERY)
    
    # Exibe a consulta que ser? executada (para fins de depura??o)
    cat("Executando consulta:\n", QRY_CODS, "\n\n")
    
    # Executa a consulta e captura poss?veis erros
    temp_result <- tryCatch({
      dbGetQuery(CONEXAODB, QRY_CODS)
    }, error = function(e) {
      cat("Erro ao executar a consulta:", e$message, "\n")
      return(data.frame())
    })
    
    # Verifica se a consulta retornou algum resultado
    if(nrow(temp_result) > 0) {
      CLI_DIA <- rbind(CLI_DIA, temp_result)
    }
    
    # Encerra a conexao com o banco de dados
    tryCatch({
      dbDisconnect(CONEXAODB)
    }, error = function(e) {
      cat("Erro ao desconectar do banco de dados:", e$message, "\n")
    })
    
    # Pausa para evitar sobrecarga (opcional)
    Sys.sleep(1)  # Reduziu para 1 segundo para otimizar o tempo
  }
  
  cat("Conclu?do processamento para a data:", data_atual, "\n\n")
}

# Busca informacoes em bases muito pesadas - Ex. Cli dia geral - [fim] ----

# Estuda campos da cli dia geral - [inicio] ----

# Le arquivo de conexao

PASTA_DADOS <- "U:/"
NOME_ARQUIVO <- "conexao.csv"
CAMINHO_ARQUIVO <- file.path(PASTA_DADOS, NOME_ARQUIVO)
LOGIN <- read_delim(CAMINHO_ARQUIVO, delim = ";", col_types = cols(.default = "c"))
MATRICULA <- LOGIN$UID
SENHA <- LOGIN$PWD

# Cria a conexao com o banco de dados
CONEXAODB <- DBI::dbConnect(
  drv = odbc::odbc(),
  dsn = "BIG64",
  UID = MATRICULA,
  PWD = SENHA
)

# Executa a busca (alterar o nome da tabela desejada)
CAMPOS_TABELA <- DBI::dbGetQuery(CONEXAODB, "SELECT column_name, data_type, data_length, data_precision, data_scale FROM all_tab_columns WHERE table_name = 'POSICAO_CLI_DIA' AND owner = 'BIG'")

# Encerra a conexao com o banco de dados
DBI::dbDisconnect(CONEXAODB)

CAMPOS_TABELA <- CAMPOS_TABELA %>%
  arrange(COLUMN_NAME)

view(CAMPOS_TABELA)

# Estuda campos da cli dia geral - [fim] ----

# Consulta um contrato - [inicio] ----

QRY_CONTRATO <- "SELECT * FROM BIG.PARCELA_PAGA_BBJ WHERE COD_CONTRATO = '00052320014004'"

# Le arquivo de conexao
PASTA_DADOS <- "U:"
NOME_ARQUIVO <- "conexao.csv"
CAMINHO_ARQUIVO <- file.path(PASTA_DADOS, NOME_ARQUIVO)
LOGIN <- read_delim(CAMINHO_ARQUIVO, delim = ";", col_types = cols(.default = "c"))
MATRICULA <- LOGIN$UID
SENHA <- LOGIN$PWD

# Cria a conexao com o banco de dados
CONEXAODB <- DBI::dbConnect(
  drv = odbc::odbc(),
  dsn = "BIG64",
  UID = MATRICULA,
  PWD = SENHA
)

CONTRATO <- DBI::dbGetQuery(CONEXAODB, QRY_CONTRATO)

# 002024003014001

# Consulta um contrato - [fim] ----

# Consulta pagamento de um contrato - [inicio] ----

# Le arquivo de conexao
PASTA_DADOS <- "U:/"
NOME_ARQUIVO <- "conexao.csv"
CAMINHO_ARQUIVO <- file.path(PASTA_DADOS, NOME_ARQUIVO)
LOGIN <- read_delim(CAMINHO_ARQUIVO, delim = ";", col_types = cols(.default = "c"))
MATRICULA <- LOGIN$UID
SENHA <- LOGIN$PWD

# Cria a conexao com o banco de dados
CONEXAODB <- DBI::dbConnect(
  drv = odbc::odbc(),
  dsn = "BIG64",
  UID = MATRICULA,
  PWD = SENHA
)

QRY_PGTO_PARC <- sprintf("SELECT COD_CLIENTE, COD_CONTRATO, COD_STATUS_CONTRATO, COD_AGENCIA, 
                    CONTA_CORRENTE, DATA_CONTRATACAO, DATA_REFERENCIA, DATA_REGISTRO, 
                    DATA_TRANSF_CL, DATA_VENCIMENTO_CONTRATO, DATA_VENCTO_PROXIMA_PARCELA, 
                    IND_PESSOA_FISCAL, IND_RENEGOCIACAO, MOTIVO_CANC_CONTRATO, PRAZO_OPERACAO, 
                    QTDE_DIAS_ATRASO, QTDE_DIAS_A_VENCER, QTDE_TOTAL_PARCELAS, SALDO_ATUAL, 
                    SALDO_CONTABIL, SALDO_CREDOR, SALDO_DEVEDOR, SISTEMA_ORIGEM, SITUACAO_OPER, 
                    VALOR_A_VENCER, VALOR_CONTRATACAO, VALOR_LIQUIDO_CONCESSAO, VALOR_TRANSFERIDO_CL
                    FROM BIG.POSICAO_CLI_DIA
                    WHERE COD_CONTRATO = '088681436'
                    AND DATA_REFERENCIA = '20240102'
                    AND ROWNUM <= 1000")

PGTO_PARC <- DBI::dbGetQuery(CONEXAODB, QRY_PGTO_PARC)

DBI::dbDisconnect(CONEXAODB)

sort(colnames(PGTO_PARC))

PGTO_PARC <- PGTO_PARC %>%
  arrange(COD_CONTRATO, DATA_REFERENCIA) %>%
  select(COD_CONTRATO, DATA_REFERENCIA, everything()) %>%
  group_by(COD_CONTRATO) %>%
  mutate(SEQ_COD_CONTRATO = row_number(),
         ULT_COD_CONTRATO = if_else(row_number() == n(), TRUE, FALSE)) %>%
  select(COD_CONTRATO, DATA_REFERENCIA, DATA_VENCIMENTO_PARCELA, VLR_PARCELA,
         VLR_PARCELA_VENCIMENTO, everything())


PGTO_PARC <- PGTO_PARC %>%
  filter(ULT_COD_CONTRATO == TRUE) %>%
  select(COD_CONTRATO, DATA_REFERENCIA, DATA_VENCIMENTO_PARCELA, VLR_PARCELA,
         VLR_PARCELA_VENCIMENTO, everything()) %>%
  select(-SEQ_COD_CONTRATO, -ULT_COD_CONTRATO) %>%
  ungroup()

sort(colnames(PGTO_PARC))

# Consulta pagamento de um contrato - [fim] ----

# Teste ultimo contrato - [inicio] ----

# Le arquivo de conexao
PASTA_DADOS <- "U:/"
NOME_ARQUIVO <- "conexao.csv"
CAMINHO_ARQUIVO <- file.path(PASTA_DADOS, NOME_ARQUIVO)
LOGIN <- read_delim(CAMINHO_ARQUIVO, delim = ";", col_types = cols(.default = "c"))
MATRICULA <- LOGIN$UID
SENHA <- LOGIN$PWD

# Cria a conexao com o banco de dados
CONEXAODB <- DBI::dbConnect(
  drv = odbc::odbc(),
  dsn = "BIG64",
  UID = MATRICULA,
  PWD = SENHA
)

QRY_TESTE <- sprintf("SELECT COD_CONTRATO, DATA_REFERENCIA
                          FROM (
                          SELECT COD_CONTRATO, DATA_REFERENCIA,
                          ROW_NUMBER() OVER (PARTITION BY COD_CONTRATO ORDER BY DATA_REFERENCIA DESC) AS rn
                          FROM BIG.POSICAO_CLI_DIA
                          WHERE DATA_REFERENCIA = '20240102'
                          AND COD_CONTRATO = '000000103278805'
                          ) sub
                          WHERE rn = 1")

TESTE <- DBI::dbGetQuery(CONEXAODB, QRY_TESTE)

DBI::dbDisconnect(CONEXAODB)

# Teste ultimo contrato - [fim] ----

# Estuda campos da pagamento parcela - [inicio] ----

# Le arquivo de conexao

PASTA_DADOS <- "U:/"
NOME_ARQUIVO <- "conexao.csv"
CAMINHO_ARQUIVO <- file.path(PASTA_DADOS, NOME_ARQUIVO)
LOGIN <- read_delim(CAMINHO_ARQUIVO, delim = ";", col_types = cols(.default = "c"))
MATRICULA <- LOGIN$UID
SENHA <- LOGIN$PWD

# Cria a conexao com o banco de dados
CONEXAODB <- DBI::dbConnect(
  drv = odbc::odbc(),
  dsn = "BIG64",
  UID = MATRICULA,
  PWD = SENHA
)

# Executa a busca (alterar o nome da tabela desejada)
CAMPOS_TABELA <- DBI::dbGetQuery(CONEXAODB, "SELECT column_name, data_type, data_length, data_precision, data_scale FROM all_tab_columns WHERE table_name = 'PAGAMENTO_PARCELA' AND owner = 'BIG'")

# Encerra a conexao com o banco de dados
DBI::dbDisconnect(CONEXAODB)

CAMPOS_TABELA <- CAMPOS_TABELA %>%
  arrange(COLUMN_NAME)

view(CAMPOS_TABELA)

# Estuda campos da pagamento parcela - [fim] ----

# Consulta pagamento de um contrato na PAGAMENTO PARCELA - [inicio] ----

# Le arquivo de conexao
PASTA_DADOS <- "U:/"
NOME_ARQUIVO <- "conexao.csv"
CAMINHO_ARQUIVO <- file.path(PASTA_DADOS, NOME_ARQUIVO)
LOGIN <- read_delim(CAMINHO_ARQUIVO, delim = ";", col_types = cols(.default = "c"))
MATRICULA <- LOGIN$UID
SENHA <- LOGIN$PWD

# Cria a conexao com o banco de dados
CONEXAODB <- DBI::dbConnect(
  drv = odbc::odbc(),
  dsn = "BIG64",
  UID = MATRICULA,
  PWD = SENHA
)

QRY_PGTO_PARC <- sprintf("SELECT *
                    FROM BIG.PAGAMENTO_PARCELA
                    WHERE SISTEMA_ORIGEM = 'BBJ'
                    AND ROWNUM <= 100")

PGTO_PARC <- DBI::dbGetQuery(CONEXAODB, QRY_PGTO_PARC)

DBI::dbDisconnect(CONEXAODB)

sort(colnames(PGTO_PARC))

PGTO_PARC <- PGTO_PARC %>%
  arrange(COD_CONTRATO, DATA_REFERENCIA) %>%
  select(COD_CONTRATO, DATA_REFERENCIA, everything()) %>%
  group_by(COD_CONTRATO) %>%
  mutate(SEQ_COD_CONTRATO = row_number(),
         ULT_COD_CONTRATO = if_else(row_number() == n(), TRUE, FALSE)) %>%
  select(COD_CONTRATO, DATA_REFERENCIA, DATA_VENCIMENTO_PARCELA, VLR_PARCELA,
         VLR_PARCELA_VENCIMENTO, everything())


PGTO_PARC <- PGTO_PARC %>%
  filter(ULT_COD_CONTRATO == TRUE) %>%
  select(COD_CONTRATO, DATA_REFERENCIA, DATA_VENCIMENTO_PARCELA, VLR_PARCELA,
         VLR_PARCELA_VENCIMENTO, everything()) %>%
  select(-SEQ_COD_CONTRATO, -ULT_COD_CONTRATO) %>%
  ungroup()

sort(colnames(PGTO_PARC))

# Consulta pagamento de um contrato na PAGAMENTO PARCELA - [fim] ----

# Busca FATO GESTAO CREDITO - [inicio] ----

# Busca parametros de conexao
PASTA_DADOS <- "I:/Agronegocios-GPIA/GPIA/9.Douglas/3_rotinas/Login_Oracle"
NOME_ARQUIVO <- "conexao.csv"
CAMINHO_ARQUIVO <- file.path(PASTA_DADOS, NOME_ARQUIVO)
LOGIN <- read_delim(CAMINHO_ARQUIVO, delim = ";", col_types = cols(.default = "c"))
MATRICULA <- LOGIN$UID
SENHA <- LOGIN$PWD

# Cria a conexao com o banco de dados
CONEXAODB <- DBI::dbConnect(
  drv = odbc::odbc(),
  dsn = "BIG64",
  UID = MATRICULA,
  PWD = SENHA
)

# A data de inicio e fim da referencia da fato eh a mesma da cli dia bkq, que eh o
# mes atual de referencia. Assim:

#inic_data_ref_fato <- min_bkq_ref_date
inic_data_ref_fato <- "20241001"

#fim_data_ref_fato <- max_bkq_ref_date
fim_data_ref_fato <- "20241031"


# Query fato que puxa o mes com data fixa

query_fato <- paste0("SELECT T0.*, T1.NOME_CONTA, 
  T1.IND_CONTA, T1.COD_PRODUTO_GESTAO 
  FROM BIG.FATO_GESTAO_CREDITO T0, BIG.CONTA T1 
  WHERE T0.COD_CONTA = T1.COD_CONTA AND 
  (T0.DATA_CONCESSAO >= '",inic_data_ref_fato ,"' AND T0.DATA_CONCESSAO <= '", fim_data_ref_fato ,"') 
  AND (T0.DATA_REFERENCIA >= '", inic_data_ref_fato ,"')
  AND ((SUBSTR(T1.COD_PRODUTO_GESTAO_BIB, 1,3) 
  IN ('113') OR SUBSTR(T1.COD_PRODUTO_GESTAO_BIB, 1, 5) IN ('12002', '12003')))")

# Realiza de fato a consulta da fato

system.time({
  
  fato <- DBI::dbGetQuery(CONEXAODB, query_fato)
  
})

# Busca FATO GESTAO CREDITO - [fim] ----

#### AGENDAMENTO DE SCRIPTS ####

install.packages("taskscheduleR")
install.packages("bizdays")

library(bizdays)
library(taskscheduleR)

# Carrega a tabela de feriados
PASTA_ARQUIVO <- "I:/Agronegocios-GPIA/GPIA/9.Douglas/3_rotinas/Bases_uteis"
NOME_ARQUIVO <- "feriados_nacionais_e_regionais_2010_2099.csv"
CAMINHO_ARQUIVO <- file.path(PASTA_ARQUIVO, NOME_ARQUIVO)
FERIADOS_DF <- readr::read_delim(CAMINHO_ARQUIVO, delim = ";", col_types = readr::cols(.default = "c"))

# Converte a coluna de datas para o formato Date e remove valores NA
FERIADOS_DF$Data_Feriado <- as.Date(FERIADOS_DF$Data_Feriado)
LISTA_FERIADOS <- unique(FERIADOS_DF$Data_Feriado)
LISTA_FERIADOS <- LISTA_FERIADOS[!is.na(LISTA_FERIADOS)]  # Remove NAs

# Verifique se todas as datas s?o v?lidas
if (all(!is.na(LISTA_FERIADOS) & is.finite(LISTA_FERIADOS))) {
  # Cria o calend?rio de dias ?teis do RS, excluindo finais de semana e feriados
  create.calendar("cal_RS", weekdays = c("saturday", "sunday"), holidays = LISTA_FERIADOS, start.date = "2010-01-01")
  cat("Calend?rio criado com sucesso!")
} else {
  cat("H? datas inv?lidas em LISTA_FERIADOS. Verifique os dados.")
}


# Verifique se o dia atual e um dia util no calendario do RS
if (is.bizday(Sys.Date(), "cal_RS")) {
  # Executa o script .R externo
  source("I:/Agronegocios-GPIA/GPIA/Rebate MP 1247/Planilha_Consulta_Rebates/Consulta_Bonus_BBJ.R")
  
  print("Executando o script porque hoje e um dia util no RS.")
} else {
  print("Hoje nao e um dia util no RS. Script nao sera executado.")
}

# Agende a tarefa para rodar todos os dias as 9h
taskscheduler_create(taskname = "Base_Rebates_Diaria",
                     rscript = "I:/Agronegocios-GPIA/GPIA/Rebate MP 1247/Planilha_Consulta_Rebates/Consulta_Bonus_BBJ.R",
                     schedule = "DAILY",
                     starttime = "11:30",
                     days = "*")

# (opcional) para listar todas as tarefas que estao no agendador
# taskscheduler_ls()

# (opcional) para deletar a tarefa do agendador
# taskscheduler_delete(taskname = "Executar_Script_Dias_Uteis")

#### LE ARQUIVO EM EXCEL ####

# Le planilha com todas as colunas em tipo texto - [inicio] ----

PASTA_ARQUIVO <- "I:/Agronegocios-GPIA/GPIA/9.Douglas/1_andamento_prioritario/1_Script_Vivi_xmls_bpw"
ARQUIVO_ARQUIVO <- "Lista 1 Vivi.xlsx"
CAMINHO_ARQUIVO <- file.path(PASTA_ARQUIVO, ARQUIVO_ARQUIVO)
LISTA_VIVI_DF <- read_xlsx(CAMINHO_ARQUIVO, col_types = 'text')

# Le planilha com todas as colunas em tipo texto - [fim] ----

# Le planilha com colunas especificas - [inicio] ----

PASTA_DADOS <- "I:/Agronegocios/DADOSCPT/BANDADOS/LINHAS"
NOME_ARQUIVO <- "LINHAS BPW X BOU.xlsm"
CAMINHO_ARQUIVO <- file.path(PASTA_DADOS, NOME_ARQUIVO)
LINHAS_DF <- read.xlsx(CAMINHO_ARQUIVO, 
                       sheet = 'DADOS', 
                       cols = c(2, 9, 18, 19))

# Le planilha com colunas especificas - [fim] ----

#### LEITURA E GRAVACAO EM .CSV ####

# Le arquivo em .csv - [inicio] ----

PASTA_ARQUIVO <- "I:/Meu/Diretorio"
NOME_ARQUIVO <- "Meu_Arquivo.csv"
CAMINHO_ARQUIVO <- file.path(PASTA_ARQUIVO, NOME_ARQUIVO)
ARQUIVO_DF <- readr::read_delim(CAMINHO_ARQUIVO, 
                                delim = ";", 
                                col_types = readr::cols(.default = "c"))

# Le arquivo em .csv - [fim] ----

# Le arquivo em .csv - codificacao ANSI ou Windows-1252 - [inicio] ----

PASTA_ARQUIVO <- "I:/Meu/Diretorio"
NOME_ARQUIVO <- "Meu_Arquivo.csv"
CAMINHO_ARQUIVO <- file.path(PASTA_ARQUIVO, NOME_ARQUIVO)
DF <- read_delim(CAMINHO_ARQUIVO, delim = ",", 
                                   col_names = TRUE, 
                                   col_types = readr::cols(.default = "c"),
                                   locale = locale(encoding = "Windows-1252"))

# Le arquivo em .csv - codificacao ANSI ou Windows-1252 - [fim] ----

# Grava arquivo em .csv - [inicio] ----

PASTA_DESTINO <- "C:/Users/B41379/Desktop/Projetos/0_BASES"
NOME_ARQUIVO <- "eqfxclt_esp_dist.csv"
CAMINHO_ARQUIVO <- file.path(PASTA_DESTINO, NOME_ARQUIVO)
write_delim(DF, CAMINHO_ARQUIVO, delim = ";")

# Grava arquivo em .csv - [fim] ----

# Grava arquivo em .xlsx com data e hora no nome - [inicio] ----

PASTA_DADOS <- "C:/Users/B41379/Desktop"
DATA_HORA <- Sys.time()
DATA_HORA <- as.character(DATA_HORA)
DATA_HORA <- gsub("[-: \\s ]", "_", DATA_HORA)
NOME_ARQUIVO <- paste0("Nome_Arquivo_",DATA_HORA,".xlsx")
CAMINHO_ARQUIVO <- file.path(PASTA_DADOS, NOME_ARQUIVO)
write_xlsx(DF, CAMINHO_ARQUIVO)

# Grava arquivo em .csv com data e hora no nome - [fim] ----

#### TRATAMENTO DE DATAS ####

# Retorna infos do padrao do sistema sobre codificacao e localidade ou idioma do sistema - [inicio] ----

# Obtem infos sobre os padroes do sistema - util para saber codificacao e idioma
sessionInfo()

## Define a localidade para portugues e codificacao UTF-8
Sys.setlocale("LC_TIME", "pt_BR.UTF-8") # geralmente em Linux ou Mac, mas no meu computador pessoal
# com Windows eh essa a localidade. Sempre eh bom usar sessionInfo() para saber a localidade e a
# codificacao, e trabalhar no mesmo padrao.

# Ou, quando for o caso, para portugues e codificacao ANSI, que eh a windows-1252

# Retorna infos do padrao do sistema sobre codificacao e localidade ou idioma do sistema - [fim] ----

# Retornar apenas partes de uma data, como dias, dias da semana, meses, etc - [inicio] ----

## Apos saber e configurar a codificacao e localizacao de datas corretas, proceder com os
## seguintes codigos abaixo a depender da necessidade.

# Retorna o mes abreviado
MES_ABREV_DATA <- format(as.Date("2024-01-01"), "%b") # retorna 'jan'

# Retorna o mes completo
MES_EXT_COMP_DATA <- format(as.Date("2024-01-01"), "%B") # retorna 'janeiro'

# Retorna o mes character
MES_NUM_DATA <- format(as.Date("2024-01-01"), "%m") # retorna '01'

# Retorna o dia da semana abreviado
DIA_ABREV_DATA <- format(as.Date("2024-01-01"), "%a") # retorna 'seg' porque, de fato,
# dia 01-01-2024 foi uma segunda-feira

# Retorna o dia da semana por extenso
DIA_EXT_COMP_DATA <- format(as.Date("2024-01-01"), "%A") # retorna 'segunda-feira'

# Retorna o dia do MES em formato character
DIA_NUM_DATA <- format(as.Date("2024-01-01"), "%d") # retorna '01'

# Retorna os dois ultimos digitos do numero do ano em formato character
ANO_NUM_ABREV_DATA <- format(as.Date("2024-01-01"), "%y") # retorna '24'

# Retorna os dois ultimos digitos do numero do ano em formato character
ANO_NUM_COMPLETO_DATA <- format(as.Date("2024-01-01"), "%Y") # retorna '2024'

# Retornar apenas partes de uma data, como dias, meses, etc - [fim] ----

# Funcao para retornar o quinto dia util de uma data - [inicio] ----

# Importa a base de feriados
FERIADOS_DF <- fread("I:/Agronegocios-GPIA/GPIA/9.Douglas/7_arquivo/2024/Bases_Uteis/feriados.csv", sep = ";")
FERIADOS_DF$DATA_FERIADO <- as.Date(FERIADOS_DF$DATA_FERIADO, format = "%Y-%m-%d")

# Cria uma funcao para identificar dias uteis
eh_dia_util <- function(data, feriados) {
  !wday(data) %in% c(1, 7) & # Exclui domingos (1) e sabados (7)
    !data %in% feriados      # Exclui feriados
}

# Calcular o quinto dia util subsequente ao mes
calcular_quinto_dia_util <- function(data_inicial, feriados) {
  # Avancar para o primeiro dia do mes subsequente
  mes_subsequente <- ceiling_date(data_inicial, "month")
  
  # Criar sequencia de datas no mes subsequente
  dias_mes <- seq(mes_subsequente, ceiling_date(mes_subsequente, "month") - days(1), by = "day")
  
  # Filtrar apenas os dias uteis
  dias_uteis <- dias_mes[eh_dia_util(dias_mes, FERIADOS_DF$DATA_FERIADO)]
  
  # Retornar o quinto dia util
  return(dias_uteis[5])
}

# Se var de referencia nao estiver em tipo 'date', converte variavel data de tipo posixct para date

DF_REFERENCIA$DATA_REFERENCIA <- as.Date(DF_REFERENCIA$DATA_REFERENCIA)

# Aplica a funcao que obtem a data de vencimento referente ao quinto dia util do mes
# subsequente ao pagamento da parcela

DF$DATA_QUINTO_DIA_UTIL <- sapply(DF_REFERENCIA$DATA_REFERENCIA, calcular_quinto_dia_util, feriados = feriados)

# Ajusta de formato de data numerica para formato date padrao no R

DF$DATA_QUINTO_DIA_UTIL <- as.Date(DF$DATA_QUINTO_DIA_UTIL, origin = '1970-01-01')

# Altera data de vencimento de tipo date para character e no formato ddmmaaaa
DF$DATA_QUINTO_DIA_UTIL <- format(DF$DATA_QUINTO_DIA_UTIL, format = "%d%m%Y")

# Funcao para retornar o quinto dia util de uma data - [fim] ----


# Funcao para calcular o ultimo dia util do mes de referencia - [inicio] ----
calcular_ultimo_dia_util <- function(data_referencia, feriados) {
  # Obter o ultimo dia do mes de referencia
  ultimo_dia_mes <- ceiling_date(data_referencia, "month") - days(1)
  
  # Criar sequencia de datas do ultimo dia do mes para o primeiro dia do mes
  dias_reverso <- seq(ultimo_dia_mes, floor_date(data_referencia, "month"), by = "-1 day")
  
  # Filtrar os dias uteis
  dias_uteis <- dias_reverso[eh_dia_util(dias_reverso, feriados)]
  
  # Retornar o primeiro dia util encontrado na sequencia reversa
  return(dias_uteis[1])
}

# Exemplo de uso - data isolada, fora de um df

data_referencia <- as.Date("2024-11-23", format = "%Y-%m-%d")
ultimo_dia_util <- calcular_ultimo_dia_util(data_referencia, FERIADOS_DF$DATA_FERIADO)
print(ultimo_dia_util)

# Exemplo de uso - data pertencente a um df

# Se var de referencia nao estiver em tipo 'date', converte variavel data de tipo posixct para date

DF_REFERENCIA$DATA_REFERENCIA <- as.Date(DF_REFERENCIA$DATA_REFERENCIA)

# Aplica a funcao que obtem a data de vencimento referente ao quinto dia util do mes
# subsequente ao pagamento da parcela

DF$DATA_ULTIMO_DIA_UTIL <- sapply(DF_REFERENCIA$DATA_REFERENCIA, calcular_ultimo_dia_util, feriados = feriados)

# Ajusta de formato de data numerica para formato date padrao no R

DF$DATA_ULTIMO_DIA_UTIL <- as.Date(DF$DATA_ULTIMO_DIA_UTIL, origin = '1970-01-01')

# Altera data de vencimento de tipo date para character e no formato ddmmaaaa
DF$DATA_ULTIMO_DIA_UTIL <- format(DF$DATA_ULTIMO_DIA_UTIL, format = "%d%m%Y")

# Funcao para calcular o ultimo dia util do mes de referencia - [fim] ----

# Cria var de siglas dos meses do ano em portugues - [inicio] ----

library(dplyr)

# Define localidade para portugues (se for codificacao UTF-8)
Sys.setlocale("LC_TIME", "pt_BR.UTF-8")
# OOOUUU
# (se for codificacao ANSI ou Windows-1252)
Sys.setlocale("LC_TIME", "Portuguese_Brazil.1252")

# Exemplo de df
DF <- data.frame(
  DATA = as.Date(c("2024-01-10", "2024-02-15", "2024-03-20", "2024-01-25", "2024-03-30"))
)

# Adiciona coluna com sigla do mês
DF <- DF %>%
  mutate(
    MES_SIGLA = toupper(format(DATA, "%b")),
    
    # Transforma em fator ordenado. Isso faz com que seja possivel depois ordenar
    # o df em ordem crescente de mes (e nao alfabetica)
    # Assim, a ordem ficara como 'JAN', 'FEV', 'MAR', etc., e nao 'ABR', 'AGO', 'DEZ', etc.
    MES_SIGLA_ORD = factor(
      MES_SIGLA,
      levels = c("JAN", "FEV", "MAR", "ABR", "MAI", "JUN",
                 "JUL", "AGO", "SET", "OUT", "NOV", "DEZ"),
      ordered = TRUE
    )
  )

# Cria var de siglas dos meses do ano em portugues - [fim] ----


# Converte formato UTC(aaaa-mm-ddThh:mm:ssZ) para date padrao - [inicio] ----

# Remove os ultimos 10 caracteres gerados pelo salvamento em .csv de uma data em formato date obtida 
# a partir de uma query em uma base odbc em sql. A data fica um string do tipo: '2023-12-01T00:48:35Z'

# Ao importar datas e horas de um banco de dados, o RStudio geralmente as converte para o formato
# POSIXct, que ? um formato de timestamp proprio do RStudio. Porem, ao salvar em .csv, o formato
# 2023-12-01T00:48:35Z que aparece (ao inves de simplesmente 2023-12-01) eh o formato ISO 8601,
# que eh um padrao internacional de representacao de datas e horas.

# O 'T' eh um delimitador que separa a parte da data da parte da hora. Eh uma convencao do padrao
# ISO 8601.

# O 'Z', ao final do padrao, indica o 'tempo universal coordenado' (UTC), que eh a 'hora zero',
# significando que a hora esta no fuso horario UTC

# Para podermos trabalhar em formato data novamente, eh preciso excluir os ultimos 10 caracteres,
# isto eh, a sequencia 'T00:48:35Z'. Para isso, eh necessario usar a seguinte sequencia de codigo:

Meu_DFf$Minha_Data <- substr(Meu_DFf$Minha_Data, 1, nchar(Meu_DFf$Minha_Data) - 10)

# Isso fara com que a sequencia de caracteres agora fique com "cara" de data: '2023-12-01'.
# Para melhor compreender o que o codigo acima faz, segue uma breve explicacao:

# 1) O primeiro elemento da funcao substr(), neste caso a variavel 'Meu_DFf$Minha_Data' 
# eh o vetor de caracteres.

# 2) O segundo argumento, '1', indica o ponto inicial da substring, isto eh, o primeiro caractere,
# que sera o inicio do recorte da cadeia de texto, nesse caso sera o primeiro algarismo '2' da data.
# Caso esse segundo argumento fosse '2', entao o inicio seria o numero '0'

# 3) O terceiro argumento, 'nchar(Meu_DFf$Minha_Data) - 10' retorna o numero de caracteres em cada
# elemento de 'Minha_Data' e, subtraindo 10, removo os ultimos 10 caracteres.

# Isso resultara em uma coluna onde cada valor teve seus ultimos 10 caracteres removidos.

# Porem, eh preciso lembrar que a variavel ainda eh uma sequencia de caracteres, isto eh, uma variavel do
# tipo texto. Portanto, eh preciso converte-la para tipo data. Isso sera feito utilizando a seguinte
# linha de codigo:

eqfx_clt_esp75$DATA_REFERENCIA <- as.Date(eqfx_clt_esp75$DATA_REFERENCIA)

# A funcao as.Date() altera automaticamente 

# Converte formato UTC(aaaa-mm-ddThh:mm:ssZ) para date padrao - [fim] ----

# Converte data em character mm/dd/yyyy para padrao R yyyy-mm-dd - [inicio] ----

DF$VAR_DATA <- mdy(DF$VAR_DATA) %>%
  as.Date(format = "%Y-%m-%d")

# Converte data em character mm/dd/yyyy para padrao R yyyy-mm-dd - [fim] ----

# Converte data em character yyyy/mm/dd para padrao R yyyy-mm-dd - [inicio] ----

DF$VAR_DATA <- ymd(DF$VAR_DATA) %>%
  as.Date(format = "%Y-%m-%d")

# Converte data em character yyyy/mm/dd para padrao R yyyy-mm-dd - [fim] ----

# Converte data em character 'dd/mm/yyyy' para date padrao - [inicio] ----

DF$VAR_DATA <- as.Date(DF$VAR_DATA, format = "%d/%m/%Y")

# OBS - a funcao as.Date(), juntamente com o argumento 'format = "%d/%m/%Y"', converte uma data
# que esteja em tipo character para date. O argumento 'format = "%d/%m/%Y"' eh responsavel nao
# pela visualizacao resultante da conversao, mas sim pela leitura do formato que esta armazenada
# a data em tipo character. Caso nao seja utilizado o argumento format corretamente, a data
# nao sera lida corretamente.

# OBS2 - nao eh possivel alterar o formato de visualizacao da data apos a conversao de character
# para date. Ou seja, uma data que estava em formato character, por exemplo, que fosse 30/10/2023,
# apos a conversao sera mostrada na forma 2023-10-30. Se tentarmos alterar a visualizacao
# utilizando a funcao format(), a variavel voltara a ser do tipo character, embora seja exibida
# como 30-10-2023. Esta eh uma limitacao do RStudio. Portanto, caso seja realmente necessario
# exibir a data em formato dd-mm-yyyy, recomenda-se realizar tal manipulacao apenas no final do
# codigo, quando nao for necessario mais utilizar a variavel como data.

# Converte data em character 'dd/mm/yyyy' para date padrao - [fim] ----

# Altera o formato da data de %Y-%m-%d (tipo date padrao do R) para %d-%m-%Y (tipo character) - [inicio] ----

DF$VAR_DATA <- format(DF$VAR_DATA, format = "%d-%m-%Y")

# Altera o formato da data de %Y-%m-%d (tipo date padrao do R) para %d-%m-%Y (tipo character) - [fim] ----

#### DATA WRANGLING ####

# Marca grupos de prorrogacoes - [inicio] ----
PRORROGS_DF_MARCADAS <- PRORROGS_DF %>%
  arrange(COD_CLIENTE, COD_CONTRATO, DATA_REFERENCIA) %>%
  # Certifica-se de que a variavel de renegociacao seja numerica, se necessario
  group_by(COD_CLIENTE, COD_CONTRATO) %>%
  # Cria uma variavel auxiliar "reneg" que e TRUE quando ha renegociacao (codigo diferente de 0, 17, 19 ou 999)
  mutate(reneg = !(COD_BASE_LEGAL_RENEGOCIACAO %in% N_PRORROG)) %>%
  select(COD_CLIENTE, COD_CONTRATO, DATA_REFERENCIA, COD_BASE_LEGAL_RENEGOCIACAO, everything())


PRORROGS_DF_MARCADAS <- PRORROGS_DF_MARCADAS %>%
  # Se houver ao menos uma renegociacao, identifique a posicao da primeira ocorrencia
  mutate(first_reneg = ifelse(any(reneg), which(reneg)[1], NA_integer_))

PRORROGS_DF_MARCADAS <- PRORROGS_DF_MARCADAS %>%
  # VAR1: linhas com numero da linha (dentro do grupo) maior ou igual a posicao da primeira renegociacao recebem 1; caso contrario 0.
  mutate(MARCA_PRORROG_PERMANENTE = ifelse(!is.na(first_reneg) & row_number() >= first_reneg, 1, 0)) %>%
  # VAR2: se houver renegociacao no grupo, apenas a ultima linha recebe 1; senao, todas 0.
  mutate(INDICA_PRORROG = ifelse(any(reneg) & row_number() == n(), 1, 0)) %>%
  ungroup() %>%
  # Remove as variaveis auxiliares
  select(-reneg, -first_reneg)

# Marca grupos de prorrogacoes - [fim] ----

# Converte todas vars numericas em um df para character - [inicio] ----

# Armazeno os nomes das variaveis numericas
VARS_NUMERICAS <- names(DF)[sapply(DF, is.numeric)]

# Converte as vars para character em um df de exportacao (util quando for exportar para PowerBI)
DF_EXP <- DF %>%
  mutate(across(all_of(VARS_NUMERICAS), as.character))

# Converte pontos para virgulas nas colunas convertidas
DF <- DF %>%
  mutate(across(all_of(VARS_NUMERICAS), ~ gsub("\\.", "\\,", .)))

# Converte todas vars numericas em um df para character - [fim] ----

# Visualiza observacoes especificas dentro de um df - [inicio] ----

# Ex. ver somente obs que um ID number eh o abaixo
# Eh possivel, por exemplo, criar listas com o auxilio do which()
# e depois usar o %in% para visualizar somente aqueles com
# determinado criterio
View(DF %>% filter(ID_NUMBER == '00000292009038'))

# Visualiza observacoes especificas dentro de um df - [inicio] ----

# Distriuicao de frequencias de uma variavel - [inicio] ----

# Obs: executando os tres codigos abaixo, sera gerado um df que eh possivel
# ordenar a variavel de interesse, assim como as frequencias. Caso seja rodada
# apenas a primeira linha, o df fica travado e nao eh possivel ordenar as variaveis
# de interesse e de frequencia da tabela gerada.

# Obs2: ignorar o aviso que ocorre ao executar o comando as.tibble().

TABELA <- as.data.frame(sort(table(DF$VAR)))
TABELA <- as.matrix.data.frame(TABELA)
TABELA <- as_tibble(TABELA)

# Distriuicao de frequencias de uma variavel - [fim] ----

# Limpeza de strings - [inicio] ----

## Limpa strings do df

# clean_names() faz uma limpeza basica no dataframe inteiro
df <- clean_names(df)

# Para aplicar as funcoes apenas nas colunas do DF:
colnames(df) <- clean_names(colnames(df))

# Cria funcao para limpar dataframe
limpar_dataframe <- function(df) {
  # Modificar os nomes das colunas
  colnames(df) <- stringr::str_replace_all(colnames(df), "[ /\\\\\\-\\.]", "_")
  
  # Iterar sobre cada coluna do dataframe
  df <- df %>% mutate(across(where(is.character), ~ {
    # Substituir caracteres especiais e remover acentos
    . <- stringi::stri_trans_general(., "Latin-ASCII")
    . <- stringr::str_replace_all(., "[/\\\\]", "-") # Substitui barras e contrabarras por hifens
    . <- stringr::str_replace_all(., "?", "c") # Substitui cedilha minusculo por c
    . <- stringr::str_replace_all(., "?", "C") # Substitui cedilha maiusculo por C
    . # Retorna a coluna modificada
  }))
  
  return(df)
}

# Criar a funcao para limpar os nomes das colunas
limpar_nomes_colunas <- function(nomes) {
  nomes %>%
    stri_trans_general("Latin-ASCII") %>%  # Remove acentos e cedilhas
    gsub("[^A-Za-z0-9]", "_", .) %>%  # Substitui todos os caracteres nao alfanumericos por underscore
    gsub("_+", "_", .) %>%  # Substitui multiplos underscores consecutivos por apenas um
    gsub("^_|_$", "", .) %>%  # Remove underscores no inicio ou fim da string
    toupper()  # Converte para maiusculas
}

# Aplicar a fun??o ao dataframe
colnames(DF) <- limpar_nomes_colunas(colnames(DF))

## Substitui espacos por underscores que por ventura nao foram

# Para nomes de colunas
colnames(ADITIVOS_DF) <- gsub(" ", "_", colnames(ADITIVOS_DF))

# Ou para colunas especificas do df
DF$VAR <- gsub(" ", "_", DF$VAR)

# Cria funcoes de limpeza de strings - [fim] ----

# Identifica duplicados e marca ate a ultima repeticao do contrato - [inicio] ----

# Identifica e marca numeros de ID (contratos, cpfs, etc,) que se repetem no df. Acredito que neste projeto os
# os registros duplicados nao deverao ser removidos por conta da diferenca de saldo contabil que deve
# ser acompanhada.

DF <- DF %>%
  arrange(ID_NUMBER) %>%
  group_by(ID_NUMBER) %>%
  mutate(SEQ_ID_NUMBER = row_number(),
         ULT_ID_NUMBER = if_else(row_number() == n(), TRUE, FALSE))

# Identifica duplicados e marca ate a ultima repeticao do contrato - [fim] ----

# Marca operacoes coletivas - [inicio] ----

DF <- DF %>%
  group_by(REF_BACEN) %>%  # Agrupa pelo ID do contrato
  mutate(OP_COLETIVA = if_else(n_distinct(CPF_CNPJ) > 1, 1, 0)) %>%  # Marca como 1 se houver mais de um cliente
  ungroup() %>% # Remove o agrupamento
  
  # Marca operacoes coletivas - [fim] ----

# Filtra para manter no df somente a ultima ocorrencia do contrato - [inicio] ----

# Se precisar, filtrar somente os ultimos cod_contrato e remove a variavel de marcacao
DF <- DF %>%
  filter(ULT_ID_NUMBER) %>%
  select(-ULT_ID_NUMBER)

# Filtra para manter no df somente a ultima ocorrencia do contrato - [fim] ----

# Converte VARS em tipo character de padrao americano numerico para padrao global numerico - ver observacao - [inicio] ----

DF <- DF %>%
  mutate(VAR = as.numeric(str_replace_all(str_replace_all(VAR, fixed("."), ""), fixed(","), ".")))

# Converte VARS em tipo character de padrao americano numerico para padrao global numerico - ver observacao - [fim] ----

# 1. Tratamento de NAs (valores faltantes) ----

# Verifica se alguma variavel tem registros NA - [inicio] ----

any(is.na(df))

# Verifica se alguma variavel tem registros NA - [fim] ----

# Conta quantos registros NA tem cada coluna - [inicio] ----

DF_NAS <- as.data.frame(colSums(is.na(DF)))

view(DF_NAS)

# Conta quantos registros NA tem cada coluna - [fim] ----

# Removendo linhas onde a coluna 'VAR' tem NA - [inicio] ----
DF_LIMPO <- DF %>% 
  filter(!is.na(VAR))

# Removendo linhas onde a coluna 'VAR' tem NA - [fim] ----

# Removo espacos antes e depois de datas em tipo string - [inicio] ----

# Se nao fizer isso, a conversao me retorna registros NA. 
# Util para executar antes de conversao de datas

DF$DATA <- trimws(DF$DATA)

# Removo espacos antes e depois de datas em tipo string - [fim] ----

# Recodifica os valores faltantes (NA) de uma var para zero - [inicio] ----
DF <- DF %>%
  mutate(VAR = replace_na(VAR, 0))

# Recodifica os valores faltantes (NA) de uma var para zero - [fim] ----

# Filtra df por um determinado mes e ano - [inicio] ----

# No caso abaixo, vai criar um df subconjunto com todas observacoes cujo mes eh igual a 5 e ano igual a 2023.

DF_SUBCONJUNTO <- subset(DF, month(VAR_DATA_HORA) == 5 & year(VAR_DATA_HORA) == 2023)

# Filtra df por um determinado mes e ano - [fim] ----

# Adiciona um zero a esquerda de uma determinada var - ver observacoes - [inicio] ----

# OBS1: Adicao para todas as observacoes, nao difere tamanho de caracteres. 
# Se uma linha tem o ID 123 e outra linha tem o ID 1234, o resultado sera 0123 e
# 01234.

# OBS2: Serve para adicionar qualquer caractere a esquerda.

DF <- DF %>%
  mutate(VAR_PADDED = str_pad(VAR, width = nchar(VAR) + 1, pad = "0")) %>%
  select(-VAR) %>%
  rename(VAR = VAR_PADDED)

# Adiciona um zero a esquerda de uma determinada var - ver observacoes - [fim] ----

# ADICIONO ZEROS a esquerda de contratos para manter uma quantidade fixa de caracteres na coluna - [inicio] ----

# Obs: no csv os cpfs e cnpjs ja vem com zeros a esquerda e 14 caracteres. Preciso ajustar
# apenas os numeros de contrato.

SHORT_IDS <- which(nchar(ADESOES_XLSX$CONTRATO_1) < 15)
ADESOES_XLSX$CONTRATO_1[SHORT_IDS] <- str_pad(ADESOES_XLSX$CONTRATO_1[SHORT_IDS], width = 15, side = "left", pad = "0")

# ADICIONO ZEROS a esquerda de contratos para manter uma quantidade fixa de caracteres na coluna - [fim]

# REMOVE ZEROS a esquerda de contratos para manter uma quantidade fixa de caracteres na coluna - [inicio] ----

# Cria uma lista que vai identificar as posicoes do df que ha cod contrato maiores que 15.

LONG_IDS <- which(nchar(FATO_DF$COD_CONTRATO) > 15)

# Atribui as posicoes da lista a alteracao de diminuir os caracteres a esquerda ate que os 
# tamanhos da var sejam iguais a 15.

FATO_DF$COD_CONTRATO[LONG_IDS] <- substr(FATO_DF$COD_CONTRATO[LONG_IDS], 
                                         start = nchar(FATO_DF$COD_CONTRATO[LONG_IDS]) - 14, 
                                         stop = nchar(FATO_DF$COD_CONTRATO[LONG_IDS]))

# REMOVE ZEROS a esquerda de contratos para manter uma quantidade fixa de caracteres na coluna - [fim] ----

#### CODIGOS UTEIS PARA ESTUDO DE UM OU MAIS DATAFRAMES ####

# Identifica diferencas entre duas variaveis - [inicio] ----

# Cria um vetor logico de diferencas entre duas vars de um mesmo DF
VETOR_DIFERENCAS <- DF$VAR1 != DF$VAR2

# Conta diferencas
NUM_DIFERENCAS <- sum(VETOR_DIFERENCAS, na.rm = TRUE)
print(paste("Numero de diferencas:", NUM_DIFERENCAS))

# Visualiza as linhas com diferencas
LINHAS_COM_DIFERENCAS <- DF[VETOR_DIFERENCAS, ]
LINHAS_COM_DIFERENCAS <- select(LINHAS_COM_DIFERENCAS, COD_CONTRATO, VAR1,
                                VAR2, everything())

# Identifica diferencas entre duas variaveis - [fim] ----

# Verifica quais COD_CONTRATO tiveram mais de um match - [inicio] ----

# 1 - Conta o numero de vezes que cada COD_CONTRATO aparece em ambos dataframes:

CONTA_PRIMEIRO_DF <- DF1 %>%
  group_by(COD_CONTRATO) %>%
  summarise(n = n())

CONTA_SEGUNDO_DF <- DF2 %>%
  group_by(COD_CONTRATO) %>%
  summarise(n = n())

# 2 - Junta as contagens dos dfs acima atraves de um left join e filtra pelos COD_CONTRATO 
# cujas contagens do segundo dataframe sao maiores do que 1.
MULTIPLOS_MATCHES <- left_join(CONTA_PRIMEIRO_DF, CONTA_SEGUNDO_DF, by = "COD_CONTRATO") %>%
  filter(n.y > 1)

View(MULTIPLOS_MATCHES)

# Verifica quais COD_CONTRATO tiveram mais de um match - [fim] ----

# Saber se existe um determinado valor ou sequencia de strings consta em uma coluna.- ler como funciona - [inicio] ----

# A funcao grepl() verifica se ha qualquer valor ou valores com essa sequencia de caracteres. 
# Caso eu quisesse saber se existem valores que TERMINEM com a sequencia de caracteres '101494330', 
# ao inves de utilizar '101494330', eu utilizaria o sinal '$' ao final da sequencia, isto eh,  '101294330$'. 
# O sinal '$' indica que estou procurando sequencias de caracteres que tenham o valor desejado AO FINAL da sequencia. 
# Caso eu nao utilize o '$', entao a funcao grepl() vai buscar a sequencia de caracteres informada em qualquer posicao
# dos valores daquela coluna.
# O comando vai retornar TRUE para toda observacao que houver aqueles valores ou FALSE caso contrario.

# Ok, e nao eh mais facil filtrar direto no filtro da visualizacao do dataframe?

# Pode ser caso a ideia seja apenas analisar. 

# Porem, se desejo obter os codigos para realizar alguma manipulacao em dataframe, preciso conhecer os comandos.

# O any(grepl('num_id', DF$COD_CONTRATO)) busca SE existe algum COD_CONTRATO no df com aquela sequencia de numeros

# tb util quando o df eh muito grande e nao eh possivel visualizar todo o df para filtrar.

EXISTE_COD_CONTRATO <- any(grepl("101494330", fato_mensal$COD_CONTRATO)) 
print(EXISTE_COD_CONTRATO)

# A funcao which(grepl('num_id', DF$COD_CONTRATO)) retorna qual linha ou linhas ha os valores dentro da variavel COD_CONTRATO

LINHAS_COD_CONTRATO <- which(grepl("102722434", DF$COD_CONTRATO))
print(LINHAS_COD_CONTRATO)

# Visualiza tudo em um dataframe

CODS_CONTRATO_102722434 <- DF[LINHAS_COD_CONTRATO, ]
View(CODS_CONTRATO_102722434)

# Para visualizar uma unica linha em forma de um dataframe, eh preciso criar um dataframe com apenas
# a linha em questao. Para isso, basta atribuir um nome qualquer ao df linha (no exemplo abaixo,
# criei um df com o nome de LINHA_108385) e associar ao dataframe desejado indicando a linha
# dentro do primeiro argumento dentro dos colchetes. No caso abaixo, a linha desejada foi a de
# numero 108385. Nao tem nada apos a virgula alem de um espaco em branco e o sinal de fechamento
# do colchete. Eh assim mesmo.

LINHA_108385 <- DF[108385, ]
View(LINHA_108385)

# Por fim, visualiza o dataframe linha com o View()

# Saber se existe um determinado valor ou sequencia de strings consta em uma coluna.- ler como funciona - [inicio] ----

# [Exemplo do codigo acima] Busca, por exemplo, se e quantos clientes possuem o nome VALIATI no df - [inicio] ----

# tb util quando o df eh muito grande e nao eh possivel visualizar todo o df para filtrar.

EXISTEM_CLIENTES <- any(grepl("VALIATI", VALIDACOES_PAGAS$NOME_BENEFICIARIO)) 
print(EXISTEM_CLIENTES)

# A funcao which(grepl('num_id', DF$COD_CONTRATO)) retorna qual linha ou linhas ha os valores dentro da variavel COD_CONTRATO

QUAIS_LINHAS_CLIENTE <- which(grepl("VALIATI", VALIDACOES_PAGAS$NOME_BENEFICIARIO)) 
print(QUAIS_LINHAS_CLIENTE)

# Visualiza tudo em um dataframe

LINHAS_VALIATI <- VALIDACOES_PAGAS[QUAIS_LINHAS_CLIENTE, ]
View(LINHAS_VALIATI)

# [Exemplo do codigo acima] Busca, por exemplo, se e quantos clientes possuem o nome VALIATI no df - [fim] ----

# Busca contratos em um DF que nao estao em outro DF - [inicio] ----

# Cria lista com IDs do df1 sem IDs do df2

# Obs: para saber os contratos que estao no DF2 e nao estao no DF1, basta inverter a ordem
# dos DFs dentro do setdiff()

IDS_EM_DF1_E_NAO_EM_DF2 <- setdiff(DF1$COD_CONTRATO, DF2$COD_CONTRATO)

# Filtra DF1 para manter apenas os contratos que estao no DF1 sem os contratos do DF2
CONTRATOS_DF1_SEM_CONTRATOS_DF2 <- VALIDACOES %>%
  filter(COD_CONTRATO %in% IDS_EM_DF1_E_NAO_EM_DF2)

# Busca contratos em um DF que nao estao em outro DF - [fim] ----

# Identifica diferencas em colunas de strings em um mesmo DF - [inicio] ----

# Cria uma lista logica de diferencas
dif_COD_MODALIDADE <- left_base_completa$COD_MODALIDADE.x != left_base_completa$COD_MODALIDADE.y

# Conta as diferencas
num_dif_COD_MODALIDADE <- sum(dif_COD_MODALIDADE, na.rm = TRUE)
print(paste("Number of differences:", num_dif_COD_MODALIDADE))

# Visualiza as linhas com as diferencas em um df separado
different_rows <- left_base_completa[dif_COD_MODALIDADE, ]
different_rows <- select(different_rows, COD_CONTRATO, COD_MODALIDADE.x, COD_MODALIDADE.y, COD_PRODUTO.x, COD_PRODUTO.y, everything())
tail_different_rows <- tail(different_rows, 10000)
view(tail_different_rows)

# Print the indices of the differences (optional)
different_indices <- which(differences)
print(paste("Indices of differences:", different_indices))

# Cria query para buscar o cadastro do cliente

qry_cliente <- paste0("SELECT * FROM BIG.CLIENTE 
 WHERE COD_CLIENTE LIKE '%32649975091%'")

# Executa a query criada

system.time({
  
  cliente_teste <- DBI::dbGetQuery(conexaoDB, qry_cliente)
  
})

view(cliente_teste)

# Identifica diferencas em colunas de strings em um mesmo DF - [inicio] ----

# MEDE o quao diferentes podem ser valores de colunas de strings em DIFERENTES DF - [inicio] ----

# Carrega os pacotes necess?rios
library(stringdist)
library(dplyr)

# Dataframes de exemplo
df1 <- data.frame(nome = c("JO?O MOTTA", "MARIA SILVA", "CARLOS SANTOS"))

df2 <- data.frame(nome = c("JO?O MOTA", "MARIA SILVA", "CARLOS DOS SANTOS"))

# Fun??o para comparar nomes entre dois dataframes com um limite de similaridade
comparar_nomes <- function(nomes1, nomes2, threshold = 0.8) {
  # Calcula a similaridade de Jaro-Winkler entre todos os pares de nomes e converte para um vetor
  similaridade_matrix <- 1 - stringdist::stringdistmatrix(nomes1, nomes2, method = "jw")
  
  # Converte a matriz de similaridade para um dataframe
  comparacoes <- as.data.frame(as.table(similaridade_matrix))
  
  # Nomeia as colunas do dataframe de compara??o
  colnames(comparacoes) <- c("index_df1", "index_df2", "similaridade")
  
  # Adiciona os nomes originais de cada dataframe
  comparacoes$nome_df1 <- nomes1[comparacoes$index_df1]
  comparacoes$nome_df2 <- nomes2[comparacoes$index_df2]
  
  # Filtra os resultados para mostrar apenas aqueles com similaridade acima do threshold
  similares <- comparacoes %>% filter(similaridade >= threshold)
  
  # Seleciona apenas as colunas relevantes
  similares <- similares %>% select(nome_df1, nome_df2, similaridade)
  
  return(similares)
}

# Executa a fun??o com uma similaridade m?nima de 0.8
resultado <- comparar_nomes(df1$nome, df2$nome, threshold = 0.8)
View(resultado)

TABELA_RS <- read_excel("C:/Users/B41379/Downloads/Nomes_RS.xlsx")

TABELA_IBGE <- read_excel("C:/Users/B41379/Downloads/Nomes_IBGE.xlsx")

COMPARACAO1 <- comparar_nomes(TABELA_RS$NOMES_RS, TABELA_IBGE$NOMES_IBGE, threshold = 0.95)

View(COMPARACAO1)

# MEDE o quao diferentes podem ser valores de colunas de strings em DIFERENTES DF - [fim] ----

# # Busca distribuicao de frequencias dos cod_especie da tabela inteira #
# 
# qry_imped_dist_esp_nat_sub_5 <- " SELECT COD_ESPECIE, COD_NATUREZA, COD_SUBGRUPO,
#                       COUNT(*) AS count 
#                       FROM BIG.V_IMPEDIMENTO_CLIENTE 
#                       WHERE COD_ESPECIE = '5'
#                       GROUP BY COD_ESPECIE, COD_NATUREZA, COD_SUBGRUPO  
#                       ORDER BY COD_ESPECIE, COD_NATUREZA, COUNT(*) DESC "
# 
# 
# 
# system.time({
#   
#   imped_dist_esp_nat_sub_5 <- DBI::dbGetQuery(conexaoDB, qry_imped_dist_esp_nat_sub_5)
#   
# })
# 
# qry_imped_dist_esp_nat_sub_10 <- " SELECT COD_ESPECIE, COD_NATUREZA, COD_SUBGRUPO,
#                       COUNT(*) AS count 
#                       FROM BIG.V_IMPEDIMENTO_CLIENTE 
#                       WHERE COD_ESPECIE = '10'
#                       GROUP BY COD_ESPECIE, COD_NATUREZA, COD_SUBGRUPO  
#                       ORDER BY COD_ESPECIE, COD_NATUREZA, COUNT(*) DESC "
# 
# 
# 
# system.time({
#   
#   imped_dist_esp_nat_sub_10 <- DBI::dbGetQuery(conexaoDB, qry_imped_dist_esp_nat_sub_10)
#   
# })
# 
# 
# 


#### [Inicio estudo] - V Impedimento Cliente ####
# 
# campos_v_imped <- DBI::dbGetQuery(conexaoDB, "SELECT column_name, data_type, data_length, data_precision, data_scale FROM all_tab_columns WHERE table_name = 'V_IMPEDIMENTO_CLIENTE' AND owner = 'BIG'")
# 
# view(campos_v_imped)
# 
# # Cria query para buscar apenas um CPF na V_IMPEDIMENTO_CLIENTE
# 
# qry_v_imped_clt6 <- paste("SELECT * FROM BIG.IMPEDIMENTO_CLIENTE 
#                                WHERE COD_CLIENTE = '29420833000193'")
# 
# 
# system.time({
#   
#   v_imped_clt6 <- DBI::dbGetQuery(conexaoDB, qry_v_imped_clt6)
#   
# })
# 
# view(v_imped_clt6)
# 
# # Cria query para buscar os impedimentos de um cliente especifico na base V impedimento cliente
# 
# campos_v_imped_clt <- DBI::dbGetQuery(conexaoDB, "SELECT column_name, data_type, data_length, data_precision, data_scale FROM all_tab_columns WHERE table_name = 'V_IMPEDIMENTO_CLIENTE' AND owner = 'BIG'")
# 
# qry_V_imped_clt <- paste0("SELECT * 
#   FROM BIG.V_IMPEDIMENTO_CLIENTE 
#   WHERE COD_CLIENTE = '00032649975091'")
# 
# system.time({
#   
#   v_imped_clt <- DBI::dbGetQuery(conexaoDB, qry_V_imped_clt)
#   
# })
# 
# view(v_imped_clt)
# 
# 
# View(cliente_teste)
# 
# #### [fim estudo] - V impedimento cliente ####
# 
# #### [inicio estudo] - impedimento cliente ####
# 
# campos_imped_clt <- DBI::dbGetQuery(conexaoDB, "SELECT column_name, data_type, data_length, data_precision, data_scale FROM all_tab_columns WHERE table_name = 'IMPEDIMENTO_CLIENTE' AND owner = 'BIG'")
# 
# view(campos_imped_clt)
# 
# qry_imped_clt <- paste0("SELECT * 
#   FROM BIG.IMPEDIMENTO_CLIENTE 
#   WHERE COD_CLIENTE = '00032649975091'")
# 
# system.time({
#   
#   imped_clt <- DBI::dbGetQuery(conexaoDB, qry_V_imped_clt)
#   
# })
# 
# view(imped_clt)
# 
# #### [fim estudo] - impedimento cliente ####
# 
# #### [inicio] Notas sobre estudo das bases v impedimento cliente e impedimento cliente ####
# 
# # Ainda nao sei qual eh a diferenca entre as bases V_IMPEDIMENTO_CLIENTE e a
# # IMPEDIMENTO_CLIENTE. Busquei o cod cliente '00032649975091', por exemplo,
# # e os dataframes que retornaram foram exatamente os mesmos, tanto em variaveis
# # como em quantidade de observacoes. Sendo assim, vou usar apenas a base
# # v_impedimento_cliente porque o tempo de execucao da consulta eh menor.
# 
# #### [fim] Notas sobre estudo das bases v impedimento cliente e impedimento cliente ####
# 
# # Marca e conta ocorrencias de cod_cliente
# 
# eqfx_clt_esp_75 <- eqfx_clt_esp_75 %>%
#   group_by(DATA_REFERENCIA) %>%
#   arrange(COD_CLIENTE) %>%
#   group_by(COD_CLIENTE) %>%
#   mutate(SEQ_COD_CLIENTE = row_number(),
#          ULT_COD_CLIENTE = if_else(row_number() == n(), 1, 0)) %>%
#   ungroup() %>%
#   select(COD_CLIENTE, DATA_REFERENCIA, SEQ_COD_CLIENTE, ULT_COD_CLIENTE, everything())
# 
# # Filtra apenas a ultima ocorrencia de cada cod_cliente e remove as vars de marcacao
# 
# eqfx_clt_esp_75_last <- eqfx_clt_esp_75 %>%
#   filter(ULT_COD_CLIENTE == 1) %>%
#   select(-ULT_COD_CLIENTE, -SEQ_COD_CLIENTE) %>%
#   select(COD_CLIENTE, DATA_REFERENCIA, everything())
# 
# # Visualiza o df resultante
# 
# view(eqfx_clt_esp_75_last)
# 
# # Salva em .csv a eqfx cliente cujos clientes foram cadastrados com cod_especie 75, que eh a especie
# # das infracoes de credito rural
# 
# base_folder <- "C:/Users/B41379/Desktop/Projetos/0_BASES"
# file_name <- "eqfxclt_esp_dist.csv"
# file_path <- file.path(base_folder, file_name)
# write_delim(eqfxclt_esp_dist, file_path, delim = ";")
# 
# view(eqfx_clt_esp_75)
# 
# # Query para estudar a distribuicao de frequencias de uma var de uma tabela na base 
# # de dados do banco
# 
# # WHERE  DATA_REFERENCIA >= TO_DATE('01-01-2021', 'DD-MM-YYYY')
# 
# qry_eqfxclt_esp_dist <- " SELECT COD_ESPECIE, 
#                       COUNT(*) as frequency 
#                       FROM BIG.EQUIFAX_CLIENTE 
#                       GROUP BY COD_ESPECIE 
#                       ORDER BY frequency DESC "
# 
# # Executa query da distribuicao de frequencias
# 
# system.time({
#   
#   eqfxclt_esp_dist <- DBI::dbGetQuery(conexaoDB, qry_eqfxclt_esp_dist)
#   
# })
# 
# view(eqfxclt_esp_dist)
# 
# ## [opcional] Le em csv
# 
# # Le clientes cod_especie 75 - restricao ao credito rural
# 
# base_folder <- "C:/Users/B41379/Desktop/Projetos/0_BASES"
# file_name <- "eqfx_clt_esp_75.csv"
# file_path <- file.path(base_folder, file_name)
# eqfx_clt_esp75 <- readr::read_delim(file_path, delim = ";", col_types = readr::cols(.default = "c"))
# 
# sort(colnames(eqfx_clt_esp75))
# 
# eqfx_clt_esp75$DATA_REFERENCIA <- substr(eqfx_clt_esp75$DATA_REFERENCIA, 1, nchar(eqfx_clt_esp75$DATA_REFERENCIA) - 10)
# eqfx_clt_esp75$DATA_PRIM_INCL <- substr(eqfx_clt_esp75$DATA_PRIM_INCL, 1, nchar(eqfx_clt_esp75$DATA_PRIM_INCL) - 10)
# eqfx_clt_esp75$DATA_ULT_INCL <- substr(eqfx_clt_esp75$DATA_ULT_INCL, 1, nchar(eqfx_clt_esp75$DATA_ULT_INCL) - 10)
# eqfx_clt_esp75$DATA_ULT_EXCL <- substr(eqfx_clt_esp75$DATA_ULT_EXCL, 1, nchar(eqfx_clt_esp75$DATA_ULT_EXCL) - 10)
# 
# eqfx_clt_esp75 <- eqfx_clt_esp75 %>%
#   select(DATA_REFERENCIA, DATA_PRIM_INCL, DATA_ULT_EXCL, DATA_ULT_INCL, everything())
# 
# # Converte as variaveis com datas em formato texto para formato data com a funcao as.Date()
# # Uma observacao importante sobre a funcao as.Date() eh que ela converte diretamente as datas do
# # tipo texto no formato yyyy-mm-dd para o tipo data no formato yyyy-mm-dd. Qualquer outro formato,
# # por exemplo, dd-mm-yyyy ou mm-dd-yyyy, deve ser alterado usando o argumento format = "%Y%m%d".
# 
# # Tenho que estudar ainda como funcionam as funcoes do pacote lubridate(), como dmy(), ymd(), etc,
# # para entender bem o que elas fazem.
# eqfx_clt_esp75$DATA_REFERENCIA <- as.Date(eqfx_clt_esp75$DATA_REFERENCIA)
# eqfx_clt_esp75$DATA_PRIM_INCL <- as.Date(eqfx_clt_esp75$DATA_PRIM_INCL)
# eqfx_clt_esp75$DATA_ULT_INCL <- as.Date(eqfx_clt_esp75$DATA_ULT_INCL)
# eqfx_clt_esp75$DATA_ULT_EXCL <- as.Date(eqfx_clt_esp75$DATA_ULT_EXCL)
# 
# # Verifica se as variaveis ficaram, de fato, em formato data
# print(class(eqfx_clt_esp75$DATA_REFERENCIA))
# print(class(eqfx_clt_esp75$DATA_PRIM_INCL))
# print(class(eqfx_clt_esp75$DATA_ULT_INCL))
# print(class(eqfx_clt_esp75$DATA_ULT_EXCL))
# 
# 
# # Retorna data da primeira inclusao
# min(eqfx_clt_esp75$DATA_PRIM_INCL)
# # Retorna data da ultima inclusao
# max(eqfx_clt_esp75$DATA_ULT_INCL)
# # Retorna data da ultima exclusao
# max(eqfx_clt_esp75$DATA_ULT_EXCL)
# 
# # Armazena os codigos de cliente em uma lista
# cods_clt_esp75 <- eqfx_clt_esp75$COD_CLIENTE
# # Converte a lista para dataframe para poder remover cods duplicados
# cods_clt_esp75_df <- as.data.frame(cods_clt_esp75)
# # Remove cods de cliente duplicados
# cods_clt_esp75_df <- distinct(cods_clt_esp75_df)
# # Renomeia o nome generico dos cod cliente para um nome correto e os ordena em ordem crescente
# cods_clt_esp75_df <- cods_clt_esp75_df %>%
#   rename(COD_CLIENTE = cods_clt_esp75) %>%
#   arrange(COD_CLIENTE)
# 
# 
# 
# view(eqfx_clt_esp75)
# 
# base_folder <- "C:/Users/B41379/Desktop/Projetos/0_BASES"
# file_name <- "cad_imped_eqfx.csv"
# file_path <- file.path(base_folder, file_name)
# cad_imped_eqfx <- readr::read_delim(file_path, delim = ";", col_types = readr::cols(.default = "c"))
# 
# # Cria uma variavel sequencial para cada subgrupo contido em uma especie. Fiz isso pq preciso criar
# # um numero identificador de ocorrencia proprio. Achei que o codigo resultante da concatenacao de um
# # cod_especie com um cod_subgrupo seria unico, porem descobri que nao eh o caso. Por exemplo, o
# # cod_subgrupo '00' do cod_especie '05' possui QUATRO ocorrencias diferentes. Logo, a concatenacao
# # '0500' me traria quatro diferentes descricoes, o que nao eh o que eu preciso. Nesse sentido, para
# # cada descricao diferente, preciso criar um numero unico, e esse numero foi o sequencial de cada
# # subgrupo de cada especie. Assim, como tenho quatro ocorrencias de subgrupo iguais dentro de um
# # cod_especie, criei um sequencial para cada especie: '1', '2', '3' e '4'.
# 
# cad_imped_eqfx <- cad_imped_eqfx %>%
#   select(COD_ESPECIE, COD_SUBGRUPO, DESCR_ESPECIE, DESCR_SUBGRUPO, everything()) %>%
#   arrange(COD_ESPECIE) %>%
#   group_by(COD_ESPECIE) %>%
#   arrange(COD_ESPECIE, COD_SUBGRUPO) %>%
#   group_by(COD_ESPECIE, COD_SUBGRUPO) %>%
#   mutate(SEQ_SUBGRUPO = row_number()) %>%
#   ungroup()
# 
# # Apesar de eu haver criado o codigo sequencial, eh importante destacar que, em alguns casos, tenho 10
# # ou mais sequenciais para um mesmo subgrupo. Isso me gera outro problema, que eh a diferenca em tamanho 
# # de caracteres do numero identificador da ocorrencia resultante da concatenacao dos codigos da especie,
# # do subgrupo e do sequencial. Por exemplo, se tenho um cod_especie '01', um cod_subgrupo '23' e um
# # sequencial '1', meu codigo de ocorrencia resultante sera '01231', isto eh, um codigo de ocorrencia de
# # cinco caracteres. Porem, se meu sequencial for '10', o codigo de ocorrencia sera '012310', ou seja,
# # seis caracteres. Para corrigir isso, eh necessario adicionar um zero a esquerda dos sequenciais que
# # possuem apenas um algarismo, de modo a tornar todos os valores do sequencial criado com dois caracteres.
# # Isso fara com que todos os codigos de ocorrencias resultantes de concatenacoes. No exemplo acima, o
# # primeiro codigo ficara '012301' (seis caracteres), e nao mais '01231' (cinco caracteres). Pode parecer
# # algo besta, mas isso eh importante especialmente quando for necessario fazer joins, onde a padronizacao
# # dos codigos eh essencial para que haja uma correspondencia adequada entre as tabelas. Feita essa
# # observacao, segue o codigo.
# 
# # A linha abaixo identifica quais linhas da variavel 'SEQ_SUBGRUPO' possui menos de 2 caracteres.
# not_two_digits <- which(nchar(cad_imped_eqfx$SEQ_SUBGRUPO) < 2)
# 
# # A linha de codigo abaixo adiciona 
# cad_imped_eqfx$SEQ_SUBGRUPO[not_two_digits] <- str_pad(cad_imped_eqfx$SEQ_SUBGRUPO[not_two_digits], width = 2, side = "left", pad = "0")
# 
# cad_imped_eqfx <- cad_imped_eqfx %>%
#   select(COD_ESPECIE, COD_SUBGRUPO, SEQ_SUBGRUPO, DESCR_ESPECIE, DESCR_SUBGRUPO, everything())
# 
# view(cad_imped_eqfx)
# 
# cad_imped_eqfx$COD_OCORRENCIA <- paste0(cad_imped_eqfx$COD_ESPECIE, cad_imped_eqfx$COD_SUBGRUPO)
# cad_imped_eqfx$DESCR_OCORRENCIA <- paste(cad_imped_eqfx$DESCR_ESPECIE, cad_imped_eqfx$DESCR_SUBGRUPO, sep = " - ")
# 
# eqfx_clt_esp75$COD_OCORRENCIA <- paste0(eqfx_clt_esp75$COD_ESPECIE, eqfx_clt_esp75$COD_SUBGRUPO)
# 
# cad_imped_eqfx <- cad_imped_eqfx %>%
#   select(COD_OCORRENCIA, DESCR_OCORRENCIA, TIPO_IMPEDIMENTO)
# 
# eqfx_clt_esp75 <- left_join(eqfx_clt_esp75, cad_imped_eqfx, by = "COD_OCORRENCIA")
# 
# eqfx_clt_esp75 <- eqfx_clt_esp75 %>%
#   select(COD_CLIENTE, COD_OCORRENCIA, TIPO_IMPEDIMENTO, DESCR_OCORRENCIA, everything()) %>%
#   select(-COD_ESPECIE, -COD_SUBGRUPO)
# 
# view(eqfx_clt_esp75)
# 
# sort(colnames(eqfx_clt_esp75))
# 
# View(eqfx_clt_esp75)
# 
# eqfx_clt_esp_75 <- eqfx_clt %>%
#   filter(COD_ESPECIE == '75')
# 
# eqfx_clt_tbl <- as.data.frame(sort(table(eqfx_clt$COD_ESPECIE)))
# 
# view(eqfx_clt_tbl)
# 
# ## Manipula todos os dfs das ops bkq acima de 1MM
# 
# # Cria lista com os nomes dos dataframes amostrais
# 
# dfs_cdiabkq <- c("cdiabkq_0723", "cdiabkq_0823", "cdiabkq_0923", 
#                  "cdiabkq_1023", "cdiabkq_1123", "cdiabkq_1223", 
#                  "cdiabkq_0124")
# 
# # Loop para iterar sobre os nomes dos dataframes
# for (df_cdiabkq in dfs_cdiabkq) {
#   
#   # Acessar o dataframe pelo nome, adicionar a coluna CRITERIO, renomear algumas variaveis
#   # e reordenar as colunas
#   df_temp <- get(df_cdiabkq) %>%
#     mutate(CRITERIO = "BKQ1MM") %>%
#     rename(DATA_VENCIMENTO = DATA_VENCIMENTO_CONTRATO,
#            REF_BACEN = NUMERO_REFERENCIA_BACEN,
#            DATA_CONCESSAO = DATA_CONTRATACAO) %>%
#     select(COD_CLIENTE, COD_CONTRATO, REF_BACEN, CRITERIO, DATA_CONCESSAO, DATA_LIBERACAO, DATA_VENCIMENTO, 
#            IND_PESSOA_FISCAL, EMPREENDIMENTO, VLR_FINANCIADO, COD_TIPO_SEGURO) # Ajuste os nomes das colunas conforme necessario
#   
#   # Atualizar o dataframe original no ambiente global
#   assign(df_cdiabkq, df_temp, envir = .GlobalEnv)
# }
# 
# 
# ## Codigos uteis para buscar a hist na BIG ##
# 
# # Como nao sei qual eh a data minima da ULT_ATUALIZACAO da big_hist, faco uma busca com
# # poucas variaveis para obter um dataframe minimo que contenha todas as observacoes
# # para que eu consiga obter o valor minimo da variavel.
# 
# # campos_big_hist <- DBI::dbGetQuery(conexaoDB, "SELECT column_name, data_type, data_length, data_precision, data_scale FROM all_tab_columns WHERE table_name = 'HIST_COMPLEMENTO_RURAL_DIA' AND owner = 'BIG'")
# 
# # print(campos_big_hist)
# 
# # system.time({
# 
# #  big_hist <- DBI::dbGetQuery(conexaoDB, paste0("SELECT COD_CONTRATO, ULT_ATUALIZACAO,
# #  SISTEMA_ORIGEM, COD_TIPO_BENEFICIARIO FROM BIG.HIST_COMPLEMENTO_RURAL_DIA"))
# 
# # })
# 
# # Visualiza as primeiras 1000 linhas da big_hist para ver se buscou direitinho.
# 
# # view(head(big_hist, 1000))
# 
# # Busca o valor minimo para poder criar o filtro da consulta SQL abaixo
# 
# # min(big_hist$ULT_ATUALIZACAO)
# 
# 
# # Busca os campos da hist_complemento_rural_dia
# 
# # campos_hist <- DBI::dbGetQuery(conexaoDB, "SELECT column_name, data_type, data_length, data_precision, data_scale FROM all_tab_columns WHERE table_name = 'HIST_COMPLEMENTO_RURAL_DIA' AND owner = 'BIG'")
# 
# # Faz a consulta da big_hist completa - notar que como a ULT_ATUALIZACAO eh uma
# # TIMESTAMP, tive que mudar o formato da consulta para:
# # WHERE (ULT_ATUALIZACAO >= TO_TIMESTAMP('2019-11-21 00:00:01', 'YYYY-MM-DD HH24:MI:SS'))
# # A consulta tamb?m registra o tempo de processamento, em razao do tamanho da tabela. 
# # Essa busca na CLI_DIA retorna a segunda parte da base de onde vamos obter os dados dos
# # clientes que possuem tres ou mais prorrogacoes.
# 
# 
# 
# ## A proxima linha de codigo le as variaveis como texto e serve para virem os zeros a esquerda
# ## das variaveis que possuem tais zeros. Caso contrario, as variaveis sao lidas como notacao
# ## cientifica.
# ## O metodo responsavel por essa diferenca eh o read_csv(), que eh diferente do read.csv(). Um
# ## tem um underline entre o "read" e o "csv" e o outro eh um ponto entre as duas palavras.
# ## Tentei ambos e o primeiro metodo eh mais robusto para essa tarefa.
# 
# # system.time({
# 
# #   big_hist <- readr::read_delim("C:/Users/B41379/Desktop/Projetos/5_aguardando_Luciana - Nova_metodologia_fiscalizacao/bases/big_hist.csv", delim = ";", col_types = readr::cols(.default = "c"))
# 
# # })
# 
# # Renomeia a vari?vel COD_CATEGORIA_CLIENTE da hist_universo para COD_PROGRAMA_RECURSO
# # para ficar igual ao dataframe bbj_universo
# 
# # big_hist_20_23 <- big_hist_20_23 %>%
# #   rename(COD_PROGRAMA_RECURSO = COD_CATEGORIA_CLIENTE)
# 
# # Recodifica a vari?vel rec?m renomeada COD_PROGRAMA_RECURSO de acordo com os c?digos
# # correspondentes aos das categorias pronaf, pronamp e demais.
# 
# # system.time({
# 
# #   big_hist_20_23 <- big_hist_20_23 %>%
# #     mutate(COD_PROGRAMA_RECURSO = recode(COD_PROGRAMA_RECURSO, "1" = "0001", "2" = "0050", "3" = "0999"))
# 
# # })
# 
# 
# # Recodifica os c?digos de finalidade da hist para ficarem no mesmo padr?o da cli_dia.
# # N?o utilizo um comando para apenas remover os zeros ? esquerda da vari?vel, pois h? 
# # observacoes que n?o possuem classifica??o e est?o classificadas com um zero apenas.
# # Ou seja, n?o sao nem 1 (comercializa??o), nem 2 (custeio), nem 3 (investimento), 
# # nem 4 (industrializa??o). Verifiquei isso utilizando a fun??o freq <- sort(table(df$variable)).
# # Seria interessante ver o que seriam essas vari?veis, mas como sao da hist e talvez sejam
# # operacoes muito antigas, n?o vale ? pena o esfor?o de ver isso agora.
# 
# # system.time({
# 
# #   big_hist_20_23 <- big_hist_20_23 %>%
# #     mutate(COD_FINALIDADE = recode(COD_FINALIDADE, "01" = "1", "02" = "2", "03" = "3", "04" = "4"))
# 
# # })
# 
# # O pr?ximo c?digo precisa ser um pouco mais detalhado serve para fazer algumas marca??es na base.
# # As m?ltiplas incid?ncias do operador pipe %>% significam que a sa?da de uma fun??o corresponde ?
# # entrada da pr?xima fun??o. Sendo assim, o operador %>% possibilita que diversas fun??es sejam
# # executadas em sequ?ncia.
# 
# # A fun??o arrange () ordena o df hist_cdia_bbj por COD_CLIENTE e tamb?m agrupa pela mesma
# # vari?vel. 
# 
# # A fun??o group_by() indica que as operacoes que ocorrerem ap?s dela ir?o acontecer
# # dentro de cada grupo cujo valor do COD_CLIENTE ? o mesmo.
# 
# # A fun??o mutate() altera ou cria vari?veis de acordo com determinados criterios.
# 
# # A primeira vari?vel criada ser? chamada "FLT_PRORROGACAO". Tal vari?vel tem como objetivo
# # contar quantas vezes um mesmo COD_CLIENTE ? repetido.
# 
# # Tal contagem ? possibilitada pela fun??o row_number(), a qual, estando encadeada pela fun??o
# # anterior group_by(), conta quantas vezes o COD_CLIENTE se repete dentro de um grupo cujo valor
# # de COD_CLIENTE ? o mesmo.
# 
# # Caso a fun??o group_by() n?o fosse utilizada, a vari?vel apenas contaria o n?mero de linhas
# # do dataframe, o que n?o ? nosso objetivo.
# 
# # A segunda vari?vel criada ? a "ULT_COD_CLIENTE", que tem por objetivo marcar a ?ltima ocorr?ncia
# # da vari?vel COD_CLIENTE. 
# 
# # Caso seja a ?ltima ocorr?ncia, a vari?vel assume o valor 1; caso contr?rio, assume o valor
# # zero. Isso ? possibilitado pela igualdade entre as fun??es row_number() e n(). Enquanto a
# # fun??o row_number() marca a posi??o de uma linha dentro de um grupo, a fun??o n() diz quantas
# # linhas aquele grupo tem no total. Sendo assim, um grupo cujo COD_CLIENTE se repete tres vezes
# # ter? um n() igual a 3. Ent?o a vari?vel ULT_COD_CLIENTE ter? valor igual a zero na primeira
# # ocorr?ncia do COD_CLIENTE porque seu row_number() ser? igual a 1 e seu n() igual a 3; na
# # segunda ocorr?ncia seu row_number() ser? igual a 2 e seu n() ser? igual a 3, o que ainda
# # significa ULT_COD_CLIENTE igual a zero. Por?m, na terceira ocorr?ncia seu row_number() ser? igual
# # a 3 e seu n() tamb?m ser? igual a 3. Logo, ULT_COD_CLIENTE ser? igual a 1, o que significa
# # que essa ? a ?ltima linha do grupo. Tal vari?vel ser? ?til para determinar, junto com a
# # FLT_PRORROGACAO, quais COD_CLIENTE serao inclusos nos criterios determin?sticos. Ou seja, se
# # ULT_COD_CLIENTE == 1 e FLT_PRORROGACAO >= 3, ent?o ? deterministica; caso contr?rio, aleat?ria.
# 
# 
# # A linha a seguir cria uma c?pia da agreg_hist e chama essa c?pia de hist_prorrogacao. Os c?digos
# # 007 e 016 sao de prorroga??o.
# 
# ## Crit?rio antigo e uma observacao sobre o cod 007. O 016 ? a reneg 4651 e a explicacao
# ## sobre ele vou fazer depois.
# 
# 
# 
# 
# 
# 
# 
# # A consulta acima retorna diversas vari?veis de duas tabelas diferentes, a FATO_GESTAO_CREDITO
# # e a CONTA. Tais vari?veis a serem selecionadas nas tabelas apresentam duas restri??es de
# # sele??o, DATA_REFERENCIA e COD_PRODUTO_GESTAO_BIB. 
# 
# # A DATA_REFERENCIA de uma tabela consiste na data de cadastro de uma informa??o em tal tabela.
# # Se a tabela ? atualizada diariamente, ent?o a data ref vai ser atualizada diariamente; 
# # se a tabela ? atualizada mensalmente, ent?o a data ref vai ser atualizada uma vez por mes,
# # sendo sempre o primeiro dia do mes de referencia.
# 
# # O COD_PRODUTO_GESTAO_BIB ? uma vari?vel que define qual ? o c?digo de um produto financeiro
# # dentro do Banrisul. Cada c?digo ? composto de sete d?gitos. As operacoes de agroneg?cios
# # consistem em todos os c?digos que iniciam pelos c?digos 113, 12002 e 12003. 
# 
# # Por exemplo, o c?digo 1130304 (que come?a com 113) se refere a operacoes de financiamento de
# # estocagem de produtos (FEE) agr?colas com utiliza??o de recursos livres para cooperativas
# # constitu?das sob a forma de pessoa jur?dica.
# 
# 
# ### A PAULA DISSE QUE NA MENSAL N?O VAMOS REMOVER AS DUPLICADAS PORQUE DESEJAMOS OS SALDOS
# ### CONT?BEIS DOS MESES PASSADOS. MARCAMOS A ?LTIMA VEZ QUE A OP APARECEU PARA TERMOS
# ### DIFERENTES VIS?ES
# 
# ### depois vemos se uma mesma op se repete na mesma data ref pq pode ser alguma "polui??o"
# ### dos dados.
# 
# 
# #### Padroniza nomes das colunas dos dfs amostrais e das ops acima de 1MMBKQ para poder empilhar
# 
# # Cria lista com os nomes dos dataframes amostrais
# 
# dfs_amostra <- c("amostra_0723", "amostra_0823", "amostra_0923", "amostra_1023", "amostra_1123", "amostra_1223", "amostra_0124")
# 
# # Obs: a data de concessao equivale a data de contratacao nas ops bbj. Fonte: servicos DW
# 
# # Loop para iterar sobre os dataframes
# for (df_amostra in dfs_amostra) {
#   
#   # Realiza as alteracoes sobre todos os dataframes da lista
#   df_temp <- get(df_amostra) %>%
#     rename(DATA_VENCIMENTO = DATA_VENCIMENTO_OPER,
#            IND_PESSOA_FISCAL = IND_PESSOA_FISCAL_CLIENTE,
#            REF_BACEN = NUMERO_REFERENCIA_BACEN,
#            DATA_CONTRATACAO = DATA_CONCESSAO) %>%
#     select(COD_CLIENTE, COD_CONTRATO, REF_BACEN, CRITERIO, DATA_CONTRATACAO, DATA_LIBERACAO,
#            DATA_VENCIMENTO, EMPREENDIMENTO, IND_PESSOA_FISCAL, VLR_FINANCIADO, COD_TIPO_SEGURO)
#   
#   # Atualizar o dataframe original no ambiente global
#   assign(df_amostra, df_temp, envir = .GlobalEnv)
# }
# 
# # Cria lista com os nomes dos dataframes amostrais
# 
# dfs_cdiabkq <- c("cdiabkq_0723", "cdiabkq_0823", "cdiabkq_0923", "cdiabkq_1023", "cdiabkq_1123", "cdiabkq_1223", "cdiabkq_0124")
# 
# # Obs: a data de concessao equivale a data de contratacao nas ops bbj. Fonte: servicos DW
# 
# # Loop para iterar sobre os dataframes
# for (df_cdiabkq in dfs_cdiabkq) {
#   
#   # Realiza as alteracoes sobre todos os dataframes da lista
#   df_temp <- get(df_cdiabkq) %>%
#     rename(DATA_CONTRATACAO = DATA_CONCESSAO) %>%
#     select(COD_CLIENTE, COD_CONTRATO, REF_BACEN, CRITERIO, DATA_CONTRATACAO, DATA_LIBERACAO,
#            DATA_VENCIMENTO, EMPREENDIMENTO, IND_PESSOA_FISCAL, VLR_FINANCIADO, COD_TIPO_SEGURO)
#   
#   # Atualizar o dataframe original no ambiente global
#   assign(df_cdiabkq, df_temp, envir = .GlobalEnv)
# }
# 
# # Lista e empilha todos os dataframes de selecoes
# 
# lista_dfs_selecoes <- list(selecao_0723, selecao_0823, selecao_0923, selecao_1023, selecao_1123, selecao_1223, selecao_0224)
# 
# selecao_df_total <- bind_rows(lista_dfs_selecoes)
# 
# # Cria um backup do df de ops selecionadas empilhado
# 
# selecao_df_total_BKP <- selecao_df_total
# 
# #### DATA WRANGLING ####
# 
# how_many_differences <- cdia_bkq_0721_merged$VLR_CONTRATADO != cdia_bkq_0721_merged$VLR_FINANCIADO
# 
# indices_of_differences <- which(how_many_differences)
# 
# df_differences <- cdia_bkq_0721_merged[indices_of_differences, ]
# 
# df_differences <- select(df_differences, COD_CONTRATO, VLR_CONTRATADO, VLR_FINANCIADO, 
#                          DATA_REFERENCIA, SALDO_CONTABIL, SALDO_ATUAL, DATA_CONTRATACAO,
#                          DATA_LIBERACAO, VALOR_A_VENCER, VALOR_VENCIDO, everything())
# 
# print(colnames(cdia_bkq_0721_merged))
# 
# view(df_differences)
# 
# print(indices_of_differences)
# 
# ### Aqui eu tenho o df da fato mensal com as ops selecionadas pelo criterio de risco e as ops
# ### "livres" do criterio de risco, mas selecionadas por haverem sido contratadas no mes de
# ### referencia e seus proponentes estarem dentro do criterio de prorrogacoes.
# 
# ## Para visualizar melhor, agrupo e ordeno por COD_CLIENTE
# 
# # fato_mensal_092023 <- fato_mensal_092023 %>%
# #   arrange(COD_CLIENTE) %>%
# #   group_by(COD_CLIENTE)
# 
# ## Reordeno as vars do df para melhor visualizacao
# 
# # fato_mensal_092023 <- select(fato_mensal_092023, COD_CLIENTE, COD_CONTRATO, CONDICAO, everything())
# 
# # View(fato_mensal_092023)
# 
# ## Faz um left join do df das ops prorrogadas com a fato para termos as vari?veis todas as vari?veis
# ## nessas ops das tabelas hist ou cli_dia e fato.
# 
# # lft_hcdia_prg_det_fato <- left_join(hist_cdia_bbj_prg_det, fato_mensal, by = "COD_CONTRATO",
# #                                 suffix = c("_hcdia", "_fato"))
# # Reordena as vars no df
# 
# # lft_hcdia_prg_det_fato <- select(lft_hcdia_prg_det_fato, COD_CONTRATO, SEQ_COD_CONTRATO, 
# #                              SEQ_COD_CONTRATO_PRG, SISTEMA_ORIGEM, CONDICAO_hcdia,
# #                              CONDICAO_fato,  COD_CLIENTE_hcdia, COD_CLIENTE_fato, everything())
# 
# # lft_hcdia_prg_det_fato <- lft_hcdia_prg_det_fato %>%
# #   arrange(COD_CONTRATO)
# 
# # View(lft_hcdia_prg_det_fato)
# 
# # Faz um left join das vars da fato no mes de referencia com o df da hist_cdia_bbj para termos
# # todas as vari?veis correspondentes a essas ops das tabelas fato e hist ou cli_dia.
# 
# # lft_fato_092023_hcdia <- left_join(fato_mensal_092023, hist_cdia_bbj_20_23, by = "COD_CONTRATO",
# #                                    suffix = c("_fato", "_hcdia"))
# 
# # lft_fato_092023_hcdia <- select(lft_fato_092023_hcdia, COD_CONTRATO, SEQ_COD_CONTRATO_fato, 
# #                                 SEQ_COD_CONTRATO_PRG, SISTEMA_ORIGEM, CONDICAO_hcdia, CONDICAO_fato,
# #                                 COD_CLIENTE_hcdia, COD_CLIENTE_fato, everything())
# 
# # lft_fato_092023_hcdia <- lft_fato_092023_hcdia %>%
# #   arrange(COD_CONTRATO)
# 
# # Passo a chamar meu df acima de universo_mmyyyy
# 
# # Identifica duplicados
# 
# duplicados_selecao_0723 <- selecao_0723 %>%
#   group_by(COD_CLIENTE) %>%
#   summarise(n = n()) %>%
#   filter(n > 1)
# 
# print(duplicados_selecao_0723)
# 
# 
# duplicados_clientes_0723 <- clientes_0723 %>%
#   group_by(COD_CLIENTE) %>%
#   summarise(n = n()) %>%
#   filter(n > 1)
# 
# print(duplicados_clientes_0723)
# 
# view(clientes_0723)













