

rm(list = ls())

### Importando os pacotes #################################################

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(GGally)
library(plotly)
library(caret)
library(randomForest)
library(caTools)
library(e1071)
library(rpart)
library(Metrics)


### Leitura e manipulacao das bases de dados ##############################

dados = read.csv("dados/data_comexstat.csv")
covariaveis = read_excel("dados/covariates.xlsx")

colSums(is.na(covariaveis)) # verificando se tem valores faltantes nas covariaveis

dados$datas = dados$date # salvando a data

# funcao separate do pacote tidyr
dados = separate(dados, date, sep ="-", into = c("Ano","Mes","dia")) #dividindo a data em dia, mes e ano

### Previsão das toneladas de soja que serão vendidas nos proximos 10 anos ####

dados$Ano

# paises que mais importam soja do brasil
paises2 = dados%>%
  filter(type == "Export" & product== "soybeans" )%>%
  group_by(country,product)%>%
  summarise(Total = sum(tons))%>%
  arrange(desc(Total))%>%
  as.data.frame()
paises2$Total=(paises2$Total/sum(paises2$Total) )*100
head(paises2,10)

paises2%>%
  head(10) %>%
  mutate(country = reorder(country, Total)) %>% 
  ggplot(aes(x = country, y = Total)) +
  geom_col()+
  coord_flip()+
  ylab("Percentual")+
  xlab("Países")

# criacao da base de dados
df_soja = dados %>%
  filter(type == "Export" & product =="soybeans" )%>%
  group_by(Ano)%>%
  summarise( total = sum(tons) )

#adicionando as covariaveis
df_soja = merge(covariaveis, df_soja, by.x = "year", by.y = "Ano", all.y = TRUE)

str(df_soja)

df_soja$year = df_soja$year - min( df_soja$year )

### Primeira abordagem = modelos de machine learning

# dividindo em treino e teste aleatoriamente 
set.seed(123)
divisao = sample.split(df_soja$total, SplitRatio = 0.70)
df_soja_treino = subset(df_soja, divisao == TRUE)
df_soja_teste = subset(df_soja, divisao == FALSE)


# ajustando modelos
modelos = c( "Random Forest", "SVM", "Linear Model", "R part"  )

drop_covariaveis = list( 
                         c("total"),
                         c("price_soybeans", "total"),
                         c("price_soybeans","price_corn","price_soybean_meal",
                           "total"),
                         c("price_corn","price_soybean_meal", "total"),
                         c("price_corn","price_soybean_meal", "total", 
                           "gdp_world", "gdp_egypt", "gdp_vietnam" ),
                         c("price_corn","price_soybean_meal",
                           "gdp_world", "gdp_egypt", "gdp_vietnam", "gdp_iran",
                           "gdp_japan", "total"),
                         c("price_soybeans","price_corn","price_soybean_meal",
                           "gdp_world", "gdp_egypt", "gdp_vietnam", "gdp_iran",
                           "gdp_japan", "total")
                         )

model_args = NULL
RMSE = NULL
MAPE = NULL
RMSE_treino = NULL
MAPE_treino = NULL
k = 0
preditor_lista = list()
X_media = list()
X_sd = list()
ID_list = NULL


for( model in  modelos ){
  for( idx in seq(1, length(drop_covariaveis)) ){
    k = k+1
    #criando a base de dados
    
    # as covariaveis para o treino
    x_treino = df_soja_treino%>%
      select( - drop_covariaveis[[idx]] ) %>%
      as.data.frame()
    
    x_bar = colMeans(x_treino[,2:ncol(x_treino)])
    x_sd = as.vector(apply(x_treino[,2:ncol(x_treino)],2,sd))
    
    x_treino = scale(x_treino[,2:ncol(x_treino)], center = x_bar,scale = x_sd )
    x_treino = as.data.frame(x_treino)
    
    #str(x_treino)
    
    # variavel resposta para o treino
    y_treino = df_soja_treino$total
    
    # as covariaveis para o teste
    x_teste = df_soja_teste%>%
      select( - drop_covariaveis[[idx]] ) %>%
      as.data.frame()
    
    x_teste = scale(x_teste[,2:ncol(x_teste)], center = x_bar,scale = x_sd )
    x_teste = as.data.frame(x_teste)
    
    #str(x_teste)
    
    # variavel resposta para o teste
    y_teste = df_soja_teste$total
    
    # banco de dados de treino
    dd = as.data.frame( cbind(x_treino,y_treino) )
    
    #head(dd)
    
    
    #random forest
      if( model == "Random Forest"){
        regressor = randomForest(x = x_treino, y = y_treino, ntree = 20)
        previsoes_teste = predict(regressor, newdata = x_teste)
        previsoes_treino = predict(regressor, newdata = x_treino)
      }
    #SVM
      if( model == "SVM"){
        regressor = svm(formula = y_treino ~ ., data = dd, type = 'eps-regression', kernel = 'linear')
        previsoes_teste = predict(regressor, newdata = x_teste)
        previsoes_treino = predict(regressor, newdata = x_treino)
      }
    #Linear model
      if( model == "Linear Model"){
        regressor = lm(formula = y_treino ~ ., data = dd)
        previsoes_teste = predict(regressor, newdata = x_teste)
        previsoes_treino = predict(regressor, newdata = x_treino)
      }
    # 
      if( model == "R part"){
        regressor = rpart(formula = y_treino ~ ., data = dd)
        previsoes_teste = predict(regressor, newdata = x_teste)
        previsoes_treino = predict(regressor, newdata = x_treino)
      }
    
    model_args = c(model_args,  paste0(model," - ", as.character(idx) ) )
    RMSE_treino = c(RMSE_treino, rmse(y_treino, previsoes_treino) )
    MAPE_treino = c(MAPE_treino, mape(y_treino,previsoes_treino) )
    RMSE = c(RMSE, rmse(y_teste, previsoes_teste) )
    MAPE = c(MAPE, mape(y_teste,previsoes_teste) )
    X_media[[k]] = x_bar
    X_sd[[k]] = x_sd
    preditor_lista[[k]] = regressor
    ID_list = c(ID_list, idx)
  }
}

seleciona = as.data.frame( cbind( model_args, RMSE_treino, RMSE,  MAPE_treino, MAPE) )
View( seleciona  )


seleciona[seleciona$MAPE==min(seleciona$MAPE),]
id = which(seleciona$MAPE==min(seleciona$MAPE))
modelo_selecionado=preditor_lista[[id]]

df_soja$previsoes =  predict(modelo_selecionado, newdata = df_soja )

df_soja%>%
  ggplot()+
  geom_line(aes(x = year, y = previsoes))+
  geom_line(aes(x = year, y = total))


ggplot(data.frame(total=1:10, previsoes = 1:10),aes(x = total, y = previsoes))+
  geom_point()+
  geom_abline(intercept = 0, slope = 1)

exclui = drop_covariaveis[[ ID_list[id] ]] 

df_futuro = covariaveis%>%
  filter(year>2019) %>%
  select(-exclui[-length(exclui)])%>%
  as.data.frame()

df_futuro$year = df_futuro$year - min(as.numeric(dados$Ano))

df_futuro2 = scale(df_futuro[,2:ncol(df_futuro)], center = X_media[[id]],scale = X_sd[[id]] )
df_futuro = as.data.frame(cbind(df_futuro$year, df_futuro2))   

previsao_soja= predict(modelo_selecionado, newdata = df_futuro)


########################
# paises que mais importam farelo de soja do brasil

paises3 = dados%>%
  filter(type == "Export" & product== "soybean_meal" )%>%
  group_by(country,product)%>%
  summarise(Total = sum(tons))%>%
  arrange(desc(Total))%>%
  as.data.frame()
paises3$Total=(paises3$Total/sum(paises3$Total) )*100
head(paises3,10)

paises3%>%
  head(10) %>%
  mutate(country = reorder(country, Total)) %>% 
  ggplot(aes(x = country, y = Total)) +
  geom_col()+
  coord_flip()+
  ylab("Percentual")+
  xlab("Países")

# criacao da base de dados
df_farelo = dados %>%
  filter(type == "Export" & product =="soybean_meal" )%>%
  group_by(Ano)%>%
  summarise( total = sum(tons) )

#adicionando as covariaveis
df_farelo = merge(covariaveis, df_farelo, by.x = "year", by.y = "Ano", all.y = TRUE)

str(df_farelo)

df_farelo$year = df_farelo$year - min( df_farelo$year )

### Primeira abordagem = modelos de machine learning

# dividindo em treino e teste aleatoriamente 
divisao = sample.split(df_farelo$total, SplitRatio = 0.70)
df_farelo_treino = subset(df_farelo, divisao == TRUE)
df_farelo_teste = subset(df_farelo, divisao == FALSE)


# ajustando modelos
modelos = c( "Random Forest", "SVM", "Linear Model", "R part"  )

drop_covariaveis = list( 
  c("total"),
  c("price_soybeans", "total"),
  c("price_soybeans","price_corn","price_soybean_meal",
    "total"),
  c("price_corn","price_soybean_meal", "total"),
  c("price_corn","price_soybean_meal", "total", 
    "gdp_world", "gdp_egypt", "gdp_vietnam" ),
  c("price_corn","price_soybean_meal", "total", 
    "gdp_world", "gdp_egypt", "gdp_vietnam", "gdp_china","gpd_netherlands" ),
  c("price_corn","price_soybean_meal", "total", 
    "gdp_world", "gdp_vietnam", "gdp_china","gpd_netherlands" ),
  c("price_corn","price_soybean_meal",
    "gdp_world", "gdp_egypt", "gdp_vietnam",
    "gpd_netherlands", "total"),
  c("price_corn","price_soybean_meal",
    "gdp_world", "gdp_egypt", "gdp_vietnam",
     "total"),
  c("price_corn","price_soybean_meal",
    "gdp_world", "gdp_egypt", "gdp_vietnam", "gdp_china","gpd_netherlands",
    "total"),
  c("price_soybeans","price_corn","price_soybean_meal",
    "gdp_world", "gdp_egypt", "gdp_vietnam",
     "total"),
  c("price_soybeans","price_corn","price_soybean_meal",
    "gdp_world", "gdp_egypt", "gdp_vietnam","gpd_netherlands",
    "total")
  
)

model_args = NULL
RMSE = NULL
MAPE = NULL
RMSE_treino = NULL
MAPE_treino = NULL
k = 0
preditor_lista = list()
X_media = list()
X_sd = list()
ID_list = NULL

library(e1071)
library(rpart)
library(Metrics)



for( model in  modelos ){
  for( idx in seq(1, length(drop_covariaveis)) ){
    k = k+1
    #criando a base de dados
    
    # as covariaveis para o treino
    x_treino = df_farelo_treino%>%
      select( - drop_covariaveis[[idx]] ) %>%
      as.data.frame()
    
    x_bar = colMeans(x_treino[,2:ncol(x_treino)])
    x_sd = as.vector(apply(x_treino[,2:ncol(x_treino)],2,sd))
    
    x_treino = scale(x_treino[,2:ncol(x_treino)], center = x_bar,scale = x_sd )
    x_treino = as.data.frame(x_treino)
    
    #str(x_treino)
    
    # variavel resposta para o treino
    y_treino = df_farelo_treino$total
    
    # as covariaveis para o teste
    x_teste = df_farelo_teste%>%
      select( - drop_covariaveis[[idx]] ) %>%
      as.data.frame()
    
    x_teste = scale(x_teste[,2:ncol(x_teste)], center = x_bar,scale = x_sd )
    x_teste = as.data.frame(x_teste)
    
    #str(x_teste)
    
    # variavel resposta para o teste
    y_teste = df_farelo_teste$total
    
    # banco de dados de treino
    dd = as.data.frame( cbind(x_treino,y_treino) )
    
    #head(dd)
    
    
    #random forest
    if( model == "Random Forest"){
      regressor = randomForest(x = x_treino, y = y_treino, ntree = 20)
      previsoes_teste = predict(regressor, newdata = x_teste)
      previsoes_treino = predict(regressor, newdata = x_treino)
    }
    #SVM
    if( model == "SVM"){
      regressor = svm(formula = y_treino ~ ., data = dd, type = 'eps-regression', kernel = 'linear')
      previsoes_teste = predict(regressor, newdata = x_teste)
      previsoes_treino = predict(regressor, newdata = x_treino)
    }
    #Linear model
    if( model == "Linear Model"){
      regressor = lm(formula = y_treino ~ ., data = dd)
      previsoes_teste = predict(regressor, newdata = x_teste)
      previsoes_treino = predict(regressor, newdata = x_treino)
    }
    # 
    if( model == "R part"){
      regressor = rpart(formula = y_treino ~ ., data = dd)
      previsoes_teste = predict(regressor, newdata = x_teste)
      previsoes_treino = predict(regressor, newdata = x_treino)
    }
    
    model_args = c(model_args,  paste0(model," - ", as.character(idx) ) )
    RMSE_treino = c(RMSE_treino, rmse(y_treino, previsoes_treino) )
    MAPE_treino = c(MAPE_treino, mape(y_treino,previsoes_treino) )
    RMSE = c(RMSE, rmse(y_teste, previsoes_teste) )
    MAPE = c(MAPE, mape(y_teste,previsoes_teste) )
    preditor_lista[[k]] = regressor
    X_media[[k]] = x_bar
    X_sd[[k]] = x_sd
    ID_list = c(ID_list, idx)
    
  }
}

seleciona = as.data.frame( cbind( model_args, RMSE_treino, RMSE,  MAPE_treino, MAPE) )
View( seleciona  )


seleciona[seleciona$MAPE==min(seleciona$MAPE),]
id = which(seleciona$MAPE==min(seleciona$MAPE))
modelo_selecionado=preditor_lista[[id]]

df_farelo$previsoes =  predict(modelo_selecionado, newdata = df_farelo )

df_farelo%>%
  ggplot(aes(x = total, y = previsoes))+
  geom_point()

exclui = drop_covariaveis[[ID_list[id]]] 

df_futuro = covariaveis%>%
  filter(year>2019) %>%
  select(-exclui[-length(exclui)])%>%
  as.data.frame()

df_futuro$year = df_futuro$year - min(as.numeric(dados$Ano))

df_futuro2 = scale(df_futuro[,2:ncol(df_futuro)], center = X_media[[id]],scale = X_sd[[id]] )
df_futuro = as.data.frame(cbind(df_futuro$year, df_futuro2))   

previsao_farelo = predict(modelo_selecionado, newdata = df_futuro)

previsao_farelo



########################
# paises que mais importam milho do brasil

paises4 = dados%>%
  filter(type == "Export" & product== "corn" )%>%
  group_by(country,product)%>%
  summarise(Total = sum(tons))%>%
  arrange(desc(Total))%>%
  as.data.frame()
paises4$Total=(paises4$Total/sum(paises4$Total) )*100
head(paises4,10)

paises4%>%
  head(10) %>%
  mutate(country = reorder(country, Total)) %>% 
  ggplot(aes(x = country, y = Total)) +
  geom_col()+
  coord_flip()+
  ylab("Percentual")+
  xlab("Países")

# criacao da base de dados
df_milho = dados %>%
  filter(type == "Export" & product =="corn" )%>%
  group_by(Ano)%>%
  summarise( total = sum(tons) )

#adicionando as covariaveis
df_milho = merge(covariaveis, df_milho, by.x = "year", by.y = "Ano", all.y = TRUE)

str(df_milho)

df_milho$year = df_milho$year - min( df_milho$year )

### Primeira abordagem = modelos de machine learning

# dividindo em treino e teste aleatoriamente 
divisao = sample.split(df_milho$total, SplitRatio = 0.70)
df_milho_treino = subset(df_milho, divisao == TRUE)
df_milho_teste = subset(df_milho, divisao == FALSE)


# ajustando modelos
modelos = c( "Random Forest", "SVM", "Linear Model", "R part"  )

drop_covariaveis = list( 
  c("total"),
  c("price_soybeans", "total"),
  c("price_soybeans","price_corn","price_soybean_meal",
    "total"),
  c("price_corn","price_soybean_meal", "total"),
  c("price_corn","price_soybean_meal", "total", 
    "gdp_world", "gdp_egypt", "gdp_vietnam" ),
  c("price_corn","price_soybean_meal",
    "gdp_world", "gdp_egypt", "gdp_vietnam", "gdp_iran",
    "gdp_japan", "total"),
  c("price_soybeans","price_corn","price_soybean_meal",
    "gdp_world", "gdp_egypt", "gdp_vietnam", "gdp_iran",
    "gdp_japan", "total")
)


model_args = NULL
RMSE = NULL
MAPE = NULL
RMSE_treino = NULL
MAPE_treino = NULL
k = 0
preditor_lista = list()



for( model in  modelos ){
  for( idx in seq(1, length(drop_covariaveis)) ){
    k = k+1
    #criando a base de dados
    
    # as covariaveis para o treino
    x_treino = df_milho_treino%>%
      select( - drop_covariaveis[[idx]] ) %>%
      as.data.frame()
    
    x_bar = colMeans(x_treino[,2:ncol(x_treino)])
    x_sd = as.vector(apply(x_treino[,2:ncol(x_treino)],2,sd))
    
    x_treino = scale(x_treino[,2:ncol(x_treino)], center = x_bar,scale = x_sd )
    x_treino = as.data.frame(x_treino)
    
    #str(x_treino)
    
    # variavel resposta para o treino
    y_treino = df_milho_treino$total
    
    # as covariaveis para o teste
    x_teste = df_milho_teste%>%
      select( - drop_covariaveis[[idx]] ) %>%
      as.data.frame()
    
    x_teste = scale(x_teste[,2:ncol(x_teste)], center = x_bar,scale = x_sd )
    x_teste = as.data.frame(x_teste)
    
    #str(x_teste)
    
    # variavel resposta para o teste
    y_teste = df_milho_teste$total
    
    # banco de dados de treino
    dd = as.data.frame( cbind(x_treino,y_treino) )
    
    #head(dd)
    
    
    #random forest
    if( model == "Random Forest"){
      regressor = randomForest(x = x_treino, y = y_treino, ntree = 20)
      previsoes_teste = predict(regressor, newdata = x_teste)
      previsoes_treino = predict(regressor, newdata = x_treino)
    }
    #SVM
    if( model == "SVM"){
      regressor = svm(formula = y_treino ~ ., data = dd, type = 'eps-regression', kernel = 'linear')
      previsoes_teste = predict(regressor, newdata = x_teste)
      previsoes_treino = predict(regressor, newdata = x_treino)
    }
    #Linear model
    if( model == "Linear Model"){
      regressor = lm(formula = y_treino ~ ., data = dd)
      previsoes_teste = predict(regressor, newdata = x_teste)
      previsoes_treino = predict(regressor, newdata = x_treino)
    }
    # 
    if( model == "R part"){
      regressor = rpart(formula = y_treino ~ ., data = dd)
      previsoes_teste = predict(regressor, newdata = x_teste)
      previsoes_treino = predict(regressor, newdata = x_treino)
    }
    
    model_args = c(model_args,  paste0(model," - ", as.character(idx) ) )
    RMSE_treino = c(RMSE_treino, rmse(y_treino, previsoes_treino) )
    MAPE_treino = c(MAPE_treino, mape(y_treino,previsoes_treino) )
    RMSE = c(RMSE, rmse(y_teste, previsoes_teste) )
    MAPE = c(MAPE, mape(y_teste,previsoes_teste) )
    
    preditor_lista[[k]] = regressor
  }
}

seleciona = as.data.frame( cbind( model_args, RMSE_treino, RMSE,  MAPE_treino, MAPE) )
View( seleciona  )


seleciona[seleciona$MAPE==min(seleciona$MAPE),]
id = which(seleciona$MAPE==min(seleciona$MAPE))
modelo_selecionado=preditor_lista[[id]]

df_milho$previsoes =  predict(modelo_selecionado, newdata = df_milho )
#########################################################################
