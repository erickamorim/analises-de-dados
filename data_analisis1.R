
rm(list = ls())


library(dplyr)
library(forecast)
library(lmtest)
library(ggplot2)

#library(readxl)
#covariates = read_excel("covariates.xlsx")
#data = read.csv("data_comexstat.csv")

#leitura dos dados
data = read.csv("4intelligence/dados/TFP.csv")

# informacoes dos dados, classe, caracteristicas, tipos, numero de linhas e colunas
class(data)
str(data)
dim(data)

# verificando a existencia de valores faltantes
colSums(is.na(data))



data %>% 
  group_by(isocode) %>% 
  summarise( observacoes = n(),
             total = sum(rtfpna),
             media = mean(rtfpna),
             mediana = median(rtfpna),
             desvio_padrao = sd(rtfpna)) %>% 
  arrange(desc(total))


smex = data %>% 
      filter(isocode == "MEX") %>%
      ggplot(aes(x = year, y = rtfpna)) +
      geom_line()

smex

scan = data %>% 
        filter(isocode == "CAN") %>%
        ggplot(aes(x = year, y = rtfpna)) +
        geom_line()

scan

susa = data %>% 
         filter(isocode == "USA") %>%
         ggplot(aes(x = year, y = rtfpna)) +
         geom_line()

susa



sfull = data %>% 
          ggplot(aes(x = year, y = rtfpna)) +
          geom_line() +
          facet_wrap(~ isocode)

sfull + xlab("Anos") + ylab("Fator de Produtividade Total")

g1 = data %>% 
      filter(isocode == "USA") %>%
      ggplot(aes(x = year, y = rtfpna)) +
      geom_line() 

g1 + xlab("Anos") + ylab("Fator de Produtividade Total")

g2 = data %>% 
       filter(isocode == "USA") %>%
       ggplot(aes(x = year, y = rtfpna)) +
       geom_point() 

g2 + xlab("Anos") + ylab("Fator de Produtividade Total")

g3 = g2 + geom_smooth(method = "lm", se = FALSE)
g3 + xlab("Anos") + ylab("Fator de Produtividade Total")

g4 = g1 + geom_point() + xlab("Anos") + ylab("Fator de Produtividade Total")
g4
g5 = g4 + geom_smooth(method = "lm", se = FALSE)
g5

series1 = data%>%
           ggplot(aes(x = year, y = rtfpna))+
           geom_point(aes(color = isocode)) +
           xlab("Anos") + ylab("Fator de Produtividade Total")

series1

series2 = data%>%
            ggplot(aes(x = year, y = rtfpna))+
            geom_line(aes(color = isocode)) +
            xlab("Anos") + ylab("Fator de Produtividade Total")

series2


dist1 = data%>%
          filter(isocode == "MEX")%>%
          ggplot(aes(x = rtfpna))+
          geom_histogram() + xlab("Fator de Produtividade Total")

dist1

dist2 = data%>%
          filter(isocode == "CAN")%>%
          ggplot(aes(x = rtfpna))+
          geom_histogram() + xlab("Fator de Produtividade Total")

dist2

dist3 = data%>%
  filter(isocode == "USA")%>%
  ggplot(aes(x = rtfpna))+
  geom_histogram() + xlab("Fator de Produtividade Total")

dist3

todasdist = data%>%
             mutate(isocode = reorder(isocode, -rtfpna)) %>%
             ggplot(aes(x = rtfpna))+
             geom_histogram() +
             facet_wrap(~ isocode , ncol = 1) + 
             xlab("Fator de Produtividade Total")

todasdist + ylab("Frequência")

boxp = data%>%
  mutate(isocode = reorder(isocode, rtfpna))%>%
  ggplot(aes(x = isocode, y = rtfpna)) +
  geom_boxplot() + ylab("Fator de Produtividade Total")+
  xlab("Países")

boxp

boxp1 = data%>%
  mutate(isocode = reorder(isocode, rtfpna))%>%
  ggplot(aes(x = isocode, y = rtfpna)) +
  geom_boxplot(aes(fill = isocode))+ ylab("Fator de Produtividade Total")+
  xlab("Países")

boxp1


var1 = data%>%
  filter(isocode =="MEX") %>%
  select(rtfpna)

var2 = data %>%
  filter(isocode=="CAN")%>%
  select(rtfpna)
  
var3 = data %>%
  filter(isocode=="USA")%>%
  select(rtfpna)

names(var1)[1] = "MEX"
names(var2)[1] = "CAN"
names(var3)[1] = "USA"

correlacao = cor(data.frame(c(var1,var2,var3)) )

correlacao %>%
  ggplot(aes(fill = correlacao)) +
  geom_tile() # +
#  scale_fill_distiller(
    # alterar o tipo da escala. pode ser divergente, sequencial e categorica
#    type = "div" ,
    # alterar a paleta. confira os possiveis valores na documentacao da funcao
#    palette = "RdBu",
    # inverter a direcao
#    direction = 1) + theme(axis.text.x = element_text(angle = 90))


s1 = data %>%
  filter(isocode == "MEX")%>%
  select(year, rtfpna)

s2 = data %>%
  filter(isocode == "CAN")%>%
  select(year, rtfpna)

s3 = data %>%
  filter(isocode == "USA")%>%
  select(year, rtfpna)


s1 = ts(s1$rtfpna, start = min(s1$year), frequency = 1)
s2 = ts(s2$rtfpna, start = min(s2$year), frequency = 1)
s3 = ts(s3$rtfpna, start = min(s3$year), frequency = 1)

s1
s2
s3

ts.plot(s1)
# as funcoes de autocorrelacao e a autocorrelacao parcial (fac e facp) nos ajudam a
# idenfificar a ordem q dos termos MA e  p do AR

# FAC - grafico da autocorrelacao contra a defasagem
# FACP - grafico da autocorrelacao pura entre duas observacoes (elimindo as correlacoes implicitas
#        estimadas passo a passo acrescentando cada defasagem )
par(mfrow = c(1,2))
acf(s1)
acf(s1, type = "partial")

par(mfrow = c(1,2))
acf(s2)
acf(s2, type = "partial")

par(mfrow = c(1,2))
acf(s3)
acf(s3, type = "partial")

ggtsdisplay(s1) 
ggtsdisplay(s2) 
ggtsdisplay(s3)

# Diferenciacao nao sazonal, a funcao retorna o numero de diferencas
# necessarias para passar no teste ao nivel alfa
ndiffs(s1)
ndiffs(s2)
ndiffs(s3)

s1_d = diff(s1, 2)
ggtsdisplay(s1_d, main = "segunda diferenca zasonal")

autoplot(s1_d)


#apos a diferenciacao notamos eh possivel ver que nao ha qualquer indicacao de sazonalidades na serie 

par(mfrow = c(3,2))
plot(s1, main = "serie")
plot(s1_d, main = "serie Diferenciada")
acf(s1, main = "FAC da serie")
acf(s1_d, main = "FAC da serie diferenciada")
acf(s1, type = "partial", main = "FACP da serie")
acf(s1_d, type = "partial", main = "FACP da serie diferenciada")

s2_d = diff(s2, 2)
ggtsdisplay(s2_d)

autoplot(s2_d)


par(mfrow = c(1,2))
acf(s2_d)
acf(s2_d, type = "partial")


s3_d = diff(s3, 1)
ggtsdisplay(s3_d)

autoplot(s3_d, ylab = "", main = "índice PTF dos Estados Unidos após a primeira diferença")

par(mfrow = c(1,2))
acf(s3_d)
acf(s3_d, type = "partial")

#plot(stl(log(serie). "periodic"))
#decomp=decompose(s3)

# Diferenciacao sazonal, a funcao retorna o numero de diferenciacoes 

nsdiffs(s1_d)
nsdiffs(s2_d)
nsdiffs(s3_d)

# Ajuste do modelo ARIMA

s1_treino = ts(s1[1:52], frequency = 1)
s1_teste = ts(s1[52:62], frequency = 1)
s2_treino = ts(s2[1:52], frequency = 1)
s2_teste = ts(s2[52:62], frequency = 1)
s3_treino = ts(s3[1:52], frequency = 1)
s3_teste = ts(s3[52:62], frequency = 1)

# usando a funcao auto.arima. A funcao auto.arima ajusta e testa varios modelos 
# selecionando e indicando para o usuario o modelo que melhor se ajusta a serie
ajuste_s1 = auto.arima(y = s1_treino)
ajuste_s2 = auto.arima(y = s2_treino)
ajuste_s3 = auto.arima(y = s3_treino)

autoplot(forecast(ajuste_s1, h =10))
autoplot(forecast(ajuste_s2, h =10))
autoplot(forecast(ajuste_s3, h =10))

########
modelo = Arima( y = s1_treino, order = c(0,2,1), seasonal = c(0,0,0))
modelo1 = Arima( y = s1_treino, order = c(1,2,0), seasonal = c(0,0,0))
# o resultado indicou um modelo integrado com 2 diferencas e MA(1) 
########

summary(modelo)
summary(modelo1)

# fazendo um teste de hipoteses para os coeficientes pacote lmtest
coeftest(modelo)
coeftest(modelo1)

plot(s1_treino)
lines(modelo$fitted, col = "blue")
lines(modelo1$fitted, col = "red")

#medidas de acuracia
accuracy(s1_treino, modelo$fitted)
accuracy(s1_treino, ajuste_s1$fitted)

# realizando previsoes para os 10 anos separados nos dados de teste
previsoes = forecast(modelo, h = 10)
previsoes1 = forecast(modelo1, h =10)

plot(previsoes)
plot(previsoes1)

plot(as.numeric(s1_teste), type = "l")
lines(as.numeric(previsoes$mean), col = "blue")

plot(as.numeric(s1_teste), type = "l")
lines(as.numeric(previsoes1$mean), col = "red")


accuracy(as.numeric(s1_teste),as.numeric(previsoes$mean) )
accuracy(as.numeric(s1_teste),as.numeric(previsoes1$mean) )

# Diagnóstico dos residuos
tsdiag(modelo)

qqnorm(modelo$residuals)
qqline(modelo$residuals)

