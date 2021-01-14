## Data science for Human Resoures
# Modelo de classificacao baseado em Random Forest
# previsao de funcionarios que tendem a sair da empresa


# biblioteca para manipulacao de dados
library(tidyverse)
library(kableExtra)


# biblioteca para construcao da matriz de confusao
library(caret)
library(FactoMineR)


# biblioteca com modelo de randomforest
library(randomForest)


# biblioteca para avaliacao do modelo
library(vip)
library(jtools)
library(plotly)


# carregando os dados
#dados = read.csv('Human_Resources.csv') # base desbalanceada gerou overfitting
dados = read.csv('HumanTest.csv') #trabalhando com a base balanceada, 237 amostras aleatórias para cada categoria


# visualizando a estrutura do dataset
head(dados) %>%
  kable()%>%
  kable_styling(bootstrap_options = "striped", full_width = F, font_size = 10)


# Removendo dados irrelevantes
dados$EmployeeCount <- NULL
dados$EmployeeNumber <- NULL
dados$Over18 <- NULL
dados$StandardHours <- NULL


# Ajustando o nome da coluna idade
dados$Age <- dados$ï..Age
dados$ï..Age <- NULL


# Estatisticas descritivas sobre as variaveis
summary(dados)


# Ajustando variaveis categoricas
dados$Attrition <- as.factor(dados$Attrition)
dados$BusinessTravel <- as.factor(dados$BusinessTravel)
dados$Department <- as.factor(dados$Department)
dados$Education <- as.factor(dados$Education)
dados$EducationField <- as.factor(dados$EducationField)
dados$EnvironmentSatisfaction <- as.factor(dados$EnvironmentSatisfaction)
dados$Gender <- as.factor(dados$Gender)
dados$JobInvolvement <- as.factor(dados$JobInvolvement)
dados$JobLevel <- as.factor(dados$JobLevel)
dados$JobRole <- as.factor(dados$JobRole)
dados$JobSatisfaction <- as.factor(dados$JobSatisfaction)
dados$MaritalStatus <- as.factor(dados$MaritalStatus)
dados$OverTime <- as.factor(dados$OverTime)
dados$PerformanceRating <- as.factor(dados$PerformanceRating)
dados$RelationshipSatisfaction <- as.factor(dados$RelationshipSatisfaction)
dados$StockOptionLevel <- as.factor(dados$StockOptionLevel)
dados$WorkLifeBalance <- as.factor(dados$WorkLifeBalance)


# Separando dados de treino e dados de teste
index_amostra <- sample(x = 1: dim(dados)[1], size = dim(dados)[1]*0.5)
amostra_treino <- dados[index_amostra,]
amostra_teste <- dados[-index_amostra,]


table(amostra_teste$Attrition)
table(amostra_treino$Attrition)


# Estimacao de um modelo por random forest com os dados de treino
# determinando numero maximo de nos para evitar overfitting
modelo1 <- randomForest(formula = Attrition ~ .,
                        data = amostra_treino,
                        maxnodes = 15,
                        importance = T)


# Construindo a matriz de confusão para os dados de treino
confusionMatrix(
  predict(object = modelo1, newdata = amostra_treino),
  amostra_treino$Attrition
)

# acuracia de 91%
# Sensitividade 92%
# Especificidade 91%


# Construindo a matriz de confusão para os dados de teste
confusionMatrix(
  predict(object = modelo1, newdata = amostra_teste),
  amostra_teste$Attrition
)

# acuracia de 70%
# Sensitividade 66%
# Especificidade 70%


# Avaliando a importancia de cada variavel
varImpPlot(modelo1, type = 1) #Media do decrescimo do erro de predicao
varImpPlot(modelo1, type = 2) #Media da perda de impureza de cada no
varImpPlot(modelo1)


# Visualizando as importancias em formato grafico
vip(modelo1, 
    geom = "col", 
    horizontal = TRUE,
    aesthetics = list(color = "green", size = 0.5)) +
  theme_bw()


# Adicionando as probabilidades no conjunto de teste
amostra_teste$predict_proba <- predict(modelo1, newdata = amostra_teste, type = "prob")
amostra_teste %>%
  kable()%>%
  kable_styling(bootstrap_options = "striped", full_width = F, font_size = 10)


