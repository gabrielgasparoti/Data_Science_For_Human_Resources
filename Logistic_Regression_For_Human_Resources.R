## Data Science for Human Resources
## Modelo Logistico Binario 
# Previsão de funcionarios que tendem a deixar a empresa


# biblioteca para manipulacao de dados
library(tidyverse)
library(kableExtra)

# biblioteca para tratamento de variaveis dummy
library(fastDummies)

# biblioteca para avaliar modelos
library(jtools)
library(lmtest)

# biblioteca para construcao da matriz de confusao
library(caret)
library(FactoMineR)

# biblioteca para construcao da curva ROC
library(pROC)
library(plotly)

# carregando os dados
dados = read.csv('Human_Resources.csv')

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

# Processamento de variaveis dummy

dados_dummy <- dummy_columns(.data = dados,
                             select_columns = c("BusinessTravel","Department",
                                                "Education","EducationField",
                                                "EnvironmentSatisfaction",
                                                "JobInvolvement","JobLevel",
                                                "JobRole","JobSatisfaction",
                                                "MaritalStatus","OverTime",
                                                "PerformanceRating",
                                                "RelationshipSatisfaction",
                                                "WorkLifeBalance", "Gender",
                                                "StockOptionLevel"),
                             remove_most_frequent_dummy = T,
                             remove_selected_columns = T)

# Visualizando a base de dados dummy
head(dados_dummy) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped")


# Estimando um primeiro modelo com procedimento stepwise
modelo <- glm(formula = Attrition ~ ., data = dados_dummy, family = 'binomial')
modelo_step <- step(object = modelo, k = qchisq(p = 0.05, df = 1, lower.tail = F))

# Visão Geral do modelo
# confirmando se o p-value ficou menor que 0.05 para cada variavel
# ou seja, se as variaveis sao estatisticamente significantes
summary(modelo_step)
summ(modelo_step, ci.width = 0.95)

# analisando o coeficiente de cada varivael
modelo_step$coefficients

# extraindo o valor do loglik LL do modelo com stepwise
logLik(modelo_step)

# comparando o LL com modelo nulo e modelo sem stepwise
lrtest(modelo_step)
lrtest(modelo, modelo_step)


# adicionando a probabilidade prevista no dataset 
dados$predict_proba <- modelo_step$fitted.values

# Construção da matriz de confusão
confusionMatrix(table(predict(modelo_step, type = "response") >= 0.5,
                      dados_dummy$Attrition == 1))

# Construção da curva ROC
ROC <- roc(response = dados_dummy$Attrition, 
           predictor = modelo_step$fitted.values)


# Plotando a Curva ROC
ggplotly(
  ggroc(ROC, color = "darkorchid", size = 1) +
    geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), 
                 color="orange", 
                 size = 0.2)+
    labs(x = "1 - Especificidade",
         y = "Sensitividade",
         title = paste("Área abaixo da curva:", 
                       round(ROC$auc, 3), 
                       "|",
                       "Coeficiente de Gini", 
                       round((ROC$auc[1] - 0.5) / 0.5, 3))) +
    theme_bw()
)

# Visualizando as probabilidades de saída da empresa em função das variáveis preditoras
# Criando uma função de suavização da curva sigmoide
smooth <- function(...) {
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), ...)
} 

ggplotly(
  dados %>%
    ggplot(aes(x = YearsAtCompany, y = predict_proba)) +
    geom_point(color = "orange", alpha = 0.7) +
    smooth(aes(color = "Fitted Values"),
           formula = y ~ x, se = F) +
    scale_color_manual("Legenda:",
                       values = "darkorchid") +
    labs(x = "Temperature",
         y = "Prob") +
    theme_bw()
)

glimpse(dados)
