library (caret)
library(readr)
library(tibble)
library(pROC)

dados <- read_delim("EINSTEINdataset_MOD_RED.csv",";", escape_double = FALSE, trim_ws = TRUE)
glimpse(dados)

dadosred <- dados
summary(dadosred)
set.seed(40)                        #semente para randomizar

dadosred <- dadosred[sample(nrow(dadosred)), ]
histogram(dadosred$SARSCOV2exam)

#Separa os dados em dois conjuntos, treinamento e teste
sample_size <- floor(0.7 * nrow(dadosred)) #numero de amostras de treinamento
train_index <- sample(seq_len(nrow(dadosred)), size = sample_size) #gera indices
train <- dadosred[train_index, ] #cria sub-matriz de treinamento
test <- dadosred[-train_index, ] #cria sub-matriz de teste
#plot(train$Age_quantile,train$SARSCOV2exam, col='blue') #plota dados de treinamento - azul
#points(test$Age_quantile,test$SARSCOV2exam,col='red') #plota dados de teste - vermelho
formula <- SARSCOV2exam ~ Age_quantile + Monocytes + Leukocytes + Platelets +RDW + MCV +Lymphocytes + MCH
model <- glm(formula,family=binomial(link='logit'),data=train) #cria modelo 
print(summary(model))

pred <- predict(model, newdata = test, type="response") #aplica modelo nos dados de teste
#print(pred) #mostra os dados previstos para o conjunto de teste

real<-deframe(test[,"SARSCOV2exam"])

#transforma valores continuos em 0 e 1
Previsto0_1 <- (ifelse(pred > (0.5), 1, 0)) #aplica threshold de 0,5 para transformar continuo em categorico

matriz_confusao <- table(Previsto0_1, real) #gera matriz de confusão
matrizConf<-confusionMatrix(matriz_confusao, positive = "1")
print(matrizConf)
print(matrizConf$table)

#faz a curva ROC
rocobj <- plot.roc(Previsto0_1, real,
                   main = "ROC", 
                   percent=TRUE,
                   ci = TRUE,                  # calcula AUC
                   print.auc = TRUE)           
ciobj <- ci.se(rocobj,                         # Intervalo de confianca da precisão
               specificities = seq(0, 100, 5)) # over a select set of specificities
plot(ciobj, type = "shape", col = "#1c61b6AA")     
plot(ci(rocobj, of = "thresholds", thresholds = "best"))
