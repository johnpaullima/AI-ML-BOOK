library (caret)
library(readr)
library(tibble)
library(pROC)

dados <- read_delim("EINSTEINdataset_MOD_RED.csv",";", escape_double = FALSE, trim_ws = TRUE)
glimpse(dados)

dadosnegativos <- subset(dados,dados[,3]==0) #seleciona apenas os casos negativos
dadospositivos <- subset(dados,dados[,3]==1) #seleciona apenas os casos positivos

dadosred <- rbind(dadospositivos,dadosnegativos) #junta os dois conjuntos
dadosred <- dadosred[1:180,] #seleciona os 83 casos positivos e 97 negativos
summary(dadosred)
set.seed(40)                        #semente para randomizar

dadosred <- dadosred[sample(nrow(dadosred)), ]
histogram(dadosred$SARSCOV2exam)

#Separa os dados em dois conjuntos, treinamento e teste
sample_size <- floor(0.7 * nrow(dadosred)) #numero de amostras de treinamento
train_index <- sample(seq_len(nrow(dadosred)), size = sample_size) #gera indices
train <- dadosred[train_index, ] #cria sub-matriz de treinamento
test <- dadosred[-train_index, ] #cria sub-matriz de teste

formula <- SARSCOV2exam ~ Age_quantile + Monocytes + Leukocytes + Platelets +RDW + MCV +Lymphocytes + MCH
model <- glm(formula,family=binomial(link='logit'),data=train) #cria modelo 
print(summary(model))

pred <- predict(model, newdata = test, type="response") #aplica modelo nos dados de teste

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
                   ci = TRUE,                  # compute AUC (of AUC by default)
                   print.auc = TRUE)           # print the AUC (will contain the CI)
ciobj <- ci.se(rocobj,                         # CI of sensitivity
               specificities = seq(0, 100, 5)) # over a select set of specificities
plot(ciobj, type = "shape", col = "#1c61b6AA")     # plot as a blue shape
plot(ci(rocobj, of = "thresholds", thresholds = "best")) # add one threshold

