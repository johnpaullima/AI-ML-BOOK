library (caret)
dados <- read.csv(file="covid_form.csv", header=TRUE, sep=";")
dadosred <- dados
dadosred$died <-ifelse(dadosred$date_died>3000-01-01,0,1)
summary(dadosred)
#Separa os dados em dois conjuntos, treinamento e teste
sample_size <- floor(0.8 * nrow(dadosred)) #numero de amostras de treinamento
set.seed(46)                    #semente para randomizar
train_index <- sample(seq_len(nrow(dadosred)), size = sample_size) #gera indices
train <- dadosred[train_index, ] #cria sub-matriz de treinamento
test <- dadosred[-train_index, ] #cria sub-matriz de teste
#plot(train$age,train$covid_res, col='blue') #plota dados de treinamento - azul
#points(test$age,test$covid_res,col='red') #plota dados de teste - vermelho
formula <- died ~ age + pneumonia + asthma + obesity + copd +hypertension +tobacco + cardiovascular
model <- glm(formula,family=binomial(link='logit'),data=train) #cria modelo 
print(summary(model))

pred <- predict(model, newdata = test, type="response") #aplica modelo nos dados de teste
#print(pred) #mostra os dados previstos para o conjunto de teste

#transforma valores continuos em 0 e 1
Mat0_1 <- ifelse(pred > 0.5, 1, 0) #aplica threshold de 0,5 para transformar continuo em categorico

Mat0_1 #mostra dados de saida ao aplicar o modelo. os numeros acima sao as posicoes randomizadas

matriz_confusao <- table(Mat0_1, test[,"covid_res"]) #gera matriz de confusão
print(matriz_confusao)
a<-confusionMatrix(matriz_confusao)
acuracia <- sum(diag(matriz_confusao))/sum(matriz_confusao) #calcula indice de acerto
print(acuracia)
sensibilidade <- matriz_confusao[1,1]/(matriz_confusao[1,1]+matriz_confusao[1,2])
precisao <- matriz_confusao[1,1]/(matriz_confusao[1,1]+matriz_confusao[2,1])
