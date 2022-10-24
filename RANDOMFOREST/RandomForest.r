#Instala pacote se ainda n?o tiver
install.packages("randomForest")

#carrega pacote randomForest
library(randomForest)
#Carrega dados da IRIS
data(iris)
head(iris)
dat <- iris
#dat <- read.csv(file="e:/wifi_loc.txt", header=TRUE, sep="\t")

#Separa os dados em dois conjuntos, treinamento e teste
sample_size <- floor(0.7 * nrow(dat)) #numero de amostras de treinamento
set.seed(46)                    #semente para randomizar
train_index <- sample(seq_len(nrow(dat)), size = sample_size) #gera indices
train <- dat[train_index, ] #cria sub-matriz de treinamento
test <- dat[-train_index, ] #cria sub-matriz de teste

#executa o RandomForest e criar o  objeto rf
rf <- randomForest(Species~.,data=train,ntree=1000)
#verifique a matriz de confus?o de RF
rf
#plota o Erro para cada ?rvore
plot(rf)

#pega apenas as colunas com variaveis de entrada
valor_teste <- test[,1:4]

#aplica as entradas no modelo e obt?m a previs?o use type votes ou response
respostas <- predict(rf,newdata =valor_teste, type ="response",predict.all = TRUE)
#mostra o ?ndice de probabilidade por vota??o
respostas

#faz a matriz de confus?o - esperado vs previsto
matriz_confusao <-table(respostas$aggregate, test[,5])
matriz_confusao

# calcula o % de acertos
indice_acerto <- sum(diag(matriz_confusao))/sum(matriz_confusao) #calcula indice de acerto
indice_acerto


