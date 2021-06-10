dados <- read.csv(file="logistic_CHD_JPHL.csv", header=TRUE, sep=",")
plot(dados)

###########################
#Separa os dados em dois conjuntos, treinamento e teste
Percenttreino <- 70
sample_size <- floor((Percenttreino/100) * nrow(dados)) #numero de amostras de treinamento
set.seed(2)                    #semente para randomizar
train_index <- sample(seq_len(nrow(dados)), size = sample_size) #gera indices
treino <- dados[train_index, ] #cria sub-matriz de treinamento
teste <- dados[-train_index, ] #cria sub-matriz de teste
plot(teste,col='red') #plota dados de teste - vermelho
plot(treino, col='blue') #plota dados de treinamento - azul

###########################
modelo <- glm(CHD ~ Age,family=binomial(link='logit'),data=treino) #cria modelo 
print(summary(modelo))

teste_idades <- seq(20, 80, by=1) #gera sequencia de idades de 20 a 80

previsao <- predict(modelo, list(Age=teste_idades), type="response") #aplica modelo no conjunto de testes
lines(teste_idades, previsao, col="green", lwd = 4)
grid()

previsao <- predict(modelo, newdata = teste, type="response") #aplica modelo nos dados de teste
print(previsao) #mostra os dados previstos para o conjunto de teste
#transforma valores continuos em 0 e 1
Mat0_1 <- ifelse(previsao > 0.5, 1, 0) #aplica threshold de 0,5 para transformar continuo em categorico
teste[,"Age"] #mostra as idades do conjunto de teste
Mat0_1 #mostra dados de saida ao aplicar o modelo. os numeros acima sao as posicoes randomizadas

matriz_confusao <- table(Mat0_1, teste[,"CHD"]) #gera matriz de confusão
print(matriz_confusao)
acuracia <- sum(diag(matriz_confusao))/sum(matriz_confusao) #calcula indice de acerto
print(acuracia)


