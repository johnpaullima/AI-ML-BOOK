#carrega as bibliotecas necessárias
library(ggplot2) #biblioteca para plotar gráficos mais bonitos

#leia os dados do arquivo wine.csv
dados <- read.csv("wine.csv",header = TRUE, sep=",")
#plota os dados, coluna a coluna
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
#teste <-dados
#treino <- dados

#cria o modelo de regressão linear
linearMod <- lm(Price ~ AGST + Age + WinterRain + HarvestRain, data=treino) 

#apresenta os coeficientes linear e angular
linearMod

#apresenta as métricas do modelo (residuo, distribuição, etc)
print(summary(linearMod))

#remove a coluna price do dataset test
X1 = subset(teste, select = -c(Price))

#Aplica a variável X1 no modelo e preveja o resultado
Ytest <- predict(linearMod, X1)
print('valor Previsto')
print(Ytest)
print('valor Esperado')
print(teste$Price)
saidas <- rbind(Ytest,teste$Price)
barplot(saidas, xlab="dado - Previsto/Esperado",ylim=c(0,10),
        col=c("darkblue","red"), beside=TRUE)

#determina o RMSE dos dados previstos vs esperados
diferencas <- teste$Price-Ytest
print('diferenca real - previsto')
print(diferencas)
#plota as diferenças entre o previsto e o esperado
barplot(diferencas)

#calcula o RMSE
RMSE <- sqrt(sum(diferencas^2)/length(diferencas))
print('RMSE')
print(RMSE)

#plota o gráfico Y vs X, com intervalo de confiança.
ggplot(dados, aes(x=AGST, y=Price)) + geom_point(shape=18, color="blue") + geom_smooth(method=lm,  linetype="dashed",
                                                                                       color="darkred", fill="pink") + geom_point()
