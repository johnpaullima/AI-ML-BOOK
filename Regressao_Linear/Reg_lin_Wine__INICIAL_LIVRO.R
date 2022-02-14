#leia os dados do arquivo wine.csv
dados <- read.csv("wine.csv", header = TRUE, sep=",")

#plota os dados, coluna a coluna
plot(dados)

#cria o modelo de regressão linear
linearMod <- lm(Price ~ AGST, data=dados) 

#apresenta os coeficientes linear (b) e angular (a)
linearMod

#apresenta as métricas do modelo (residuo, distribuição, etc)
print(summary(linearMod))

#remove a coluna price do dataset dados
X1 <- subset(dados, select = -c(Price))

#Aplica a variável X1 no modelo e preveja o resultado
Ytest <- predict(linearMod, X1)
print('valor Previsto')
print(Ytest)
print('valor Esperado')
print(dados$Price) 

#plota os dados originais e o calculado
plot(dados$AGST,dados$Price)
points(dados$AGST,Ytest,col = "blue")

#calcula o RMSE
diferencas <- dados$Price-Ytest
RMSE <- sqrt(sum(diferencas^2)/length(diferencas))
print('RMSE')
print(RMSE)
