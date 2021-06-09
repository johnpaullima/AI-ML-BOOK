dados <- read.csv(file="logistic_CHD_JPHL.csv", header=TRUE, sep=",")
plot(dados)

model <- glm(CHD ~.,family=binomial(link='logit'),data=dados) #cria modelo 
print(summary(model))

teste_idades = seq(20, 80, by=1) #gera sequencia de idades de 20 a 80

previsao = predict(model, list(Age=teste_idades), type="response") #aplica modelo no conjunto de testes
#plot(dat)
lines(teste_idades, previsao, col="green", lwd = 4)
grid()


