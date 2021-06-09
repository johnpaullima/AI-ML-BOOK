
#visualização
library(ggplot2)

# carrega a base que se deseja trabalhar
dados <- read.csv("customerJPHL2.csv",header = TRUE, sep=",")
# alguns dados estão como "texto". Troque para valores numéricos
dados[, c(2,3,5:7)] <- sapply(dados[, c(2,3,5:7)], as.numeric)
#plota o gráfico coluna vs coluna
plot(dados)
#mostra a distribuição dos gastos e da renda dos clientes - histograma
hist(dados$Spend)
hist(dados$INCOME)

# Faz um gráfico de Spend vs INCOME. A escala de cores mostra as diferentes regiões
plot(dados$INCOME,dados$Spend)

#separa apenas as colunas 2 a 7 para analisar os dados
preprocessed <- dados[,2:7]
k <- 3 # especifica o número máximo de clusters que se deseja separar

  
  # roda o modelo k-means
  # nstart = número de inicializações aleatórias; a melhor será a usada
  output <- kmeans(preprocessed, centers = k, nstart = 20)
  
  # Adiciona o número do cluster associado para cada amostra no dataset original
  Nome_coluna <- paste("cluster", k, sep="_")
  dados[,(Nome_coluna)] <- output$cluster
  dados[,(Nome_coluna)] <- factor(dados[,(Nome_coluna)], levels = c(1:k))
  
  
  # Graph clusters
  cluster_graph <- ggplot(dados, aes(x = INCOME, y = Spend))
  cluster_graph <- cluster_graph + geom_point(aes(colour = dados[,(Nome_coluna)]))
  colors <- c('red','orange','green3','deepskyblue','blue','darkorchid4','violet','pink1','tan3','black')
  cluster_graph <- cluster_graph + scale_colour_manual(name = "Cluster Group", values=colors)
  cluster_graph <- cluster_graph + xlab("INCOME")
  cluster_graph <- cluster_graph + ylab("Spend")
  title <- paste("Solução k-means com", k, sep=" ")
  title <- paste(title, "Clusters", sep=" ")
  cluster_graph <- cluster_graph + ggtitle(title)
  print(cluster_graph)
  
#Verifica como são os grupos de clusters. Relaciona os dados separados em 2 clusters
nclusters <- 3
par(mfrow=c(nclusters,6))

k <- 0
for (k in 1:nclusters ) {
  insights <- dados[ which(dados[9]==(k)),]
  hist(insights$Age,main = ("H Age"),xlim = range(10,70))
  hist(insights$Region,main = ("H Region"),xlim = range(0,4))
  hist(insights$INCOME,main = ("H Income"),xlim = range(100,420))
  hist(insights$Spend,main = ("H Spend"),xlim = range(00,250))
  hist(insights$Number_of_shopping,main = ("H Num"),xlim = range(0,50))
  hist(insights$tickmedio,main = ("H ticket"),xlim = range(0,50))
}

