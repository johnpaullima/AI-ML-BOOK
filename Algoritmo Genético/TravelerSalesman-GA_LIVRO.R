#carrega as bibliotecas necessárias
library(GA)
#as distancias entre as cidades já existem no R. Carrega os dados
data("eurodist", package = "datasets")
dados <- as.matrix(eurodist)

#Função para calcular a distância da rota
DistRota <- function(tour, distMatrix) {
  tour <- c(tour, tour[1])
  route <- embed(tour, 2)[,2:1]
  sum(distMatrix[route])
}

#Função de fitness (desempenho) que deverá ser maximizada
#é o inverso da distância da rota
funcaoFitness <- function(tour, ...) 1/DistRota(tour, ...)

modeloGA <- ga(type = "permutation", fitness = funcaoFitness, distMatrix = dados,
         lower = 1, upper = attr(eurodist, "Size"), popSize = 20, maxiter = 100,
         run = 500, pmutation = 0.2)

summary(modeloGA)

out <- plot(modeloGA)

#Mostra a melhor rota encontrada
mds <- cmdscale(eurodist)
x <- mds[, 1]
y <- -mds[, 2]
plot(x, y, type = "n", asp = 1, xlab = "", ylab = "")
abline(h = pretty(range(x), 10), v = pretty(range(y), 10),
       col = "light gray")
rota <- modeloGA@solution[1, ]
rota <- c(rota, rota[1])
n <- length(rota)
arrows(x[rota[-n]], y[rota[-n]], x[rota[-1]], y[rota[-1]],
       length = 0.15, angle = 25, col = "steelblue", lwd = 2)
text(x, y, labels(eurodist), cex=0.8)

