#carrega dados de entrada
dados <- read.csv("maca_perceptron.csv", sep=";")

#função para normalização dos dados
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
#aplica a normalização nos dados de entrada
dados[,1] <- normalize(dados[,1])
dados[,2] <- normalize(dados[,2])
#função de treinamento do perceptron
perceptron <- function(x, y, learning_r , niter) {
  # cria vetores dos pesos e do número de erros por épocas (niter)
  #os pesos são inicializados de maneira aleatória (runif(1))
  peso <- rep(runif(1), dim(x)[2] + 1)
  numamostras <- dim(x)[1]
  erros <- rep(0, niter)
  acuracia <- rep(0, niter)
  
 print("ITERACAO, PESOS, ACURACIA")
 # repete o procedimento de treinamento niter vezes
 for (jj in 1:niter) {
  
  # varre todos os dados de treinamento
  for (ii in 1:length(y)) {
      
    # aplica a unidade de soma e a função de ativação
    z <- sum(peso[2:length(peso)] * 
               as.numeric(x[ii, ])) + peso[1]
    if(z < 0) {
      ypred <- 0
    } else {
      ypred <- 1
    }
      
    # modifica o peso, quando há diferença entre o previsto e real
    pesodiff <- learning_r  * (y[ii] - ypred) * 
      c(1, as.numeric(x[ii, ]))
    peso <- peso + pesodiff

    # Atualiza o número de amostras erradas
    if ((y[ii] - ypred) != 0.0) {
      erros[jj] <- erros[jj] + 1
    }
      
  }
   #mostra no console a época e os pesos atuais
   acuracia[jj] <- (100*(numamostras-erros[jj])/numamostras)
   print(c(jj,peso,acuracia[jj]), digits = 3)
 }
  
  # mostra os pesos obtidos após a ultima época
  print("Pesos finais")
  print(peso)

  return(acuracia)
}
#define o número máximo de épocas 
nepocas <- 30
#define a taxa de aprendizagem
tx_aprendizagem <- 0.01
acuracia <- perceptron(dados[,1:2], dados[,3], tx_aprendizagem, nepocas)
plot(1:nepocas, acuracia, type="l", lwd=2, col="red", xlab="épocas", ylab="acurácia",ylim=c(0,100))
title("Acurácia vs épocas")

