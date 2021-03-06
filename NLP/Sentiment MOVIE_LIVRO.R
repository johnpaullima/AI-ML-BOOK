library(tm)
library(e1071)
library(dplyr)
library(caret)

#l� o arquivo com 2000 reviews de filmes
dados<- read.csv("movie-pang02.csv", stringsAsFactors = FALSE)
glimpse(dados) #mostra no terminal alguns exemplos

# muda de maneira aleat�ria a ordem dos reviews (est� sequenciado)
set.seed(1)
dados <- dados[sample(nrow(dados)), ]
glimpse(dados) #mostra no terminal alguns exemplos

# Converte a vari�vel 'class' de tipo character para factor.
dados$class <- as.factor(dados$class)

corpus <- Corpus(VectorSource(dados$text)) #cria o Corpus
# exibe o corpus
corpus

inspect(corpus[1:3]) #PRECISA LIBRARY


# Usa a biblioteca dplyr's  %>% para remover elementos textuais.
corpus.clean <- corpus %>%
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removePunctuation) %>% #remove pontua��o
  tm_map(removeNumbers) %>% #remove n�meros
  tm_map(removeWords, stopwords(kind="en")) %>%
  tm_map(stripWhitespace) # remove espa�o

# gera uma matriz tipo bag of words (DTM)
dtm <- DocumentTermMatrix(corpus.clean)
# Inspeciona o document-term matrix(dtm)
inspect(dtm[40:50, 10:15])

#separa em conjuntos de treinamento e teste
dados.train <- dados[1:1500,]
dados.test <- dados[1501:2000,]

#separa o BOW (DTM) em treinamento e teste
dtm.train <- dtm[1:1500,]
dtm.test <- dtm[1501:2000,]

#exibe o tamanho do DTM
dim(dtm.train)

#separa o corpus limpo em treinamento e teste
corpus.clean.train <- corpus.clean[1:1500]
corpus.clean.test <- corpus.clean[1501:2000]

#seleciona os termos que possuem 5 ou mais "apari��es"
fivefreq <- findFreqTerms(dtm.train, 5)
length((fivefreq))

# Use apenas os termos mais frequentes para (fivefreq) criar o BOW

dtm.train.nb <- DocumentTermMatrix(corpus.clean.train, control=list(dictionary = fivefreq))

dim(dtm.train.nb)

dtm.test.nb <- DocumentTermMatrix(corpus.clean.test, control=list(dictionary = fivefreq))

dim(dtm.test.nb)

# Func�o para converter a frequencia de palavras em YES (presen�a) e NO (ausencia)
converte_contagem <-  function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}

# Aplica a fun��o para obter os DTMs de treinamento e teste
trainNB <- apply(dtm.train.nb, 2, converte_contagem )
glimpse(trainNB)
testNB <- apply(dtm.test.nb, 2, converte_contagem )

# Treina o classificador de naive bayes
system.time( modelo <- naiveBayes(trainNB, dados.train$class, laplace = 1) )

# Us o classificador treinado para fazer previs�es no conjunto de teste
system.time( pred <- predict(modelo, newdata=testNB) )

# Cria a matrix de confus�o e exibe suas informa��es
conf.mat <- confusionMatrix(pred, dados.test$class)

conf.mat

conf.mat$byClass

conf.mat$overall

conf.mat$overall['Accuracy']

