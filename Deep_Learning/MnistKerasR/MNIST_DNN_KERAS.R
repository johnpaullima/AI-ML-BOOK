## instale o keras
# install.packages("keras")
## se não tiver python instalado no computador, ele vai sugerir instalar o Miniconda. Instale
## reinicie o PC
## carregue a library keras
# library(keras)
## execute o comando no install_keras()
# install_keras()
## quando terminar, rode o código a seguir. Ele deve executar, sem problemas.

library(keras)

# Preparação dos dados - hiperparâmetros -----------------------------------------------------
batch_size <- 128
num_classes <- 10 #saídas
epocas <- 5

# Dimensões das imagens de entrada
img_rows <- 28
img_cols <- 28

# Carrega os dados e separa em treinamento e teste
mnist <- dataset_mnist()
x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y

# Redefinindo as colunas/dimensões das entradas
x_train <- array_reshape(x_train, c(nrow(x_train), img_rows, img_cols, 1))
x_test <- array_reshape(x_test, c(nrow(x_test), img_rows, img_cols, 1))
input_shape <- c(img_rows, img_cols, 1)

# Transformando valores de intensidade (RGB*) na escala entre [0,1]
x_train <- x_train / 255
x_test <- x_test / 255

#plota no console o número de amostras de treinamento e teste
cat('x_train_shape:', dim(x_train), '\n')
cat(nrow(x_train), 'train samples\n')
cat(nrow(x_test), 'test samples\n')

# Converte vetores de classe em matrizes de classes binárias
y_train <- to_categorical(y_train, num_classes)
y_test <- to_categorical(y_test, num_classes)

# Define Model -----------------------------------------------------------

# Especifica o modelo de rede neural
model <- keras_model_sequential() %>%
  layer_conv_2d(filters = 4, kernel_size = c(3,3), activation = 'relu',
  #layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = 'relu',
                input_shape = input_shape) %>% 
  layer_conv_2d(filters = 8, kernel_size = c(3,3), activation = 'relu') %>% 
  #layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = 'relu') %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_dropout(rate = 0.25) %>% 
  layer_flatten() %>% 
  layer_dense(units = 128, activation = 'relu') %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = num_classes, activation = 'softmax')


# Compila (forma) o modelo
model %>% compile(
  loss = loss_categorical_crossentropy,
  optimizer = optimizer_adadelta(),
  metrics = c('accuracy')
)

# Mostra no console os parâmetros e camadas do modelo=
summary(model)

# Treina o modelo
model %>% fit( x_train, y_train, batch_size <- batch_size,
  epocas <- epocas, validation_split = 0.2
)

# Avalia o modelo
scores <- model %>% evaluate(x_test, y_test, verbose = 0)

# Métricas de saída
cat('Test loss:', scores[[1]], '\n')
cat('Test accuracy:', scores[[2]], '\n')

# Aplica o modelo no conjunto de teste
previsao <- predict(model,x_test)
#mostra o resultado do modelo, com as 10 classes e a prob.
previsao[1,]

#encontra qual é a saída prevista e real (coluna com maior prob)
predicted_labels <- max.col((previsao)) - 1
real_labels <-max.col((y_test)) - 1
#mostra a matriz de confusão
print(table(predicted_labels, real_labels))

# o código abaixo serve para exibir um número do dataset como imagem
x_train<- data.frame(x_train)
indicenumero <- 6
numero <- t(x_train[indicenumero,])
print(y_train[indicenumero,])
matrizIMG <- numero[1:28]
for (i in 1:27)
  matrizIMG<-rbind(matrizIMG, numero[(i*28):(i*28+27)])

matrizIMG <- t(matrizIMG)
image(matrizIMG)
