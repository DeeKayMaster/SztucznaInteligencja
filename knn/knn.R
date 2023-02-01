library(ggplot2)
library(class)
burger_king_csv <- read.csv("C:/Users/user/Desktop/Studia/3rok/Sztuczna_inteligencja/projekt_koncowy/burger-king-menu.csv")
data = burger_king_csv[,-1]

nor=function(x)
{(x-min(x))/(max(x)-min(x))} # funkcja normalizująca(przeskalowywująca) dane na zakres 0-1

idx = sample(1:nrow(data),0.9*nrow(data)) # losujemy 90% indexów z data i przypisujemy je do idx

cl = data$Category # cl przechowuje od teraz zawartość kolumny Category

clTrain = cl[idx] # 90%
clTest = cl[-idx] # 10%

# lapply aplikuje funkcje nor na każdą kolumne oprócz pierwszej
# wynik jest zwracany jako lista
# dlatego as.data.frame() przekształca liste na tablice
data_norm=as.data.frame(lapply(data[,-1], nor))

train = data_norm[idx,] # zbior treningowy z już przeskalowanymi na 0-1 danymi
test = data_norm[-idx,]

#knn(train, test, tablica zawierająca etykiety klas dla danych treningowych, liczba_sasiadow_do_rozwazenia przy klasyfikacji)
model = knn(train, test,cl=clTrain,k=10) 

#tworzenie confusionMatrix
confusionMatrix=table(model,clTest)

jakosc = sum(diag(confusionMatrix))/sum(confusionMatrix) #jakość klasyfikatora
confusionMatrix
paste('jakość całkowita klasyfikatora: ', jakosc)



