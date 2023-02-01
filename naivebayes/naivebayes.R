library(naivebayes)

burger_king_csv <- read.csv("C:/Users/user/Desktop/Studia/3rok/Sztuczna_inteligencja/projekt_koncowy/burger-king-menu.csv")

data = burger_king_csv[,-1]

xtabs(~Category,data = data)
idx = sample(2, nrow(data),replace = T, prob = c(0.9, 0.1))

train = data[idx==1,]
test = data[idx==2,]

# stworzenie modelu na danych treningowych
model = naive_bayes(Category ~ ., data = train) 
plot(model)

# -1 oznacza, że pomijamy kolumne o index=1(Item)
p = predict(model,train[,-1])

#confusion matrix
tab = table(p,train$Category)
tab

#jakość klasyfikatora
sum(diag(tab))/sum(tab) 

p1 = predict(model,test[,-1])
#confusion matrix
tab1 = table(p1,test$Category)  
tab1

#jakość klasyfikatora
jakosc = sum(diag(tab1))/sum(tab1)
paste('jakość całkowita klasyfikatora: ', jakosc)
