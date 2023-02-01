library("party")
library(randomForest)

burger_king_csv=read.csv("C:/Users/user/Desktop/Studia/3rok/Sztuczna_inteligencja/projekt_koncowy/burger-king-menu.csv", header=TRUE, stringsAsFactors=TRUE)
data = burger_king_csv[,-1] # pomijamy pierwszą kolumne

idx=sample(2, length(data), replace = TRUE, prob = c(0.7, 0.3))
idx
train = data[idx == 1,]
test = data[idx == 2,]

model = randomForest(Category~.,data=train)
p=predict(model,test)
confusionMatrix=table(p,test$Category)
jakosc = sum(diag(confusionMatrix))/sum(confusionMatrix)
confusionMatrix
paste('jakość całkowita klasyfikatora: ', jakosc)