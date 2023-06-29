setwd("C:/Users/lukas/OneDrive/Pulpit/Nowy folder")

antro=read.csv("dane_antropo_clear.csv",header=T,sep=";",dec=",")
inbo=read.csv("dane_inbody_clear.csv",header=T,sep=";",dec=",")
d_stack=read.csv("dane_stack.csv",header=T,sep=";",dec=",")

pairs(~.,data=antro[,4:11])


attach(d_stack)
plot(Wysokosc.1, WysokoscSiedzeniowa.1)
cor(Wysokosc.1, WysokoscSiedzeniowa.1)
wykres=lm(Wysokosc.1~WysokoscSiedzeniowa.1)
wykres$coefficients
confint(wykres, level=0.99)
abline(wykres, col=3, lwd=4)
anova(wykres)

library(car)

scatterplotMatrix(d_stack[,c(4:12)],pch=20)

#proba tworzenia wykresow

dane = na.omit(d_stack) #usuniecie NA z danych

scatterplotMatrix(dane[,c(4:12)],pch=20)
summary(dane)

#stworzenie kolumny BMI
wysokosc = c(dane$Wysokosc.1/100)
waga = c(dane$Weight.1)
BMI = waga/(wysokosc*wysokosc)
dane["BMI"] = BMI

#testowanie wykresow
attach(dane)
plot(Age.1, BMI)
plot(Wysokosc.1, BMI)
plot(Weight.1, BMI)
scatterplotMatrix(dane[,c(4:12,124)],pch=20)

#usuniecie niepotrzebych kolumn
d_stack_c = d_stack[,-c(25:123)]
#usuniecie wszystkich NA
dane2 = na.omit(d_stack_c)
#stworzenie kolumny BMI
wysokosc = c(dane2$Wysokosc.1/100)
waga = c(dane2$Weight.1)
BMI = waga/(wysokosc*wysokosc)
dane2["BMI"] = BMI
#wykresy
scatterplotMatrix(dane2[,c(4:12,25)],pch=20)
attach(dane2)
plot(Age.1, BMI)

stripchart(BMI,method="jitter")
#usuwanie niepotrzebnych kolumn
column = c(1:4, 6:7, 9:12, 23:25)
dane3 = dane2[,column]
#przypisanie BMI do 4 wartości
#-1 = niedowaga
#0 = waga prawidłowa 
#1 = nadwaga
#2 = otyłosc
columnBMI=13
nrow(dane3)
BMIlevel = data.frame()
for(w in c(1:nrow(dane3)))
{
  record = dane3[w,columnBMI]
  record
  if(record < 18.5)
  {
    BMIlevel <- rbind(BMIlevel, -1)
  }
  else if(18.5 <= record & record <= 24.9)
  {
    BMIlevel <- rbind(BMIlevel, 0)
  }
  else if(24.9 < record & record <= 29.9)
  {
    BMIlevel <- rbind(BMIlevel, 1)
  }
  else if(29.9 < record & record <= 39.9)
  {
    BMIlevel <- rbind(BMIlevel, 2)
  }
}
#dodanie kolumny z wartoscia BMI
names(BMIlevel)[1] <- "BMIlevel"
dane3 = cbind(dane3, BMIlevel)


# uzyta ta sama funkcja, co na zajeciach
ocena<<-function(macierz.bledow, nazwa.klasyfikatora = "nazwa")
{
  # macierz postaci |TP FP|
  #                 |FN TN|
  tp = macierz.bledow[1,1]
  fp = macierz.bledow[1,2]
  fn = macierz.bledow[2,1]
  tn = macierz.bledow[2,2]
  
  acc = (tp + tn)/(tp + fp + tn + fn)
  sen = tp/(tp + fn)
  spe = tn/(tn + fp)
  pre = tp/(tp + fp)
  f1 = 2*pre*sen/(pre + sen)
  jakosc = c(acc, sen, spe, pre, f1)
  nazwy = c("dokladnosc", "czulosc", "specyficznosc", "precyzja", "f1")
  
  while(names(dev.cur()) != "null device") dev.off()
  png(paste(nazwa.klasyfikatora, ".png", sep = ""), width = 1000, height = 800)
  barplot(jakosc, main = nazwa.klasyfikatora, names = nazwy, ylim = c(0,1))
  dev.off()
  jakosc.ramka = data.frame(acc, sen, spe, pre, f1)
  return(jakosc.ramka)
}

attach(dane3)
plot(ObwodSzyi.1, BMI)
plot(ObwodKlatkiPiersiowej.1, BMI)
plot(ObwodPasa.1, BMI)
plot(ObwodBioder.1, BMI)
plot(ObwodUda.1, BMI)
plot(ObwodRamienia.1, BMI)

usun = c(1:4, 11:13)
dane3 = dane3[,-usun]


library(caTools)
split=sample.split(dane3$BMIlevel,SplitRatio=0.8)
split

train_set=subset(dane3,split==TRUE)
test_set=subset(dane3,split==FALSE)

train_X=train_set[,-ncol(dane3)]
test_X=test_set[,-ncol(dane3)]
train_Y=train_set[,ncol(dane3)]
test_Y=test_set[,ncol(dane3)]

train_X_s=scale(train_X)
test_X_s=scale(test_X)

# knn
library(class)
knn.pred = knn(train_X_s, test_X_s, train_Y, k = 5)
wyn = table(knn.pred, test_Y)
wyn.knn = ocena(wyn, "KNN_5")
wyn.knn

knn.pred = knn(train_X_s, test_X_s, train_Y, k = 10)
wyn = table(knn.pred, test_Y)
wyn.knn = ocena(wyn, "KNN_10")
wyn.knn

# random forest - sen i spe wychodzi odpowiednio 1 i 0...
library(randomForest)
rf=randomForest(formula=BMIlevel~.,data=train_set, method="class", ntree=50)
rf_pred=predict(rf,test_set)
wynik_rf=table(test_set$BMIlevel,rf_pred)
wynik_rFor=ocena(wynik_rf,"Random Forest")
wynik_rFor

# decision tree
library(rpart)
library(rpart.plot)
dt=rpart(formula=BMIlevel~.,data = train_set,method = "class",control = rpart.control(maxdepth = 5))
rpart.plot(dt,box.palette = "RdBu",digit=-2)
dt_pred=predict(dt,test_set,type="class")
wynik_dt=table(test_set$BMIlevel,dt_pred)
wynik_dTr=ocena(wynik_dt,"Simple Decision Tree")
wynik_dTr














