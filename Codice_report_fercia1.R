install.packages("mboost")
library(mboost)
library(pls)
library(FactoMineR) 
library(tidyverse)
library(dslabs)
library('ggplot2')
load("~/Desktop/datascience/Analisi_bigdata/REPORT/Dati_Fercia.RData")
View(dati)

ds <- dati
View(ds)

summary(ds)
str(ds)

attach(ds)
plot(Unit_Ticket, col = 'blue', main = 'prezzo biglietto')
boxplot(Unit_Ticket, col = 'red', xlab = 'Prezzi Ticket')

hist(Fidelity, col = 'green', main = '')
hist(Unit_Ticket, col = 'purple', main = '')
hist(BookTime, col='yellow',main = '')
hist(NPax, col = 'blue', main = '')

par(mfrow = c(1,3))
par(mfrow = c(1,1))
barplot(table(Seat), col = c('pink', 'green'), main = 'Prenotazione posto a sedere')
barplot(table(PriorBoard), col = c('pink', 'green'), main = 'Prenotazione imbarco priority')
barplot(table(Luggage), col = c('pink', 'green'), main = 'Prenotazione Luggage+')
barplot(table(Return), col = c('pink', 'green'), main = 'Prenotazione ritorno')
barplot(table(Departure), col = c('pink', 'green','blue','orange','yellow','red'), main = 'Partenza')
barplot(table(Arrival), col = c('pink', 'green','blue','orange','yellow','red') ,main = 'Arrivo')
barplot(table(ModPag), col = c('Yellow', 'green'), main = 'Prenotazione imbarco priority')
warnings()

pairs(ds)
pairs(~Unit_Ticket+Fidelity+BookTime+Discount, ds) #nessuna particolare relazione
pairs(~Unit_Ticket+Departure, ds)
pairs(~Data+Unit_Ticket, ds)
pairs(~Unit_Ticket+ModPag+Taxes+NPax, ds)

library(corrplot)
corrplot(cor(ds[,-c(1,3,4,5,6,12,13,14)]), diag = F, method = "pie", type = "upper" )

gg1 <-ggplot(ds, aes(x=Data, y=Fidelity, size=Unit_Ticket, color=Departure,)) +geom_point() 
gg1 

gg2 <-ggplot(ds, aes(x=Data, y=Unit_Ticket, size=Fidelity, color=Departure,)) +geom_point() 
gg2 

ds_3 <- ds %>% 
  filter(Departure == 'Venice') %>% 
  select(Unit_Ticket, Fidelity)
mean(ds_3$Fidelity) #[1] 56.03924 media fideliti voli partenza venezia

ds_4 <- ds %>% 
  filter(Departure != 'Venice') %>% 
  select(Unit_Ticket, Fidelity)
mean(ds_4$Fidelity) #[1] 42.57611 media fidelity voli partenza non venezia 

ds_Paris <- ds %>%
  filter(Arrival=='Paris')
View(ds_Paris)
gg3 <-ggplot(ds_Paris, aes(x=Data, y=Unit_Ticket, size=Fidelity, color=Departure,)) +geom_point() 
gg3 #voli a paris dal 2010 al 2020 

ds_Paris2 <- ds %>%  
  filter(Arrival=='Paris' & Departure == 'Venice')
View(ds_Paris2)
gg4 <-ggplot(ds_Paris2, aes(x=Data, y=Unit_Ticket, color=Fidelity,)) +geom_point() 
gg4  #solo voli dal 2010 al 2020 da venezia a paris 

ds_Venice <- ds %>% 
  filter(Departure=='Venice')
gg5 <-ggplot(ds_Venice, aes(x=Data, y=Unit_Ticket, color=Fidelity,)) +geom_point() 
gg5 #solo voli partenza venezia 

gg6 <-ggplot(ds, aes(x=NBookMonth, y=Unit_Ticket, color=Discount,)) +geom_point() 
gg6 #fidelity e num prenotazioni last 30gg

gg7 <-ggplot(ds, aes(x=Arrival, y=Departure, color=Discount,)) +geom_point() 
gg7

ds_Paris_Madrid <- ds %>%
  filter(Arrival==c('Paris','Madrid'))
view(ds_Paris_Madrid)
gg8 <-ggplot(ds_Paris_Madrid, aes(x=Data, y=Unit_Ticket, color=Arrival,)) +geom_point()
gg8

gg9 <- dati%>%ggplot(aes(Arrival))+geom_bar(aes( fill=Arrival))+theme_bw(7)+
  ggtitle("Partenze per ogni aeroporto")+facet_wrap(~Departure)
gg9

gg8 <-ggplot(ds, aes(x=Unit_Ticket, y=Departure, color=Arrival,)) +geom_point() 
gg8

ds_1 <- ds %>% 
  filter(Luggage=='Yes' & PriorBoard=='Yes' & Seat == 'Yes') %>% 
  select(Unit_Ticket, Fidelity)
ds_1
mean(ds_1$Unit_Ticket) #[1] 128.0509

ds_2 <- ds %>% 
  filter(Luggage=='No' & PriorBoard=='No' & Seat == 'No') %>% 
  select(Unit_Ticket, Fidelity) 
ds_2
mean(ds_2$Unit_Ticket) #[1] 147.3924
#strano i prezzi in media di chi non prende nesun plus sono maggiori di chi li prende 

?pairs
pairs(ds[,-c(1,3,4,5,6,12,13,14)], col = Departure, 
      upper.panel = NULL, pch = 16, cex = 0.5)
legend("topright", bty = "n", legend = c('Bergamo','Milan','Rome', 'Plermo','Naples','Venice'), pch = 16, 
       col = c(1,2,3,4,5,6),
       xpd = T, cex = 2, y.intersp = 0.5)

pairs(ds[,-c(1,3,4,5,6,12,13,14)], col = ModPag, 
      upper.panel = NULL, pch = 16, cex = 0.5)
legend("right", bty = "n", legend = c('C_credito','C_debito'), pch = 16, 
       col = c(1,2),
       xpd = T, cex = 2, y.intersp = 0.5)
#il pairs colorato per la modalita di pagamento fa emergere delle chiare differenze tra chi usa la carta di credito e la di debito


pairs(~NPax+Discount+Taxes, ds[,-c(1,3,4,5,6,12,13,14)], col = Departure, upper.panel = NULL, pch = 16, cex = 0.5)
legend("topright", bty = "n", legend = c('Bergamo','Milan','Rome', 'Plermo','Naples','Venice'), pch = 16, 
       col = c('red','yellow','green','purple', 'orange','blue'),
       xpd = T, cex = 2, y.intersp = 0.5)

pairs(ds[,-c(1,3,4,5,6,12,13,14)], col = Arrival, 
      upper.panel = NULL, pch = 16, cex = 0.5)
legend("topright", bty = "n", legend = c('Paris','London','Madrid','Amsterdam'), pch = 16, 
       col = c('red','yellow','green','purple'),
       xpd = T, cex = 2, y.intersp = 0.5)
#-------------------------------------------------------------------

ds_pca <- ds[,-c(1,3,4,5,6,12,13,14)]

res <- PCA(ds,quali.sup=c(1,3,4,5,6,12,13,14),scale.unit = TRUE, ncp=8)
summary(res)

#Call:
#  PCA(X = ds_pca, ncp = 8) 
#Eigenvalues
#                        Dim.1   Dim.2   Dim.3   Dim.4   Dim.5   Dim.6   Dim.7   Dim.8   Dim.9
#Variance               2.007   1.806   1.254   1.220   1.000   0.946   0.810   0.795   0.574
#% of var.             18.249  16.421  11.401  11.090   9.088   8.602   7.364   7.225   5.220
#Cumulative % of var.  18.249  34.671  46.072  57.162  66.250  74.852  82.215  89.441  94.661

plot(res, choix="var", title="Graph of the variables", axes=3:4)

dimdesc(res)
plot(res, cex=0.8, invisible="quali", title="Graph of the individuals")
plotellipses(res)

#tolgo var meno significative pca n1, risultati migliorano abbatsnza

res_1 <- PCA(ds,quali.sup=c(1,3,4,5,6,12,13,14),quanti.sup = c(18), scale.unit = TRUE, ncp=8)
summary(res_1)

#Call:
#  PCA(X = ds, scale.unit = TRUE, ncp = 8, quanti.sup = c(18), quali.sup = c(1, 3, 4, 5, 6, 12, 13, 14)) 


#Eigenvalues
#                       Dim.1   Dim.2   Dim.3   Dim.4   Dim.5   Dim.6   Dim.7   Dim.8   Dim.9  Dim.10
#Variance               2.007   1.806   1.254   1.218   0.947   0.810   0.796   0.574   0.407   0.180
#% of var.             20.074  18.064  12.542  12.181   9.467   8.100   7.956   5.743   4.074   1.799
#Cumulative % of var.  20.074  38.138  50.679  62.860  72.327  80.428  88.384  94.127  98.201 100.000

plot(res_1, choix="ind", cex=0.8, habillage=13, 
     title="Graph of the individuals", axes=3:4)

plot(res_1, choix="var", title="Graph of the variables", axes=2:3)


res_2 <- PCA(ds,quali.sup=c(1,3,4,5,6,12,13,14,16,18),scale.unit = TRUE, ncp=8)
summary(res_2)
#Call:
#PCA(X = ds, scale.unit = TRUE, ncp = 8, quali.sup = c(1, 3, 4,  
                                                    #    5, 6, 12, 13, 14, 16, 18)) 


#Eigenvalues
#Dim.1   Dim.2   Dim.3   Dim.4   Dim.5   Dim.6   Dim.7   Dim.8   Dim.9
#Variance               2.007   1.709   1.254   0.947   0.886   0.809   0.575   0.408   0.404
#% of var.             22.305  18.993  13.929  10.524   9.840   8.994   6.385   4.539   4.491
#Cumulative % of var.  22.305  41.298  55.227  65.751  75.592  84.586  90.970  95.509 100.000

plot(res_2, choix="var", title="Graph of the variables", axes=3:4)


res_3 <- PCA(ds,quali.sup=c(1,3,4,5,6,12,13,14,16,17,18),scale.unit = TRUE, ncp=8)
summary(res_3)

#Call:
#PCA(X = ds, scale.unit = TRUE, ncp = 8, quali.sup = c(1, 3, 4, 5, 6, 12, 13, 14, 16, 17, 18)) 

#Eigenvalues
#                       Dim.1   Dim.2   Dim.3   Dim.4   Dim.5   Dim.6   Dim.7   Dim.8
#Variance               2.007   1.572   1.254   0.947   0.809   0.575   0.428   0.407
#% of var.             25.092  19.654  15.670  11.835  10.119   7.184   5.353   5.094
#Cumulative % of var.  25.092  44.746  60.416  72.251  82.370  89.554  94.906 100.000

plot(res_3, choix="var", title="Graph of the variables", axes=3:4)



ds_pca <- ds[,-c(1,3,4,5,6,12,13,14,18)]
??prcomp
PCA_2 <- prcomp(ds_pca, scale. = T, center = T) #principal c regression
summary(PCA_2)

#Importance of components:
#                           PC1    PC2    PC3    PC4     PC5    PC6     PC7     PC8     PC9    PC10
#Standard deviation     1.4168 1.3440 1.1199 1.1037 0.97300 0.9000 0.89199 0.75784 0.63830 0.42409
#Proportion of Variance 0.2007 0.1806 0.1254 0.1218 0.09467 0.0810 0.07956 0.05743 0.04074 0.01799
#Cumulative Proportion  0.2007 0.3814 0.5068 0.6286 0.72327 0.8043 0.88384 0.94127 0.98201 1.00000

k <- summary(PCA_2)
plot(k$importance[1,], type = 'b')
PCA_2$rotation
PCA_2$x
names(PCA_2)

biplot(PCA_2, choices = 3:4)

plot(PCA_2$x[,1:2], col=ModPag, main = 'Carte e componenti principali')
plot(PCA_2$x[,1:2], col=Arrival)
plot(PCA_2$x[,2:3], col=Arrival)
plot(PCA_2$x[,1:2], col=Departure)
legend("top", bty = "n", legend = c('Bergamo','Milan','Rome', 'Plermo','Naples','Venice'), pch = 16, 
       col = c(1,2,3,4,5,6),
       xpd = T, cex = 2, y.intersp = 0.5)
plot(PCA_2$x[,2:3], col=Departure)
plot(PCA_2$x[,1:2], col=NPax)

BiocManager::install("pcaMethods")
library(pcaMethods)
PCA_3 <- pca(ds[,-c(1,3,4,5,6,12,13,14,18)], 
                      scale = "uv", # unit variance
                      center = T, #variabili sclaate e centrate
                      nPcs = 8, 
                      method = "svd")
PCA_3
slplot(PCA_3,scoresLoadings = c(T,T))

#svd calculated PCA
#Importance of component(s):
#                 PC1    PC2    PC3    PC4     PC5     PC6     PC7     PC8
#R2            0.2007 0.1806 0.1254 0.1218 0.09467 0.08101 0.07956 0.05743
#Cumulative R2 0.2007 0.3814 0.5068 0.6286 0.72327 0.80428 0.88384 0.94127
#10 	Variables
#26002 	Samples
#0 	NAs ( 0 %)
#8 	Calculated component(s)
#Data was mean centered before running PCA 
#Data was scaled before running PCA 
#Scores structure:
#  [1] 26002     8
#Loadings structure:
#  [1] 10  8



#_______________________________________________________________
#divido ds in test e training

set.seed(5)
train=sample(nrow(ds), nrow(ds)*0.66)
test=-train
ds_train <- ds[train, ]
ds_test <- ds[test, ]
View(ds_train)
View(ds_test)

ds_pca_train <- ds_train[,-c(1,3,4,5,6,12,13,14,16,17,18)]
view(ds_pca_train)
#seleziono solo var con cui ho ottenuto risultati migliori nelle pca precedenti

#------------------------------------------------------------------------------------
#utilizzo la principal componenet regression usando come var di risposta unit_ticket(prezzo biglietto)
attach(ds_pca_train)
??pcr
pcr_model_4_train <- pcr(as.numeric(Unit_Ticket) ~ ., data = ds_pca_train, 
                 scale = TRUE, 
                 validation = "CV")
summary(pcr_model_4_train)
par(mfrow = c(1,2))
validationplot(pcr_model_4_train) #grafico
# Plot the cross validation MSE
validationplot(pcr_model_4_train, val.type="MSEP") 
# Plot the R2
validationplot(pcr_model_4_train, val.type = "R2") 
predplot(pcr_model_4_train)
coefplot(pcr_model_4_train)

#selezione var da inserire nella PCA

pcr_model_6_train <- pcr(Unit_Ticket ~ ., ncomp = 5, data = ds_pca_train, 
                         scale = TRUE, 
                         validation = "CV")
summary(pcr_model_6_train)
validationplot(pcr_model_6_train) #grafico
# Plot the cross validation MSE
validationplot(pcr_model_6_train, val.type="MSEP") 
# Plot the R2
validationplot(pcr_model_6_train, val.type = "R2") 
predplot(pcr_model_6_train)
coefplot(pcr_model_6_train)
#i valori che ottengo sono scarsi per cui Uniticket non è correlato con le altre var da relazioni abastanza forti


#--------------------------
#provare un po di linear model e poi utilizzare il boosting di tipo l2 per var quantitative di regressione

#var risposta Uni ticket


lm1 <- lm(Unit_Ticket~.,data = ds_train)
summary(lm1)
#Residual standard error: 15.05 on 17136 degrees of freedom
#Multiple R-squared:  0.1353,	Adjusted R-squared:  0.1341 
#F-statistic: 111.7 on 24 and 17136 DF,  p-value: < 2.2e-16

lm2 <- lm(Unit_Ticket~ Seat+PriorBoard+Luggage+Fidelity+Departure, data = ds_train)
summary(lm2)
#Residual standard error: 15.05 on 17151 degrees of freedom
#Multiple R-squared:  0.1345,	Adjusted R-squared:  0.134 
#F-statistic: 296.1 on 9 and 17151 DF,  p-value: < 2.2e-16


#proviamo boosting L2 selzioniamo tutte le var 

bm <- glmboost(Unit_Ticket ~ .,
                   data = ds_train,
                   control = boost_control(center = TRUE, #con questo modello seleziono tutte le var perche voglio fare var selection
                                           trace = TRUE))  #center true, ovvero centrare i dati
summary(bm)
#Final risk: 3899610 
#Call:
#  glmboost.formula(formula = Unit_Ticket ~ ., data = ds_train,     control = boost_control(center = TRUE, trace = TRUE))
#Squared Error (Regression) 
#Loss function: (y - f)^2 
#Number of boosting iterations: mstop = 100 
#Step size:  0.1 
#Offset:  139.329 

#Coefficients: 
#  (Intercept)          SeatYes    PriorBoardYes       LuggageYes         Fidelity 
#5.82470787      -4.99731120      -4.43031712      -8.31902570       0.01393430 
#Discount             NPax DepartureBergamo 
#-0.06783944       0.36463221       0.68600666 
#attr(,"offset")
#[1] 139.329

#Selection frequencies:
#  LuggageYes          SeatYes    PriorBoardYes             NPax DepartureBergamo 
#0.29             0.21             0.20             0.18             0.05 
#Fidelity         Discount 
# 0.04             0.03 
  
??AIC
mstop(aic <- AIC(bm)) #no riuscito ad eseguirlo per troppo calcolo computazionale
mstop(aic <- AIC(bm))
coef(bm[mstop(aic)])
coef(bm[mstop(aic)], off2int=TRUE)

par(mfrow=c(1,2))
plot(bm[100], off2int = TRUE,
     main = "with intercept") ## default plot, offset added to intercept
## now change ylim to the range of the coefficients without intercept (zoom-in)
preds <- names(ds_train[, names(ds_train) != "Unit_Ticket"]) ## names of predictors
plot(bm[100], ylim = range(coef(bm[100], which = preds)),
     main = "without intercept")

#previsione, #utilizzo l'mstop 80 perche il codice di sopra è impossibile da eseguire 
mse_boost_L2<-sum((ds_train$Unit_Ticket-
                     predict(bm[80]))^2)/nrow(ds_train)
mse_boost_L2
#[1] 227.7502 errore mse bf_glm[100]

mse_lm<-sum((ds_train$Unit_Ticket-
               predict(lm2))^2)/nrow(ds_train)
mse_lm
#[1] 226.2928  errore mse lm2
#errore sul trainig praticamente uguale

##test set

mse_boost_L2_test<-sum((ds_test$Unit_Ticket-
                     predict(bm[80]))^2)/nrow(ds_test)
mse_boost_L2_test
#[1] 563.4193 #mse boosting su test set leggermente migliore di quello lm

mse_lm_test<-sum((ds_test$Unit_Ticket-
               predict(lm2))^2)/nrow(ds_test)
mse_lm_test
#[1] 583.3317 #mse lm2 su test performa peggio del boostng 


mse_pcr_test<-sum((ds_test$Unit_Ticket-
                    predict(pcr_model_4_train))^2)/nrow(ds_test)
mse_pcr_test
# [1] 3600.45 errore enormemente più alto deli altri modelli


#modello boosting gam
gam1 <- gamboost(Unit_Ticket~ 
                   bols(Luggage) + bols(Seat) + bbs(NPax),
                 data = ds,
                 control = boost_control(trace = TRUE))

par(mfrow = c(1,3))
plot(gam1)

#dai modelli gam non individuo relazioni non lineari a parte un leggero effetto su npax

mse_gam1<-sum((ds_train$Unit_Ticket-
                     predict(gam1[80]))^2)/nrow(ds_train)
mse_gam1
#[1] 433.8901

mse_gam1_test<-sum((ds_test$Unit_Ticket-
                 predict(gam1[80]))^2)/nrow(ds_test)
mse_gam1_test
#[1] 846.8713
#entrambi i tassi di errore sono molto piu alti dei due precedenti modelli



