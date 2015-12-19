#' Tworzenie schematu trenowania modelu
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           ## Estimate class probabilities
                           classProbs = TRUE,
                           ## Evaluate performance using 
                           ## the following function
                           summaryFunction = twoClassSummary)



set.seed(12423)

#' Wczytywanie danych oraz unifikacja nazw zmiennych
data_user_train <- read.table("data_prepared/data_data_user_train.csv", sep = ";", header = T)
data_expert_train <- read.table("data_prepared/data_data_expert_train.csv", sep = ";", header = T)
data_all <- read.table("data_prepared/data_all.csv", sep = ";", header = T)
names(data_user_train) <- gsub("_", "", names(data_user_train))
names(data_expert_train) <- gsub("_", "", names(data_expert_train))
names(data_all) <- gsub("_", "", names(data_all))

sentiment <- read.table("data_prepared/sentiment.txt", sep = ";", header = T)
stat <- read.table("data_prepared/stats.csv", sep = ",", header = T)
names(sentiment)[1]<- "huid"
names(stat)[1]<- "huid"

#' Problemy jakie moga pojawic sie z danymi jesli sie zle wczytuja
#' Dziel/!0
#' , zamiast . w csv 
str(data_user_train)
str(data_expert_train)
str(data_all)

#' Joining sentiment and segment features
data_user_train <- left_join(data_user_train, sentiment, by = "huid")
data_expert_train <-left_join(data_expert_train, sentiment, by = "huid")
data_all <- left_join(data_all, sentiment, by = "huid")

data_user_train <- left_join(data_user_train, stat, by = "huid")
data_expert_train <-left_join(data_expert_train, stat, by = "huid")
data_all <-left_join(data_all, stat, by = "huid")


data_user_train$isbulw <- factor(ifelse(data_user_train$isbulw == 1, "True","False" ))
data_expert_train$isbulw <- factor(ifelse(data_expert_train$isbulw == 1, "True","False" ))


#' Modeling of user data set:
#' Lasy losowe
rfFit <- train(isbulw ~ .-huid, 
               data = data_user_train, 
               method='rf',
               trControl = fitControl,
               ntree = 100,
               tuneGrid = data.frame(.mtry = c(4, 7, 10)))
rfFit

#' Regresja logistyczna
glmFit <- train(isbulw ~ .-huid, 
                data = data_user_train, 
                method='glm',
                trControl = fitControl)
glmFit

#' Liniowa analiza dyskryminacyjna
ldaFit <- train(isbulw ~ .-huid, 
                data = data_user_train, 
                method='lda',
                trControl = fitControl)
ldaFit

#' Knn
knnFit <- train(isbulw ~ .-huid, 
                data = data_user_train, 
                method='knn',
                trControl = fitControl)
knnFit

predict(ldaFit, type = "prob")

pred_glm <- predict(glmFit, newdata = data_all, type = "prob")
pred_lda <- predict(ldaFit, newdata = data_all, type = "prob")
pred_knn <- predict(knnFit, newdata = data_all, type = "prob")
pred_rf <- predict(rfFit, newdata = data_all, type = "prob")


#' Predykcja prawdopodobieństw dla poszczególnych modeli
preds <- cbind(huid = data_all$huid, 
               rf = pred_rf$True, 
               knn = pred_knn$True, 
               lda = pred_lda$True, 
               glm = pred_glm$True)


write.table(preds, file = "predykcja_prawdopodobienstwa.csv", sep = "\t")

