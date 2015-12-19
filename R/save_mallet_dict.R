#' Zapis_klastrów plików jednego typu Tabloid/Nie-tabloid wg expertów i wg uczestników
wynik_expert <- read.table("./dane/dane_expert/EXAMPLES_expert.txt", sep = "\t", header = T)
wynik_user <- read.table("./dane/dane_user/EXAMPLES_participants.txt", sep = ";", header = T)
wynik_expert <- wynik_expert[, 1:2]
#' Standaryzacja outputu:
etykiety <- c("huid", "Tabloid")
names(wynik_user) <- etykiety <- c("huid", "Tabloid")
names(wynik_expert) <- etykiety <- c("huid", "Tabloid")
wynik_expert$Tabloid <- ifelse(wynik_expert$Tabloid == 1, TRUE, FALSE)

#' Funkcja służąca do dodania odpowiednich tagow przy analizie textu
#'  url = ./dane/dane_expert/docs_collection_baseform2
#' lub
#' url = ./dane/dane_user/docs_collection_baseform2
#' katalog = "expert lub katalog = "user"
 
zamien_pliki <- function(wynik = wynik_expert, 
                         url = "./dane/dane_expert/docs_collection_baseform2", 
                         katalog = "expert")
  {
  for (i in 1:dim(wynik)[1]){
    file <- readLines(paste(url, "/", wynik[i,1], ".txt.bas", sep = ""))
    znacznik <- ifelse(wynik[i,2], "T", "F")
    writeLines(file, paste("./Pomocnicze/pliki_malet/", katalog, "/",znacznik, 
                           "/",  wynik[i,2], "_",wynik[i,1], ".txt", sep = ""))
  }
}

#' Dodanie tagow oraz zapis plikow do odpowiednich folderow
zamien_pliki(wynik = wynik_expert, 
             url = "./dane/dane_expert/docs_collection_baseform2", 
             katalog = "expert")
zamien_pliki(wynik = wynik_user, 
             url = "./dane/dane_user/docs_collection_baseform2", 
             katalog = "user")

#' Funkcja przygotowująca slowniki tematyczne oraz wysuje word cloud
przygotuj_slowniki <- function(katalog = "expert", znacznik = "T"){
  documents = mallet.read.dir(paste("./Pomocnicze/pliki_malet/", katalog, "/",znacznik, sep = ""))
  mallet.instances <- mallet.import(id.array = documents$id, 
                                    text.array = documents$text, stoplist.file = "./Slowniki/pl.txt",
                                    token.regexp = "[A-za-zĘÓĄŚŁŻŹĆŃęóąśłżźćń]+")
  if(katalog == "expert")
  {
    num.topics = 3
  }
  else 
  {
    num.topics = 10
  }
  
  if(znacznik == "T")
    colors = "red"
  else
    colors = "black"
  topic.model <- MalletLDA(num.topics = num.topics)
  
  topic.model$loadDocuments(mallet.instances)
  
  vocabulary <- topic.model$getVocabulary()
  word.freqs <- mallet.word.freqs(topic.model)
  
  topic.model$setAlphaOptimization(20, 50)
  
  ## Now train a model. Note that hyperparameter optimization is on, by default.
  ## We can specify the number of iterations. Here we'll use a large-ish round number.
  topic.model$train(200)
  
  ## NEW: run through a few iterations where we pick the best topic for each token,
  ## rather than sampling from the posterior distribution.
  topic.model$maximize(10)
  
  ## Get the probability of topics in documents and the probability of words in topics.
  ## By default, these functions return raw word counts. Here we want probabilities,
  ## so we normalize, and add "smoothing" so that nothing has exactly 0 probability.
  doc.topics = mallet.doc.topics(topic.model, smoothed=T, normalized=T)
  topic.words = mallet.topic.words(topic.model, smoothed=T, normalized=T)
  
  colnames(topic.words) = vocabulary
  rownames(doc.topics) = dir("corpus") 
  no.of.words <- 20
  current.topic = sort(topic.words[1,], decreasing = T)[1:no.of.words]
  df <- data.frame(slowo = names(current.topic), wartosc = as.vector(current.topic), grupa = 1)
  wordcloud(names(current.topic),current.topic, random.order = FALSE, rot.per = 0, colors= colors)
  if(katalog == "expert")
  for(i in 2:num.topics){
    current.topic = sort(topic.words[i,], decreasing = T)[1:no.of.words]
    df_tmp <- data.frame(slowo = names(current.topic), wartosc = as.vector(current.topic), grupa = i)
    df <- rbind(df, df_tmp) 
    wordcloud(names(current.topic),current.topic, random.order = FALSE, rot.per = 0, colors= colors)
  }
  df$znacznik <- znacznik
  df$katalog <- katalog
  return(df)
}

#' przygotowanie slownikow przed zapisem
slownik_user_T <- przygotuj_slowniki(katalog = "user", znacznik = "T")
slownik_expert_T <- przygotuj_slowniki(katalog = "expert", znacznik = "T")
slownik_user_F <- przygotuj_slowniki(katalog = "user", znacznik = "F")
slownik_expert_F <- przygotuj_slowniki(katalog = "expert", znacznik = "F")

zapisz_slownik <- function(slowniki = slownik_expert_T){
  ile <- dim(slowniki)[1]/20
  znacznik <- slowniki$znacznik[1]
  katalog <- slowniki$katalog[1]
  for(i in 0:(ile-1)){
    df_temp <- slowniki[(1:20)+i*20,1:2]
    write.csv(x = df_temp, file = paste("./Slowniki/Slowniki_mallet/", 
                                        znacznik,"_",
                                        katalog, "_",
                                        i, ".csv", sep = ""))
  }
}

zapisz_slownik(slownik_expert_T)
zapisz_slownik(slownik_expert_F)
zapisz_slownik(slownik_user_T)
zapisz_slownik(slownik_user_F)