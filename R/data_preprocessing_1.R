
#' @description 
#' Script to perform several elements of data prerpocessing: 
#' - create data features based on punctuation stats
#' - create data features based on speech parts type
#' - create data features based on number of syllabes in word
#' - merge these features into data set
#' - merge response data (based on both expert and users data)
#' 
#' @author 
#' martakarass@gmail.com

# install.packages('devtools')
# install.packages('koRpus')
# install.packages('rJava')
# install.packages('data.table')
# install.packages('bit64')


library(koRpus)
library(rJava)
library(data.table)
library(bit64)
library(dplyr)


# Define path for directries with data 
termstats_PATH    <- './dane/dane_expert/ARTICLES_termstats.dat'
metadata_PATH     <- './dane/dane_expert/ARTICLES_metadata.dat'
interpunkcja_PATH <- './dane/pochodne/interpunkcja.csv'
sylaby_PATH       <- './dane/pochodne/sylaby.csv'

EXAMPLES_expert_PATH <- './dane/dane_expert/EXAMPLES_expert.txt'
EXAMPLES_participants_PATH <- './dane/dane_user/EXAMPLES_participants.txt'

data_preprocessed1_learning_PATH <- './data_prepared/data_preprocessed1_learning.csv'
data_preprocessed1_labelled_PATH <- './data_prepared/data_preprocessed1_labelled.csv'




# -------------------------------------------------------------------------------------------------------
###############################
#####     TERMSTATS ###########
###############################


# http://nkjp.pl/poliqarp/help/ense2.html

# Rzeczownik: subst, depr, noun, ger, ppron12, ppron3, siebie
# Przymiotnik: adj, adja, adjp
# Przysłówek: adv, pred, qub (dokładniej: przysłowek niestopniowalny i partykuły)
# Liczebnik: num, numcol
# Zaimek: ppron12 (osobowy 1 i 2 osoba), ppron3 (3 osoba), siebie, ppron
# Czasownik: fin, bedzie, aglt, praet, impt, imps, inf, pcon, pant, ger, pact, ppas,
# winien, verb
# Przyimek: prep
# Spójnik: conj
# Interpunkcja: interp

termstats_DATA_base = as.data.frame(fread(termstats_PATH))
names(termstats_DATA_base) = c('huid', 'fleksem', 'pct', 'total_cnt')

fleksem_to_speech_part <- function(fleksem){
  
  if(fleksem %in% c('subst', 'depr', 'noun', 'ger', 'ppron12', 'ppron3', 'siebie')){
    return('rzeczownik')
    
  } else if(fleksem %in% c('adj', 'adja', 'adjp')){
    return('przymiotnik')
    
  } else if(fleksem %in% c('adv', 'pred', 'qub')){
    return('przyslowek')
    
  } else if(fleksem %in% c('num', 'numcol')){
    return('liczebnik')
    
  } else if(fleksem %in% c('ppron12', 'ppron3', 'siebie', 'ppron')){
    return('zaimek')
    
  } else if(fleksem %in% c('fin', 'bedzie', 'aglt', 'praet', 'impt', 'imps', 'inf', 
                           'pcon', 'pant', 'ger', 'pact', 'ppas', 'winien', 'verb')){
    return('czasownik')
    
  } else if(fleksem %in% c('prep')){
    return('przyimek')
    
  } else if(fleksem %in% c('conj')){
    return('spojnik')
    
  } else if(fleksem %in% c('interp')){
    return('interpunkcja')
    
  } else  {
    return('nierozpoznane')
  }
}

# Convert fleksems into speech parts 
termstats_DATA_base$speech_part <- sapply(termstats_DATA_base$fleksem, fleksem_to_speech_part)
table(termstats_DATA_base$speech_part)

# Define count 
termstats_DATA_base <- termstats_DATA_base %>% mutate(fleksem_cnt = ceiling(pct*total_cnt),
                                                      speech_part_cnt = ceiling(pct*total_cnt))

# Fleksem df 
termstats_DATA_fleksem <- termstats_DATA_base[, c('huid', 'fleksem', 'fleksem_cnt')]
termstats_DATA_fleksem_g <- 
  termstats_DATA_fleksem %>% 
  group_by(huid, fleksem) %>% 
  summarize(fleksem_cnt = sum(fleksem_cnt))
termstats_DATA_fleksem_g <- as.data.frame(termstats_DATA_fleksem_g)

# Melt data frame 
termstats_DATA_fleksem_g_res <- dcast(termstats_DATA_fleksem_g,  huid ~ fleksem, 
                                        value.var="fleksem_cnt", drop = FALSE, fill = 0) 

str(termstats_DATA_fleksem_g_res)
# 'data.frame':	47539 obs. of  36 variables:


# Speach part df 
termstats_DATA_speech_part <- termstats_DATA_base[, c('huid', 'speech_part', 'speech_part_cnt')]
termstats_DATA_speech_part_g <- 
  termstats_DATA_speech_part %>% 
  group_by(huid, speech_part) %>% 
  summarize(speech_part_cnt = sum(speech_part_cnt))
termstats_DATA_speech_part_g <- as.data.frame(termstats_DATA_speech_part_g)

termstats_DATA_speech_part_res <- dcast(termstats_DATA_speech_part_g,  huid ~ speech_part, 
                                        value.var="speech_part_cnt", drop = FALSE, fill = 0) 
# 'data.frame':	47539 obs. of  10 variables:


# total_counts 
termstats_DATA_total_cnt <- termstats_DATA_base[, c('huid', 'total_cnt')] %>% distinct()
termstats_DATA_total_cnt_res <- as.data.frame(termstats_DATA_total_cnt)


# termstats_DATA_total_cnt SUMMARY
termstats_DATA_fleksem_g_res$huid <- as.character(termstats_DATA_fleksem_g_res$huid)
termstats_DATA_speech_part_res$huid <- as.character(termstats_DATA_speech_part_res$huid)
termstats_DATA_total_cnt_res$huid <- as.character(termstats_DATA_total_cnt_res$huid)



termstats_DATA <- termstats_DATA_fleksem_g_res %>% 
  left_join(termstats_DATA_speech_part_res, by = 'huid') %>%
  left_join(termstats_DATA_total_cnt_res, by = 'huid')

# names(termstats_DATA)
# [1] "huid"          "adj"           "adja"          "adjp"         
# [5] "adv"           "aglt"          "bedzie"        "conj"         
# [9] "depr"          "fin"           "ger"           "ign"          
# [13] "imps"          "impt"          "inf"           "interp"       
# [17] "num"           "pact"          "pant"          "pcon"         
# [21] "ppas"          "ppron12"       "ppron3"        "praet"        
# [25] "pred"          "prep"          "qub"           "siebie"       
# [29] "subst"         "tdate"         "tmail"         "tnum"         
# [33] "tsym"          "ttime"         "turi"          "winien"       
# [37] "czasownik"     "interpunkcja"  "liczebnik"     "nierozpoznane"
# [41] "przyimek"      "przymiotnik"   "przyslowek"    "rzeczownik"   
# [45] "spojnik"       "total_cnt"  




collumns_sub <- c("czasownik"  ,   "interpunkcja" , "liczebnik"    , "nierozpoznane", 
                  "przyimek"   ,   "przymiotnik"  , "przyslowek" ,   "rzeczownik" ,
                  "spojnik"    )

termstats_DATA_RES <- termstats_DATA[, c("huid", "total_cnt")]

# Add fraction information
for( nom_col in collumns_sub){
  termstats_DATA_RES[paste0(nom_col, "_FRAC")] = termstats_DATA[nom_col] / termstats_DATA["total_cnt"]
}

head(termstats_DATA_RES)
# huid total_cnt czasownik_FRAC interpunkcja_FRAC liczebnik_FRAC
# 1 1852551900       126      0.3333333         0.3968254     0.00000000
# 2 1852667650       104      0.3269231         0.4423077     0.00000000
# 3 1852804630       609      0.2857143         0.2660099     0.00000000
# 4 1852838251       250      0.2800000         0.4000000     0.04800000
# 5 1852915256       506      0.3636364         0.3359684     0.01976285
# 6 1852983174       176      0.2954545         0.3977273     0.03409091
# nierozpoznane_FRAC przyimek_FRAC przymiotnik_FRAC przyslowek_FRAC
# 1         0.11111111     0.1428571        0.1904762       0.1269841
# 2         0.03846154     0.2884615        0.2500000       0.1346154
# 3         0.01970443     0.2528736        0.1970443       0.1018062
# 4         0.02400000     0.2320000        0.1680000       0.1280000
# 5         0.05138340     0.1897233        0.1818182       0.2490119
# 6         0.02272727     0.1477273        0.1818182       0.1818182
# rzeczownik_FRAC spojnik_FRAC
# 1       0.7936508   0.06349206
# 2       0.6153846   0.05769231
# 3       0.7980296   0.10837438
# 4       0.6080000   0.11200000
# 5       0.5454545   0.11857708
# 6       0.6590909   0.15909091


# -------------------------------------------------------------------------------------------------------
###############################
#####     METADANA  ###########
###############################


metadata_DATA_base = as.data.frame(fread(metadata_PATH))
metadata_DATA_base = metadata_DATA_base[, c("uuid_h2", "fakt", "pap", "image", "mediagallery")]
names(metadata_DATA_base) <- c("huid"  ,    "fakt"     ,    "pap"     ,     "image"   ,  "mediagallery")
metadata_DATA_base$huid <- as.character(metadata_DATA_base$huid)





# -------------------------------------------------------------------------------------------------------
###############################
#####     INTERPUNKCJA  #######
###############################


interpunkcja_BASE = as.data.frame(fread(interpunkcja_PATH))
names(interpunkcja_BASE) <- c("huid", "przecinek", "srednik", "dwukropek", "wykrzyknik", "znakzapytania",
                              "kropka", "cudzyslow", "trzykropek")
interpunkcja_BASE$huid <- as.character(interpunkcja_BASE$huid)


interpunkcja_RES <- termstats_DATA[, c("huid", "total_cnt")]
interpunkcja_l <- interpunkcja_RES %>% left_join(interpunkcja_BASE, by = "huid")


# Add fraction information
for( nom_col in c("przecinek", "srednik", "dwukropek", "wykrzyknik", "znakzapytania",
                  "kropka", "cudzyslow", "trzykropek")){
  interpunkcja_RES[paste0(nom_col, "_FRAC")] = interpunkcja_l[nom_col] / interpunkcja_l["total_cnt"]
}

if("total_cnt" %in% names(interpunkcja_RES)){
  interpunkcja_RES <- interpunkcja_RES[, -which(names(interpunkcja_RES) == "total_cnt")]
}

head(interpunkcja_RES)






# -------------------------------------------------------------------------------------------------------
#############################
#####     SYLABY      #######
#############################


sylaby_BASE = as.data.frame(fread(sylaby_PATH))
names(sylaby_BASE) <- c("huid", "syl_1", "syl_2", "syl_3", "syl_4", "syl_5", "syl_6andmore")
sylaby_BASE <- sylaby_BASE %>% mutate(syl_more = syl_5 + syl_6andmore)

if("syl_6andmore" %in% names(sylaby_BASE)){
  sylaby_BASE <- sylaby_BASE[, -which(names(sylaby_BASE) %in% c('syl_6andmore', 'syl_5') )]
}

names(sylaby_BASE)
sylaby_BASE$huid <- as.character(sylaby_BASE$huid)



sylaby_RES <- termstats_DATA[, c("huid", "total_cnt")]
sylaby_l <- sylaby_RES %>% left_join(sylaby_BASE, by = "huid")


# Add fraction information
for( nom_col in c("syl_1"  ,  "syl_2"   , "syl_3"   ,
                  "syl_4"  ,  "syl_more")){
  sylaby_RES[paste0(nom_col, "_FRAC")] = sylaby_l[nom_col] / sylaby_l["total_cnt"]
}


if("total_cnt" %in% names(sylaby_RES)){
  sylaby_RES <- sylaby_RES[, -which(names(sylaby_RES) == "total_cnt")]
}

head(sylaby_RES)





# # -------------------------------------------------------------------------------------------------------
# ###################################
# #####     STATSY SŁÓW       #######
# ###################################
# 
# stats_tabloid_nietabloid_DATA = as.data.frame(fread("/home/marta/Documents/erka/statystyki_macka - statystyki_macka.csv"))
# 
# stats_tabloid_nietabloid_DATA <- read.csv(stats_tabloid_nietabloid_PATH, sep = ";", header = FALSE, colClasses = c("character", "numeric", "numeric"))
# 
# names(stats_tabloid_nietabloid_DATA) <- c("huid", "words_tabloid", "words_nietabloid")
# 
# stats_tabloid_nietabloid_DATA$words_tabloid <- 
#   as.numeric(stats_tabloid_nietabloid_DATA$words_tabloid)
# 
# stats_tabloid_nietabloid_DATA$Tabloid <- sapply(stats_tabloid_nietabloid_DATA$Tabloid, function(val){
#   gsub(',', '.', val)
# })
# stats_tabloid_nietabloid_DATA$Nietabloid <- sapply(stats_tabloid_nietabloid_DATA$Nietabloid, function(val){
#   gsub(',', '.', val)
# })
# 
# stats_tabloid_nietabloid_DATA$Tabloid <- as.numeric(stats_tabloid_nietabloid_DATA$Tabloid)
# stats_tabloid_nietabloid_DATA$Nietabloid <- as.numeric(stats_tabloid_nietabloid_DATA$Nietabloid)
# 
# all(is.numeric(stats_tabloid_nietabloid_DATA$Nietabloid))
# all(is.numeric(stats_tabloid_nietabloid_DATA$Tabloid))
# 
# which(is.na(stats_tabloid_nietabloid_DATA$Nietabloid))




# -------------------------------------------------------------------------------------------------------
### JOIN IT ALL

# Learning data sample
data_sample <- termstats_DATA_RES %>% 
  left_join(metadata_DATA_base, by = 'huid') %>%
  left_join(interpunkcja_RES, by = 'huid') %>%
  left_join(sylaby_RES, by = 'huid')

write.csv(data_sample, file = data_preprocessed1_learning_PATH)



# Add response 

# PARTICIPANTS

EXAMPLES_participants_DATA <- as.data.frame(fread(EXAMPLES_participants_PATH))
EXAMPLES_participants_DATA$is_bulw <- sapply(EXAMPLES_participants_DATA$Value, function(val){
  if(val == "FALSE"){
    return(0)
  } else {
    return(1)
  }
})

EXAMPLES_participants_DATA <- EXAMPLES_participants_DATA[, c('uuid_h2' , 'is_bulw')]
names(EXAMPLES_participants_DATA) <- c('huid', 'is_bulw')
EXAMPLES_participants_DATA$huid <- as.character(EXAMPLES_participants_DATA$huid)

EXAMPLES_partic <- EXAMPLES_participants_DATA %>% 
  group_by(huid) %>%
  summarize(is_bulw = mean(is_bulw)) %>%
  filter(is_bulw != 0.5) %>%
  mutate(is_bulw = round(is_bulw))

EXAMPLES_partic <- as.data.frame(EXAMPLES_partic)
EXAMPLES_partic$huid <- as.character(EXAMPLES_partic$huid)



# EXPERTS 

EXAMPLES_expert_DATA <- as.data.frame(fread(EXAMPLES_expert_PATH))
EXAMPLES_expert_DATA <- EXAMPLES_expert_DATA[, c(1,2)]
names(EXAMPLES_expert_DATA) <- c('huid', 'is_bulw' )
EXAMPLES_expert_DATA$huid <- as.character(EXAMPLES_expert_DATA$huid)

EXAMPLES_RES <- rbind(EXAMPLES_partic, 
                      EXAMPLES_expert_DATA)

table(EXAMPLES_RES$is_bulw)

EXAMPLES_RES <- EXAMPLES_RES %>% 
  group_by(huid) %>%
  summarize(is_bulw = mean(is_bulw)) %>%
  filter(is_bulw != 0.5) %>%
  mutate(is_bulw = round(is_bulw))

res_sample <- EXAMPLES_RES
table(res_sample$is_bulw)


# Build learn sample
LEARN_SAMPLE <- EXAMPLES_RES %>% left_join(data_sample, by = 'huid')

class_name_vec <- c("czasownik_FRAC","interpunkcja_FRAC","liczebnik_FRAC","nierozpoznane_FRAC",
                    "przyimek_FRAC","przymiotnik_FRAC","przyslowek_FRAC","rzeczownik_FRAC","spojnik_FRAC",
                    "fakt","pap","image","mediagallery","przecinek_FRAC","srednik_FRAC","dwukropek_FRAC",
                    "wykrzyknik_FRAC","znakzapytania_FRAC","kropka_FRAC","cudzyslow_FRAC", "trzykropek_FRAC",
                    "syl_1_FRAC","syl_2_FRAC","syl_3_FRAC","syl_4_FRAC","syl_more_FRAC")

for(class_name in class_name_vec){
  
  res <- unlist(LEARN_SAMPLE[, "is_bulw"])
  class <- unlist(LEARN_SAMPLE[, class_name])
  
  print(boxplot(class~res, main=paste0(class_name, "\nw podziale na BULWAROWOŚĆ")))
  
}

LEARN_SAMPLE_RES <- as.data.frame(LEARN_SAMPLE)


# Save to file 
write.csv(LEARN_SAMPLE_RES, file = data_preprocessed1_labelled_PATH, fileEncoding = "UTF-8")

