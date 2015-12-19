#' @description
#' Project configuration file.
#' 
#' Contains: 
#'  - setting of working directory (depenfing on user's comupter name)
#'  - loading all libraries used in the project
#'  - sourcing of all util scipts used in project


# ------------------------------------------------------------------------------
# Set working directory

computer.name <- Sys.info()["nodename"] 

if (computer.name == "ojojoj") 
  wd.tmp <- "/home/marta/Documents/Tabloid"
if (computer.name == "JANUSZ"){
  wd.tmp <- "C:/Repository/Onet_wyjsciowy/Tabloid"
  Sys.setenv(JAVA_HOME='C:/Program Files (x86)/Java/jre1.8.0_66/')
}

setwd(wd.tmp)
message(getwd())

# ------------------------------------------------------------------------------
# Load libraries

library(corrgram)
library(ggplot2)
require(stringr)
library(caret)
library(pROC)
library(smbinning)
library(reshape)
library(dplyr)
library(MASS)
library(pander)
# to install under Ubuntu, follow: http://stackoverflow.com/questions/13403268/error-while-loading-rjava
library(rJava)
library(RWekajars)
library(RWeka)
library(FSelector)
library(fpc)
library(cluster)
library(mlbench)
library(stylo)
library(mallet)
library(wordcloud)
library(xml2)
library(Rcpp)
library(devtools)
library(ngram)
library(randomForest)
library(koRpus)
library(rJava)
library(data.table)
library(bit64)
library(dplyr)
# 
# install.packages('corrgram')
# install.packages('ggplot2')
# install.packages('stringr')
# install.packages('caret')
# install.packages('pROC')
# install.packages('smbinning')
# install.packages('reshape')
# install.packages('dplyr')
# install.packages('MASS')
# install.packages('pander')
# # to install under Ubuntu, follow: http://stackoverflow.com/questions/13403268/error-while-loading-rjava
# install.packages('rJava')
# install.packages('RWekajars')
# install.packages('RWeka')
# install.packages('FSelector')
# install.packages('fpc')
# install.packages('cluster')
# install.packages('mlbench')
# install.packages('stylo')
# install.packages('mallet')
# install.packages('wordcloud')
# install.packages('xml2')
# install.packages('Rcpp')
# install.packages('devtools')
# install.packages('ngram')
# install.packages('randomForest')
# install.packages('devtools')
# install.packages('koRpus')
# install.packages('rJava')
# install.packages('data.table')
# install.packages('bit64')