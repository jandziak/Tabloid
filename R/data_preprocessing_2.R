#' @description 
#' Script to perform final elements of data prerpocessing: 
#' 
#' - binding data from different sources for train and test set
#' 
#' @author 
#' JanIdziak@gmail.com

topic <- read.csv("./data_prepared/topic_modeling.csv")
tabloid_dict <- read.csv("./data_prepared/tabloid_dict.csv")
data_labeled <- read.csv("./data_prepared/data_preprocessed1_labelled.csv")
data_nonlabeled <- read.csv("./data_prepared/data_preprocessed1_nonlabelled.csv")

data_final_label <- data_labeled %>% left_join(topic, by = "huid") %>%
  left_join(tabloid_dict, by = "huid")

data_final <- data_nonlabeled %>% left_join(topic, by = "huid") %>%
  left_join(tabloid_dict, by = "huid")

write.csv(data_final, "./data_prepared/data_final.csv", row.names = F)
write.csv(data_final_label, "./data_prepared/data_final_label.csv", row.names = F)
