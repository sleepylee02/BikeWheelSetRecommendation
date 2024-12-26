options(scipen = 999)

list.of.packages <- c("tidyverse", "data.table","tidytext","dplyr","rvest", "quanteda", 
                      "stringr","readtext","RCurl","tm", "openxlsx")
new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dependencies = T)

require(tidyverse)
require(data.table)
require(tidytext)
require(dplyr)
require(rvest)
require(quanteda, warn.conflicts = FALSE, quietly = TRUE)
require(stringr)
require(readtext)
require(RCurl)
require(tm)
require(openxlsx)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

wheelset <- read.csv("wheelset.csv") # opening handmade csv which includes informations on wheelset
wheelset_annotated <- read.csv("wheelset_annotated.csv") 
wheelset <- data.frame(wheelset)
wheelset

wheelset_annotated <- data.frame(wheelset_annotated)
wheelset_annotated

wheelset_name <- list(paste(unlist(wheelset_annotated[,1]), unlist(wheelset_annotated[,2])))
wheelset_name

wheelset %>% group_by(brand) %>% count  # number of wheel per brand

wheel_factor <- list("강성", "에어로", "항속성")
wheel_factor

# web scrapping
search_html_list = list()
search_wheel_list = list()

for (i in wheelset_name[[1]]){
  search = gsub(" ", "+", i)
  search_wheel_list <- append(search_wheel_list, i) 
  search_html_list[[i]] = c()
  for (j in 1:10){
    search_html = paste0("https://corearoadbike.com/board/board.php?t_id=Menu03Top1&sch_W=title_content&sch_O=AND&sch_T=", 
                         search,"&page=", as.character(j))
    search_html_list[[i]] <- append(search_html_list[[i]], search_html)
  }
}

search_html_list
search_wheel_list

# this is a custom function that i got from internet to prevent disconnection in "for statement"
# bc of the network error or by the restriction in internet
# so, to summarize this function when error "empty reply from server" happens it whats for 1 min and 
# after 1 min it tries read_html again ; it repeats this for 5 times and after that it stops
# also if other error happens it also stops

retry_connection <- function(url, max_attempts = 5, wait_time = 60) {
  attempt <- 1
  success <- FALSE
  result <- NULL
  
  while(attempt <= max_attempts & !success) {
    tryCatch({ # tryCatch is a function that deals with exceptions
      result <- read_html(url)  
      success <- TRUE
    }, error = function(e) {
      if (grepl("Empty reply from server", e$message)) { # grepl is a function that returns logical after grep
        message(paste("Attempt", attempt, "failed: Empty reply from server. Retrying in", wait_time, "seconds..."))
        Sys.sleep(wait_time)
        attempt <- attempt + 1
      } else {
        stop(e)  # Stop if it's a different error
      }
    })
  }
  
  if (!success) {
    stop("Failed to connect after", max_attempts, "attempts.")
  }
  
  return(result)
}

# Scrapping list
search_html_final_list = list()
count_name = 0
for (i in wheelset_name[[1]]){
  search_html_final_list[[i]] = c()
  count_name = count_name + 1
  search_specific <- search_html_list[[count_name]]
  for (j in search_specific) {
    search_webpage <- retry_connection(j)
    temp <- search_webpage%>%
      html_nodes("a") %>% html_attr("href")
    temp <- temp[grep("no=",temp)]
    search_html_final_list[[i]] <- append(search_html_final_list[[i]],temp)
  }
  print(count_name)
}

str(search_html_final_list,1) # nicely done!!!
search_html_final_list

search_html_final_list <- lapply(search_html_final_list , function(x) sub(".", "https://corearoadbike.com/board", x))

count_name2 = 0
search_html_export_df = data.frame()
for (i in wheelset_name[[1]]){
  count_name2 <- count_name2 + 1
  temp_df <- data.frame(search_html_final_list[[count_name2]])
  temp_df <- temp_df %>% cbind(data.frame(rep(c(i),nrow(temp_df))))
  search_html_export_df <- search_html_export_df %>% rbind(temp_df)
}

#### exporting csv
#View(search_html_export_df)
head(search_html_export_df)

colnames(search_html_export_df) <- c("html", "wheel")
write.csv(search_html_export_df, file = "search_html_export_df.csv",fileEncoding = "UTF-8")


#### reading csv

search_html_imported <- read.csv("search_html_export_df.csv")

head(search_html_imported)
class(search_html_imported)
#View(search_html_imported)


to.sentence <- function(x){ # function to make text in html to sentences
  x <- retry_connection(x)
  content_html <- x %>% 
    html_elements("td.view_content") %>% html_text2() 
  content_html <- gsub("\n",".", content_html)
  content_html <- content_html %>% tokens(what = "sentence")
  # content_html <- gsub(stopword_kor, "",content_html) %>% corpus()
  
  coment_html <- x %>%
    html_elements("td.view_coment") %>% html_text2() 
  coment_html <- gsub("\n",".", coment_html)
  coment_html <- coment_html %>% tokens(what = "sentence")
  #coment_html <- gsub(stopword_kor, "",coment_html) %>% corpus()
  
  content_sentence <- content_html
  coment_sentence <- coment_html 
  
  # used make.unique to combine different token from same document
  names(content_sentence) <- make.unique(rep("content", length(content_sentence))) 
  names(coment_sentence) <- make.unique(rep("comment", length(coment_sentence)))
  
  combined_sentence <- c(content_sentence, coment_sentence)
  return(combined_sentence)
  Sys.sleep(3) 
}

search_sentence = list()
for (i in search_wheel_list){
  specific_wheel <- search_html_imported %>% filter(wheel == i) %>% select(html)
  wheel_html <- lapply(unlist(specific_wheel$html), to.sentence)
  search_sentence[[i]] <- append(search_sentence[[i]], wheel_html)
  print(i)
  print(length(wheel_html))
}

View(search_sentence)
class(search_sentence)
str(search_sentence,1)
length(search_sentence)

# unlisting the tokens into the text and then make it as a dataframe
search_sentence_df = data.frame()
for (i in wheelset_name[[1]]){
  temp_wheel <- sprintf('search_sentence$"%s"',i)
  for (j in 1:length(eval(parse(text = temp_wheel)))){
    temp_wheel_2 <- paste0(sprintf('search_sentence$"%s"',i),"[",j,"]")
    temp_token <- lapply((eval(parse(text = temp_wheel_2))), paste, collaspe= " " )

    # since I needed to call the items using $ i found a function
    # eval(parse(text=)) in internet to call the text inside the str to be used as a function 
    temp_token_df <- cbind(data.frame(temp_token), rep(c(i),length(temp_token)))
    colnames(temp_token_df) <- c("sentence","wheel")
    search_sentence_df <- rbind(search_sentence_df,temp_token_df)
  }
  
}

View(search_sentence_df)
head(search_sentence_df)
dim(search_sentence_df)

#### exporting csv
#View(search_html_export_df)
head(search_sentence_df)
write.csv(search_sentence_df, file = "search_sentence_df.csv",fileEncoding = "UTF-8")


#### reading csv
search_sentence_imported <- read.csv("search_sentence_df.csv")

head(search_sentence_imported)
class(search_sentence_imported)
View(search_sentence_imported)


# the korean sentiment anyalizer from KNU
senti_dic <- read_tsv("SentiWord_Dict.txt")
class(senti_dic)
#View(senti_dic)

colnames(senti_dic) = c("word", "sScore")
head(senti_dic)
nrow(senti_dic)

 senti_dic <- senti_dic %>% mutate(emotion = "")

senti_dic[senti_dic$sScore >=1,]$emotion <- "positive"
senti_dic[senti_dic$sScore == 0,]$emotion <- "neutral"
senti_dic[senti_dic$sScore <= -1,]$emotion <- "negative"

head(senti_dic)
senti_dic %>% group_by(emotion) %>% count


# testing on broad emotion of the wheelset
emotion_list <- list()
detailed_df <- data.frame()

for (i in wheelset_name[[1]]){
  temp_sentence_df <- search_sentence_imported %>% filter(wheel == i)
  temp_evaluate_df <- data.frame()
  for (j in 1:nrow(temp_sentence_df)){
    temp_unlist <- unlist(temp_sentence_df$sentence[j])
    temp_tibble <- tibble(temp_unlist) %>% 
      unnest_tokens(input = temp_unlist,output = "word") %>% left_join(senti_dic) 
    
    temp_tibble <- temp_tibble %>% mutate(sScore = ifelse(is.na(sScore), 0, sScore))
    
    temp_evaluate_df <- rbind(temp_evaluate_df, temp_tibble)
  }
  
  detailed_df <- rbind(detailed_df, temp_evaluate_df)
  emotion_list[[i]] = sum(temp_evaluate_df$sScore)
  print(i)
}  

emotion_df <- data.frame(t(data.frame(emotion_list)))
colnames(emotion_df) <- c("emotion")
#View(emotion_df)

emotion_df$wheel <- wheelset_name[[1]]
emotion_df

sentence_per_wheel_kor <- search_sentence_imported %>% group_by(wheel) %>% count
sentence_per_wheel_kor
#emotion_df
emotion_df <- emotion_df %>% inner_join(sentence_per_wheel_kor, by = join_by(wheel))
emotion_df$sent_score <- round(emotion_df$emotion / emotion_df$n * 10, 2)
emotion_df

View(detailed_df)
(nrow(detailed_df) - sum(is.na(detailed_df$emotion))) / nrow(detailed_df) * 100


write.csv(detailed_df, file = "detailed_df_imported.csv",fileEncoding = "UTF-8")
write.csv(emotion_df, file = "emotion_df_imported.csv",fileEncoding = "UTF-8")

detailed_df_imported <- read.csv("detailed_df_imported.csv")
emotion_df_imported <- read.csv("emotion_df_imported.csv")


# hypo 1 : try a new way of matching the words from the knu dict
# actually the way I tried earlier was the way the knu dict introduced to use as 
# make function that does not seperate the sentence in to words
# it rather analyze the word matching if there is word from senti_dic in the sentence.

counting_sentence <- function(sent, word = senti_dic$word){
  counting_list <- lapply(word, function(x) str_count(sent, paste0("\\b", str_escape(x), "\\b")) )
  counting_df <- data.frame(t(data.frame(counting_list)))
  counting_df <- cbind(senti_dic, counting_df)
  colnames(counting_df) <- c("word","sScore","emotion", "counts")
  print(sent)
  return(sum(counting_df$sScore * counting_df$counts))
}

View(search_sentence_imported)

# lapply 로 3000개 돌리는데 40분정도 걸렸음....
# 현재 데이터가 9만개가 넘으므로 1대1 comparison은 어려울 것 같고 휠 두가지만 골라서 분석해보자

View(search_sentence_imported %>% group_by(wheel) %>% count)
# 아비아브 에어로엑스 ; 시마노 c60
View(detailed_df_hypo1 %>% filter(wheel %in% c("아비아브 에어로엑스", "시마노 c60"))) 



detailed_df_hypo1 <- search_sentence_imported %>% filter(wheel %in% c("아비아브 에어로엑스", "시마노 c60")) 
emotion_df_hypo1 <- lapply(detailed_df_hypo1$sentence, counting_sentence)
emotion_df_hypo1 <- data.frame(t(data.frame(emotion_df_hypo1)))
colnames(emotion_df_hypo1) <- "emotion"

#### exporting csv
head(emotion_df_hypo1)
write.csv(emotion_df_hypo1, file = "emotion_df_hypo1.csv",fileEncoding = "UTF-8")
 

#### reading csv

emotion_df_hypo1_imported <- read.csv("emotion_df_hypo1.csv")
#View(emotion_df_hypo1_imported)

emotion_df_hypo1_imported$X <- NULL


# Handle data to get the sent_score
emotion_df_hypo1_imported <- emotion_df_hypo1_imported %>% cbind(search_sentence_imported %>% 
                                                 filter(wheel %in% c("아비아브 에어로엑스", "시마노 c60")))
emotion_df_hypo1_imported <- emotion_df_hypo1_imported %>% group_by(wheel) %>% 
  summarize(sum(emotion)) %>% left_join(sentence_per_wheel_kor, by=join_by(wheel))

colnames(emotion_df_hypo1_imported) <- c("wheel","emotion","n")

emotion_df_hypo1_imported$sent_score <- round(emotion_df_hypo1_imported$emotion / emotion_df_hypo1_imported$n * 10, 2)

View(emotion_df_hypo1_imported)
View(emotion_df_imported %>% filter(wheel %in% c("아비아브 에어로엑스","시마노 c60")))
# if we compare hypo0 and hypo1 we conclude that there is no siginificant difference between two
# since, no siginificant difference we reject hypo1


#hypo2 : translate and than use quanteda package

# translate the text using google translator
# actually api on google translator and papago needs to be paid...
# using package to translate was not an exciting experience. so, I just made the original file as a excel

# But I encountered error saying that the file might be corrupted
# so, I divided the file into ten pieces but still the file from 30001:40000 was called corrupted
# after rigorous repetition I found out that there was a table inside 3 post that I web scrapped
# that was causing all the problem translating so, I decided to make a new data set excluding such data.

# when you just want to make various df it might be better to just handcode
# than using iteration technique
search_sentence_translate_1 <- search_sentence_imported[1:10000,]
search_sentence_translate_2 <- search_sentence_imported[10001:20000,]
search_sentence_translate_3 <- search_sentence_imported[20001:30000,]

search_sentence_translate_4_1 <- search_sentence_imported[30001:30646,]
search_sentence_translate_4_2 <- search_sentence_imported[31225:35143,]
search_sentence_translate_4_3 <- search_sentence_imported[35576:40000,]

search_sentence_translate_5 <- search_sentence_imported[40001:50000,]
search_sentence_translate_6 <- search_sentence_imported[50001:60000,]
search_sentence_translate_7 <- search_sentence_imported[60001:70000,]
search_sentence_translate_8 <- search_sentence_imported[70001:80000,]
search_sentence_translate_9 <- search_sentence_imported[80001:90000,]
search_sentence_translate_10 <- search_sentence_imported[90001: nrow(search_sentence_imported),]


# export all files to translate it
for (i in c(1,2,3,"4_1","4_2","4_3",5,6,7,8,9,10)){
  print(i)
  temp_translate_name <- paste0("search_sentence_translate_",i)
  write.xlsx(eval(parse(text = temp_translate_name)), file = paste0(temp_translate_name,".xlsx"))
}

# read the translated file as one data frame
translated_sentence_imported <- data.frame()
for (i in c(1,2,3,"4_1","4_2","4_3",5,6,7,8,9,10)){
  temp_translated_df <- read.xlsx(paste0("translated_sentence_", i , ".xlsx"))
  translated_sentence_imported <- rbind(translated_sentence_imported, temp_translated_df)
}

View(translated_sentence_imported) 


# start sentiment analysis using quanteda package
translated_corpus <- corpus(translated_sentence_imported$sentence)

translated_sentiment <-
  translated_corpus %>%
  tokens(remove_punct = T,
         remove_symbols = T) %>%
  tokens_remove(stopwords("english")) %>%
  tokens_lookup(dictionary = data_dictionary_LSD2015) %>%
  dfm

translated_sentiment_df <- convert(translated_sentiment, to = "data.frame")
translated_sentiment_df

# finalize the sentiment score
translated_sentiment_df$sent_score <- 
  (translated_sentiment_df[,3] + translated_sentiment_df[,5]) -
    (translated_sentiment_df[,2] + translated_sentiment_df[,4])

sentence_per_wheel_eng <- translated_sentence_imported %>% group_by(wheel) %>% count
sentence_per_wheel_eng
 
 
translated_sentiment_final <- cbind(translated_sentence_imported, translated_sentiment_df)

translated_sentiment_score <- translated_sentiment_final %>% group_by(wheel) %>% summarise(sent_score = sum(sent_score)) 
translated_sentiment_score 
translated_sentiment_score$sent_score <- round((translated_sentiment_score$sent_score / 
                                                  sentence_per_wheel_eng$n) * 10, 2)
View(translated_sentiment_score)

translated_sentiment_score




# compare it with hypo0

# lets unlist the sentence into words
imported_sentence_df <- (data.frame(translated_sentence_imported$sentence))
View(imported_sentence_df)
colnames(imported_sentence_df) <- "sentence"

compare_translated_word <- data.frame()
for (i in nrow(imported_sentence_df)){
  temp_compare <-  imported_sentence_df %>% 
    unnest_tokens(word,sentence)
  compare_translated_word <- rbind(compare_translated_word, temp_compare)
}
compare_translated_word
View(compare_translated_word)
compare_translated_corpus <- corpus(compare_translated_word$word)
compare_translated_sentiment <- compare_translated_corpus  %>%
  tokens(remove_punct = T,
         remove_symbols = T) %>%
  tokens_remove(stopwords("english")) %>%
  tokens_lookup(dictionary = data_dictionary_LSD2015) %>%
  dfm

compare_translated_sentiment_df <- (convert(compare_translated_sentiment, to = "data.frame"))
View(compare_translated_sentiment_df)

compare_translated_sentiment_df$add <- compare_translated_sentiment_df[,2] + compare_translated_sentiment_df[,3] +
  compare_translated_sentiment_df[,4] + compare_translated_sentiment_df[,5]

sum(compare_translated_sentiment_df$add > 0) / nrow(compare_translated_sentiment_df) * 100
# since text matching percentage was significantly larger in hypo2, hypo0 is rejected


# evaluating wheel_factor for each wheel
# search_wheel factor in korean and find the index of the sentence 
# get information from translated version and evalutate the sentiment score
wheel_factor 

View(search_sentence_imported)
wheel_factor_index <- list()
for (i in 1:3){
  wheel_factor_index <- append(wheel_factor_index, 
                               list(grep(wheel_factor[[i]], search_sentence_imported$sentence)))
}

remove_value <- function(lst, value) {
  return(Filter(function(x) !any(x %in% value), lst))
}

wheel_factor_index <- lapply(wheel_factor_index, remove_value, c(30647: 31224, 35144:35575))

subtracted_index <- function(lst) {
  return(lapply(lst, function(x) {
    if (x >= 35576) {
      x - (length(30647:31224) + length(35144:35575))
    } else if (x >= 31225) {
      x - length(30647:31224)
    } else {
      x
    }
  }))
}

subtracted_wheel_factor_index <- lapply(wheel_factor_index, subtracted_index)

wheel_factor_1 <- data.frame()
for (i in subtracted_wheel_factor_index[[1]]){
  wheel_factor_1 <- wheel_factor_1 %>% rbind(translated_sentence_imported[i,])
}

wheel_factor_2 <- data.frame()
for (i in subtracted_wheel_factor_index[[2]]){
  wheel_factor_2 <- wheel_factor_2 %>% rbind(translated_sentence_imported[i,])
}

wheel_factor_3 <- data.frame()
for (i in subtracted_wheel_factor_index[[3]]){
  wheel_factor_3 <- wheel_factor_3 %>% rbind(translated_sentence_imported[i,])
}

# analyzing wheel factor 1 강성
wheel_factor_1_corpus <- corpus(wheel_factor_1$sentence)

wheel_factor_1_sentiment <-
  wheel_factor_1_corpus %>%
  tokens(remove_punct = T,
         remove_symbols = T) %>%
  tokens_remove(stopwords("english")) %>%
  tokens_lookup(dictionary = data_dictionary_LSD2015) %>%
  dfm

wheel_factor_1_sentiment_df <- convert(wheel_factor_1_sentiment, to = "data.frame")

wheel_factor_1_sentiment_df$sent_score <- 
  (wheel_factor_1_sentiment_df[,3] + wheel_factor_1_sentiment_df[,5]) -
  (wheel_factor_1_sentiment_df[,2] + wheel_factor_1_sentiment_df[,4])


wheel_factor_1_sentiment_final <- cbind(wheel_factor_1, wheel_factor_1_sentiment_df)

wheel_factor_1_sentiment_score <- wheel_factor_1_sentiment_final %>% 
  group_by(wheel) %>% summarise(sent_score = sum(sent_score)) 
 

wheel_factor_1_sentiment_score <- wheel_factor_1_sentiment_score %>% inner_join(sentence_per_wheel_eng, by=join_by(wheel))
wheel_factor_1_sentiment_score$sent_score <- round((wheel_factor_1_sentiment_score$sent_score / 
                                                           wheel_factor_1_sentiment_score$n) * 1000, 2)


# analyzing wheel factor2 에어로
wheel_factor_2_corpus <- corpus(wheel_factor_2$sentence)

wheel_factor_2_sentiment <-
  wheel_factor_2_corpus %>%
  tokens(remove_punct = T,
         remove_symbols = T) %>%
  tokens_remove(stopwords("english")) %>%
  tokens_lookup(dictionary = data_dictionary_LSD2015) %>%
  dfm

wheel_factor_2_sentiment_df <- convert(wheel_factor_2_sentiment, to = "data.frame")

wheel_factor_2_sentiment_df$sent_score <- 
  (wheel_factor_2_sentiment_df[,3] + wheel_factor_2_sentiment_df[,5]) -
  (wheel_factor_2_sentiment_df[,2] + wheel_factor_2_sentiment_df[,4])


wheel_factor_2_sentiment_final <- cbind(wheel_factor_2, wheel_factor_2_sentiment_df)

wheel_factor_2_sentiment_score <- wheel_factor_2_sentiment_final %>% 
  group_by(wheel) %>% summarise(sent_score = sum(sent_score)) 


wheel_factor_2_sentiment_score <- wheel_factor_2_sentiment_score %>% inner_join(sentence_per_wheel_eng, by=join_by(wheel))
wheel_factor_2_sentiment_score$sent_score <- round((wheel_factor_2_sentiment_score$sent_score / 
                                                      wheel_factor_2_sentiment_score$n) * 1000, 2)



# analyzing wheel factor3 항속성
wheel_factor_3_corpus <- corpus(wheel_factor_3$sentence)

wheel_factor_3_sentiment <-
  wheel_factor_3_corpus %>%
  tokens(remove_punct = T,
         remove_symbols = T) %>%
  tokens_remove(stopwords("english")) %>%
  tokens_lookup(dictionary = data_dictionary_LSD2015) %>%
  dfm

wheel_factor_3_sentiment_df <- convert(wheel_factor_3_sentiment, to = "data.frame")

wheel_factor_3_sentiment_df$sent_score <- 
  (wheel_factor_3_sentiment_df[,3] + wheel_factor_3_sentiment_df[,5]) -
  (wheel_factor_3_sentiment_df[,2] + wheel_factor_3_sentiment_df[,4])


wheel_factor_3_sentiment_final <- cbind(wheel_factor_3, wheel_factor_3_sentiment_df)

wheel_factor_3_sentiment_score <- wheel_factor_3_sentiment_final %>% 
  group_by(wheel) %>% summarise(sent_score = sum(sent_score)) 


wheel_factor_3_sentiment_score <- wheel_factor_3_sentiment_score %>% inner_join(sentence_per_wheel_eng, by=join_by(wheel))
wheel_factor_3_sentiment_score$sent_score <- round((wheel_factor_3_sentiment_score$sent_score / 
                                                      wheel_factor_3_sentiment_score$n) * 1000, 2)

View(wheel_factor_1_sentiment_score)
View(wheel_factor_2_sentiment_score)
View(wheel_factor_3_sentiment_score)
View(translated_sentiment_score)

wheel_factor_1_sentiment_score$n <- NULL
wheel_factor_2_sentiment_score$n <- NULL
wheel_factor_3_sentiment_score$n <- NULL

colnames(wheel_factor_1_sentiment_score) <- c("wheel", "강성_sent_score")
colnames(wheel_factor_2_sentiment_score) <- c("wheel", "에어로_sent_score")
colnames(wheel_factor_3_sentiment_score) <- c("wheel", "항속성_sent_score")

colnames(translated_sentiment_score) <- c("wheel", "total_sent_score")
final_wheelset_analyze <- translated_sentiment_score %>% full_join(wheel_factor_1_sentiment_score, by="wheel") %>%
  full_join(wheel_factor_2_sentiment_score, by="wheel") %>% full_join(wheel_factor_3_sentiment_score, by="wheel")

final_wheelset_analyze <-  final_wheelset_analyze %>% mutate_all(~replace(., is.na(.), 0))
View(final_wheelset_analyze)



#### exporting csv
write.csv(final_wheelset_analyze, file = "final_wheelset_analyze_imported.csv",fileEncoding = "UTF-8")
#### reading csv
final_wheelset_analyze_imported <- read.csv("final_wheelset_analyze_imported.csv")


