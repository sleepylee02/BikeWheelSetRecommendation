options(scipen = 999)

require(tidyverse)
require(data.table)
require(tidytext)
require(dplyr)
require(rvest)
require(quanteda, warn.conflicts = FALSE, quietly = TRUE)
require(stringr)
require(readtext)
require(RCurl)
library(tm)

require(xml2)

require(jsonlite)
require(httr)

require(readxl)



#require(deeplr)

#require(deeplr)

#install.packages("RcppMeCab")
#nrequire(epubr)
#require(RcppMeCab)
#pkg_v <- c("tidyverse", "tidytext", "epubr", "RcppMeCab")

#install.packages("devtools") # 현재 CRAN 버전은 Rcpp 지원이 늦어 버전업을 하지 못하고 있습니다. Github 버전을 설치하기 위해 devtools 패키지를 설치합니다.
#install.packages("devtools")
#library(devtools)
#install.packages("devtools", dependencies = TRUE)
#install.packages('RccpMeCab')
#require(RccpMeCab)
#install.packages(c("Rcpp", "BH", "RcppEigen"))


#install.packages("remotes")
#remotes::install_github("junhewk/RcppMeCab", INSTALL_opts=c("--no-multiarch", "--no-test-load"), verbose=TRUE)
#require(RcppMeCab)

#pop("가나다")
#RcppMeCab::pos()
#require(devtools)
#install_github("junhewk/RcppMeCab")
#pos("가나다")

#update.packages("cli")
#library(cli)
#ins

### KoNLP 설치

# jdk 설치 
install.packages("multilinguer")
library(multilinguer)

install_jdk

# KoNLP 설치 
install.packages("remotes")
remotes::install_github("haven-jeon/KoNLP",
                        force = T,
                        upgrade = "never", 
                        INSTALL_opts = c("--no-multiarch"))

# PKG
library(KoNLP)
useNIADic()
useSejongDic()

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


wheelset <- read.csv("wheelset.csv") # opening handmade csv which includes informations on wheelset
wheelset_annotated <- read.csv("wheelset_annotated.csv") 

#View(wheelset_annotated)
# 최대한 묶을 수 있는 것은 묶어서 처리 하기로함 이것이 정보처리량면에서 훨씬 유익함 
# 생각보다 시간이 엄청 걸리고 너무 세분화시 검색 결과도 줄어서 조금 대중적으로 묶어주기
# 브랜드명 중 캄파뇰로, 디티 스위스 같이 길고 특정적인 단어는 생략하였음
# 검색어를 얼마나 포괄적 아니면 구체적으로 만들지에 대한 고민을 해보자
# 유저들이 어떤 키워드로 검색을 하는지에 대한 분석이 필요했음

# 여기서 검색을 더 잘 될 수 있도록 추가 정리를 해보자
# 짚 파이어크레스트 같은 경우 짚을 날리고 파이어크레스트로 검색했을때 결과값이 훨씬 많이 나왔다



wheelset <- setDT(wheelset)
wheelset_annotated <- setDT(wheelset_annotated)

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
    search_html = paste0("https://corearoadbike.com/board/board.php?t_id=Menu03Top1&sch_W=title_content&sch_O=AND&sch_T=", search,
    "&page=", as.character(j))
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

search_wheel_list
  
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
 # names(content_sentence) <- make.unique(rep("content", length(content_sentence))) 
  names(coment_sentence) <- make.unique(rep("comment", length(coment_sentence)))

  combined_sentence <- c(content_sentence, coment_sentence)
  return(combined_sentence)
  Sys.sleep(10) 
}

#### 돌리기 전에 컴퓨터 꺼지지 않게 세팅해두기~!!!!!!!!!!!!!!!!!!!!!
search_sentence = list()
for (i in search_wheel_list){
  specific_wheel <- search_html_imported %>% filter(wheel == i) %>% select(html)
  wheel_html <- lapply(unlist(specific_wheel$html), to.sentence)
  search_sentence[[i]] <- append(search_sentence[[i]], wheel_html)
  print(i)
}

View(search_sentence)
class(search_sentence)
str(search_sentence,1)
length(search_sentence)
search_sentence


# unlisting the token into text and then make it as a dataframe
search_sentence_df = data.frame()
for (i in wheelset_name[[1]]){
  temp_wheel <- sprintf('search_sentence$"%s"[[1]]',i)
  temp_token <- unlist(eval(parse(text=temp_wheel))) # since I needed to call the items using $ i found a function
  # eval(parse(text=)) in internet to call the text inside the str to be used as a function 
  temp_token_df <- cbind(data.frame(temp_token), rep(c(i),length(temp_token)))
  search_sentence_df <- rbind(search_sentence_df,temp_token_df)
}

View(search_sentence_df)
head(search_sentence_df)
dim(search_sentence_df)

#### exporting csv
#View(search_html_export_df)
head(search_sentence_df)

colnames(search_sentence_df) <- c("sentence", "wheel")
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
View(emotion_df)
emotion_df %>% group_by(emotion) %>% count

View(detailed_df)
(nrow(detailed_df) - sum(is.na(detailed_df$emotion)))/ nrow(detailed_df) * 100
# so, shows how much the words corresponded in R (only 3 percent....)
# how can we increase this 

# hypo 1 there was mistake on my procedure
# -> try instead of unnesting the token and using the left_join from the senti_dic
# hypo 2 the knu dict is not good to analyze on community space 
# -> translate the text to english and use other library

#hypo 1 : try a new way of matching the words from the knu dict
# actually the way I tried earlier was the way the knu dict introduced to use as 

emotion_list_hypo1 <- list()
detailed_df_hypo1 <- data.frame()

for (i in wheelset_name[[1]]){
  temp_sentence_df_hypo1 <- search_sentence_df %>% filter(wheel == i)
  temp_evaluate_df_hypo1 <- data.frame()
  print(i)
  if (i == wheelset_name[[1]][2]) break
  
  for (j in 1:nrow(temp_sentence_df_hypo1)){
    temp_unlist_hypo1 <- unlist(temp_sentence_df_hypo1$sentence[j])
    temp_tibble_hypo1 <- tibble(temp_unlist_hypo1) #%>%
    #print(temp_unlist_hypo1)
    #print(temp_tibble_hypo1)
    detailed_df_hypo1 <- rbind(detailed_df_hypo1, temp_tibble_hypo1)
    print(temp_tibble_hypo1)
    #break
     # unnest_tokens(input = temp_unlist_hypo1,output = "word") %>% left_join(senti_dic) 
    
    #temp_tibble_hypo1 <- temp_tibble_hypo1 %>% mutate(sScore = ifelse(is.na(sScore), 0, sScore))
    
    #temp_evaluate_df_hypo1 <- rbind(temp_evaluate_df_hypo1, temp_tibble_hypo1)
  }
  #break
  #detailed_df_hypo1 <- rbind(detailed_df_hypo1, temp_evaluate_df_hypo1)
  #emotion_list_hypo1[[i]] = sum(temp_evaluate_df_hypo1$sScore)
  #print(i)
}  
detailed_df_hypo1
class(detailed_df_hypo1)
str(detailed_df_hypo1)
colnames(detailed_df_hypo1) <- c("sentence")
class(senti_dic)
str(senti_dic)

View(detailed_df_hypo1)
detailed_df_hypo1$sentence[1]
class(detailed_df_hypo1)


counting_sentence <- function(sent, word = senti_dic$word){
  counting_list <- lapply(word, function(x) str_count(sent, paste0("\\b", str_escape(x), "\\b")) )
  counting_df <- data.frame(t(data.frame(counting_list)))
  counting_df <- cbind(senti_dic, counting_df)
  colnames(counting_df) <- c("word","sScore","emotion", "counts")
  return(sum(counting_df$sScore * counting_df$counts))
}

a <- lapply(detailed_df_hypo1$sentence, counting_sentence)
q <- data.frame(t(data.frame(a)))
View(q)
sum(q)
counting_sentence(q)

View(emotion_df)
View(senti_dic)

# hypo 1 is rejected
# 봐봐 이게 결과론적으로 깨닫은게 어떻게 text를 매칭시켜도 senti dic 자체가 너무 구체적이어서
# 매칭하기에 불리함 
# so, 해결책으로 senti dic을 형태소 단위로 쪼개서 만들었어야 함
# 근데 현재 이거는 불가능하므로 스킵



##################################################
class(emotion_df_hypo1_imported)
nrow(emotion_df_hypo1_imported) # sentence based
nrow(detailed_df) # word based
# can not compare these two
# so we compare how much the Sscore had changed

emotion_df_hypo1_final <- cbind(search_sentence_imported, emotion_df_hypo1_imported)
View(emotion_df_hypo1_final)

emotion_score_hypo1 <- emotion_df_hypo1_final %>% group_by(wheel) %>% summarise(sum(Sscore)) 
emotion_score_hypo1

colnames(emotion_df_imported) <- c("wheel", "emotion")

emotion_df_imported$wheel
emotion_score_hypo1$wheel

emotion_hypo1_wheel <- gsub(" ", "." , emotion_score_hypo1$wheel)
emotion_hypo1_wheel[1:5] <- paste0("X", emotion_hypo1_wheel[1:5])
emotion_hypo1_wheel

emotion_score_hypo1$wheel <- emotion_hypo1_wheel
emotion_score_hypo1

emotion_compare <- emotion_df_imported %>% inner_join(emotion_score_hypo1, by= "wheel") 
emotion_compare
#################################


#hypo2 : translate and than use quenta library

# translate the text using google translator
# actually api on google translator and papago needs to be paid...
# using package to translate was not an exciting experience. so, i just made the original file as a excel
# since the text limit in papago for file is 10,000 split file into 10 files


# exported csv를 엑셀에서 직접 엑셀 문서로 읽어주기!
# 이후 파파고 문서 번역으로 번역 완료
search_sentence_translated <- read.csv("search_sentence_translated.csv")
search_sentence_translated

search_sentence_translated <- search_sentence_translated %>% cbind(search_sentence_imported$wheel)
View(search_sentence_translated)
colnames(search_sentence_translated) <- c("sentence", "wheel")


translated_corpus <- corpus(search_sentence_translated$sentence)

translated_sentiment <-
  translated_corpus %>%
  tokens(remove_punct = T,
         remove_symbols = T) %>%
  tokens_remove(stopwords("english")) %>%
  tokens_lookup(dictionary = data_dictionary_LSD2015) %>%
  dfm

translated_sentiment_df <- convert(translated_sentiment, to = "data.frame")
translated_sentiment_df

translated_sentiment_df$sent_score <- log(
  (translated_sentiment_df[,3] + translated_sentiment_df[,5] + 0.5) /
    (translated_sentiment_df[,2] + translated_sentiment_df[,4] + 0.5)
) 

sum(translated_sentiment_df$sent_score != 0)
nrow(translated_sentiment_df)

?tokens

translated_sentiment

topfeatures(translated_sentiment, n = 50)

ncol(translated_sentiment)
translated_sentiment
dfm_trim(translated_sentiment, min_docfreq = 3)










search_sentence_imported$sentence[start:last]

2050/5


# using package to translate was not an exciting experience. so, i just made the original file as a excel

deeplr::usage2("e6873216-c621-479b-a513-e7da26d5c142:fx")

?`deeplr-package`
search_sentence_translated <- data.frame()
for (i in search_sentence_imported$sentence){
    trans <- deeplr::translate2(
    text = i
    target_lang = "EN",
    auth_key = "e6873216-c621-479b-a513-e7da26d5c142:fx"
    search_sentence_translated %>% rbind(data.frame(trans))
    
  )
  
  
}



searh_sentence_translated <- translate_df(dataset = search_sentence_imported,
             column.name = "sentence",
             source.lang = "KO",
             target.lang = "EN",
             auth_key = "e6873216-c621-479b-a513-e7da26d5c142:fx")



















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












######
search_sentence_imported %>% filter(grep("강성",search_sentence_imported$sentence))

View(search_sentence_imported[grep("강성",search_sentence_imported$sentence),])

  #  [search_sentence_imported$sentence %in% "강성"]

grep("강성",search_sentence_imported)













#detailed_df_hypo1

#z <- lapply(senti_dic$word, function(x) str_count(q$sentence[1], paste0("\\b", str_escape(x), "\\b")) )
#z <- data.frame(t(data.frame(z)))
#z <- cbind(senti_dic, z)
#colnames(z) <- c("word","sScore","emotion", "counts")
#sum(z$sScore * z$counts)

#z %>% lef
#senti_dic
#class(z)
#sum(z$V1)

#View(z)
#class(z)
#sum(z)

#count_words_in_sentence(q$sentence[1], senti_dic)
#q$sentence[1]
  
#count_words_in_sentence <- function(sentence, words) {
 # counts <- sapply(words, function(word) str_count(sentence, paste0("\\b", str_escape(word), "\\b")))
  #names(counts) <- words
  #return(counts)
#}

#count_words_in_sentence(q[1], senti_dic)
#q <- data.frame(detailed_df_hypo1$sentence)
#colnames(q) <- c("sentence")
# Apply the function to each sentence in df1

#results <- q %>% 
 # rowwise() %>%
  #mutate(word_counts = list(count_words_in_sentence(sentence, senti_dic$word))) %>%
#  unnest_wider(word_counts) %>%
 # setNames(c("sentence", senti_dic$word))
# Print the results
#results

#emotion_df_hypo1 <- t(data.frame(emotion_list_hypo1))
#View(emotion_df_hypo1)
#emotion_df_hypo1 %>% count() 


# I believe that knu dict could not pick up the sentiment well
#for (i in wheelset_name[[1]]){
 # q <- search_sentence_df %>% filter(wheel == i)
  #p <- data.frame()
  #for (j in 1:length(q)){
   # z <- unlist(q$sentence[j])
    #s <- tibble(z) %>% 
     # unnest_tokens(input = z,output = "word") %>% left_join(senti_dic)
    #print(sum(is.na(s)))
  #}
#}  
#x
#is.na(x)

z<-method_1 %>% full_join(senti_dic) %>% mutate(sScore = ifelse(is.na(sScore), 0, sScore))
z
 
?outer_join
a <- grep("강성",search_sentence_imported$sentence)
class(a)
c(a)

search_sentence_imported$wheel[40]
search_sentence_imported$sentence[40]

search_sentence_imported$wheel[50]
search_sentence_imported$sentence[50]

search_sentence_imported$wheel[54]
search_sentence_imported$sentence[54]


grep("강성",search_sentence_imported$sentence)
grep("에어로",search_sentence_imported$sentence)
grep("항속",search_sentence_imported$sentence)


search_sentence_imported$sentence 

search_sentence_imported$sentence[2]
View(z)
method_1 <- tibble(c) %>% 
  unnest_tokens(input = c,output = "word") 

method_1

z<-method_1 %>% inner_join(senti_dic) %>% mutate(sScore = ifelse(is.na(sScore), 0, sScore)) %>% 
  summarise(score = sum(sScore))
View(z)

#%>% 
 # filter(str_length(word) > 1) %>% 
  #count(word)
?tibble

mtibblemethod_1

#
unnest_tokens(input= c,)
  
  tibble(lyrics = c) %>% 
  unnest_tokens(
    input = lyrics,
    output = "word"
  ) %>% 
  filter(str_length(word) > 1) %>% 
  count(word, sort = TRUE) %>% 
  head(20)

method_1


  #  \\bHello\\b

#stopword_korrr <- scan("stopwordsss.txt",what = "character", sep = "\n", fileEncoding = "UTF-8")
#stopword_korrr
#stopword_korrr <- unlist(lapply(stopword_korrr, function(i) {
#  paste0("\\b", i, "\\b")
#}))
#stopword_korrr


#stopword_korrr <- paste(stopword_korrr, collapse = "|") # make it as a one text to use it for gsub

#stopword_patternsss <- paste(stopword_korrr, collapse = "|")
#stopword_patternsss

#a <- read_html("https://corearoadbike.com/board/board.php?t_id=Menu03Top1&no=2298000")
#b <- a %>% 
#  html_elements("td.view_content") %>% html_text2()
#d <- gsubfn(stopword_korrr, "", b)
#b
#d


#d <- gsub(c(stopword_korrr), "",b) 
#%>% corpus()
#c <- tokens(b)
#b                     
#d
#library(gsubfn)
#install.packages("stringr")

#d <- gsubfn(stopword_korrr, "", b)

##require(stringr)
#gsubfn(".", list("'" = "", " " = "_"), x)
# [1] "ab_c"









###
###

# 어떻게 문장 단위로 text를 뽑아낼 수 있을지에 대한 분석을 해보기!!

###
###

class(c)

corp <- corpus(c)
corp %>% tokens %>%

corp
#View(temp)
class(temp)
str(temp,1)
grep("no=",temp, ignore.case = T)
temp
temp[grep("no=",temp, ignore.case = T)]

example.webpage %>%
  html_elements(css = "h3") %>% # 지금 여기서는 글자 크기가 h3인 css를 다 가져온것
  html_text2


?xml_text
a = xml
corp <- corpus(c("A corpus is a set of documents",
                 "This is the second document in the corpus"))
corp

summary(corp)

corp %>%
  tokens(remove_punct = T) %>%
  tokens_wordstem %>%
  tokens_remove(stopwords("english")) %>%
  dfm

dfm(
  tokens_remove(
    tokens_wordstem(
      tokens(corp, remove_punct = T)),
    stopwords("english")))

