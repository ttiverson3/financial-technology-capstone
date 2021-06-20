pacman::p_load(data.table,dplyr,wordcloud2,tidytext,topicmodels,ggplot2,purrr,tidyr)
# 設定畫圖顏色
mycolors <- c("#F08080","#FFEC8B","#98FB98","#EE82EE","#63B8FF","#A9A9A9")

# PTT原始資料
ptt <- fread("Loan_ting_articleMetaData.csv",encoding = "UTF-8") %>% as.data.frame()

# load原始資料
load("../file3.rdata")

# PTT斷詞後資料
token <- fread("Loan_ting_artWord.csv",encoding = "UTF-8") %>% as.data.frame()

# 台新
# 文章數量
# 將日期欄位格式由chr轉為date
ptt$artDate = ptt$artDate %>% as.Date("%Y/%m/%d")
token$artDate = token$artDate %>% as.Date("%Y/%m/%d")

TS_url <- token %>% filter(word == "台新") %>% select(artUrl)


# 文章數量圖

Total_num<-ptt %>% 
  group_by(date = format(artDate,"%Y-%m")) %>%
  summarise(total_num= n())

rs_num<-file3 %>% 
  group_by(date = format(yyyymm,"%Y-%m"),rs_prod_03) %>% 
  summarise(n = n()) %>% 
  filter(rs_prod_03 == 1) %>% 
  select(-rs_prod_03) %>% ungroup()

TS_num<- ptt %>% 
  filter(artUrl %in% TS_url$artUrl) %>% 
  group_by(date = format(artDate,"%Y-%m")) %>%
  summarise(TS_num= n()) %>% 
  as.data.frame()

Time<-merge(Total_num,TS_num)
Time<-merge(Time,rs_num)
Time <- Time %>% gather(col,num, -date)

Time %>% ggplot(aes(x = date, y = num, group = col, color = col),size = 2)+
  geom_line()+
  geom_point(size = 3)+
  labs(x = "時間" , y = "數量")+
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size=18),
        axis.title.y =element_text(vjust = 0.5, angle = 360, size=18))

# 情緒
senti <- fread("Loan_ting_artSen.csv",encoding = "UTF-8") %>% as.data.frame()
# 將日期欄位格式由chr轉為date
senti$artDate = senti$artDate %>% as.Date("%Y/%m/%d")

senti_score<-senti %>% 
  group_by(date = format(artDate,"%Y-%m")) %>% 
  summarise(positive = sum(positive_emotion_grade),
            negative = sum(negative_emotion_grade),
            neutral = sum(neutral_emotion_grade)) %>% 
  gather(sentiment, score,-date)


TS_senti<- senti %>%  
  filter(artUrl %in% TS_url$artUrl) %>% 
  group_by(date = format(artDate,"%Y-%m")) %>% 
  summarise(TS_positive = sum(positive_emotion_grade),
            TS_negative = sum(negative_emotion_grade),
            TS_neutral = sum(neutral_emotion_grade)) %>% 
  gather(TS_sentiment, TS_score,-date)

Total_sen <- merge(senti_score, rs_num)
TS_sen <- merge(TS_senti, rs_num)

Total_sen %>% ggplot()+
  geom_line(aes(x = date, y = n, group = 1), color = "orange",size = 1)+
  geom_point(aes(x = date, y = n), color = "orange",size = 3)+
  geom_line(mapping = (aes(x = date, y= score*2, group = sentiment, color = sentiment)),size = 1)+ #調整 range
  geom_point(aes(x = date, y = score*2, group = sentiment, color = sentiment),size = 3)+
  scale_y_continuous(name = "n",
                     sec.axis = sec_axis(~./2))+
  labs(x = "時間" , y = "數量")+
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size=18),
        axis.title.y =element_text(vjust = 0.5, angle = 360, size=18))
 

TS_sen %>% ggplot()+
  geom_line(aes(x = date, y = n, group = 1), color = "orange",size = 1)+
  geom_point(aes(x = date, y = n), color = "orange",size = 3)+
  geom_line(mapping = (aes(x = date, y= TS_score*10, group = TS_sentiment, color = TS_sentiment)),size = 1)+ #調整 range
  geom_point(aes(x = date, y = TS_score*10, group = TS_sentiment, color = TS_sentiment),size = 3)+
  scale_y_continuous(name = "n",
                     sec.axis = sec_axis(~./10))+
  labs(x = "時間" , y = "數量")+
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size=18),
        axis.title.y =element_text(vjust = 0.5, angle = 360, size=18))


# 文字雲
TS_word <- token %>% 
  filter(artUrl %in% TS_url$artUrl) %>% 
  group_by(word) %>%
  summarise(freq = n())%>% 
  arrange(desc(freq)) %>% 
  as.data.frame()

TS_word %>% filter(freq > 500 & !(word %in% remove_word)) %>% wordcloud2(size = 0.38)

# 主題分析(LDA)
pacman::p_load(data.table,dplyr,wordcloud2,tidytext,topicmodels,ggplot2,purrr,tidyr)

TS_token<-token %>% 
  filter(artUrl %in% TS_url$artUrl) 
TS_count <- left_join(TS_token , TS_word)
TS_count <- TS_count %>% filter(!(word %in% remove_word))

# 將資料轉換為 Document Term Matrix (DTM)
TSdtm <- TS_count %>% filter(freq > 5) %>% cast_dtm(artUrl, word, freq)
TSdtm

# LDA主題分析:測試不同主題數分群結果
# TSldas = c()
# topics = c(2,3,4,5,6,7,8,9,10,15)
# for(topic in topics){
#   start_time <- Sys.time()
#   lda <- LDA(TSdtm, k = topic, control = list(seed = 2020))
#   TSldas =c(TSldas,lda)
#   print(paste(topic ,paste("topic(s) and use time is ", Sys.time() -start_time)))
#   save(TSldas,file = "TS_ldas_result")
# }

topics = c(2,3,4,5,6,7,8,9,10,15)
load("TS_ldas_result")

# 評估主題數：Optimal number of topics (smaller is better)
# data_frame(k = topics,
#            perplex = map_dbl(TSldas, topicmodels::perplexity)) %>%
#   ggplot(aes(k, perplex)) +
#   geom_point() +
#   geom_line() +
#   labs(title = "Evaluating LDA topic models",
#        subtitle = "Optimal number of topics (smaller is better)",
#        x = "Number of topics",
#        y = "Perplexity")


load("TS_ldas_result")

# rev_word <- c("貸款","信用卡","銀行")
TS_lda = TSldas[[7]] 

par(mfrow=c(2,4))

# 看各群的常用詞彙
tidy(TS_lda, matrix = "beta") %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = as.factor(topic), term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = topic)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free",nrow = 2) +
  theme_bw()+
  coord_flip() +
  scale_x_reordered()+ 
  scale_fill_manual(values=c("#A9A9A9","#F08080","#FFEC8B","#90EE90","#FFBBFF","#63B8FF","#EE82EE","#FFEFD5"))

topic_name = c("申辦資格",'各銀行比較','方案諮詢','還款資訊','費用計算','薪轉戶','台新','信貸規定')

tmResult <- posterior(TS_lda)
doc_pro <- tmResult$topics 
dim(doc_pro)               # nDocs(DTM) distributions over K topics

TS<-ptt %>% filter(artUrl %in% TS_url$artUrl)
# get document topic proportions 
document_topics <- doc_pro[TS$artUrl,]
document_topics_df =data.frame(document_topics)
colnames(document_topics_df) = topic_name
rownames(document_topics_df) = NULL
TS_topic = cbind(TS,document_topics_df)


TS_topic[,c(11:18)] =sapply(TS_topic[,c(11:18)] , as.numeric)
TS_topic <- TS_topic[,-c(7:9)]

# 每個時間的主題比例
TS_topic %>%
  group_by(date = format(artDate,"%Y-%m")) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  melt(id.vars = "date")%>%
  group_by(date)%>%
  mutate(total_value =sum(value))%>%
  ggplot( aes(x=date, y=value/total_value, fill=variable)) + 
  geom_bar(stat = "identity") + ylab("proportion")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "時間" , y = "比例")+
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size=18),
        axis.title.y =element_text(vjust = 0.5, angle = 360, size=18),
        axis.text.x = element_text(angle = 60, hjust = 0.5, 
                                   vjust = 0.5,color = "black",size=15))

topic <- TS_topic %>%
  group_by(date = format(artDate,"%Y-%m")) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  melt(id.vars = "date")%>%
  group_by(date)%>%
  mutate(total_value =sum(value)) %>% 
  as.data.frame()

# 方案諮詢的文章
library(stringr)
ask <- ptt[str_detect(ptt$sentence, "欲貸金額"),]
ask_num <- ask %>%
  group_by(date = format(artDate,"%Y-%m")) %>% 
  summarise(n = n()) %>%
  as.data.frame()

percent <- merge(ask_num,TS_num)
percent <- merge(percent,Total_num)

percent$preb <- paste(round(percent$n/percent$total_num*100,2),"%")
colnames(percent) <- c("日期","方案諮詢篇數","台新文章篇數","PTT總篇數","比例")
write.csv(percent,"topic2.csv")
write.csv(ask,"ask.csv")
