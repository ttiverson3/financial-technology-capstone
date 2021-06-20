# 套件
pacman::p_load(dplyr,ggplot2)
options(scipen = 999)

# load原始資料
load("file3.rdata")
# 設定顏色
mycolors <- c("#F08080","#FFEC8B","#98FB98","#EE82EE","#63B8FF","#A9A9A9")

file1 = read.csv("file1.csv")
zipcode = read.csv("zipcode.csv")

# 不要的: 教育程度1 + 年齡20~22 +離島 +職位99
island = zipcode %>% filter(縣市 %in% c("連江縣","999","澎湖縣","金門縣"))
Data <- file1 %>% 
  filter(edu != 1) %>% 
  filter(age >= 23) %>% 
  filter(!(zip %in% island$zip)) %>% 
  filter(!(job == 99))

Data2 <- file3 %>% filter(srno %in% Data$srno)

# 計算單筆消費金額、總帳務金額、總消費金額
numcol <- Data2[,c(1,19:62)]
A <- rowsum(numcol[,-1], numcol$srno)
B <- A %>% mutate(srno = rownames(A))

X <- lapply(1:193379, function(i){
  C <- data.frame(srno = B[i,45],
                  total_bill = rowSums(B[i,1:6]),
                  pmt_a = rowSums(B[i,7:25]),
                  pmt_c = rowSums(B[i,26:44]))

})#%>% as.data.frame()

# 轉換成Dataframe
Data3 <- data.frame(matrix(unlist(X), nrow=length(X), byrow=TRUE))
Data3 <- cbind(Data3[,-1],B$srno)
names(Data3) <- c("total_bill","pmt_a","pmt_c","srno")
Data3$srno <- as.character(Data3$srno)


Data1 <- Data %>% select(srno, age, income)

Data2 <- Data2 %>% group_by(srno) %>%
  summarise(aum01 = mean(aum01),
            CNT_Web_Login = mean(CNT_Web_Login))

Final <- merge(Data1,Data2)
Final <- merge(Final,Data3)
save(Final, file = "Final.rdata")

load("Final.rdata")

# 單筆消費金額的分群
Data1 <- Data %>% select(srno, age, income)

Data4 <- Final %>% group_by(srno) %>%
  mutate(pmt = pmt_a/pmt_c) %>% ungroup()

Data4[is.na(Data4$pmt),9] = 0

Final2 <- merge(Data1,Data4)
Final2 <- Final2 %>% select(-pmt_a,-pmt_c)

save(Final2, file = "Final2.rdata")
load("Final2.rdata")
Group2 <- Final2 %>% select(-srno)

# 標準化
scData2 = scale(Group2) %>% data.frame

# 分群(7群)

Group2$grp = kmeans(scData2,7)$cluster
table(Group2$grp)  # 族群大小
gc()
require(RColorBrewer)
mycolors <- colorRampPalette(brewer.pal(6, "Pastel1"))(6)

name = c("年齡","收入","資產配置類別1","數位通路互動指標","總支付帳務金額","單筆消費金額")
# 畫圖
par(cex=0.8)
split(scData2,Group2$grp) %>% sapply(colMeans) %>% barplot(beside=T,col=mycolors)
legend('topright',legend=name,fill=mycolors,cex = 2)
plot(rnorm,lty=0,bty='n',xaxt='n',yaxt='n',xlab='',ylab='',xlim=c(-1,1),ylim=c(-1,1));

# 標準化尺度
sapply(split(scData2,Group2$grp), colMeans) %>% round(2) 
# 原始尺度
GroupData <- sapply(split(Group2,Group2$grp), colMeans) %>% round(2)  
write.csv(GroupData,file = "GroupData.csv")

# 分群加入購買次數(factor轉成數字再計算總合)
file3$rs_prod_03<-ifelse(file3$rs_prod_03 == 1,1,0)

#計算每個顧客的購買次數
Buy<-file3 %>% filter(srno %in% Final2$srno) %>% 
  group_by(srno) %>% 
  summarise(buynum = sum(rs_prod_03)) %>% ungroup()

Buy<-Buy %>% mutate(buy = ifelse(buynum > 0 ,1,0))
table(Buy$buy) %>% prop.table()

# 分群名單
grouplist <- cbind(Final2$srno,Group2$grp) %>% as.data.frame()
# write.csv(grouplist, file = "grouplist.csv")
# names(grouplist) <- c("srno","grp")

# G_rs <- merge(grouplist,Buy)
# save(scData2,Group2,Buy,G_rs,file = "Group.rdata")
load("Group.rdata")

# 每群人數
table(G_rs$grp)

# 每群購買人數/比例
table(G_rs$buy,G_rs$grp)
table(G_rs$buy,G_rs$grp) %>% prop.table(2) %>% round(4)
