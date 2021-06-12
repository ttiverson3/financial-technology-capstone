# 訓練集：前四個月資料作為訓練集
trainData = Data %>% filter(yyyymm >= "2020-07-01", yyyymm <= "2020-10-01")
trainY = filter(Data, yyyymm >= "2020-08-01", yyyymm <= "2020-11-01")$y
trainData$y = trainY
# 保留切割完成的訓練集
trainDataSave = trainData

# 驗證集
testData = Data %>% filter(yyyymm == "2020-11-01")
testY = filter(Data, yyyymm == "2020-12-01")$y
testData$y = testY
# 保留切割完成的驗證集
testDataSave = testData

# 拿掉 srno、yyyymm
trainData$srno = NULL; trainData$yyyymm = NULL
srno = testData$srno; testData$srno = NULL; testData$yyyymm = NULL

# 查看 y 比例
table(trainData$y)
scale_pos_weight = as.integer(sum(trainData$y == "0") / sum(trainData$y == "1"))

###################################### 資料標準化

# 將連續型變數做標準化
num_name = c(
  "aum", "bill", "pmt", "dep", "AMT", "CNT_Exchange", "income"
)
num_name = select(trainData, contains(num_name)) %>% names()
a = names(outer)[-1]
b = names(PTT)[-13]
num_name = append(num_name, a)
num_name = append(num_name, b)



max = unname(apply(trainData[, which(names(trainData) %in% num_name)], 2, max)) # 對每個col算max
min = unname(apply(trainData[, which(names(trainData) %in% num_name)], 2, min)) # 對每個col算min

for (i in c(1:length(names(trainData[, which(names(trainData) %in% num_name)])))){
  a = names(trainData[, which(names(trainData) %in% num_name)])[i]
  if(max[i] != 0){
    trainData[a] = (trainData[a] - min[i]) / (max[i] - min[i])
    testData[a] = (testData[a] - min[i]) / (max[i] - min[i])
  }
}

# 將level=2的分類變數直接轉成numeric
factor_level = trainData %>% 
  select(-num_name) %>% 
  apply(2, function(x) length(unique(x)))
two_level_factor = names(factor_level[as.numeric(factor_level)==2])

trainData = trainData %>% 
  mutate_at(vars(all_of(two_level_factor)), list(as.numeric)) %>%
  mutate_at(vars(all_of(two_level_factor)), list( ~.-1 ))

testData = testData %>% 
  mutate_at(vars(all_of(two_level_factor)), list(as.numeric)) %>%
  mutate_at(vars(all_of(two_level_factor)), list( ~.-1 ))


# One hot encoding
library(fastDummies)
fac_Col = names(which(sapply(trainData, class) == "factor")) #取出類別型col所在的位置

trainData = dummy_cols(.data = trainData, select_columns = fac_Col, 
                       remove_first_dummy = F, remove_most_frequent_dummy = F,
                       remove_selected_columns = T)
testData = dummy_cols(.data = testData, select_columns = fac_Col, 
                      remove_first_dummy = F, remove_most_frequent_dummy = F,
                      remove_selected_columns = T)

sum(colnames(trainData) != colnames(testData))
trainDataSave = trainData
testDataSave = testData

###################################### 不平衡樣本處理
# rose (Random Over Sampling Examples)
data.rose = ROSE(y ~., data = trainDataSave, seed = 1, p = 0.2)$data
sum(data.rose$y == 0) / sum(data.rose$y == 1)
scale_pos_weight = sum(data.rose$y == 0) / sum(data.rose$y == 1)
table(data.rose$y)

# undersampling
data_balanced_under = ovun.sample(y ~ ., data = trainDataSave, method = "under", p = 0.2,seed = 1)$data
sum(data_balanced_under$y == 0) / sum(data_balanced_under$y == 1)
sum(data_balanced_under$y == 1) / sum(data_balanced_under$y == 0)
scale_pos_weight = sum(data_balanced_under$y == 0) / sum(data_balanced_under$y == 1)
table(data_balanced_under$y)

# both
data_balanced_both = ovun.sample(y ~ ., data = trainDataSave, method = "both", p = 0.2, seed = 1)$data
sum(data_balanced_both$y == 0) / sum(data_balanced_both$y == 1)
scale_pos_weight = sum(data_balanced_both$y == 0) / sum(data_balanced_both$y == 1)
table(data_balanced_both$y)

# smote
data.smote = SMOTE(X = subset(trainDataSave, select = -y), target = trainDataSave$y, K = 4, dup_size = 5)$data
data.smote$y = data.smote$class
data.smote$class = NULL
prop.table(table(data.smote$y))
sum(data.smote$y == 0) / sum(data.smote$y == 1)
