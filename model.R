
#################################### 訓練模型

# 原始資料跑 xgboost 變數重要性
# train_x = trainDataSave %>% select(-y) %>% as.matrix()
# train_y = trainDataSave %>% select(y) %>% as.matrix()

train_x = data_balanced_under %>% select(-y) %>% as.matrix()
train_y = data_balanced_under %>% select(y) %>% as.matrix()

# train_x = data.rose %>% select(-y) %>% as.matrix()
# train_y = data.rose %>% select(y) %>% as.matrix()

# train_x = data_balanced_both %>% select(-y) %>% as.matrix()
# train_y = data_balanced_both %>% select(y) %>% as.matrix()

test_x = testDataSave %>% select(-y) %>% as.matrix()
test_y = testDataSave %>% select(y)  %>% as.matrix()

# 將Train與Test轉為xgboost矩陣格式
trainData = xgb.DMatrix(data =  train_x, label = train_y)   
testData = xgb.DMatrix(data =  test_x, label = test_y)       

watchlist = list(train = trainData, validation = testData)

#1. XGBoost 固定參數版
xgbModel = xgb.train(data = trainData, 
                     booster = "gbtree", 
                     eta = 0.06, 
                     max_depth = 8, 
                     subsample = 0.8,
                     eval_metric = "error",
                     watchlist = watchlist,
                     maximize = F,
                     nrounds = 500,
                     verbose = T, 
                     objective = "binary:logistic",
                     nthread = 48,
                     scale_pos_weight = scale_pos_weight)

# 保存訓練好的model，以及import之前訓練好的model
# xgb.save(xgbModel, "0.02_xgb.model")

# 模型特徵重要度
important = xgb.importance(xgbModel, feature_names = colnames(train_x))

# 保存特徵重要度表
# write.csv(important, "xg_importance.csv", row.names = F)

DF = head(arrange(important, desc(Gain)), 30)
DF
ggplot(DF , aes(x=reorder(Feature, Gain), y=Gain, fill=Gain)) +
  geom_bar(stat="identity", position="dodge") + coord_flip() +
  ylab("變數重要性") + xlab("") +
  scale_fill_gradient(low="red", high="blue") +
  theme(axis.title = element_text(size=18), axis.text.y = element_text(size=20))

#-------------- 預測結果 Train Data 訓練集資料------------------
# 將訓練集資料代入模型做預測
predictResult = predict(xgbModel, trainData) %>% as.data.frame() 
trainDataPred = as.data.frame(cbind(train_y, pre = predictResult$.))

# 查看Train分類機率在不同門檻值下的預測情況
summaryTrainCut = trainDataPred %>% 
  mutate(interval = cut(pre, breaks = c(0,0.5,0.55,0.6,0.65,1), include.lowest = T)) %>% 
  group_by(interval) %>%
  summarise(count = n(),
            accuracy_rate = mean(y))  # 將預測做短期存款的分類機率做區間查看其準確率
summaryTrainCut

library("pROC")
# xgboost_roc = roc(trainDataSave$y, trainDataPred$pre)
xgboost_roc = roc(data_balanced_under$y, trainDataPred$pre)
# xgboost_roc = roc(data.rose$y, trainDataPred$pre)
plot.roc(xgboost_roc, print.thres = "best", 
         print.thres.best.method = "youden", 
         print.auc = T,
         print.thres.col = 'red',
         legacy.axes=T)

cutoff = 0.069

#--------------- 計算混淆矩陣 ------------------
# 建立混淆矩陣
predictResult = as.numeric(trainDataPred %>% select(pre) > cutoff)  

confuseMatrix = table(train_y, predictResult, dnn = c("實際","預測")) # 將預測結果與實際結果做成混淆矩陣
confuseMatrix

TP = confuseMatrix[2,2]; FP = confuseMatrix[1,2]; FN = confuseMatrix[2,1]; TN = confuseMatrix[1,1]

# Precision, TP/(TP+FP)
precision = TP/(TP+FP)
precision 

# Recall, TP/(TP+FN)
train_recall = TP/(TP+FN)
train_recall

# 整體準確率(accuracy), accuracy = (TP+TN)/(TP+FP+FN+TN)
accuracy = (TP+TN)/(TP+FP+FN+TN)
accuracy

# 計算F1 Score, F1 Score = 2TP/(2TP+FN+FP)
train_F1_score = 2*TP/(2*TP+FN+FP)
train_F1_score

#-------------- 預測結果 Test Data 測試集資料------------------
# 將測試集資料代入模型做預測
predictResult = predict(xgbModel, testData) %>% as.data.frame()             
testDataPred = as.data.frame(cbind(test_y, pre = predictResult$.))  

xgboost_roc = roc(testDataSave$y, testDataPred$pre)
plot.roc(xgboost_roc, print.thres = "best", 
         print.thres.best.method = "youden", 
         print.auc = T,
         print.thres.col = 'red',
         legacy.axes=T)


# 查看測試集分類機率在不同門檻值下的預測情況
summaryTestCut = testDataPred %>% 
  mutate(interval = cut(pre, breaks = c(0,0.5,0.55,0.6,0.65,1), include.lowest = T)) %>% 
  group_by(interval) %>%
  summarise(count = n(), 
            accuracy_rate = mean(y))

summaryTestCut
#--------------- 計算混淆矩陣 ------------------
# 建立混淆矩陣
predictResult = as.numeric(testDataPred %>% select(pre) > cutoff)  

confuseMatrix = table(test_y, predictResult, dnn = c("實際","預測")) # 將預測結果與實際結果做成混淆矩陣
confuseMatrix

TP = confuseMatrix[2,2]; FP = confuseMatrix[1,2]; FN = confuseMatrix[2,1]; TN = confuseMatrix[1,1]

# Precision, TP/(TP+FP)
precision = TP/(TP+FP)
precision 

# Recall, TP/(TP+FN)
test_recall = TP/(TP+FN)
test_recall

# 整體準確率(accuracy), accuracy = (TP+TN)/(TP+FP+FN+TN)
accuracy = (TP+TN)/(TP+FP+FN+TN)
accuracy

# 計算F1 Score, F1 Score = 2TP/(2TP+FN+FP)
test_F1_score = 2*TP/(2*TP+FN+FP)
test_F1_score

################### 模型預測結果保存

# df_under = data.frame(
#  name = character(),
#  train_F1 = numeric(),
#  test_F1 = numeric(),
#  train_recall = numeric(),
#  test_recall = numeric()
# )

name = "p_0.05"
df_under = df_under %>% add_row(name = name, train_F1 = train_F1_score, test_F1 = test_F1_score, train_recall = train_recall, test_recall = test_recall)
write.csv(df_under, "./df_under.csv", row.names = F)