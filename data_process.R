rm(list = ls())
set.seed(1)
pacman::p_load(tidyverse, data.table, ranger, sqldf, d3heatmap, corrplot, xgboost, zoo, ROSE, sqldf, smotefamily, tibble)
options(scipen=999)

# 讀取 file1、file2 合併後之資料集 (file3)
load("~/0514/file3.rdata")
file3$y = file3$rs_prod_03
file3$rs_prod_03 = NULL
Data = file3

# 新增外部變數
outer = fread("./margin-short.csv")
outer$yyyymm = as.Date(outer$yyyymm)
# PTT 借貸版
load("~/PTT.rdata")
PTT$date = paste(PTT$date, "-01", sep = "")
PTT$yyyymm = as.Date(PTT$date)
PTT$date = NULL
# 合併資料集
Data = left_join(Data, outer)
Data = left_join(Data, PTT)


# 縮小樣本: 扣除教育程度 1 + 年齡 20 ~ 22 + 離島 + 職位 99
Data = Data %>% filter(edu != 1 & age > 22 & city != 999 & city != 20 & city != 21 & city != 22 & job != 99)
Data$Branch_Dist = NULL
Data$FLG_WEB = NULL
# 內部變數 : 總支付帳務金額、總一般消費金額、單筆消費金額(總消費金額/總消費次數)
total_bill = rowSums(Data[, c(19:24)]) # 總支付帳務金額
total_pmt = rowSums(Data[, c(25:43)]) # 總一般消費金額
total_pmt_times = rowSums(Data[, c(44:62)]) # 總消費次數
one_pmt = total_pmt / total_pmt_times # 單筆消費金額
a = is.nan(one_pmt)
one_pmt[a] = 0

# 新增 t - 1、t - 2、t - 3 欄位...
# 數值欄位名
num_data = c("srno" , "aum", "bill", "pmt", "dep", "AMT_Exchange", "CNT_Exchange")
# 前 20 名重要的連續型變數
num_data_top20 = c("srno" , "pmt_c10", "pmt_c19", "pmt_a01", "pmt_a02", "dep_c2", "dep_c4", "dep_a1", "bill_a6",
                   "aum01", "bill_a4", "bill_a5", "bill_a2", "dep_a4", "bill_a3", "dep_a3", "aum02", "pmt_a19", 
                   "pmt_a10", "dep_a2", "pmt_a06", "aum13", "pmt_a03", "dep_c3", "pmt_a11", "pmt_a16")
# 其他 DF
fac_df = select(Data, -contains(num_data))
# 前 20 連續型 DF
num_df = select(Data, contains(num_data_top20))

# t - 1
first_num_df = num_df %>% group_by(srno) %>% mutate_all(lag) %>% ungroup()
names(first_num_df)[-1] = paste("first.", names(first_num_df)[-1], sep = "")

# t - 2
second_num_df = first_num_df %>% group_by(srno) %>% mutate_all(lag) %>% ungroup()
names(second_num_df)[-1] = str_replace_all(names(second_num_df)[-1], "first.", "second.")

# t - 3
third_num_df = second_num_df %>% group_by(srno) %>% mutate_all(lag) %>% ungroup()
names(third_num_df)[-1] = str_replace_all(names(third_num_df)[-1], "second.", "third.")

# 合併數值 t ~ t - 3
first_num_df$srno = NULL; second_num_df$srno = NULL; third_num_df$srno = NULL
num_df = cbind(num_df, first_num_df, second_num_df, third_num_df)
# 合併資料集
Data = cbind(fac_df, num_df)

# 新增內部變數欄位
Data$total_bill = total_bill
Data$total_pmt = total_pmt
Data$total_pmt_times = total_pmt_times
Data$one_pmt = one_pmt

