
library("tidyverse")
library("qgcomp")

##以性别分层为例 其他同理
##1.性别分层--------------------
### 加载数据
load("FEM分析数据.Rdata")
### 对各暴露采用了多种“健康”的定义方式
smoke <- c("smoke")
drink <- c("drink")
diet <- c("diet")
activity <- c("activity")
sleep <- c("sleep")
o_var <- c("kdm_advance")

##按性别拆分数据
id_male <- data_FEM[data_FEM$sex == 1 & data_FEM$wave == 2,]$ID
data_male <- data_FEM %>% filter(ID %in% id_male == T) 
data_female <- data_FEM %>% filter(ID %in% id_male == F) 

###1.1 男性------------
### 建立存批量结果的list
library(plm)
confound.var <- c("age","base_age","region","ethnic","education","occuption","marital", "energy",###调整当期年龄
                  "depression","anxiety","beverage","diet_supp","DB_report","CVD_report","Cancer_report")
### 建立存批量结果的list
Result_all <- NULL
Result_HLI <- NULL
Result_HLI_c <- NULL
Result_prior <- NULL

### plm分析数据
exps <- c(smoke,drink,diet,activity,sleep)
a_data <- pdata.frame(data_male,index=c("ID","wave"))
### 多暴露同时分析 #####
formula <- as.formula(paste(o_var,"~",paste(exps,collapse = "+"),"+", paste(confound.var,collapse= "+")))
### 建立双重固定效应模型
model_within <- plm(formula,data = a_data,effect = "twoways",model = "within")
outcome <- coef(summary(model_within))
### 建立存结果的表
result <- data.frame(outcome = o_var,smoke = smoke,drink = drink,diet = diet,
                     activity = activity,sleep = sleep,exps = rep(NA,5),
                     estimate = NA,lowbound = NA,upbound = NA,
                     n = NA,se = NA,z = NA,P = NA)
### 存结果
result$exps <- row.names(outcome)[1:5]
result$estimate <- outcome[c(1:5),1]
result$lowbound <- outcome[c(1:5),1]-1.96*outcome[c(1:5),2]
result$upbound <- outcome[c(1:5),1]+1.96*outcome[c(1:5),2]
result$n <- nrow(a_data)/2
result$se <- outcome[c(1:5),2]
result$z <- outcome[c(1:5),3]
result$P <- outcome[c(1:5),4]
### 合并结果
Result_all <- rbind(Result_all,result)

### HLI综合分析#####
formula_HLI <- as.formula(paste(o_var,"~ HLI +", paste(confound.var,collapse= "+")))
### 建立个体固定效应模型
model_HLI <- plm(formula_HLI,data = a_data,effect = "twoways",model = "within")
outcome_HLI <- coef(summary(model_HLI))
### 建立存结果的表
result1 <- data.frame(outcome = o_var,smoke = smoke,drink = drink,diet = diet,
                      activity = activity,sleep = sleep,
                      estimate = NA,lowbound = NA,upbound = NA,
                      n = NA,se = NA,z = NA,P = NA)
### 存结果
result1$estimate <- outcome_HLI[1,1]
result1$lowbound <- outcome_HLI[1,1]-1.96*outcome_HLI[1,2]
result1$upbound <- outcome_HLI[1,1]+1.96*outcome_HLI[1,2]
result1$n <- nrow(a_data)
result1$se <- outcome_HLI[1,2]
result1$z <- outcome_HLI[1,3]
result1$P <- outcome_HLI[1,4]
### 合并结果
Result_HLI <- rbind(Result_HLI,result1)

### HLI_c综合分析#####
formula_HLI_c <- as.formula(paste(o_var,"~ HLI_c +", paste(confound.var,collapse= "+")))
model_HLI_c <- plm(formula_HLI_c,data = a_data,effect = "twoways",model = "within")
outcome_HLI_c <- coef(summary(model_HLI_c))
### 建立存结果的表
result2 <- data.frame(outcome = o_var,smoke = smoke,drink = drink,diet = diet,
                      activity = activity,sleep = sleep,
                      estimate = NA,lowbound = NA,upbound = NA,
                      n = NA,se = NA,z = NA,P = NA)
### 存结果
result2$estimate <- outcome_HLI_c[1,1]
result2$lowbound <- outcome_HLI_c[1,1]-1.96*outcome_HLI_c[1,2]
result2$upbound <- outcome_HLI_c[1,1]+1.96*outcome_HLI_c[1,2]
result2$n <- nrow(a_data)
result2$se <- outcome_HLI_c[1,2]
result2$z <- outcome_HLI_c[1,3]
result2$P <- outcome_HLI_c[1,4]
### 合并结果
Result_HLI_c <- rbind(Result_HLI_c,result2)

### QGC分析#####
### 手动构造差分数据再建模
### 排序数据
mf <- model.frame(formula, data = a_data)
### 生成建模的数据矩阵
modmat <- model.matrix(formula, data = mf, model = "within")
### 划分两期数据并作差——得到差分数据
singular <- seq(from=1,to=nrow(mf),by=2)
dual <- singular + 1
wave_1 <- modmat[singular,] %>% as.data.frame() %>%
  cbind(.,mf[singular,o_var])
wave_2 <- modmat[dual,] %>% as.data.frame() %>%
  cbind(.,mf[dual,o_var])
data_FEM <- wave_2 - wave_1
colnames(data_FEM)[ncol(data_FEM)] <- o_var
data_FEM <- data_FEM[,-1]
### 利用差分数据建模qgc
regressor <- colnames(data_FEM)[-ncol(data_FEM)]
formula1 <- as.formula(paste(o_var,"~",paste(regressor,collapse = "+")))
qgc_model <- qgcomp(formula1,expnms = exps, data = data_FEM, q = NULL)

## 结果表
result3 <- data.frame(outcome = o_var,smoke = smoke,drink = drink,diet = diet,
                      activity = activity,sleep = sleep,psi = coef(summary(qgc_model))[2,1],
                      low_CI = coef(summary(qgc_model))[2,3],up_CI = coef(summary(qgc_model))[2,4],
                      p = coef(summary(qgc_model))[2,5],pos = qgc_model[["pos.psi"]],neg = qgc_model[["neg.psi"]],
                      exps = rep(NA,5),estimate = NA,weight = NA)
result3$exps <- rownames(as.data.frame(qgc_model[["fit"]][["coefficients"]][2:6]))
result3$estimate <- qgc_model[["fit"]][["coefficients"]][2:6]
result3$weight <- qgc_model[["neg.weights"]]
Result_prior <- rbind(Result_prior,result3)

###结果整理---------
result_FEM <- select(Result_all,exps,estimate,lowbound,upbound)
HLI_FEM <- select(Result_HLI,estimate,lowbound,upbound)
HLI_c_FEM <- select(Result_HLI_c,estimate,lowbound,upbound)
result_FEM[6,] <- c("HLI",HLI_FEM[1,])
result_FEM[7,] <- c("HLI_c",HLI_c_FEM[1,])

result_FEM <- result_FEM %>%
  mutate_at(.vars = c("estimate","lowbound","upbound"),.funs = as.numeric)%>%
  mutate(CI = paste0(sprintf("%0.2f",estimate),
                     "(",sprintf("%0.2f",lowbound),
                     ",",sprintf("%0.2f",upbound),")"))

###1.2 女性------------
confound.var <- c("age","base_age","region","ethnic","education","occuption","marital", "energy",###调整当期年龄
                  "depression","anxiety","menopause","beverage","diet_supp","DB_report","CVD_report","Cancer_report")
### 建立存批量结果的list
Result_all <- NULL
Result_HLI <- NULL
Result_HLI_c <- NULL
Result_prior <- NULL

### plm分析数据
exps <- c(smoke,drink,diet,activity,sleep)
a_data <- pdata.frame(data_female,index=c("ID","wave"))
### 多暴露同时分析 #####
formula <- as.formula(paste(o_var,"~",paste(exps,collapse = "+"),"+", paste(confound.var,collapse= "+")))
### 建立双重固定效应模型
model_within <- plm(formula,data = a_data,effect = "twoways",model = "within")
outcome <- coef(summary(model_within))
### 建立存结果的表
result <- data.frame(outcome = o_var,smoke = smoke,drink = drink,diet = diet,
                     activity = activity,sleep = sleep,exps = rep(NA,5),
                     estimate = NA,lowbound = NA,upbound = NA,
                     n = NA,se = NA,z = NA,P = NA)
### 存结果
result$exps <- row.names(outcome)[1:5]
result$estimate <- outcome[c(1:5),1]
result$lowbound <- outcome[c(1:5),1]-1.96*outcome[c(1:5),2]
result$upbound <- outcome[c(1:5),1]+1.96*outcome[c(1:5),2]
result$n <- nrow(a_data)/2
result$se <- outcome[c(1:5),2]
result$z <- outcome[c(1:5),3]
result$P <- outcome[c(1:5),4]
### 合并结果
Result_all <- rbind(Result_all,result)

### HLI综合分析#####
formula_HLI <- as.formula(paste(o_var,"~ HLI +", paste(confound.var,collapse= "+")))
### 建立个体固定效应模型
model_HLI <- plm(formula_HLI,data = a_data,effect = "twoways",model = "within")
outcome_HLI <- coef(summary(model_HLI))
### 建立存结果的表
result1 <- data.frame(outcome = o_var,smoke = smoke,drink = drink,diet = diet,
                      activity = activity,sleep = sleep,
                      estimate = NA,lowbound = NA,upbound = NA,
                      n = NA,se = NA,z = NA,P = NA)
### 存结果
result1$estimate <- outcome_HLI[1,1]
result1$lowbound <- outcome_HLI[1,1]-1.96*outcome_HLI[1,2]
result1$upbound <- outcome_HLI[1,1]+1.96*outcome_HLI[1,2]
result1$n <- nrow(a_data)
result1$se <- outcome_HLI[1,2]
result1$z <- outcome_HLI[1,3]
result1$P <- outcome_HLI[1,4]
### 合并结果
Result_HLI <- rbind(Result_HLI,result1)

### HLI_c综合分析#####
formula_HLI_c <- as.formula(paste(o_var,"~ HLI_c +", paste(confound.var,collapse= "+")))
model_HLI_c <- plm(formula_HLI_c,data = a_data,effect = "twoways",model = "within")
outcome_HLI_c <- coef(summary(model_HLI_c))
### 建立存结果的表
result2 <- data.frame(outcome = o_var,smoke = smoke,drink = drink,diet = diet,
                      activity = activity,sleep = sleep,
                      estimate = NA,lowbound = NA,upbound = NA,
                      n = NA,se = NA,z = NA,P = NA)
### 存结果
result2$estimate <- outcome_HLI_c[1,1]
result2$lowbound <- outcome_HLI_c[1,1]-1.96*outcome_HLI_c[1,2]
result2$upbound <- outcome_HLI_c[1,1]+1.96*outcome_HLI_c[1,2]
result2$n <- nrow(a_data)
result2$se <- outcome_HLI_c[1,2]
result2$z <- outcome_HLI_c[1,3]
result2$P <- outcome_HLI_c[1,4]
### 合并结果
Result_HLI_c <- rbind(Result_HLI_c,result2)

### QGC分析#####
### 手动构造差分数据再建模
### 排序数据
mf <- model.frame(formula, data = a_data)
### 生成建模的数据矩阵
modmat <- model.matrix(formula, data = mf, model = "within")
### 划分两期数据并作差——得到差分数据
singular <- seq(from=1,to=nrow(mf),by=2)
dual <- singular + 1
wave_1 <- modmat[singular,] %>% as.data.frame() %>%
  cbind(.,mf[singular,o_var])
wave_2 <- modmat[dual,] %>% as.data.frame() %>%
  cbind(.,mf[dual,o_var])
data_FEM <- wave_2 - wave_1
colnames(data_FEM)[ncol(data_FEM)] <- o_var
data_FEM <- data_FEM[,-1]
### 利用差分数据建模qgc
regressor <- colnames(data_FEM)[-ncol(data_FEM)]
formula1 <- as.formula(paste(o_var,"~",paste(regressor,collapse = "+")))
qgc_model <- qgcomp(formula1,expnms = exps, data = data_FEM, q = NULL)

## 结果表
result3 <- data.frame(outcome = o_var,smoke = smoke,drink = drink,diet = diet,
                      activity = activity,sleep = sleep,psi = coef(summary(qgc_model))[2,1],
                      low_CI = coef(summary(qgc_model))[2,3],up_CI = coef(summary(qgc_model))[2,4],
                      p = coef(summary(qgc_model))[2,5],pos = qgc_model[["pos.psi"]],neg = qgc_model[["neg.psi"]],
                      exps = rep(NA,5),estimate = NA,weight = NA)
result3$exps <- rownames(as.data.frame(qgc_model[["fit"]][["coefficients"]][2:6]))
result3$estimate <- qgc_model[["fit"]][["coefficients"]][2:6]
result3$weight <- qgc_model[["neg.weights"]]
Result_prior <- rbind(Result_prior,result3)
###结果整理---------
result_FEM <- select(Result_all,exps,estimate,lowbound,upbound)
HLI_FEM <- select(Result_HLI,estimate,lowbound,upbound)
HLI_c_FEM <- select(Result_HLI_c,estimate,lowbound,upbound)
result_FEM[6,] <- c("HLI",HLI_FEM[1,])
result_FEM[7,] <- c("HLI_c",HLI_c_FEM[1,])

result_FEM <- result_FEM %>%
  mutate_at(.vars = c("estimate","lowbound","upbound"),.funs = as.numeric)%>%
  mutate(CI = paste0(sprintf("%0.2f",estimate),
                     "(",sprintf("%0.2f",lowbound),
                     ",",sprintf("%0.2f",upbound),")"))

