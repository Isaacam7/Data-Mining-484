library(performance)
library(lme4)
library(glmnet)
library(dplyr)
library(tidyr)

schooldata <- as.data.frame(read.csv('schooldata.csv'))
schooldata <- tidyr::drop_na(schooldata)
schooldata$STATE <- factor(schooldata$STATE)
numericcols <-  names(schooldata)[sapply(schooldata, is.numeric)]
m_numericcols <- numericcols[!(numericcols %in% c("NCESID", "YRDATA", "STATE",'Fall_membership','ALL_RLA00pctprof'))]
r_numericcols <- numericcols[!(numericcols %in% c("NCESID", "YRDATA", "STATE",'Fall_membership','ALL_MTH00pctprof'))]

#feature selection with lasso regression
varmatrix <- model.matrix(ALL_MTH00pctprof~.-1,data=schooldata[, m_numericcols])
response <- schooldata$ALL_MTH00pctprof

cv.lasso <- cv.glmnet(varmatrix, response, alpha=1)

#select only the variables that kept a significant coefficient
coefdf <- data.frame(varnames = rownames(coef(cv.lasso)),cf = coef(cv.lasso)[1:length(coef(cv.lasso))])
df_nonzero <- coefdf[abs(coefdf$cf) >= 1, ]

m_numericcols <- df_nonzero$varnames
m_numericcols <- m_numericcols[!(m_numericcols %in% c("(Intercept)"))]

#create formula for variables
mymathformula <- as.formula(paste('ALL_MTH00pctprof',"~",paste('1','(1|STATE)',paste(m_numericcols, collapse = "+"), paste("(0 +", m_numericcols, "|| STATE)", collapse = " + "), sep = " + ")))
math_model <- lmer(mymathformula, REML=TRUE, data = schooldata)
#saveRDS(math_model,file="math_model.rds")

#do the same for RLA
rlavarmatrix <- model.matrix(ALL_RLA00pctprof~.-1,data=schooldata[, r_numericcols])
rlaresponse <- schooldata$ALL_RLA00pctprof

cv.lasso <- cv.glmnet(rlavarmatrix, rlaresponse, alpha=1)

coefdf <- data.frame(varnames = rownames(coef(cv.lasso)),cf = coef(cv.lasso)[1:length(coef(cv.lasso))])
df_nonzero <- coefdf[abs(coefdf$cf) >= 1, ]

r_numericcols <- df_nonzero$varnames
r_numericcols <- r_numericcols[!(r_numericcols %in% c("(Intercept)"))]

myrlaformula <- as.formula(paste('ALL_RLA00pctprof',"~",paste('1','(1|STATE)',paste(r_numericcols, collapse = "+"), paste("(0 +", r_numericcols, "|| STATE)", collapse = " + "), sep = " + ")))
rla_model <- lmer(myrlaformula, REML=TRUE, data = schooldata)

#saveRDS(rla_model,file="rla_model.rds")

library(performance)

#math model contains no per student data because of time constraints
math_model <- readRDS("math_model.rds")
rla_model <- readRDS("rla_model.rds")


print("rla metrics:")
model_performance(rla_model)
fixedrlaform <- as.formula(paste('ALL_RLA00pctprof',"~",paste('1',paste(r_numericcols, collapse = "+"), sep = " + ")))
lr_rla_model <- lm(fixedrlaform,data=schooldata)
model_performance(lr_rla_model)


print("math metrics:")
model_performance(math_model)
fixedmathform <- as.formula(paste('ALL_MTH00pctprof',"~",paste('1',paste(m_numericcols, collapse = "+"), sep = " + ")))
#Linear model contains per student data
lr_mth_model <- lm(fixedmathform,data=schooldata)
model_performance(lr_mth_model)
