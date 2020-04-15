library(readr)

Developed_3_Factors <- read_csv("sample_data/Developed_3_Factors.csv", 
                                col_types = cols(t = col_skip()))

tresh <- floor(dim(Developed_3_Factors)[1] * 0.75)
  
train <- Developed_3_Factors[1:tresh,1:4]
test <- Developed_3_Factors[tresh:dim(Developed_3_Factors)[1],1:4]




# some initial t0 in train
t0 <- dim(train)[1]/2+10

# extract data to matrix form
# panels <- cbind(Developed_3_Factors$`Mkt-RF`,Developed_3_Factors$SMB,Developed_3_Factors$HML,Developed_3_Factors$RF)
panels <- cbind(train$`Mkt-RF`,train$SMB,train$HML,train$RF)

# OLS Estimator
B = do.call("rbind", lapply(1:4, function(x) sample(c(1, rep(0, 3)), 1))) # solve(t(panels) %*% panels) %*% t(panels)

# initial break point estimator
Y = panels %*% B
Y

# get matrixes X and X_hat
X <-panels[1:t0,1:4]
X_t <- panels[t0:dim(panels)[1],1:4]

# Recalculate OLS Estimators
B_hat = solve(t(X) %*% X) %*% t(X) %*% Y[1:t0,1]
B_tilde = solve(t(X_t) %*% X_t) %*% t(X_t) %*% Y[t0:dim(panels)[1],1]
