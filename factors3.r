library(readr)

Developed_3_Factors <- read_csv("sample_data/Developed_3_Factors.csv", 
                                col_types = cols(t = col_skip()))






# OLS Estimator
B = solve(t(X) %*% X) %*% t(X) #


# break point estimator
y = X[1,] %*% B
