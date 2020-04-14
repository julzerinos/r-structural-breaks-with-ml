library(readr)

Developed_3_Factors <- read_csv("sample_data/Developed_3_Factors.csv", 
                                col_types = cols(t = col_skip()))


# Initial break-point
t0 = '200001'

# matrix X
X <- cbind(Developed_3_Factors$`Mkt-RF`,Developed_3_Factors$SMB,Developed_3_Factors$HML,Developed_3_Factors$RF)

# matrix X\


y =  