data = read.csv("sample_data/Developed_6_Portfolios_ME_INV.csv")  # read csv file

x = rbind(c(1, 2, 3, 4))
B = cbind(c(4, 3, 2, 1))
d = cbind(c(1, 1, 1, 1))
I = 1

e = .5

y = x %*% (B + d * I) + e