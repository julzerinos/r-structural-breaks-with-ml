mydata = read.csv("sample_data/Developed_6_Portfolios_ME_INV.csv")  # read csv file

# y(i, t) = [x(i, t) ^ t] x (B(i) + d(i) * I(t >= t0)) + e(i, t)
#
# Where 
#
# 1 <= i <= N
#   panels, N goes to inf
#
# 1 <= t <= T
#   time, fixed
#
# x = ( x(i, t)[1], x(i, t)[2], ... x(i, t)[l] )^T
#   Vector of explanatory variables (random or nonrand design points)
#
# B (vector of R^d)
#   Unknown regression vector
#
# e(i, t)
#   Cross-sectionally correlated errors
#
# I(bool)
#   Indicator function (1 if true, 0 if false)
#
# What we are observing
#   in ith panel, regression parameters changes as
#   B(i+1) = B(i) + d(i)
#   at unknown t0 (called change point or break point)

x = rbind(c(1, 2, 3, 4))
B = cbind(c(4, 3, 2, 1))
d = cbind(c(1, 1, 1, 1))
I = 1



e = .5

y = x %*% (B + d * I) + e
y
