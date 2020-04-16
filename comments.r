# Comments

# Source
# http://centaur.reading.ac.uk/79661/1/final-pdf-Antoch-et-al.pd

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

