y1 = read.csv("Lab5_Exe1_data.csv", header = F)
print(y1)
y = data.matrix(y1)
hist(y, breaks=10)
mode = unique(y)
mode = mode[which.max(tabulate(match(y, mode)))]
med = median(y)
mn = mean(y)
abline(v=mn,col="blue",lwd=2)
abline(v=med,col="orange",lwd=2)
abline(v=mode,col="red",lwd=2)

sample_min = min(y)
sample_max = max(y)
sample_range = range(sample_max, sample_min)
sample_variance = var(y)
qqnorm(y)

subEq = y - mean(y)
n = length(y)
form = (sum(subEq^3)*n^0.5)/(sum(subEq^2))^(3/2)

library(moments)
skewness(y)
