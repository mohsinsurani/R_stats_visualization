#Generate input data set from 0 to 10 with sampling interval of 0.4
theta_1 = 2
theta_2 = 0.5
theta_3 = 0.3

x = seq(0, 10, 0.4)
y = theta_1 + theta_2*(x) + theta_3*(x^2)
y1 = y + rnorm(n = length(x), 0, 0.8)
plot(x, y1, type = "p")

# (XT X)
ones = matrix(data = 1, nrow = length(x), ncol = 1)
x1 = matrix(data = x, nrow = length(x), ncol = 1)
x2 = matrix(data = x^2, nrow = length(x), ncol = 1)
z = cbind(ones, x1, x2)

theta_bar = solve(t(z)%*%z)%*%t(z)%*%y
print(theta_bar)
theta = cbind(theta_1, theta_2, theta_3)
print(theta_bar-t(theta))
#yˆ = X
y_bar = z%*%theta_bar
print(y_bar)
plot(y_bar, x, col = "blue", type = "o")
lines(y1, x, col = "green")
error = y_bar - y1
norm(error,type = "2")^2

hist(error)
