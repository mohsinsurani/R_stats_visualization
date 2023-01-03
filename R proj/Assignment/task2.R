X = read.csv("X.csv")
Y = read.csv("Y.csv")
time = read.csv("time.csv")

X = data.frame(X)
Y = data.frame(Y)
t = data.frame(time)

df = cbind(X, Y, t)
mdf = data.matrix(df)

m1 = function(x1, x2, theta_bias) {
  return(cbind(x1 ^ 3, x1 ^ 5, x2, theta_bias))
}

m2 = function(x1, x2, theta_bias) {
  return(cbind(x1, x2, theta_bias))
}

m3 = function(x1, x2, theta_bias) {
  return(cbind(x1, x1 ^ 2, x1 ^ 4, x2, theta_bias))
}

m4 = function(x1, x2, theta_bias) {
  return(cbind(x1, x1 ^ 2, x1 ^ 3, x1 ^ 5, x2, theta_bias))
}

m5 = function(x1, x2, theta_bias) {
  return(cbind(x1, x1 ^ 3, x1 ^ 4, x2, theta_bias))
}

getThetaHat = function(Q, R) {
  print(t(Q)%*%Q)

  return(solve(t(Q)%*%Q)%*%t(Q)%*%R)
}

getY_hat = function(i, theta_hat) {
  return(i %*% theta_hat)
}

getRSS = function(y, Y_hat) {
  return(sum((y - Y_hat) ^ 2))
}

theta_bias = matrix(data = 1,
                    nrow = length(x1),
                    ncol = 1)
x1 = mdf[, 1]
x2 = mdf[, 2]

listMatrix = list(m1(x1, x2, theta_bias), m2(x1, x2, theta_bias), m3(x1, x2, theta_bias), m4(x1, x2, theta_bias), m5(x1, x2, theta_bias))
listRSS = list()
listThetaHat = list()
listYHat = list()

for (i in listMatrix) {
  theta_hat = getThetaHat(i, mdf[, 3])
  theta_hat
  Y_hat = getY_hat(i, theta_hat)
  Y_hat
  RSS = getRSS(mdf[, 3], Y_hat)
  RSS
  listThetaHat = append(listThetaHat, list(theta_hat))
  listYHat = append(listYHat, list(Y_hat))
  listRSS = append(listRSS, RSS)
}
print("Task 2.1")
print("All theta_hat values each containing individual theta values are") #each index contains values of theta_hat of that model
print(listThetaHat) #each index contains values of theta_hat of that model

print("Task 2.2")
print("All RSS values for each model is") #each index contains values of theta_hat of that model
print(unlist(listRSS)) #each index contains values of RSS of that model

print("All Y-hat values for each model is")
print(listYHat) #each index contains values of y_hat of that model


log_like_fun = function(RSS, n) {
  coVariance = RSS / (n - 1)
  a1 = -n * log(2 * pi) / 2
  a2 = n / 2 * log(coVariance)
  a3 = RSS / (2 * coVariance)

  return(a1 - a2 - a3)
}

logLikeFuncList = list()

for (i in listRSS) {
  logFn = log_like_fun(i, length(mdf[, 3]))
  logLikeFuncList = append(logLikeFuncList, logFn)
}

print(logLikeFuncList) #each index contains values of y_hat of that model

print("Task 2.3")
print("Each index contains values of logLikeFuncList of that model is ")
print(unlist(logLikeFuncList)) #each index contains values of logLikeFuncList of that model

calcAIC = function(k, log_like) {
  return(2*k - 2*log_like)
}

calcBIC = function(k, log_like, n) {
  return(k*log(n) - 2*log_like)
}

estParams = list(4,3,5,6,5)
listAIC = list()
listBIC = list()

for(i in 1:length(logLikeFuncList)){
  log_like = logLikeFuncList[[i]]
  print(log_like)
  k = ncol(listMatrix[[i]])
  n = nrow(listMatrix[[i]])
  listAIC = append(listAIC, calcAIC(k, log_like))
  listBIC = append(listBIC, calcBIC(k, log_like, n))
}

print("Task 2.4")
print("Each index contains values of AIC of that model is ")
print(unlist(listAIC)) #each index contains values of RSS of that model
print("Each index contains values of BIC of that model is ")
print(unlist(listBIC)) #each index contains values of RSS of that model

plotDistOfEachModel = function() {
  print("Task 2.5")
  print("Plotting Each Distribution from list of the matrix of y_hat as well as hist")

  for(i in (1:length(listMatrix))) {#listMatrix = List of y_hat calculated above
    print(i)
    error = mdf[, 3] - listYHat[[i]]
    which_model = NULL
    if(i == 1) {
      which_model = "1st"
    } else if (i == 2) {
      which_model = "2nd"
    } else if (i == 3) {
      which_model = "3rd"
    } else {
      which_model = sprintf("%ith", i)
    }
    mainTitle = sprintf("Distributon error of %s model", which_model)
    qqnorm(error, main = mainTitle)
    qqline(error, lwd = 3, col = 'red')

    print(norm(error,type = "2")^2)

    hist(error,xlab = "Residual values", ylab = "Frequency", freq=FALSE, main = sprintf("Histogram of error distributon of %s model", which_model))
    curve(dnorm(x,mean=mean(error),sd=sd(error)), add=TRUE,col="red")
  }
}

set.seed(111)
print("Splitting data: Training = 70%, Tesitng = 30%")
smp_size <- floor(0.70 * nrow(mdf))
ind <- sample(seq_len(nrow(mdf)), size = smp_size)

computeModel = function() {

  m_trainDf <- mdf[ind, ]
  #m_trainDf = data.matrix(trainDf)

  a = m_trainDf[, 1]
  b = m_trainDf[, 2]
  theta_bias = matrix(data = 1,
                      nrow = length(a),
                      ncol = 1)

  train = m3(a, b, theta_bias) #reusing the function for 3rd model
  train_theta_hat = getThetaHat(train, m_trainDf[, 3])
  print("Task 2.7.1")

  print("Theta hat value of training data is")
  print(train_theta_hat)

  y_hat = getY_hat(train, train_theta_hat)
  print("y hat value of training data is")
  print(y_hat)

  RSS = getRSS(m_trainDf[, 3], y_hat)
  print("RSS value of training data is")
  print(RSS)

  plot(m_trainDf[, 1],m_trainDf[, 3], col = "blue", type = "p", xlab = "Input signal x1", ylab = "Output signal y of training data", main = "Training data Distribution")
  points(m_trainDf[, 1],y_hat, col = "green", type = "p")
}

computePred = function() {
  m_testDf <- mdf[-ind, ]

  c = m_testDf[, 1]
  d = m_testDf[, 2]
  theta_bias = matrix(data = 1,
                      nrow = length(c),
                      ncol = 1)

  test = m3(c, d, theta_bias)
  test_theta_hat = getThetaHat(test, m_testDf[, 3])

  y_hat_test = getY_hat(test, test_theta_hat)
  print("Task 2.7.2")

  print("y_hat_test value of testing data is")
  print(y_hat_test)

  rss = getRSS(m_testDf[, 3], y_hat_test)
  print("RSS value of testing data is")
  print(rss)

  plot(m_testDf[, 1],m_testDf[, 3], col = "blue", type = "p", xlab = "Input signal x1", ylab = "Output signal y of test data", main = "Test data Distribution")
    points(m_testDf[, 1],y_hat_test, col = "green", type = "p")
}

plotConfInterval = function() {
  print("Task 2.7.3")

  mConf = test

  var_y_hat = matrix(0 , nrow(mConf) , 1)
  number_of_parameters = length(listThetaHat[[3]])

  error = (mConf[, 3] - y_hat_test)
  SSE = norm(error, type = "2")^2
  sigma_2 = SSE/( nrow(mConf) - 1 ) # error variance sigma^2
  cov_thetaHat = sigma_2 * (solve(t(mConf) %*% mConf))

  for( i in 1:nrow(mConf)) {
    X_i = matrix(mConf[i] , 1 , number_of_parameters)
    invers = solve(t(mConf) %*% mConf)
    mid = X_i %*% invers
    var_y_hat[i,1] = sigma_2 * (mid %*% t(X_i) )
  }

  CI = 2 * sqrt(var_y_hat)
  print("CI value of testing data is")
  print(CI)

  time_test = m_testDf[, 4]

  library(ggplot2)
  output_col = "greenyellow"
  element_color = "Black"
  error_bar_col = "goldenrod"

  theme_sel = theme(
    axis.title.x = element_text(color = element_color),
    axis.title.y = element_text(color = element_color)
  ) + theme_minimal()

  point_dots = ggplot(mapping = aes(x= time_test, y = y_hat_test)) +
    geom_point() +
    geom_line() +
    theme_sel + xlab("Time t of test data") + ylab("Output y_hat")

  print(point_dots)

  point_dots + geom_errorbar(mapping =aes(ymin = m_testDf[,3]-CI, ymax = m_testDf[,3]+CI), color=error_bar_col)+
    geom_point(aes(m_testDf[,4], y = m_testDf[,3]), color=output_col) +
    ggtitle("95% Confidence Interval with error bars") +
    theme_sel
}

plot3 = function() {
  print("Task 3")

  x = listThetaHat[3]
  vals = head(sort(x[[1]], decreasing=FALSE), length(listThetaHat[[3]]))
  three_min = vals[1:3]
  two_max = vals[4:5]
  varyingNum = 0.5
  totalVal = 1111

  print("Task 3.1")
  print("the 2 parameters with largest
absolute valuesin your least squares estimation (Task 2.1) of the selected model are")
  print(two_max)
  print("And fixing remaing values are")
  print(three_min)


  val_range_x = c(vals[4] - varyingNum * vals[4], vals[4] + varyingNum * vals[4])
  val_range_y = c(vals[5] - varyingNum * vals[5], vals[5] + varyingNum * vals[5])

  print("Task 3.2")
  print("fixing range of first variable in between is")
  print(val_range_x)
  print("fixing range of second variable in between is")
  print(val_range_y)

  set.seed(200)
  range_x_uni = runif(totalVal, val_range_x[1], val_range_x[2])
  range_y_uni = runif(totalVal, val_range_y[1], val_range_y[2])

  print("Range of first variable in 1111 value is")
  print(range_x_uni)
  print("Range of second variable in 1111 value is")
  print(range_y_uni)

  x_val_disp = sprintf("Range of uniform values of %f", vals[4])
  y_val_disp = sprintf("Range of uniform values of %f", vals[5])

  print("Task 3.3")
  print("Drawing Uniform distribution from samples")
  plot(range_x_uni, range_y_uni, type = 'p', lwd = 1, col='blue',
       xlab=x_val_disp, ylab=y_val_disp, main='Uniform Distribution Range plot')

  listDeltaEst = list()
  lista_est = list()
  lista_bst = list()
  lista_error = list()

  print("thresold/tolerance value set is 10.5")
  for (i in 1:totalVal) {

    estTheta_hat = matrix(c(range_x_uni[i], three_min[3], three_min[1], three_min[2], range_y_uni[i]))
    y_hat_est = listMatrix[[3]] %*%  estTheta_hat
    error_est = mdf[, 3] - y_hat_est
    delta_est = getRSS(mdf[, 3], y_hat_est) / length(error_est)
    listDeltaEst = append(listDeltaEst, delta_est[1])

    if(delta_est < 10.5) {
      lista_est = append(lista_est, range_x_uni[i])
      lista_bst = append(lista_bst, range_y_uni[i])
    }
  }


  print("drawing delta distribution")
  plot(unlist(listDeltaEst), xlab = "Index of Delta list", ylab = "Values of Delta", main = "Delta points", col = "steelblue")

  library(LaplacesDemon)

  range1 <- unlist(lista_est)
  range2 <- unlist(lista_bst)

  print("task 3.4")
  print("drawing joint posterior distribution for those 2 parameters")
  joint.density.plot(range1, range2, Title="Joint Posterior Distribution",
                     contour=TRUE, color=FALSE, Trace=NULL)

  library(devtools)
  library(nimbleCarbon)

  print("drawing marginal posterior distribution for those 2 parameters")
  postHPDplot(
    range1,
    prob = 0.9,
    bw = "SJ",
    hpd.col = adjustcolor("steelblue", alpha = 0.4),
    line.col = "darkgrey",
    rnd = 2,
    HPD = TRUE,
    show.hpd.val = TRUE
  )

  postHPDplot(
    range2,
    prob = 0.9,
    bw = "SJ",
    hpd.col = adjustcolor("steelblue", alpha = 0.4),
    line.col = "darkgrey",
    rnd = 2,
    HPD = TRUE,
    show.hpd.val = TRUE
  )
}



