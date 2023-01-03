plotConfInterval = function() {
  mConf = test # delete column 2

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

  time_test = m_testDf[, 4]
  plot(time_test, y_hat_test , type = "point", xlab = "Time t of test data", ylab = "Output Y_hat of test data", main = "95% Confidence Interval")
  lines(time_test, y_hat_test)
  segments(time_test, y_hat_test-CI, time_test, y_hat_test+CI)
}
