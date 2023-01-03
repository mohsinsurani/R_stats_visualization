library(MASS)

lab2ex = function() {
  val = c(1/6, 1/8, 1/4, 1/6, 1/8, 1/6)
  matrix = matrix(val, ncol = 3, nrow = 2)
  fractions(matrix)
  fractions(matrix[1,1])
  fractions(matrix[1,2])
  total = fractions(matrix[1,1]) + fractions(matrix[1,2])
  print(total)
  sum(matrix)
  px = sum(fractions(matrix[1,1]) + fractions(matrix[1,2]) + fractions(matrix[1,3]))
  print(px)


  q3 = fractions(matrix[1,2] + matrix[2, 2])

  #q4Write a code to compute P(X = 1|Y = 0)
  q4 = matrix[2,1]/(matrix[1,1] + matrix[2,1])
  fractions(q4)
}

library(cubature)
integra = function(x) {
  f = function(x) {4 * x[1] * x[2]}
  adaptIntegrate(f, lowerLimit = c(0, 0), upperLimit = c(1, 1))
}

distri = function() {
  dbinom(1,10,0.2) #0.2684355 provavility of one answer correct
  dbinom(10,10,0.2) # 1.024e-07 provavility of all answer correct

  #q1-lab4
#find the probability that James fails the test. A mark of 4 or less is
  #considered a failure.

  dbinom(4,10,0.8)
}

