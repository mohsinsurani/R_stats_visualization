# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

hello <- function() {
  print("Hello, world!")
  print(4 * (5 + 23))

  pi
}

lab2ex = function() {
  seq(0,2*pi,0.1)

  x = seq(0, 2*pi, by=0.1)
  print(x)
  y = sin(5 * x)
  plot(x, y, type = "l", xlab = "Time", xlim = c(0, 6), ylim = c(-1, 1))
}

lab3ex = function() {
  k = seq(0, 10000, by=1)
  print(k)
  sum = 0
  for(i in k) sum = sum + (-1)^i/(2*i+1)
  print((sum*4))
}
