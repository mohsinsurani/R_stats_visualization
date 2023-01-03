library(ggplot2)

X = read.csv("X.csv")
Y = read.csv("Y.csv")
time = read.csv("time.csv")

X = data.frame(X)
Y = data.frame(Y)
time = data.frame(time)

df = cbind(X, Y, time)
print(df)

dfNeut = df[df$x2==0,]
dfEmo = df[df$x2==1,]

isBalanced = nrow(dfNeut) == nrow(dfEmo)
print(nrow(dfNeut) == nrow(dfEmo))
sprintf("The dataset is balanced as rows equality is %s", isBalanced)

mNeut = data.matrix(dfNeut)
mEmo = data.matrix(dfEmo)

plotTimeSeries = function() {
  print("task 1.1")

  str1 = "Input signal x1"
  str2 = "Time t in seconds"
  str3 = "Output signal y"


 tp1 = ggplot() +
    geom_line(data=dfNeut, aes(x=time, y=x1), color = "steelblue") +
    geom_line(data=dfEmo, aes(x=time, y=x1), color = "green") +
    ggtitle("Time series plot of input audio x1 and output signal") +
    xlab(str2) + ylab(str1)

 tp2 = ggplot() +
    geom_line(data=dfNeut, aes(x=time, y=y), color = "steelblue") +
    geom_line(data=dfEmo, aes(x=time, y=y), color = "green") +
    ggtitle("Time series plot of output audio y and output signal") +
    xlab(str2) + ylab(str3)

  require(gridExtra)
  grid.arrange(tp1, tp2, ncol=1, nrow = 2)
}

plotDistribution = function() {
  print("task 1.2")

  geoHist = geom_histogram(colour = 1, fill = 'gray')
  geoDen = geom_density(lwd = 1, fill = 4, colour = 9, alpha = 0.25)

 plot1 = ggplot(mapping = aes(x=mNeut[,1], y = ..density..)) +
    geoHist + geoDen + xlab("Neutral input signal x1")

  plot2 = ggplot(mapping = aes(x=mEmo[,1], y = ..density..)) +
    geoHist + geoDen + xlab("Emotional input signal x1")

  plot3 = ggplot(mapping = aes(x=mNeut[,3], y = ..density..)) +
    geoHist + geoDen + xlab("Neutral output signal y")

  plot4 = ggplot(mapping = aes(x=mEmo[,3], y = ..density..)) +
    geoHist + geoDen + xlab("Emotional output signal y")

  require(gridExtra)
  grid.arrange(plot1, plot2, plot3, plot4, ncol=2, nrow = 2)
}


plotScattered = function() {
  print("task 1.3")

 cor_neut = cor.test(mNeut[,1], mNeut[,3])
 cor_emo = cor.test(mEmo[,1], mEmo[,3])
 print(cor_neut)
 print(cor_emo)

  x_ax = "Input signal x1"
  y_ax = "Output signal y"
  cor_title_emo = "Emotional signal correlation"
  cor_title_neu = "Neutral signal correlation"
  colr = "lightgray"
  fill_col = "steelblue"

  library("ggpubr")
  ggscatter(dfEmo, x = "x1", y = "y",color = fill_col,
            add = "reg.line", conf.int = TRUE,
            cor.coef = TRUE, cor.method = "pearson",add.params = list(color = "red"),
            xlab = x_ax, ylab = y_ax, title = cor_title_emo)

  ggscatter(dfNeut, x = "x1", y = "y",
            add.params = list(color = "red"),color = fill_col,
            add = "reg.line", conf.int = TRUE,
            cor.coef = TRUE, cor.method = "pearson",
            xlab = x_ax, ylab = y_ax, title = cor_title_neu)

  plot1 = ggqqplot(dfEmo$x1, ylab = "Input signal x1")
  plot2 = ggqqplot(dfNeut$x1, ylab = "Input signal x1")

 require(gridExtra)
 grid.arrange(plot1, plot2, ncol=2, nrow = 1)
}

boxplotting = function() {
  print("task 1.4")

  bplot = boxplot(mEmo[,3], mNeut[,3], col = "orange",
          border = "brown",
          horizontal = TRUE,
          notch = TRUE,
          main = "Emotional and Neutral output box plot", xlab = "Input signal x1", names = c("emotional", "neutral"))

  print(bplot)
}

