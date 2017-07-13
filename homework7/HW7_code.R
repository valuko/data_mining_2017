
# Homework 7 submission. 
# Victor Aluko

x_points <- c(0.6364,0.2727,0.2353,0.4118)
y_points <- c(0.6923,0.5538,0.6667,0.8)

xd <- c(0:1); yd <- xd
plot(x_points, y_points, main = "ROC Space", xlab = "FPR", ylab = "TPR or Sensitivity", type = "p", xlim = 0:1, ylim = 0:1)
lines(c(0:1), c(0:1), type="l")