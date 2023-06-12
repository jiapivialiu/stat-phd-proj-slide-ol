library(CVThresh) # get doppler signals by `dopp`
library(glmgen)

## doppler example
#x <- dopp()$x
#location <- x * length(x) + 1 # 1:1024
#dop <- dopp()$meanf
#set.seed(1256)
#y = dop[201:500] + rnorm(300, 0, 0.2)
#x <- x[201:500]
#location <- location[201:500]

x <-  seq(.2,.5,length.out = 300)
dop <- sqrt(x * (1-x)) * sin(2 * pi * 1.05/(x+0.05))
set.seed(848)
y <-  dop + rnorm(300, 0, 0.2)
location <- x * length(x) + 1

## cubic trend filtering
tf_mod <- trendfilter(y = y, x = location, k = 3L, lambda = 300)
tf_mod$df # get dp
tf_beta <- tf_mod$beta

## cubic smoothing spline with the df in TF
smoothsig <- smooth.spline(x = y, w = location, df = tf_mod$df)
## cubic smoothing spline with a higher (arbitrarily chosen) df
smoothsig2 <- smooth.spline(x = y, w = location, df = 20)

## plots
pdf("gfx/doppler6.pdf")
plot(x, y, main = "", col="grey")
lines(x = x, y = tf_beta[, 1], col = "blue", lwd=2)
lines(x = x, y = smoothsig$y, col = "orange", lwd=2)
dev.off()

pdf("gfx/doppler6-20.pdf")
plot(x, y, ylab = "", xlab = "", main = "", col="grey")
lines(x = x, y = tf_beta[, 1], col = "blue", lwd=2)
lines(x = x, y = smoothsig2$y, col = "orange", lwd=2)
dev.off()
