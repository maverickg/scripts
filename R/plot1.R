
## This draws a series of density plots along the X-axis
## 
drawbeta <- function(x, alpha, beta) {
   u <- seq(0, 1, length.out = 100)
   y <- dbeta(u, alpha, beta)
   x2 <- x + y
   idx <- (u > (alpha/(alpha+beta) * .7)) & (u < (alpha/(alpha+beta) * 1.35))
   lines(x2[idx], u[idx], type = 'l')
}

png(width = 800, height = 600)
plot(seq(0, 100, length.out = 10), seq(0, 1, length.out = 10), type='n', xlab =
'', ylab = '', xaxis='n', yaxix='n')


drawbeta(10, 23, 20)
drawbeta(20, 45, 20)
drawbeta(30, 35, 30)
drawbeta(40, 40, 40)
drawbeta(50, 70, 40)
drawbeta(60, 40, 60)
drawbeta(70, 40, 80)
drawbeta(80, 30, 80)
drawbeta(90, 40, 80)

x <- seq(10, 90, by = 10)
m <- c(23/40, 45/65, 35/65, .5, 7/11, .4, 1/3, 3/11, 1/3)

fit <- loess(m ~ x, span = .8)

# lines(x, predict(fit), type = 'l', lwd = 2, col = 'blue')
lines(x, m, type = 'l', lwd = 2, col = 'blue')

dev.off()

