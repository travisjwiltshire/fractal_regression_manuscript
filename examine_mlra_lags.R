require(fractalRegression)
library(ggplot2)
library(crqa)
data(crqa)

scales <- logscale(scale_min = 16, scale_max = length(handmovement$P1_TT_d)/4, scale_ratio = 1.1)

mlra.hand.out.100 <- mlra(scale(handmovement$P1_TT_d), scale(handmovement$P2_TT_d), order = 1, scales = scales, lags = 100, direction = 'p')
x <- 0:100
y <- scales
#fields::image.plot(x, y, mlra.hand.out.100$betas, axes=TRUE, legend.lab = "Beta Coefficient", ylab="Scale", xlab="Lag", main="Hand Movement Lag 100",zlim=c(-3,3))
min(mlra.hand.out.100$betas)
max(mlra.hand.out.100$betas)
range(mlra.hand.out.100$betas[101,])



mlra.hand.out.150 <- mlra(scale(handmovement$P1_TT_d), scale(handmovement$P2_TT_d), order = 1, scales = scales, lags = 150, direction = 'p')
x <- 0:150
y <- scales
fields::image.plot(x, y, mlra.hand.out.150$betas, axes=TRUE, legend.lab = "Beta Coefficient", ylab="Scale", xlab="Lag", main="Hand Movement Lag 150",zlim=c(-3,3))
min(mlra.hand.out.150$betas)
max(mlra.hand.out.150$betas)
range(mlra.hand.out.150$betas[101,])
range(mlra.hand.out.150$betas[151,])

mlra.hand.out.200 <- mlra(scale(handmovement$P1_TT_d), scale(handmovement$P2_TT_d), order = 1, scales = scales, lags = 200, direction = 'p')
x <- 0:200
y <- scales
fields::image.plot(x, y, mlra.hand.out.200$betas, axes=TRUE, legend.lab = "Beta Coefficient", ylab="Scale", xlab="Lag", main="Hand Movement Lag 200",zlim=c(-3,3))
min(mlra.hand.out.200$betas)
max(mlra.hand.out.200$betas)
range(mlra.hand.out.200$betas[101,])
range(mlra.hand.out.200$betas[151,])
range(mlra.hand.out.200$betas[201,])


