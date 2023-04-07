require(fractalRegression)
library(ggplot2)
library(crqa)
data(crqa)

scales <- logscale(scale_min = 16, scale_max = length(handmovement$P1_TT_d)/4, scale_ratio = 1.1)

#### Original

mlra.hand.out.100 <- mlra(scale(handmovement$P1_TT_d), scale(handmovement$P2_TT_d), order = 1, scales = scales, lags = 100, direction = 'p')
x <- 0:100
y <- scales
fields::image.plot(x, y, mlra.hand.out.100$betas, axes=TRUE, legend.lab = "Beta Coefficient", ylab="Scale", xlab="Lag", main="Hand Movement Lag 100",zlim=c(-3,3))
min(mlra.hand.out.100$betas)
max(mlra.hand.out.100$betas)
range(mlra.hand.out.100$betas[101,])

#### Surrogates

mlra.surr.out.100 <- mlra(permute::shuffle(scale(handmovement$P1_TT_d)), permute::shuffle(scale(handmovement$P2_TT_d)), order = 1, scales = scales, lags = 100, direction = 'p')
x <- 0:100
y <- scales
fields::image.plot(x, y, mlra.surr.out.100$betas, axes=TRUE, legend.lab = "Beta Coefficient", ylab="Scale", xlab="Lag", main="Surr Hand Movement Lag 100",zlim=c(-3,3))
min(mlra.surr.out.100$betas)
max(mlra.surr.out.100$betas)
range(mlra.surr.out.100$betas[101,])

#### Original

mlra.hand.out.150 <- mlra(scale(handmovement$P1_TT_d), scale(handmovement$P2_TT_d), order = 1, scales = scales, lags = 150, direction = 'p')
x <- 0:150
y <- scales
fields::image.plot(x, y, mlra.hand.out.150$betas, axes=TRUE, legend.lab = "Beta Coefficient", ylab="Scale", xlab="Lag", main="Hand Movement Lag 150",zlim=c(-3,3))
min(mlra.hand.out.150$betas)
max(mlra.hand.out.150$betas)
range(mlra.hand.out.150$betas[101,])
range(mlra.hand.out.150$betas[151,])

#### Surrogates

mlra.surr.out.150 <- mlra(permute::shuffle(scale(handmovement$P1_TT_d)), permute::shuffle(scale(handmovement$P2_TT_d)), order = 1, scales = scales, lags = 150, direction = 'p')
x <- 0:150
y <- scales
fields::image.plot(x, y, mlra.surr.out.150$betas, axes=TRUE, legend.lab = "Beta Coefficient", ylab="Scale", xlab="Lag", main="Surr Hand Movement Lag 150",zlim=c(-3,3))
min(mlra.surr.out.150$betas)
max(mlra.surr.out.150$betas)
range(mlra.surr.out.150$betas[101,])

#### Original


mlra.hand.out.200 <- mlra(scale(handmovement$P1_TT_d), scale(handmovement$P2_TT_d), order = 1, scales = scales, lags = 200, direction = 'p')
x <- 0:200
y <- scales
fields::image.plot(x, y, mlra.hand.out.200$betas, axes=TRUE, legend.lab = "Beta Coefficient", ylab="Scale", xlab="Lag", main="Hand Movement Lag 200",zlim=c(-3,3))
min(mlra.hand.out.200$betas)
max(mlra.hand.out.200$betas)
range(mlra.hand.out.200$betas[101,])
range(mlra.hand.out.200$betas[151,])
range(mlra.hand.out.200$betas[201,])

#### Surrogates
mlra.surr.out.200 <- mlra(permute::shuffle(scale(handmovement$P1_TT_d)), permute::shuffle(scale(handmovement$P2_TT_d)), order = 1, scales = scales, lags = 200, direction = 'p')
x <- 0:200
y <- scales
fields::image.plot(x, y, mlra.surr.out.200$betas, axes=TRUE, legend.lab = "Beta Coefficient", ylab="Scale", xlab="Lag", main="Surr Hand Movement Lag 200",zlim=c(-3,3))
min(mlra.surr.out.200$betas)
max(mlra.surr.out.2100$betas)
range(mlra.surr.out.200$betas[101,])

#### IAAFT
P1.i <- iaafft(scale(handmovement$P1_TT_d),N=1)
P2.i <- iaafft(scale(handmovement$P2_TT_d),N=1)
  mlra.iaafft.out.100 <- mlra(P1.i$Re.sgates., P2.i$Re.sgates., order = 1, scales = scales, lags = 100, direction = 'p')
  x <- 0:100
  y <- scales
  fields::image.plot(x, y, mlra.iaafft.out.100$betas, axes=TRUE, legend.lab = "Beta Coefficient", ylab="Scale", xlab="Lag", main="iaafft Hand Movement Lag 100",zlim=c(-3,3))
  min(mlra.iaafft.out.100$betas)
  max(mlra.iaafft.out.100$betas)
  range(mlra.iaafft.out.100$betas[101,])


  mlra.iaafft.out.150 <- mlra(P1.i$Re.sgates., P2.i$Re.sgates., order = 1, scales = scales, lags = 150, direction = 'p')
  x <- 0:150
  y <- scales
  fields::image.plot(x, y, mlra.iaafft.out.150$betas, axes=TRUE, legend.lab = "Beta Coefficient", ylab="Scale", xlab="Lag", main="iaafft Hand Movement Lag 150",zlim=c(-3,3))
  min(mlra.iaafft.out.150$betas)
  max(mlra.iaafft.out.150$betas)
  range(mlra.iaafft.out.100$betas[101,])

  mlra.iaafft.out.200 <- mlra(P1.i$Re.sgates., P2.i$Re.sgates., order = 1, scales = scales, lags = 200, direction = 'p')
  x <- 0:200
  y <- scales
  fields::image.plot(x, y, mlra.iaafft.out.200$betas, axes=TRUE, legend.lab = "Beta Coefficient", ylab="Scale", xlab="Lag", main="iaafft Hand Movement Lag 200",zlim=c(-3,3))
  min(mlra.iaafft.out.200$betas)
  max(mlra.iaafft.out.200$betas)
  range(mlra.iaafft.out.200$betas[101,])



  ####### Trying out increased detrending order

  #### Original

  mlra.hand.out.100.o2 <- mlra(scale(handmovement$P1_TT_d), scale(handmovement$P2_TT_d), order = 2, scales = scales, lags = 100, direction = 'p')
  x <- 0:100
  y <- scales
  fields::image.plot(x, y, mlra.hand.out.100.o2$betas, axes=TRUE, legend.lab = "Beta Coefficient", ylab="Scale", xlab="Lag", main="Hand Movement Lag 100 O2",zlim=c(-3,3))
  min(mlra.hand.out.100.o2$betas)
  max(mlra.hand.out.100.o2$betas)
  range(mlra.hand.out.100.o2$betas[101,])

  mlra.hand.out.150.o2 <- mlra(scale(handmovement$P1_TT_d), scale(handmovement$P2_TT_d), order = 2, scales = scales, lags = 150, direction = 'p')
  x <- 0:150
  y <- scales
  fields::image.plot(x, y, mlra.hand.out.150.o2$betas, axes=TRUE, legend.lab = "Beta Coefficient", ylab="Scale", xlab="Lag", main="Hand Movement Lag 150 O2",zlim=c(-3,3))
  min(mlra.hand.out.150.o2$betas)
  max(mlra.hand.out.150.o2$betas)
  range(mlra.hand.out.150.o2$betas[101,])

  mlra.hand.out.100.o3 <- mlra(scale(handmovement$P1_TT_d), scale(handmovement$P2_TT_d), order = 3, scales = scales, lags = 100, direction = 'p')
  x <- 0:100
  y <- scales
  fields::image.plot(x, y, mlra.hand.out.100.o2$betas, axes=TRUE, legend.lab = "Beta Coefficient", ylab="Scale", xlab="Lag", main="Hand Movement Lag 100 O3",zlim=c(-3,3))
  min(mlra.hand.out.100.o3$betas)
  max(mlra.hand.out.100.o3$betas)
  range(mlra.hand.out.100.o3$betas[101,])

  mlra.hand.out.150.o3 <- mlra(scale(handmovement$P1_TT_d), scale(handmovement$P2_TT_d), order = 2, scales = scales, lags = 150, direction = 'p')
  x <- 0:150
  y <- scales
  fields::image.plot(x, y, mlra.hand.out.150.o3$betas, axes=TRUE, legend.lab = "Beta Coefficient", ylab="Scale", xlab="Lag", main="Hand Movement Lag 150 O3",zlim=c(-3,3))
  min(mlra.hand.out.150.o3$betas)
  max(mlra.hand.out.150.o3$betas)
  range(mlra.hand.out.150.o3$betas[101,])
