
####start from here
Q = matrix(c(-0.4,0.4,0.2,-0.2),ncol = 2,byrow = TRUE)
x1<-mmhp(Q, delta = c( 1/3, 2/3), lambda0 = 1, lambda1 = 1.6, alpha = 0.8, beta = 1.2)

simulatemmhp(x1,nsim=5)->y1
#drawUniMMHPIntensity(x1,y1)

UniMMHPIntensity(x1,y1)->z
drawUniMMHPIntensity(x1,y1)
m<-length(y1$x)
curve(z,y1$x[1],y1$x[m])
integrate(z,0,y1$tau[2])$value
r<-rescaled(z,y1$tau)
r
qqexp(r)
mmhpNegLogLik(x1,y1$tau)

y1

use_testthat()
use_test("mmhp")



t <- y$tau
state <- y$z
state_time <- y$x
lambda0 <- x$lambda0
lambda1 <- x$lambda1
alpha <- x$alpha
beta <- x$beta
