
####start from here
Q = matrix(c(-0.4,0.4,0.2,-0.2),ncol = 2,byrow = TRUE)
x1<-mmhp(Q, delta = c( 1/3, 2/3), lambda0 = 1, lambda1 = 1.6, alpha = 0.8, beta = 1.2)
#x2<-list(q1=0.4,q2=0.2, delta = c( 1/3, 2/3), lambda0 = 1, lambda1 = 1.6, alpha = 0.8, beta = 1.2)

#x1<-mmhp(Q=Q_mean, delta = delta_mean, lambda0 = lambda0_mean, lambda1 = lambda1_mean, alpha = alpha_mean, tau=time_vec-1,beta = beta_mean)


y1<-simulatemmhp(x1,nsim=5)
y2<-simulatemmhp(x1,nsim=5,given_state = T,states=list(x=y1$x,z=y1$z,ending = max(y1$tau)))


intensity(x1,y1)->z
drawUniMMHPIntensity(x1,y1)
y1
m<-length(y1$x)
curve(z,y1$x[1],y1$x[m])
integrate(z,0,y1$tau[2])$value
r<-rescaled(z,y1$tau)
r
r<-compensator(x1,t=y1$tau,pzt=NULL)
r
qqexp(r)
negloglik(x1,y1$tau)


y1

use_testthat()
use_test("mmhp")



simulatemmhp(x1,nsim=5,given_state = T,states=list(x=y1$x,z=y1$z,ending = max(y1$tau)))

#####ignore the following
haha<-mmhp(lambda0=mean(sim_mmhp_sep$lambda0[,pair]),
           lambda1=mean(sim_mmhp_sep$lambda1[,pair]),
           alpha=mean(sim_mmhp_sep$alpha[,pair]),
           beta=mean(sim_mmhp_sep$beta[,pair]))
UniMMHPIntensity(haha,method = "numeric",
           event = list(tau = current_event_time,time_segment=time_segment,
                        latent_mean=latent_mean))
rawresidual(haha, t=current_event_time, time.vec=time_segment,latent.vec=latent_mean)
pearsonresidual(haha, t=current_event_time, time.vec=time_segment,latent.vec =latent_mean,latent_event = latent_event)


