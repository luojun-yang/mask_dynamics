library(deSolve)
library(doSNOW) 
library(foreach)

source('sim_funcs.R')

t_end <- 365

times <- seq(0,t_end,1)
x0 <- 0

### Epidemiological pars and initial conditions
R0 <- 2.2
mu <- 0
sigma <- 1/5
gamma <- 1/5
delta <- 1/365
beta <- (mu+gamma)*R0

E0 <- 1e-6
y0 <- c(S=1-E0,E=E0,I=0,R=0,i=0)
pars_sir <- c(beta=beta,mu=mu,sigma=sigma,gamma=gamma,delta=delta)

alpha_coef <- 40 #coefficients for plotting incidence
r_vec <- data.frame(lsoda(y=y0,
                          times=times,
                          func=sir,
                          parms=pars_sir))$i*alpha_coef

s_vec <- c(rep(0.5,100),rep(0.9,100),rep(0.5,t_end-200))

### parameters for threshold model
pars <- c(n=10000,
          kappa=0.1,
          alpha=11/alpha_coef,
          w=0.5,
          r=0.2,
          d=1,
          p=0.7,
          s=0.8,
          sigma_d=0.05,
          sigma_p=0.05,
          sigma_b=0.05)

### Simulations under tight/loose culture & low/high policy stringency
ws <- c(0.8, 0.1)
ss <- c(0.8, 0.55)

out <- data.frame(time=numeric(),
                  mask=numeric(),
                  w=numeric()
)

label_x <- c(210,140,205,155)
label_y <- c(0.95,0.1,0.75,0.7)

case <- 1
for (w in ws){
  for (s in ss){
    pars['w'] <- w
    s_vec <- c(rep(0.5,100),rep(s,100),rep(0.5,t_end-200))
    temp <- sim_all(t_end,x0,s_vec,r_vec,pars)
    out <- rbind(out,data.frame(time=times,mask=temp,w=w,s=s,case=case,label_x=label_x[case],label_y=label_y[case]))
    case <- case + 1
  }
}

map <- setNames(c("TH", "TL", "LH", "LL"), c(1,2,3,4))
out$case_m <- factor(map[unlist(out$case)],levels=c("TH", "TL", "LH", "LL"))
write.csv(out, "../results/cases.csv")

### Simulate interactions with policy timing, duration, and stringency
numOfClusters <- 4
cl <- makeCluster(numOfClusters) # define your clusters
registerDoSNOW(cl)

ws <- c(0.8, 0.1) 
s_starts <- seq(0,200,2)
ss <- seq(0.5,1,0.005)

results_timing <- data.frame(X=numeric(),
                             s=numeric(),
                             end=numeric(),
                             peak=numeric(),
                             auc=numeric(),
                             w=numeric(),
                             X_var=character())

for (w in ws){
  pars['w'] <- w
  out_timing_end <- c()
  out_timing_peak <- c()
  out_timing_int <- c()
  data <- expand.grid(X=s_starts,s=ss)

  output <- foreach(s = ss, .packages=c('pracma'), .combine='rbind') %:% 
    foreach(s_start = s_starts, .combine='rbind') %dopar% {
      s_vec <- c(rep(0.5,s_start),rep(s,100),rep(0.5,t_end-100-s_start))
      temp <- sim_all(t_end,x0,s_vec,r_vec,pars)
      data <- c(end=mean(tail(temp,5)),
                peak=max(temp),
                auc=trapz(times,temp),
                w=w,
                X_var='Timing'
      )
    }
  output <- cbind(data,as.data.frame(output))
  results_timing <- rbind(results_timing,output)
}


s_durs <- seq(0,200,2)

results_dur <- data.frame(X=numeric(),
                          s=numeric(),
                          end=numeric(),
                          peak=numeric(),
                          auc=numeric(),
                          w=numeric(),
                          X_var=character())
for (w in ws){
  pars['w'] <- w
  out_dur_end <- c()
  out_dur_peak <- c()
  out_dur_int <- c()
  output <- foreach(s = ss, .combine='rbind')%:%
    foreach(s_dur = s_durs, .combine='rbind')%dopar%{
      s_vec <- c(rep(0.5,100),rep(s,s_dur),rep(0.5,t_end-100-s_dur))
      temp <- sim_all(t_end,x0,s_vec,r_vec,pars)
      data <- c(end=mean(tail(temp,5)),
                peak=max(temp),
                auc=trapz(times,temp),
                w=w,
                X_var='Duration'
      )
    }
  
  data <- expand.grid(X=s_durs,s=ss)
  output <- cbind(data,as.data.frame(output))
  results_dur <- rbind(results_dur,output)
}

results <- rbind(results_timing, results_dur)
results <- results %>% group_by(w) %>% mutate(label=ifelse(w==0.1, 'Loose', 'Tight'))

write.csv(results, "../results/auc.csv")
