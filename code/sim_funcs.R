#### functions for simulation
# calculate thresholds
thresholds <- function(pars){
  b <- pars['alpha']*pars['r']
  if (pars['p']<=0)  p_dist <- rep(0,pars['n'])
  else p_dist <- rgamma(pars['n'], shape=pars['p']^2/pars['sigma_p'],
                        scale=pars['sigma_p']/pars['p'])
  if (b<=0)  b_dist <- rep(0,pars['n'])
  else b_dist <- rgamma(pars['n'], shape=b^2/pars['sigma_b'],
                        scale=pars['sigma_b']/b)-pars['kappa']
  if (pars['d']<=0)  p_dist <- rep(0,pars['n'])
  else d_dist <- rgamma(pars['n'], shape=pars['d']^2/pars['sigma_d'],
                        scale=pars['sigma_d']/pars['d'])
  x <- 0.5+p_dist*(1-2*pars['s'])/(2*d_dist)-b_dist/(2*pars['w']*d_dist)
  return(x)
}

# simulate for one step
sim_step <- function(x, pars, k){
  ## k: rate of change
  thresh_t <- thresholds(pars)
  x_change <- k * (sum(thresh_t <= x)/pars['n'] - x)
  return(x+x_change)
}

# simulate over time
sim_all <- function(t_steps, x0, s_vec, r_vec, pars){
  out <- rep(0, t_steps)
  out[1] <- x0
  for (i in seq_len(t_steps)){
    pars['r'] <- r_vec[i]
    pars['s'] <- s_vec[i]
    x_i <- sim_step(out[i], pars, k=0.3)
    if (is.na(x_i)) browser()
    out[i+1] <- x_i
  }
  return(out)
}

# SEIRS simulator
sir <- function(t,y,parms){
  lambda <- parms['beta']#*(1+cos(2*pi*t))
  dS <- -lambda*y['I']*y['S'] - (1-y['S'])*parms['mu'] + parms['delta']*y['R']
  dE <- lambda*y['I']*y['S'] - (parms['sigma'] + parms['mu'])*y['E']
  dI <-  parms['sigma']*y['E'] - (parms['gamma'] + parms['mu'])*y['I']
  dR <-  parms['gamma']*y['I'] - (parms['delta']+parms['mu'])*y['R']
  di <- parms['sigma']*y['E'] - y['i']
  return(list(c(dS,dE,dI,dR,di)))
}
