system_plot = function(out) {
  
  par(mfrow=c(3,2))
  matplot(t, out[,2], type = "l", ylab = "Grass near water", xlab = 'years', ylim=c(0, 2400))
  matplot(t, out[,3], type = "l", ylab = "Grass not near water", xlab = 'years', ylim=c(0, 2400))
  matplot(t, out[,4], type = "l", ylab = "Cattle", xlab = 'years', ylim=c(0, max(out[,4])))
  matplot(t, out[,5], type = "l", ylab = "Bison", xlab = 'years', ylim=c(0, max(out[,5])))
  matplot(t, out[,6], type = "l", ylab = "Rabbits", xlab = 'years', ylim=c(0, max(out[,6])))
  matplot(t, out[,7], type = "l", ylab = "Coyotes", xlab = 'years', ylim=c(0, max(out[,7])))
  par(mfrow=c(1,1))
  
}

rabbit_coyote_plot = function(out){
  par(mfrow=c(2,2))
  matplot(t, out[,2], type = "l", ylab = "Grass near water", xlab = 'years', ylim=c(0, 2000))
  matplot(t, out[,3], type = "l", ylab = "Grass not near water", xlab = 'years', ylim=c(0, 2000))
  matplot(t, out[,6], type = "l", ylab = "Rabbits", xlab = 'years', ylim=c(0, max(out[,6])))
  matplot(t, out[,7], type = "l", ylab = "Coyotes", xlab = 'years', ylim=c(0, max(out[,7])))
  par(mfrow=c(1,1))
}

# bison vs cattle population
state = c(1000, 1000, .4, .3, 0, 0)
params['fixed_grazing_pressure'] = .5
params['tB']  =.4
params['dhB'] = 1
params['dhC'] = .3
out = ode(y = state, times = 1:50, func = model, parms = params)
system_plot(out)