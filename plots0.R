
# manipulating
Gw0 = 2100
Gd0 = 1000
C0 = 0 # .4 #.4 # consider this as stocking rate, held constant by management
B0 = .4 # 2
R0 = 2.7 # 10
K0 = 3 #40

state = c(
  Gw0,
  Gd0,
  C0,
  B0,
  R0,
  K0
)

t = 1:30  # 30 years prediction
#t = 1:200 # to get equilibria
t = 1:200
#t = 1:50

# just grasses
#state = c( 1000, 1000, 0, 0, 0, 0)

# grasses with cows, medium grazing pressure
#state = c( 1000, 1000, .4, 0, 0, 0)

# grasses with cows, heavy grazing pressure
state = c( 1000, 1000, .8, 0, 0, 0)
params['w2t'] = .2

cows_water_vs_grazing = function(state){
  C0v = seq(.1, 1, .2)
  w2t = seq(.01, .99, .02)
  data = matrix(, nrow = length(w2t), ncol = length(C0v) )
  for( j in 1:length(C0v)) {
    for( i in 1:length(w2t)){
      prms = params
      prms['w2t'] = w2t[i]
      stat = state
      stat[3] = C0v[j]
      out = ode(y = stat, times = t, func = model, parms = prms)
      data[i, j] = out[nrow(out), 2]
    }
    
  }
  
  matplot(w2t, data, type='p', pch=21, main=cat('Grass Biomass at Grazing Pressure: ', state[3]),
          xlab='Pecentage of range near water', 
          ylab = 'Grass Biomass')
  
  return(data)
}
#data = cows_water_vs_grazing(state)




# grasses with bison and cows, medium grazing pressure
state = c( 1000, 1000, .4, .4, 0, 0)
params['dhB'] = .19

cows_vs_bison = function(state) {
  C0v = c(.2, .4, .6, .8)
  B0v = c(.2, .4, .6, .8)
  
}


cows_bison_tB_vs_grazing = function(state){
  #C0v = seq(.1, 1, .2)
  C0v = c(.2, .8)
  w2t = .5
  tB = seq(w2t-.2, w2t+.1, .02)
  
  prms = params
  prms['w2t'] = w2t
  stat = state
  
  grassWater = matrix(, nrow = length(tB), ncol = length(C0v) )
  grassDry   = matrix(, nrow = length(tB), ncol = length(C0v) )
  
  for( j in 1:length(C0v)) {
    stat[3] = C0v[j]
    
    for( i in 1:length(tB)){
      prms['tB'] = tB[i]
      out = ode(y = stat, times = t, func = model, parms = prms)
      grassWater[i, j] = out[nrow(out), 2]
      grassDry[i, j] = out[nrow(out), 3]
      
    }
    
  }
  #layout(matrix(c(1,2),nrow=1), width=c(4,1)) 
  #par(mar=c(5,4,4,0)) #No margin on the right side
  par(mfrow=c(1,3))
  matplot(tB, grassWater, type='p', pch=c(21, 24), ylim=c(0,2000),
          xlab='Buffalo selection for grass near water', 
          ylab = 'Near Water Grass Biomass Density')
  
  matplot(tB, grassDry, type='p', pch=c(21, 24), ylim=c(0,2000),
          xlab='Buffalo selection for grass near water', 
          ylab = 'Dry Area Grass Biomass Density')
  
  plot(c(0,1), type='n', axes=F, xlab="", ylab="")
  legend("left", c("Low Grazing Pressure", "High Grazing Pressure"), pch=c(21, 24), pt.bg=seq_len(length(C0v)))
  par(mfrow=c(1,1))
  
  return(data)
}
data = cows_bison_tB_vs_grazing(state)




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
  timesteps = nrow(out)
  matplot(1:timesteps, out[,2], type = "l", ylab = "Grass near water", xlab = 'years', ylim=c(0, 2000))
  matplot(1:timesteps, out[,3], type = "l", ylab = "Grass not near water", xlab = 'years', ylim=c(0, 2000))
  matplot(1:timesteps, out[,6], type = "l", ylab = "Rabbits", xlab = 'years', ylim=c(0, max(out[,6])))
  matplot(1:timesteps, out[,7], type = "l", ylab = "Coyotes", xlab = 'years', ylim=c(0, max(out[,7])))
  par(mfrow=c(1,1))
}

# bison vs cattle population
state = c(1000, 1000, 1, .3, 0, 0)
params['fixed_grazing_pressure'] = .4
out = ode(y = state, times = 1:50, func = model, parms = params)
system_plot(out)

# calculated a change in grazing pressure capacity due to bison ?

# rabbits and things
state = c( 1000, 1000, .4, .4, 4, 0)

state = c( 1000, 1000, 0, 0, 4, 0)  # just rabbits
params['w2t'] = .5
params['dhB'] = .22
params['fixed_grazing_pressure'] = .4
out = ode(y = state, times = t, func = model, parms = params)
system_plot(out)

# just rabbits
# grasses = 830
# rabbits = 50


# rabbit/coyote system gives good dynamics
# adjust dhK
state = c( 1027, 1027, 0, 0, 35, 1.6)  # stable dynamics
#state = c( 1027, 1027, 0, 0, 35, 8) # perturbation - lots of coyote
#state = c( 1027, 1027, 0, 0, 35, 1.6) # perturbation - lots of rabbit
state = c( 1500, 1500, 0, 0, 35, 1.6)  # stable dynamics
#state = c( 1500, 1500, 0, 0, 1, 30)  # perturb

params['kR'] = 50
params['fixed_grazing_pressure'] = 0
params['kG'] = 1500
params['dhK'] = .27
params['aK'] = 130  # adjusting this changes dynamics - if coyotes do or do not have other food?
params['bK'] = 30   # dynamics are also very sensitive to this parameterization
params['deer'] = 0
params['eR'] = .5
out = ode(y = state, times = 1:50, func = model, parms = params)
rabbit_coyote_plot(out)


rabbit_coyote_plot(out[10:50,])
system_plot(out[10:50,])
rabbit_coyote_out = out


# perturbation - lots of rabbit, w. heavily controlled coyote
state = c( 1027, 1027, 0, 0, 70, 1.6) # perturbation - lots of rabbit
params['kR'] = 70 # perturbation - lots of rabbit
params['kG'] = 1500
params['dhK'] = .7
params['aK'] = 130
params['bK'] = 30
params['deer'] = 0
out = ode(y = state, times = 1:50, func = model, parms = params)
rabbit_coyote_plot(out)
high_control_perturbation = out

# perturbation - lots of rabbit, w. uncontrolled coyote
state = c( 1494, 1494, 0, 0, 70, 2) # perturbation - lots of rabbit
params['kR'] = 70 # perturbation - lots of rabbit
params['kG'] = 1500
params['dhK'] = .2
params['aK'] = 130
params['bK'] = 30
params['deer'] = 0
out = ode(y = state, times = 1:50, func = model, parms = params)
rabbit_coyote_plot(out)
low_control_perturbation = out

perturbation_plot = function(high_control_perturbation, low_control_perturbation) {
  time = 15
  matplot(1:time, cbind(high_control_perturbation[1:time,6], low_control_perturbation[1:time,6]), type='l',
          ylab = "Rabbit Density After Perturbation", xlab="Years")
}
perturbation_plot(high_control_perturbation, low_control_perturbation)




grass_rabbit_coyote_control_plot = function(rabbit_coyote_out) {
  par(mfrow=c(1,2))
  col = c(6,2,1)
  grass_data = cbind(rabbit_coyote_out[,2], 1027, 830)
  matplot(rabbit_coyote_out[20:50,1], grass_data[20:50,], col=col, lty = 1, type = "l", ylab = "Cattle Range Grass Density", xlab = 'Years', ylim=c(0, 2000))
  lagomorph_data = cbind(rabbit_coyote_out[,6], 35, 50)
  matplot(rabbit_coyote_out[20:50,1], lagomorph_data[20:50,], col=col, lty = 1, type = "l", ylab = "Lagomorph Density", xlab = 'Years', ylim=c(0, 50))
  
  #plot(c(0,1), type='n', axes=F, xlab="", ylab="")
  #legend("top", c("Coyotes Uncontrolled", "Coyotes Moderately Controlled", "Coyotes Completely Absent"))
  par(mfrow=c(1,1))
}
grass_rabbit_coyote_control_plot(rabbit_coyote_out)


# adjusting deer gives good dynamics here as well
state = c( 1000, 1000, 0, 0, 4, 3)
params['kR'] = 50
params['kG'] = 1500
params['dhK'] = .2
params['aK'] = 130
params['bK'] = 30
params['deer'] = .4
out = ode(y = state, times = t, func = model, parms = params)
rabbit_coyote_plot(out)

# putting some cattle into it
state = c( 1000, 1000, 0, .4, 1, 1)
params['kR'] = 50
params['fixed_grazing_pressure'] = 1
params['kG'] = 1500
params['dhK'] = .2
params['aK'] = 130
params['bK'] = 30
params['deer'] = .4
out = ode(y = state, times = t, func = model, parms = params)
rabbit_coyote_plot(out)

system_plot(out)


# rabbit/coyote system 
state = c( 1000, 1000, 0, 0, 2, .1)
#params['aK'] = 1200
#params['bK'] = 30
#params['eK'] = .001  # is this biomass conversion,, or actually rabbit to coyote conversion?
#params['aR'] = 45
#params['eR'] = .4 # bump up biomass conversion per year ?? for rabbits due to lots of babies
#t = 1:50
params['kR'] = 20
params['dhK'] = .2
params['aK'] = 200
params['bK'] = 30
t = 1:50
out = ode(y = state, times = t, func = model, parms = params)
rabbit_coyote_plot(out)


# rabbit/coyote/cattle system
state = c( 1000, 1000, .8, .4, 2, 2)
params['aK'] = 20
params['bK'] = 4
params['fixed_grazing_pressure'] = 0
#params['eR'] = .7 # bump up biomass conversion per year ?? for rabbits due to lots of babies
out = ode(y = state, times = t, func = model, parms = params)
system_plot(out)
#rabbit_coyote_plot(out)
