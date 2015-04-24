system_plot = function(out) {
  
  par(mfrow=c(3,2))
  timesteps = nrow(out)
  matplot(1:timesteps, out[,2], type = "l", ylab = "Grass near water", xlab = 'years', ylim=c(0, 2400))
  matplot(1:timesteps, out[,3], type = "l", ylab = "Grass not near water", xlab = 'years', ylim=c(0, 2400))
  matplot(1:timesteps, out[,4], type = "l", ylab = "Cattle", xlab = 'years', ylim=c(0, max(out[,4])))
  matplot(1:timesteps, out[,5], type = "l", ylab = "Bison", xlab = 'years', ylim=c(0, max(out[,5])))
  matplot(1:timesteps, out[,6], type = "l", ylab = "Rabbits", xlab = 'years', ylim=c(0, max(out[,6])))
  matplot(1:timesteps, out[,7], type = "l", ylab = "Coyotes", xlab = 'years', ylim=c(0, max(out[,7])))
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
state = c(1000, 1000, .4, .3, 0, 0)
params['fixed_grazing_pressure'] = .5
params['tB']  =.4
params['dhB'] = .1
params['dhC'] = .3
out = ode(y = state, times = 1:50, func = model, parms = params)
system_plot(out)



# bison vs cattle population w/ rabbits
state = c(1000, 1000, .1, .1, 0, 0)
state = c(1000, 1000, .3, .1, 2, 2)
#params['fixed_grazing_pressure'] = .15
params['tB']  =.4
params['dhB'] = .2
params['bC'] = kG / 100
params['aCE'] = 100
#params['dhC'] = .3
out = ode(y = state, times = 1:50, func = model, parms = params)
system_plot(out)
print(grazing_deficit(out[50,], params))
deficits = sapply(1:50, function(t){
  d = grazing_deficit(out[t, ], params)
  return(d)
})
plot(deficits)


# bison cattle rabits with parameterization
gp = .2
state = c( 1027, 1027, gp, .2, 35, 1.6)  # stable dynamics
params['kR'] = 50
params['fixed_grazing_pressure'] = 1
params['kG'] = 1500
params['dhK'] = .2
params['aK'] = 130
params['bK'] = 30
params['deer'] = 0
params['bC'] = kG / 100

state[1] = 556 # equilibrium states adjustedfor inclusions of starting bison
state[2] = 911

params['w2t'] = .5
gp_low = .3
gp_high = .5
dnK_low = .2
dnK_high = .8

state[3] = gp_low
params['dhK'] = dnK_low
free_coyote_data_low_pressure = ode(y = state, times = 1:50, func = model, parms = params)
state[3] = gp_high
free_coyote_data_high_pressure = ode(y = state, times = 1:50, func = model, parms = params)
state[3] = gp_low
params['dhK'] = dnK_high
controlled_coyote_data_low_pressure = ode(y = state, times = 1:50, func = model, parms = params)
state[3] = gp_high
controlled_coyote_data_high_pressure = ode(y = state, times = 1:50, func = model, parms = params)


params['w2t'] = .3
state[3] = gp_low
params['dhK'] = dnK_low
free_coyote_data_low_pressure_w = ode(y = state, times = 1:50, func = model, parms = params)
state[3] = gp_high
free_coyote_data_high_pressure_w = ode(y = state, times = 1:50, func = model, parms = params)
state[3] = gp_low
params['dhK'] = dnK_high
controlled_coyote_data_low_pressure_w = ode(y = state, times = 1:50, func = model, parms = params)
state[3] = gp_high
controlled_coyote_data_high_pressure_w = ode(y = state, times = 1:50, func = model, parms = params)



#free_coyote = out_out
#controlled_coyote = out_out

#system_plot(free_coyote_data)
#system_plot(controlled_coyote_data_high_pressure)

#out = free_coyote
#out = controlled_coyote_data_high_pressure

get_deficits = function(data) {
  deficits = sapply(1:50, function(t){
    d = grazing_deficit(data[t, ], params)
    return(d)
  })
  return(deficits)
}

par(mfrow=c(2,2))
deficits = cbind(get_deficits(free_coyote_data_low_pressure))
deficits = cbind(deficits,get_deficits(free_coyote_data_high_pressure))
matplot(deficits[1:30,], type='l', ylab="Grazing Deficit", ylim=c(0,2000) )
title('Uncontrolled Coyotes')

deficits = cbind(get_deficits(controlled_coyote_data_low_pressure))
deficits = cbind(deficits,get_deficits(controlled_coyote_data_high_pressure))
matplot(deficits[1:30,], type='l', ylab='', ylim=c(0,2000))
title('Heavily Controlled Coyotes')


deficits = cbind(get_deficits(free_coyote_data_low_pressure_w))
deficits = cbind(deficits,get_deficits(free_coyote_data_high_pressure_w))
matplot(deficits[1:30,], type='l', ylab="Grazing Deficit", ylim=c(0,2500))

deficits = cbind(get_deficits(controlled_coyote_data_low_pressure_w))
deficits = cbind(deficits,get_deficits(controlled_coyote_data_high_pressure_w))
matplot(deficits[1:30,], type='l', ylab='', ylim=c(0,2500))

par(mfrow=c(1,1))






plot(deficits, type='l', xlab='Year', ylab='Cattle Grazing Deficit Index')

system_plot(controlled_coyote_data_low_pressure)




# calculating a grazing deficit
grazing_deficit = function(row, params){
  Gw = row[2]
  C  = row[4]
  consumption = ((params['aC'] + params['aCE']) * Gw / (params['bC'] + Gw)) * C / params['w2t']
  dietaryRequirement = params['aC'] * C / params['w2t']
  deficit = dietaryRequirement - consumption
  if(deficit < 0){
    deficit = 0
  }
  return(deficit)
}



# bison vs cattle population w/ rabbits and coyote
state = c(1000, 1000, .1, .1, 0, 0)

gp = .4
params['fixed_grazing_pressure'] = gp
state = c(1000, 1000, gp, .1, 2, 2.5)
state = c(1000, 1000, .2, .2, 2, 0)
params['tB']  =.4
params['dhB'] = .1
params['bC'] = kG / 100
params['aCE'] = 100
params['kR'] = 50
params['kG'] = 1500
params['dhK'] = .2
params['aK'] = 130
params['bK'] = 30

out = ode(y = state, times = 1:50, func = model, parms = params)
system_plot(out)
print(grazing_deficit(out[50,], params))
deficits = sapply(1:50, function(t){
  d = grazing_deficit(out[t, ], params)
  return(d)
})
plot(deficits, type='l', xlab='Year', ylab='Cattle Grazing Deficit Index')





