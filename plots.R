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


# typing a grazing deficit
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
state = c(1000, 1000, .2, .1, 2, 0)
params['tB']  =.4
params['dhB'] = .2
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

