library(deSolve)

# Variable Definitions

# Grass / Plants
# Gw0 = initial grass density of area near water
# Gd0 = initial grass density of area not near water
# w2t = proportion of range that is near water
# rG = intrinsic growth rate of grasses, assume the same in both habitats
# kG = carrying density of grasses

# Cattle
# C0 = initial density of cattle within entire space
# aC = grass consumption by cattle per captia per timestep
# eC = cattle biomass conversion efficiency
# dhC = rate of cattle harvest by humans

model = function(t, y, parms) {
  
  Gw = y[1]
  Gd = y[2]
  C = y[3]
  
  with(as.list(params), {
    
    dGw = Gw * rG * ( 1 - Gw / kG ) - Gw * aC * C / w2t
    dGd = Gd * rG * ( 1 - Gw / kG )
    dC  = eC * aC * C * Gw * w2t - dhC * C
    
    differentials = c(dGw, dGd, dC)
    list(differentials)
  
  })
}

params = c(
        rG = .9, 
        w2t = .25,
        aC = .01,  # from stocking rates whitepapers?
        eC = .1,
        dhC = .15,
        kG = 1000
        )

Gw0 = 1000
Gd0 = 1000
C0 = 10
  
state = c(
          Gw0,
          Gd0,
          C0
        )


t = 1:100  # months ??

out = ode(y = state, times = t, func = model, parms = params)

par(mfrow=c(1,3))
matplot(t, out[,2], type = "l", ylab = "Grass near water", xlab = 'months')
matplot(t, out[,3], type = "l", ylab = "Grass not near water", xlab = 'months')
matplot(t, out[,4], type = "l", ylab = "Cattle", xlab = 'months', ylim=c(0, max(out[,4])))
par(mfrow=c(1,1))



