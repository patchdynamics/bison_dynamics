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

# Bison
# B0 = initial densiry of bison within entire space
# rb = bison intrinsic growth rate = .22 / year = .018 / month
# dhB = rate of bison culling by humans
# tB = proportion of time bison spend in range areas near water

model = function(t, y, parms) {
  
  Gw = y[1]
  Gd = y[2]
  C = y[3]
  B = y[4]
  
  with(as.list(params), {
    
    dGw = Gw * rG * ( 1 - Gw / kG ) - Gw * ( aC * C / w2t + tB * B)
    dGd = Gd * rG * ( 1 - Gd / kG ) - Gd * (1 - tB) * aB * B
    dC  = eC * aC * C * Gw * w2t - dhC * C
    dB  = rB * B - dhB * B
    
    # [ (1 - tB) * Gd + tB * Gw ] 
    differentials = c(dGw, dGd, dC, dB)
    list(differentials)
  
  })
}

params = c(
        rG = .9, 
        w2t = .25,
        aC = .01,  # from stocking rates whitepapers?
        eC = .1,
        dhC = .15,
        kG = 10000,
        aB = .01,  # unused
        eB = .1,   # unused
        rB = .018,  # intrinsic growth rate
        dhB = .012, # culling rate, allowing some growth
        tB = .2  # this can from that paper
        )

Gw0 = 600
Gd0 = 1000
C0 = 15
B0 = 0
  
state = c(
          Gw0,
          Gd0,
          C0,
          B0
        )


t = 1:200  # months ??

out = ode(y = state, times = t, func = model, parms = params)

par(mfrow=c(2,2))
matplot(t, out[,2], type = "l", ylab = "Grass near water", xlab = 'months')
matplot(t, out[,3], type = "l", ylab = "Grass not near water", xlab = 'months')
matplot(t, out[,4], type = "l", ylab = "Cattle", xlab = 'months', ylim=c(0, max(out[,4])))
matplot(t, out[,5], type = "l", ylab = "Bison", xlab = 'months', ylim=c(0, max(out[,5])))
par(mfrow=c(1,1))



