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
# aC = grass consumption by cattle per capita per timestep
# eC = cattle biomass conversion efficiency
# dhC = rate of cattle harvest by humans

# Bison
# B0 = initial densiry of bison within entire space
# rb = bison intrinsic growth rate = .22 / year = .018 / month
# dhB = rate of bison culling by humans
# tB = proportion of time bison spend in range areas near water

# Jackrabbits
# R0 = initial density of rabbits within enter space
# aR = grass consumption by rabbits per capita per timestep
# eR = rabbit biomass conversion efficiency
# dnR = intrinsic rabbit death rate

# Coyote
# K0 = initial density of coyote within entire space
# aK = asymtotic consumption of rabbits by coyote
# bK = density at half maximum of coyote consumption
# eK = coyote biomass conversion efficiency
# dnK = coyote death rate imposed by humans

model = function(t, y, parms) {
  
  Gw = y[1]
  Gd = y[2]
  C = y[3]
  B = y[4]
  R = y[5]
  K = y[6]
  
  with(as.list(params), {
    
    GwT = Gw * w2t # Density of grasses near water scaled to total area
    GdT = Gd * ( 1 - w2t ) # Density of grasses not near water scaled to total area
    
    dGw = Gw * ( rG * ( 1 - Gw / kG )  -  aC * C / w2t  -  tB * aB * B  -  aR * R )
    
    dGd = Gd * ( rG * ( 1 - Gd / kG ) -  (1 - tB) * aB * B - aR * R )
    
    dC  = eC * aC * C * GwT - dhC * C
    
    dB  = rB * B - dhB * B
    
    dR  = eR * aR * ( GwT + GdT ) * R * (1 - R / kR) - ( (aK * R) / (bK + R) ) * K 
    
    dK  = eK * ( (aK * R) / (bK + R) ) * K  - dnK * K
    
    # [ (1 - tB) * Gd + tB * Gw ] 
    differentials = c(dGw, dGd, dC, dB, dR, dK)
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
        tB = .2,  # this came from that paper
        eR = .1,
        aR = .01,
        kR = 50,
        aK = 10,
        bK = 10,
        eK = .1,
        dnK = .8
        )

Gw0 = 600
Gd0 = 1000
C0 = 15
B0 = 2
R0 = 10
K0 = 40
  
state = c(
          Gw0,
          Gd0,
          C0,
          B0,
          R0,
          K0
        )


t = 1:400  # months ??

out = ode(y = state, times = t, func = model, parms = params)

par(mfrow=c(3,2))
matplot(t, out[,2], type = "l", ylab = "Grass near water", xlab = 'months')
matplot(t, out[,3], type = "l", ylab = "Grass not near water", xlab = 'months')
matplot(t, out[,4], type = "l", ylab = "Cattle", xlab = 'months', ylim=c(0, max(out[,4])))
matplot(t, out[,5], type = "l", ylab = "Bison", xlab = 'months', ylim=c(0, max(out[,5])))
matplot(t, out[,6], type = "l", ylab = "Rabbits", xlab = 'months', ylim=c(0, max(out[,6])))
matplot(t, out[,7], type = "l", ylab = "Coyotes", xlab = 'months', ylim=c(0, max(out[,7])))
par(mfrow=c(1,1))



