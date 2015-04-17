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

# Population Densities
# Population densities for animals are in individuals per hectare
# Densities for grasses are in grams biomass per hectare

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
    GT = GwT + GdT # Total density of grass at scale of total area
    

    dGw = Gw * ( rG * ( 1 - Gw / kG ))     - (aC * Gw / (bC + Gw)) * C / w2t    -  tB * aB * B    -  (aR * Gw / (bR + Gw)) * R
    
    dGd = Gd * ( rG * ( 1 - Gd / kG ) )    - (1 - tB) * aB * B     -  (aR * Gw / (bR + Gw)) * R
    
    dC  = (eC * (aC * Gw / (bC + Gw)) / Cw) * C   - dhC * C
    
    dB  = (eB * aB / Bw) * B   - dhB * B
    
    dR  = (eR * (aR * Gw / (bR + Gw)) / Rw) * R * (1 - R / kR)    - dnRI    - ( (aK * R) / (bK + R) ) * K 
    dR = 0
    # evidence is that jackrabbits are not actually limited by food
    
    #dK  = eK * ( (aK * R) / (bK + R) ) * K  - dnK * K
    dK = 0  # Coyotes held constant by management
    
    # [ (1 - tB) * Gd + tB * Gw ] 
    differentials = c(dGw, dGd, dC, dB, dR, dK)
    list(differentials)
  
  })
}

kG = 2100 # carrying capcity density of grasses  kg / ha
params = c(
        Cw = 453, # avg weight of a cow
        Bw = 680, # avg weight of a bison
        Rw = 2, # avg weight of a rabbit
  
        rG = 12592.5, # intrinsic (max) growth rate of grasses  kg / year / ha
        kG = kG, # carrying capcity density of grasses  kg / ha
        
        w2t = .25, # amount of area that is 'near water,' i.e. grazable by cattle
        
        aC = 4080,  # ideal kilos of grass per year per cattle 
        bC = kG / 4, # density of grasses at half maximum consumption, value is a rough guess
                     # consumption should saturate quickly
        eC = .1, # median conversion efficiency
        dhC = .15, # cattle harvest rate, percentage
        
        aB = 4080,  # ideal kilos of grass per year per mature bison 
        eB = .04,   #  bison biomass conversion efficiency
        #rB = .22,  # intrinsic growth rate, bison growth per year - individuals per year, not biomass.  equates to above values
        dhB = .012, # culling rate, allowing some growth, a percentage
        tB = .2,  # percentage time bison spend feeding near water, may be influenced by precipitation
        
        eR = .1, # current research suggests that grass availability is not the limiting factor on jackrabbit density
        aR =  54.75, # kg forage pre year per rabbit
        bR = kG / 4, # saturate forage rate quickly 
        kR = 2,  # max jackrabbits per hectare
        dnRI = 18.25, # density independent natural rabbit deaths - not including predation 
        
        aK = 10,
        bK = 10,
        eK = .1,
        dnK = .8
        )

Gw0 = 600
Gd0 = 1000
C0 = 0 #15
B0 = 0 #2
R0 = 0 #10
K0 = 0 #40
  
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
t = 1:10

out = ode(y = state, times = t, func = model, parms = params)

par(mfrow=c(3,2))
matplot(t, out[,2], type = "l", ylab = "Grass near water", xlab = 'months')
matplot(t, out[,3], type = "l", ylab = "Grass not near water", xlab = 'months')
matplot(t, out[,4], type = "l", ylab = "Cattle", xlab = 'months', ylim=c(0, max(out[,4])))
matplot(t, out[,5], type = "l", ylab = "Bison", xlab = 'months', ylim=c(0, max(out[,5])))
matplot(t, out[,6], type = "l", ylab = "Rabbits", xlab = 'months', ylim=c(0, max(out[,6])))
matplot(t, out[,7], type = "l", ylab = "Coyotes", xlab = 'months', ylim=c(0, max(out[,7])))
par(mfrow=c(1,1))



