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
    
    Gw_growth =  rG * ( 1 - Gw / kG ) 
    Gw_cattle_consumption =  (aC * Gw / (bC + Gw)) * C / w2t 
    #print(cat(Gw_growth, Gw_cattle_consumption))
    dGw =  Gw_growth  -  Gw_cattle_consumption -  tB * (aB * Gw/(bB + Gw)) * B    -  (aR * Gw / (bR + Gw)) * R
    
    dGd =  rG * ( 1 - Gd / kG )    - (1 - tB) * (aB * Gd/(bB + Gd)) * B     -  (aR * Gd / (bR + Gd)) * R
    
    #dC  = (eC * (aC * Gw / (bC + Gw)) / Cw) * C   - dhC * C
    dC = 0 # stocking rate held constant by management
    
    dB  =  eB * (tB * (aB * Gw/(bB + Gw)) * B) / Bw   +   ( eB * (1 - tB) * (aB * Gd/(bB + Gd)) * B ) / Bw   -  dhB * B
    
    dR  = (eR * (aR * Gw / (bR + Gw)) / Rw) * R * (1 - R / kR)    - dnR * R    - ( (aK * R) / (bK + R) ) * K 
    # evidence is that jackrabbits are not actually limited by food
    
    dK  = eK * ( (aK * R) / (bK + R) ) * K  + deer * K - dnK * K
    #dK = 0  # Coyotes held constant by management
    
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
  
        #rG = 60, # kg / year / kg
        rG = 6000, # intrinsic (max) growth rate of grasses  kg / year / ha
        kG = kG, # carrying capcity density of grasses  kg / ha
        
        w2t = .5, # precentage of area that is 'near water,' i.e. grazable by cattle
        
        aC = 4080,  # ideal kilos of grass per year per cattle 
        bC = kG / 100, # density of grasses at half maximum consumption, value is a rough guess
                     # consumption should saturate quickly
        eC = .1, # median conversion efficiency
        #dhC = .5, # cattle harvest rate, percentage
        
        aB = 4080,  # ideal kilos of grass per year per mature bison 
        bB = kG / 100, # consumption saturates quickly
        eB = .04,   #  bison biomass conversion efficiency
        #rB = .22,  # intrinsic growth rate, bison growth per year - individuals per year, not biomass.  equates to above values
        dhB = .01, # culling rate, allowing some growth, a percentage
        tB = .2,  # percentage time bison spend feeding near water, may be influenced by precipitation
        
        eR = .4, # this is tuned to allow one rabbit to create 8 rabbits a year, a conservative estimate
        aR =  54.75, # kg forage pre year per rabbit
        bR = kG / 100, # saturate forage rate quickly 
        kR = 30,  # max jackrabbits per hectare  # they may not be limited actually
        dnR = 0.05, # natural rabbit death rate - not including predation 
        
        aK = 20,
        bK = 5,
        eK = .1,
        deer = 0, # density of fawns consumed by coyote, can be managed by managing deer population
        dnK = 1 #coyote held constant by management
        )

Gw0 = 2100
Gd0 = 1000
C0 = 0 # .4 #.4 # consider this as stocking rate, held constant by management
B0 = 3 #.4 # 2
R0 = 0 #2.7 # 10
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
t = 1:40

out = ode(y = state, times = t, func = model, parms = params)

par(mfrow=c(3,2))
matplot(t, out[,2], type = "l", ylab = "Grass near water", xlab = 'years', ylim=c(0, 2400))
matplot(t, out[,3], type = "l", ylab = "Grass not near water", xlab = 'years', ylim=c(0, 2400))
matplot(t, out[,4], type = "l", ylab = "Cattle", xlab = 'years', ylim=c(0, max(out[,4])))
matplot(t, out[,5], type = "l", ylab = "Bison", xlab = 'years', ylim=c(0, max(out[,5])))
matplot(t, out[,6], type = "l", ylab = "Rabbits", xlab = 'years', ylim=c(0, max(out[,6])))
matplot(t, out[,7], type = "l", ylab = "Coyotes", xlab = 'years', ylim=c(0, max(out[,7])))
par(mfrow=c(1,1))



