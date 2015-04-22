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
# dhK = coyote death rate imposed by humans

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
  
  with(as.list(parms), {
    
    Gw_growth =  rG * ( 1 - Gw / kG ) 
    Gw_cattle_consumption =  ((aC + aCE) * Gw / (bC + Gw)) * C / w2t 
    
    # aR here should be scaled to the relative size of the area
    # aR in the 25% area would be 25 percent of total aR, for instance
    # again questioning if the handling of bison is also correct
    # i think that the RATES need to be scaled to the proportion available.
    # actually that is also weird because sure, rabbits could eat at their total rate near water
    dGw =  Gw_growth  -  Gw_cattle_consumption -  tB * (aB * Gw/(bB + Gw)) * B    -  (aR * Gw / (bR + Gw)) * R
    
    dGd =  rG * ( 1 - Gd / kG )    - (1 - tB) * (aB * Gd/(bB + Gd)) * B     -  (aR * Gd / (bR + Gd)) * R
    
    # this implementation of fixed pressure not working
    if(fixed_grazing_pressure > 0){ # && C >= fixed_grazing_pressure) {
      #dC = - dhC * C # stocking rate held constant by management   
      #print(dC)
      dC = 0
    } else {
      dC  = (eC * (aC * Gw / (bC + Gw)) / Cw) * C   - dhC * C
    }
    
    # for bison it's the same, they may be hitting their max rate twice here
    # dB  =  eB * (tB * (aB * GwT/(bB + GwT)) * B) / Bw     +   ( eB * (1-tB) * (aB * GdT/(bB + GdT)) * B ) / Bw    -  dhB * B
    BisonUptakeRateFromGw = tB * (aB * Gw / (bB + Gw)) * w2t
    BisonUptakeRateFromGd = (1 + tB) * (aB * Gd / (bB + Gd)) * (1 - w2t)

    dB = eB * BisonUptakeRateFromGw * B / Bw    + eB * BisonUptakeRateFromGd * B / Bw     -  dhB * B
    
    if( R < 0 ){
      dR = .001  # rescue the population, so ode doesn't crash on divide by 0
    } else {
      #dR  = (eR * (aR * GwT / (bR + GwT)) / Rw) * R * (1 - R / kR)    +   (eR * (aR * GdT / (bR + GdT)) / Rw) * R * (1 - R / kR)     - dnR * R    - ( (aK * R) / (bK + R) ) * K 
      
      #above is wrong because aR could be the maximum amount of COMBINED forage from BOTH pools
      #these rabbits are double eating!  they can't be allowed to reach aR in both pools, only the combined pool
      # maybe just averaging the pools would work ?  the main thing, is that here, they should be able to be achive aR twice
      # aR is their total rate to be spread across BOTH pools
      
      # The below is true, but is it equiv to what we see in grass equations??
      # only if aR in grass equations is scaled?
      # i think that the aR, bR scaling strategy might be more correct than what I am doing below
      #TotalGrassDensity = GwT + GdT
      #R  = eR * R * aR * TotalGrassDensity / (bR + TotalGrassDensity) * (1 - R / kR)    -  dnR * R    -  ( (aK * R) / (bK + R) ) * K
      
      # can also be thought about as.. if rabbits get (aR * Gw / (bR + Gw)) density of biomass from Gw, what density of biomass do they get on the whole spatial scale.
      # well, it's wtc of it.
      RabbitUptakeRateFromGw = (aR * Gw / (bR + Gw)) * w2t  # need to verify these units -> it's a rate, which we then convert to per hectare for the entire space
      RabbitUptakeRateFromGd = (aR * Gd / (bR + Gd)) * (1 - w2t)
      dR  =  eR * RabbitUptakeRateFromGw * R * (1 - R / kR) / Rw    +   eR * RabbitUptakeRateFromGd  * R * (1 - R / kR) / Rw    - dnR * R    - ( (aK * R) / (bK + R) ) * K 
      
    }
    # evidence is that jackrabbits are not actually limited by food
    
    # so, why are we dividing by Kw here?  Rabbits consumption is # of rabbits per coyote per year
    # eK should be an efficienty of rabbit to coyote numericaly, there's no dividing by biomass
    #dK  = eK * ( (aK * R) / (bK + R) ) * K   +  .001 * 30 * ( 1 - R / (bK + R) ) * K     - dhK * K
    
    dK  = eK * ( (aK * R) / (bK + R) ) * K   +   ( 1 - R / (bK + R) ) * deer     - dhK * K
    
    
    
    # eK should be adjusted...
    print(.09 * ( 1 - R / (bK + R) )  )
    
    
    
    #dK  = eK * ( (aK * R) / (bK + R) ) * K     - dhK * K
    
    # [ (1 - tB) * Gd + tB * Gw ] 
    differentials = c(dGw, dGd, dC, dB, dR, dK)
    list(differentials)
  
  })
}

kG = 2100 # carrying capcity density of grasses  kg / ha
unavailable_forage = 600 # kg/ha average used by other species, not controllable
kG = kG - unavailable_forage
params = c(
        Cw = 453, # avg weight of a cow
        Bw = 680, # avg weight of a bison
        Rw = 2, # avg weight of a rabbit
        Kw = 13, #avg weight of a coyote
  
        #rG = 60, # kg / year / kg
        rG = 6000, # intrinsic (max) growth rate of grasses  kg / year / ha
        kG = kG, # carrying capcity density of grasses  kg / ha
        
        w2t = .5, # precentage of area that is 'near water,' i.e. grazable by cattle
        
        aC = 4080,  # ideal kilos of grass per year per cattle 
        aCE = 300,  # extra grass that cattle may eat beyond ideal
        bC = kG / 150, # density of grasses at half maximum consumption, value is a rough guess
                     # consumption should saturate quickly
        eC = .1, # median conversion efficiency
        dhC = .5, # cattle harvest rate, percentage
        fixed_grazing_pressure = 1, # set to 1 to hold cattle constant, 0 to allow cattle population to grow
        
        aB = 4080,  # ideal kilos of grass per year per mature bison 
        bB = kG / 100, # consumption saturates quickly
        eB = .04,   #  bison biomass conversion efficiency
        #rB = .22,  # intrinsic growth rate, bison growth per year - individuals per year, not biomass.  equates to above values
        dhB = .1, # culling rate, allowing some growth, a percentage
        tB = .2,  # percentage time bison spend feeding near water, may be influenced by precipitation
        
        eR = .4, # this is tuned to allow one rabbit to create 8 rabbits a year, a conservative estimate
        aR =  54.75, # kg forage per year per rabbit
        bR = kG / 100, # saturate forage rate quickly 
        kR = 30,  # max jackrabbits per hectare  # they may not be limited actually
        dnR = 0.05, # natural rabbit death rate - not including predation 
        
        aK = 20,  # jackrabbits per individual per year
        bK = 4,   # these numbers were adjusted to produce stable, expected dynamics
        eK = .01, # trophic, number of rabbits to make a coyote 
                  # (10% of rabbit biomass available to make full coyote biomass, 
                  # figured using mass of each species)
        deer = 0, # fixed availability of deer fawns for predation
        dhK = 1 #coyote control rate held constant by management
        )





