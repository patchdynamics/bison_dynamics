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
    
    GwT = Gw * w2t # Density of grasses near water scaled to total area
    GdT = Gd * ( 1 - w2t ) # Density of grasses not near water scaled to total area
    GT = GwT + GdT # Total density of grass at scale of total area
    
    Gw_growth =  rG * ( 1 - Gw / kG ) 
    Gw_cattle_consumption =  (aC * Gw / (bC + Gw)) * C / w2t 
    #print(cat(Gw_growth, Gw_cattle_consumption))
    
    # aR here should be scaled to the relative size of the area
    # aR in the 25% area would be 25 percent of total aR, for instance
    # again questioning if the handling of bison is also correct
    # i think that the RATES need to be scaled to the proportion available.
    # actually that is also weird because sure, rabbits could eat at their total rate near water
    dGw =  Gw_growth  -  Gw_cattle_consumption -  tB * (aB * Gw/(bB + Gw)) * B    -  (aR * Gw / (bR + Gw)) * R
    
    dGd =  rG * ( 1 - Gd / kG )    - (1 - tB) * (aB * Gd/(bB + Gd)) * B     -  (aR * Gd / (bR + Gd)) * R
    
    if(fixed_grazing_pressure == 1) {
      dC = 0 # stocking rate held constant by management    
    } else {
      dC  = (eC * (aC * Gw / (bC + Gw)) / Cw) * C   - dhC * C
    }
    
    dB  =  eB * (tB * (aB * GwT/(bB + GwT)) * B) / Bw     +   ( eB * (1-tB) * (aB * GdT/(bB + GdT)) * B ) / Bw    -  dhB * B
    
    
    if( R < 0 ){
      dR = .001  # rescue the population
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
    
    
    dK  = eK * ( (aK * R) / (bK + R) ) * K / Kw  + eK * ( 1 - (aK * R) / (bK + R) ) * deer * K / Kw - dhK * K
    #dK = 0  # Coyotes held constant by management
    
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
        bC = kG / 100, # density of grasses at half maximum consumption, value is a rough guess
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
        eK = .1, # trophic
        deer = 0, # density of fawns consumed by coyote, can be managed by managing deer population
        dhK = 1 #coyote control rate held constant by management
        )

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
  matplot(t, out[,2], type = "l", ylab = "Grass near water", xlab = 'years', ylim=c(0, 2000))
  matplot(t, out[,3], type = "l", ylab = "Grass not near water", xlab = 'years', ylim=c(0, 2000))
  matplot(t, out[,6], type = "l", ylab = "Rabbits", xlab = 'years', ylim=c(0, max(out[,6])))
  matplot(t, out[,7], type = "l", ylab = "Coyotes", xlab = 'years', ylim=c(0, max(out[,7])))
  par(mfrow=c(1,1))
}

# bison vs cattle population
state = c(1000, 1000, .2, 2, 2, 0)
params['fixed_grazing_pressure'] = 0
out = ode(y = state, times = t, func = model, parms = params)
system_plot(out)

# rabbits and things
state = c( 1000, 1000, .4, .4, 4, 0)
params['w2t'] = .5
params['dhB'] = .22
params['fixed_grazing_pressure'] = 1
out = ode(y = state, times = t, func = model, parms = params)
system_plot(out)

state = c( 1000, 1000, 0, 0, 4, 3)
params['kR'] = 50
params['fixed_grazing_pressure'] = 0
params['kG'] = 1500
out = ode(y = state, times = t, func = model, parms = params)
rabbit_coyote_plot(out)
#system_plot(out)


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





