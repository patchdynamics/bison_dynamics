

# grass
totalPerYear = 12592500

# cows
aC = 340 # kg/mo
aC = aC * 12 # kg / yr
aC = aC * 1000 # g / yr
print(aC)


# bison
eB = .22 / 4080 # (bison / (yr * bison))  / (kg / ( year * bison)) 
print(eB) # bison / kg grass
eB = eB * 1500 # kg bison / kg grass
print(eB)


# rabbits
kR = 71.4 # rabbits per sq km = rabbits per 1,000,000 sq m
kR = kR * 10000 / 1000000 # convert to hectares
print(kR)

dnR = 0.05  # mortality rate  rabbits per hectare per day
dnR = dnR * 365 # per year
print(dnR)

f = 150 # g dry forage / day
f = f * 365 / 1000
print(f) # kg forage / year