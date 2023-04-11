#Tesrt calculo
V_camara <- 1.171 #dm³
S <- 0.78 #dm²

Patm = 0.9869233 #atm #1003.7 #mBa
Temp = 273.15 #Kelvin #17.7 #ºC

R = 0.08205 #dm3·atm·mol-1·K-1

Slope = 0.5 #ppm/sec
#Calculo pdf latex
V <- (1* R*Temp)/Patm
pCO2 <-(Slope/1000000 * R * Temp)/V
Flujo = pCO2*(V_camara/(R*Temp*S))
Flujo_mmol = Flujo*1000*0.01*86400
Flujo_mg <- Flujo_mmol*44

#De otra forma:
#Cuantos moles aire tengo en la camara?
naire = (Patm*V_camara)/(R*Temp)
Flujo2 <- ((Slope/1000000)*naire)/S
Flujo_mmol2 <- Flujo2*1000*0.01*86400
Flujo_mg2 <- Flujo_mmol*44

#Otra forma
moles_aire <- Patm*1000/(R*Temp)
(moles_aire*(Slope/1000))*44
