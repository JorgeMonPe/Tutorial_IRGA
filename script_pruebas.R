library(tidyverse)
#Open IRGA text files
Data_file <- read_table2('./_bookdown_files/IRGA_files/23031010.TXT',col_names = F, skip = 2, comment = 'Start')
Data_file <- read_csv('./_bookdown_files/IRGA_files/23031311.TXT', skip = 2, col_names = FALSE, comment = 'Start')
#Remove rows with "End"
Data_file <- Data_file %>% filter(X1 != "End" | X1 != "Zero")
#Select just rows with M5
Data_file <- Data_file %>% filter(X1 == "M5")
#Select just column with relevant information
#When the files is generated from a SRC process, then parameter 3 (column 20) is DT
Data_file <- Data_file[,c(1:8,20)]
#Change column names
colnames(Data_file) <- c('Device',"Date",'Hour','Plot','Record_number','CO2_ppm','P_mBa','Flow',"DT")
#remove row with NAs
Data_file <- drop_na(Data_file, Plot)
#Give Date format to Date column
Data_file$Date <- as.Date(Data_file$Date, format = "%d/%m/%Y")
#DT is repeated some times 
#I create a new variable with difference between number record (seconds)
Data_file <- Data_file %>% group_by(Plot) %>% mutate(Seconds = Record_number-first(Record_number))
#Remove files with text 'End'
#Data_file <- Data_file[-which(Data_file[,1] == 'End' |Data_file[,1] == 'Zero'),]
#Plot 
library(ggplot2)
ggplot(Data_file, aes(x = DT, y = CO2_ppm))+
  geom_point()+
  facet_wrap(~Plot, scales = 'free')
#Estaría interesante reducir el número de puntos de la siguiente forma:
#Cuando hay varios valores iguales consecutivos, hacer la media del tiempo y usar un solo punto en mitad


#Vemos que el IRGA al final de muchos plots mete un monton de puntos que caen rápidamente
#Vamos a quitar esos artefactos.
#Elimino las filas que vayan más allá de los 180 segundos de medida
Data_file <- Data_file %>% filter(Seconds <= 180)

ggplot(Data_file, aes(x = DT, y = CO2_ppm))+
  geom_point()+
  facet_wrap(~Plot, scales = 'free')
#Ya nos hemos quitado ese problema de en medio
#Ahora mismo voy a hacer un ajuste lineal a lo bestia, sin limpiar
data <- Data_file %>% filter(Plot == 2)
#Una función para obtener la pendiente, la R y el intervalo de confianza
fit_rates <- function(data){
  lm <- lm(CO2_ppm~Seconds, data = data)
  cor <- cor.test(formula = ~Seconds +CO2_ppm, data = data)
  ajustes <- data.frame(R2 = as.numeric(cor$estimate^2), pvalue = cor$p.value, slope = as.numeric(lm$coefficients[2]), Intercept = as.numeric(lm$coefficients[1]), n = nrow(data), Conf_2.5 = confint(lm)[2,1], Conf_97.5 = confint(lm)[2,2])
  return(ajustes)
}
fit_rates(test)
Results <- Data_file %>% group_by(Plot) %>% group_modify(~fit_rates(data=.x))

#Now we can have a look into the plots with the fits
ggplot(Data_file, aes(x = DT, y = CO2_ppm))+
  geom_point()+
  facet_wrap(~Plot, scales = 'free')+
  geom_abline(data = Results, aes(intercept = Intercept, slope = slope), color = "red")+
  geom_text(data = Results, aes(x= 160, y = 430, label = R2))

#I'm going to reduce the number of points
#when a cocnentration remains constante i'm going to use just one point in the middle
reduce_points <- function(data){
  data$rep <- NA
  i <- 1
  for(p in 1:nrow(data)){
    j <- i+1
    if(j <= nrow(data)){
      while (data$CO2_ppm[i] == data$CO2_ppm[j]) {
        data$rep[j-1]<-i
        j <- j+1
        if(is.na(data$CO2_ppm[j]))
          break
      }
      data$rep[j-1]<-i
      i <- j
    }
  }
  result <- data %>% group_by(rep) %>% summarise(across(where(is.numeric), ~mean(.)), across(Hour, ~seconds_to_period(mean(.))), across(Date, ~first(.)))
  return(result)
}
#Now, we apply the function
Reduced_data <- Data_file %>% group_by(Plot) %>% group_modify(~reduce_points(data=.x))

#And calcualte again the rates
Results <- Reduced_data %>% group_by(Plot) %>% group_modify(~fit_rates(data=.x))

ggplot(Reduced_data, aes(x = DT, y = CO2_ppm))+
  geom_point()+
  facet_wrap(~Plot, scales = 'free')+
  geom_abline(data = Results, aes(intercept = Intercept, slope = slope), color = "red")+
  geom_text(data = Results, aes(x= 160, y = 410, label = R2))
