##Problem set 2 ##
##Nicolas Borda 202210560##
R.version.string

library(pacman)
library(rio)
p_load(data.table)
library(tidyverse)

##Importar/exportar bases de datos##

##1.1 Importar##

location <- import(file="C:/Users/nicob/OneDrive/Documentos/Taller de R}/problem-sets/pset-2/input/Módulo de sitio o ubicación.dta")
identification <- import(file="C:/Users/nicob/OneDrive/Documentos/Taller de R}/problem-sets/pset-2/input/Módulo de identificación.dta")

##1.2 Exportar##

location.rds <- export(location, file="C:/Users/nicob/OneDrive/Documentos/Taller de R}/problem-sets/pset-2/output/Módulo de sitio o ubicación.dta")
identification.rds <- export(identification, file = "C:/Users/nicob/OneDrive/Documentos/Taller de R}/problem-sets/pset-2/output/Módulo de identificación.dta")

##2. Generar variables##

##Usando la variable grupos 4, se debe generar una nueva variable llamada bussiness_type, que tomará los siguientes valores:##

##-Agricultura cuando grupos 4 sea igual a 1. 
##– Industria manufacturera cuando grupos 4 sea igual a 2. 
##– Comercio cuando grupos 4 sea igual a 3.
##– Servicios cuando grupos 4 sea igual



identification <- mutate(identification, bussines_type=case_when(identification$GRUPOS4 == "01" ~"agricultura",
                           identification$GRUPOS4 == "02" ~ "Industria Manufacturera",
                           identification$GRUPOS4 == "03" ~ "Comercio",
                           identification$GRUPOS4 == "04" ~ "Servicios"))
##2.2 Se debe crear una variable llamada grupo_etario que divida a los propietarios de micronegocios encuatro grupos etarios. Los rangos de edades seleccionados deben ser justificados.

numeros_aleatoria <- sample(18:70, 84753, replace = TRUE)
identification$edad <- numeros_aleatoria

  identification <- mutate(identification, 
                       grupo_etario= case_when(identification$edad>=18 & identification$edad< 30~"joven",
                                              identification$edad>=30 & identification$edad< 40~"adulto_joven",
                                              identification$edad>=40 & identification$edad< 60~"adulto",
                                              identification$edad>=60 & identification$edad<=70~"adulto_mayor"))


##2.3 Sobre el objeto location, genere una variable llamada ambulante, que sera igual a 1 si la variableP3053 es igual a 3, 4 o 5.

location <-mutate(location, ambulante=case_when(location$P3053 =="3"~ "1",
                    location$P3053== "2" ~"2"
                    ,location$P3053== "4" ~"3"))

                                                                            

##3. Eliminar filas/columnas de un conjunto de datos##
##3.1 Almacene en un objeto llamado identification_sub las variables DIRECTORIO, SECUENCIA_P, SECUENCIA_ENCUESTA, grupo_etario, ambulante, COD_DEPTO y F_EXP.



ambulante <- location$ambulante

identification$ambulante <- location$ambulante

identification_sub <- identification %>% select(DIRECTORIO, SECUENCIA_P,SECUENCIA_ENCUESTA, grupo_etario,ambulante, COD_DEPTO, F_EXP) 


##3.2 Del objeto location seleccione solo las variables DIRECTORIO, SECUENCIA_P, SECUENCIA_ENCUESTA, ambulante P3054, P469, COD_DEPTO, F_EXP y guárdelo en nuevo objeto llamado location_sub.

Location_sub <- location %>%  select(DIRECTORIO,SECUENCIA_P,SECUENCIA_ENCUESTA,ambulante,P3054,P469,COD_DEPTO,F_EXP)

##4. Combinar bases de datos
##• 4.1 Use las variables DIRECTORIO, SECUENCIA_P y SECUENCIA_ENCUESTA para unir en una única basede datos, los objetos location_sub y identification_sub.

tabla_0 <- merge(location,identification, by = "DIRECTORIO")

##5. Descriptivas##
##5.1 Utilizando funciones como skim o summary, cree breves estadísticas descriptivas de la base de datos creada previamente. (HINT: Observaciones en NA, conteo de variables únicas)
tabla_0$DIRECTORIO

summary(tabla_0$DIRECTORIO)
summary(tabla_0$SECUENCIA_P.x)
##5.2. Use las funciones group_by y summarise para extraer variables descriptivas, como la cantidad de asociados por departamento, grupo etario, entre otros. Además, cree un pequeño párrafo con los hallazgos que encuentre.
                        
tabla_0%>%
  group_by(grupo_etario)%>%
  summarise(mean(edad))



tabla_0%>%
  group_by(bussines_type)%>%
  summarise(min(edad))

tabla_0%>%
  group_by(bussines_type)%>%
  summarise(mean(edad))









