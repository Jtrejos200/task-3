#Taller final
#Jaime Andres Trejos -201914673
rm(list=ls())
require(pacman)
p_load (readxl,haven,dplyr,plyr,data.table,sf,tidyverse,skimr,leaflet,ggsn,ggspatial,plm,AER,margins,stargazer,outreg,arm,rockchalk,plyr,XML,rvest,xm12)
#Taller A 
#1)Datos espaciales
#1.1 Importar datos espaciales
#1.1.1
file.choose()
vias <- st_read("D:\\Documents\\task3\\task_3\\data\\input\\VIAS.shp") #importamos Vias
ggplot (vias) +geom_sf() #visualizamos
puntos <- st_read("D:\\Documents\\task3\\task_3\\data\\input\\MGN_URB_TOPONIMIA.shp") #importamos puntos
ggplot (puntos) +geom_sf() #visualizamos puntos
#1.1.2
c_medico<- puntos %>% dplyr:: filter(CSIMBOL =='021001'|
                                       CSIMBOL== '021002'|
                                       CSIMBOL=='021003') #Creamos objeto
ggplot()+
  geom_sf(data= vias)+
  geom_sf(data=c_medico) #visualizamos
#1.1.3
c_poblado <- readRDS(file="D:\\Documents\\task3\\task_3\\data\\input\\c poblado (2017).rds") %>%
  filter(cod_dane>=54001 & cod_dane<55000) #creamos objeto y filtramos
depto<-readRDS(file="D:\\Documents\\task3\\task_3\\data\\input\\dp deptos (2017).rds")%>%
  filter(name_dpto=='NORTE DE SANTANDER') #creamos objeto y filtramos
mapmuse<- readRDS(file="D:\\Documents\\task3\\task_3\\data\\input\\victimas_map-muse.rds")%>%
  filter(cod_mpio >=54001 & cod_mpio<55000) #creamos objeto y filtramos
#1.2
ls()
skim(c_poblado)
skim(depto)
skim(mapmuse)
#hacemos exploracion con skim a las variables del punto anterior
mapmuse$tipo_accidente %>% table()
mapmuse$condicion %>% table()
mapmuse$genero %>% table()
mapmuse$estado %>% table()
mapmuse$actividad %>% table()
#Tabla de frecuencia para algunas variables
#1.3.1
c_medico %>% st_bbox() 
c_medico %>% st_crs() 
depto %>% st_bbox() 
depto %>% st_crs()
c_poblado %>% st_bbox() 
c_poblado %>% st_crs()
mapmuse %>% st_bbox() 
mapmuse %>% st_crs()
puntos %>% st_bbox() 
puntos %>% st_crs()
vias %>% st_bbox() 
vias %>% st_crs()
#con el comando st_bbox obtenemos la caja de coordenadas y con el comando st_crs sistema de coordenadas de georeferencia
#1.3.2
c_medico_t <- c_medico%>%
  st_transform(crs= "+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs") #creamos nuevo objeto con el archivo reproyectado
c_medico_t %>% st_crs()
#repetimos el proceso para cada uno
c_poblado_t <- c_poblado%>%
  st_transform(crs= "+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs")
c_depto_t <- depto%>%
  st_transform(crs= "+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs")
c_mapmuse_t <- mapmuse%>%
  st_transform(crs= "+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs")
c_puntos_t <- puntos%>%
  st_transform(crs= "+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs")
#1.4
mapmuse_norte <- st_intersection(x=mapmuse,y=depto) #filtamos los puntos unicamente para Norte de santander
ggplot(mapmuse)+geom_sf()
ggplot(mapmuse_norte) +geom_sf() #Pintamos el objeto recortado obtenido
#1.4.2
view(c_poblado)
#en el municipio Abrego hay un centro poblado
abrego <- c_poblado %>% filter(cod_dane=='54003')
ggplot(abrego) +geom_sf() #visualizamos el municipio de Abrego
via_abrego<- st_intersection(x=vias,y=abrego) #Unimos las vias con abrego para encontrar las vias dentro de abrego
via_abrego
ggplot()+
  geom_sf(data=abrego)+
  geom_sf(data=via_abrego) #pintamos las vias en abrego
via_abrego<- via_abrego %>% mutate(Largo=st_length(via_abrego)) %>%
  arrange(desc(Largo)) #Calculamos el largo de las vias
via_abrego
ggplot()+
  geom_sf(data=abrego)+
  geom_sf(data=via_abrego)+
  geom_sf(data=via_abrego[1:2,],color='red',size=1.5) #pintamos las vias mmas largas
#1.5
leaflet(data = depto) %>% 
  addTiles() %>% addProviderTiles(providers$OpenStreetMap) %>% 
  addPolygons(fill = FALSE, stroke = TRUE, color = "black") %>% #pintamos el departamento
  addPolygons(data = c_poblado,fill = TRUE,stroke = TRUE,color = 'red') %>% #filtramos para los centros poblados en color rojo
  addCircleMarkers(data = c_medico,radius = 2.5,color = 'black') #filtramos para los centros medicos que quedan en color negro
#pintamos el mapa
#1.5.2
c_poblado <- mutate(c_poblado,Lugar = factor(c('Centros Poblados')))
c_medico <- mutate(c_medico,Lugar = factor(c('Centros medicos')))
#creamos variable para identificar los lugares
Mapa_1 <- ggplot() + 
  geom_sf(data = depto,fill = 'white',color = 'black') +
  geom_sf(data = c_poblado,aes(fill = Lugar,colour = Lugar)) +   
  geom_sf(data = c_medico,aes(fill = Lugar,colour = Lugar),size = 2,shape = 1) +
  labs(title = "Poblados y centros medicos del Norte de Santander", 
       caption = "Fuente: Elaboración propia basado en datos del DANE", 
       x = 'Longitud',
       y = 'Latitud') +
  theme(panel.background = element_rect(fill = "white"), #Color del plot
        plot.background = element_rect(fill = "white"), #Color del fondo del plot
        plot.title = element_text(hjust = 0.5, #Titulo del mapa centrado
                                  size = 16,    #Tamaño del titulo
                                  family = "serif", #Fuente de letra
                                  face = "bold"),   #Tipo de letra
        panel.grid = element_blank(), #Eliminar cuadricula del mapa
        legend.key.size = unit(0.5,"cm"), #Tamaño de la leyenda
        legend.key.width = unit(0.8,'line'),
        legend.position = c(0.2,0.15)) + #Lugar de la leyenda 
  annotation_scale()   +    #Este comando añade la escala en el mapa
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_nautical, 
                         height = unit(2.5, "cm"),
                         width = unit(2.5, "cm"))
#para exportar
ggsave(filename= "Mapa 1.pdf",
plot= Mapa_1,
width=20,
height=15,
unit='cm')
#2)Regresiones
#2.1
df_mapmuse<-readRDS(file="D:\\Documents\\task3\\task_3\\data\\output\\f_mapmuse.rds") #Importamos la base
names (df_mapmuse) #nombre de variables
apply(df_mapmuse[,c(1,4,5,6,11)],2,table) #tabla de frecuencia para las variables
#La variable tipo de actividad tiene muchas categorias lo que dificulta la regresion
F1<- fallecido~ factor(tipo_accidente) + factor(condicion)+ factor(genero)+dist_hospi+dist_cpoblado+dist_vias #modelo estimado
ols <- lm(formul=F1,data=df_mapmuse) #nombramos y estimamos como OLS
ols
summary(ols)
#2.2
#Cargamos la funcion coefplot de la pagina web
source("https://www.r-statistics.com/wp-content/uploads/2010/07/coefplot.r.txt")

#Graficamos y exportamos la figura
png(file = "Coeficientes.png") 
coefplot(ols)
dev.off()
#Exportamos y añadimos intervalos de confianza
#2.3
Logit<- glm(formula=F1,data=df_mapmuse,family=binomial(link="logit"))
Logit%>% summary()
#Estimamos el modelo 1 por Logit
Probit<- glm(formula=F1,data=df_mapmuse,family=binomial(link="probit"))
Probit%>% summary()
#Estimamos el modelo 1 por probit
#2.4
lista_modelos <- list(ols,Logit,Probit) #Unimos los modelos
Tabla1<- outreg::outreg(setNames(lista_modelos,c('ols','Logit','Probit')))
write.csv(Tabla1,'Tabla1.csv',row.names=F)    
#Exportamos el modelo en una misma tabla
#2.5
png(file="Efecto Logit.png")
cplot(Logit,"dist_hospi")
dev.off()
#Exportamos grafica de efecto marginal para Logit
png(file="Efecto Probit.png")
cplot(Probit,"dist_hospi")
dev.off()
#Exportamos grafica de efecto marginal para Probit
#3) Web scrapping
browseURL("https://es.wikipedia.org/wiki/Departamentos_de_Colombia",browser=getOption('browser')) #Obtenemos la direccion
Direccion<- 'https://es.wikipedia.org/wiki/Departamentos_de_Colombia' #establecemos direccion
my_html<-read_html(Direccion) #reestablecemos el objeto
class(my_html) #verificamos que sea el tipo correcto de archivo
#3.2 
Titulo <- my_html %>% 
  html_nodes(xpath = '//*[@id="firstHeading"]') %>% 
  html_text() # Convertir en texto
#De my_html filtramos el titulo utilizando el codigo
Titulo
#3.3
tfinal <- read_html(Direccion) %>% htmlParse()#extraemos el html de las tablas
tablas <-tfinal %>% readHTMLTable(header = T)#Extraer todas las tablas
Departamentos <- tablas[[4]] #escogemos la tabla 4 de los departamentos de Colombia
View(Departamentos)



