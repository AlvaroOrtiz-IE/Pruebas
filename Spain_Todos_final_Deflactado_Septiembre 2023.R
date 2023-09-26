######## MASTER GIFS DE GRÁFICOS WEB######
# Clean internal memory
rm(list=ls())
gc()

setwd("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R")
library("ggplot2")
library("lubridate")
library("gganimate")
library("dplyr")
library("tidyverse")
library("gifski")
library("tweenr")
library('magrittr')
library('stringr')
library('ggtext')
library('reshape2')
library('scales')
library('ggrepel')
library('plotly')
library('htmlwidgets')
library('magick')
library('readr')

##set date of last data available
Sys.Date <- '2021-08-01'

date <- '2023-09-30'
#fecha del eje X para ir sumando de 15 en 15
graph_date <- '2023-09-30'
max_date <- '2023-09-30'

#Defining  dates language for PC
Sys.setlocale("LC_TIME", "en_US.UTF-8")

#set the resolution for the gif
options(gganimate.dev_args = list(res=180))

theme_BBVA_grey <- theme_grey(base_line_size = 0.1) + 
  #theme(plot.title = element_text(color = "#02A59C",size=12,face="bold",family = "Arial")) +
  #theme(plot.subtitle = element_text(color = "#02A59C",size=9)) +
  theme(plot.title = element_text(color = "#072146",size=14,face="bold",family = "Arial")) +
  theme(plot.subtitle = element_text(color = "#072146",size=10)) +
  
  
  theme(plot.caption = element_text(color = "#072146",size=6)) +
  theme(axis.title.x = element_text(color = "#072146",size=8)) +
  theme(axis.title.y = element_text(color = "#072146",size=8)) + 
  theme(axis.text.x = element_text( color= "#072146", size=7))+ 
  theme(axis.text.y = element_text( color= "#666666", size=8)) +
  theme(axis.line = element_blank() ,
        panel.border = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.1, color="#FFFFFF" ), 
        panel.grid.major.x = element_blank() ,
        panel.grid.minor.x = element_blank() ) +
  theme (legend.position = "bottom", # 'none' to disable label legends, 'bottom' to enable
         legend.justification = "center",
         legend.margin=margin(0,0,0,0),
         legend.box.margin=margin(-21,0,-5,0),
         legend.text = element_text(size=10, colour = "#072146" ))  + 
  theme(axis.ticks.y = element_blank())  + 
  theme( panel.grid = element_blank(), panel.spacing = unit(1, "lines"),
         strip.text = element_text(size = 10, colour ='#072146' ), 
         strip.background = element_blank(),
         legend.margin=margin(0,0,0,0),
         legend.box.margin=margin(-20, 0,-5,0),
         legend.key.height = unit(10,"pt") ,
         legend.key.width = unit(10,"pt"),
         legend.spacing.x = unit(.5,"lines"),
         legend.box.just = 'center' 
  )

#definir función de la transformación para ver los datos mejor

# John and Draper's modulus transformation
modulus_trans <- function(lambda){
  trans_new("modulus",
            transform = function(y){
              if(lambda != 0){
                yt <- sign(y) * (((abs(y) + 1) ^ lambda - 1) / lambda)
              } else {
                yt = sign(y) * (log(abs(y) + 1))
              }
              return(yt)
            },
            inverse = function(yt){
              if(lambda != 0){
                y <- ((abs(yt) * lambda + 1)  ^ (1 / lambda) - 1) * sign(yt)
              } else {
                y <- (exp(abs(yt)) - 1) * sign(yt)
                
              }
              return(y)
            }
  )
}

##########################################################################
###TOTAL CONSUMPTION sectores españa
##########################################################################
Sys.setlocale("LC_TIME", "English")

library(magick) 
library(ggplot2)
library(gganimate)
library(lubridate)
library(png)
library(ggplot2)



##################3
#Spanish
##################33
# Indicate file to read:
country<-"Spain"
file <- 'hoja sectorial Spain deflactado_v2.xlsx'

pais <- openxlsx::read.xlsx(file, sheet = country , startRow = 1)[,1:124]

names(pais)[1] <- 'Date'
names(pais)[2] <- 'Consumo Tarjetas'
names(pais)[3] <- 'Efectivo'
names(pais)[4] <- 'On-Line'
names(pais)[5] <- 'Físico'
names(pais)[6] <- 'Alimentación'
names(pais)[7] <- 'Ocio'
names(pais)[8] <- 'Restaurantes'
names(pais)[9] <- 'Hoteles'
names(pais)[10] <- 'Salud'
names(pais)[11] <- 'Bienes'
names(pais)[12] <- 'Servicios'
names(pais)[13] <- 'Consumo Transportes'
####### Datos de Inversion
names(pais)[14] <- 'Construccion Vivienda'
names(pais)[15] <- 'Construccion Otros '
names(pais)[16] <- 'Construccion '
names(pais)[17] <- 'Inversión Transporte'
names(pais)[18] <- 'Inversion B. Equipo'
names(pais)[19] <- 'Inversión Maquinaria y Eq'
names(pais)[20] <- 'Intangibles '
names(pais)[21] <- 'Inversion Total'
names(pais)[22] <- 'Inversion Digital'
#######3 Datos de Sector Exterior
names(pais)[23] <- 'Importaciones'
names(pais)[24] <- 'Exportaciones'
names(pais)[25] <- 'Consumo Total (%YoY)'
names(pais)[26] <- 'Tarjetas'
names(pais)[27] <- 'Domiciliaciones'
names(pais)[28] <- 'Transferencias'
names(pais)[29] <- 'Efectivo'
names(pais)[30] <- 'Alquileres'
names(pais)[31] <- 'Tarjetas Online'
names(pais)[32] <- 'Tarjetas Offline'
names(pais)[47] <- 'Consumo Total (% ToT CVEC)'
names(pais)[48] <- 'AEAT Total  (% ToT CVEC)'
names(pais)[49] <- 'AEAT Consumo (% ToT CVEC)'

########Series YoY#################

names(pais)[50] <- 'AEAT Total YoY'
names(pais)[51] <- 'AEAT Core YoY'
names(pais)[52] <- 'AEAT Manufacturas YoY'
names(pais)[53] <- 'AEAT Alimentacion YoY'
names(pais)[54] <- 'AEAT Bebidas Tabaco YoY'
names(pais)[55] <- 'AEAT Textil YoY'
names(pais)[56] <- 'AEAT Papel YoY'
names(pais)[57] <- 'AEAT Quimica YoY'
names(pais)[58] <- 'AEAT P.Farmaceuticos YoY'
names(pais)[59] <- 'AEAT Caucho y Plasticos YoY'
names(pais)[60] <- 'AEAT Metalurgia YoY'
names(pais)[61] <- 'AEAT Informatica y Electronica YoY'

names(pais)[62] <- 'AEAT Vehiculos YoY'
names(pais)[63] <- 'AEAT Maderas YoY'
names(pais)[64] <- 'AEAT Maquinaria y Equipo YoY'
names(pais)[65] <- 'AEAT Refineria YoY'
names(pais)[66] <- 'AEAT Energia  YoY'

names(pais)[67] <- 'AEAT Costruccion YoY'
names(pais)[68] <- 'AEAT Construccion Edificios YoY'
names(pais)[69] <- 'AEAT Construccion Ingenieria YoY'
names(pais)[70] <- 'AEAT Construccion Especializada YoY'

names(pais)[71] <- 'AEAT Comercio por Mayor YoY'
names(pais)[72] <- 'AEAT Ventas y Rep Vehiculos YoY'
names(pais)[73] <- 'AEAT Comercio por Mayor ex Autos YoY'

names(pais)[74] <- 'AEAT Comercio por Menor ex autos  YoY'
names(pais)[75] <- 'AEAT Comercio por Menor no espec  YoY'
names(pais)[76] <- 'AEAT Comercio por Menor Combustible  YoY'
names(pais)[77] <- 'AEAT Resto Comercio por Menor YoY'

names(pais)[78] <- 'AEAT Transporte y Almac. YoY'
names(pais)[79] <- 'AEAT Hosteleria YoY'
names(pais)[80] <- 'AEAT Alojamiento YoY'
names(pais)[81] <- 'AEAT Serv. Comidas y Bebidas YoY'

names(pais)[82] <- 'AEAT Info y Cmunicaciones YoY'
names(pais)[83] <- 'AEAT Actividades Profesionales YoY'

########Series ToT#################

names(pais)[84] <- 'AEAT Total ToT'
names(pais)[85] <- 'AEAT Core ToT'
names(pais)[86] <- 'AEAT Manufacturas ToT'
names(pais)[87] <- 'AEAT Alimentacion ToT'
names(pais)[88] <- 'AEAT Bebidas Tabaco ToT'
names(pais)[89] <- 'AEAT Textil ToT'
names(pais)[90] <- 'AEAT Papel ToT'
names(pais)[91] <- 'AEAT Quimica ToT'
names(pais)[92] <- 'AEAT P.Farmaceuticos ToT'
names(pais)[93] <- 'AEAT Caucho y Plasticos ToT'
names(pais)[94] <- 'AEAT Metalurgia ToT'
names(pais)[95] <- 'AEAT Informatica y Electronica ToT'

names(pais)[96] <- 'AEAT Vehiculos ToT'
names(pais)[97] <- 'AEAT Maderas ToT'
names(pais)[98] <- 'AEAT Maquinaria y Equipo ToT'
names(pais)[99] <- 'AEAT Refineria ToT'
names(pais)[100] <- 'AEAT Energia  ToT'

names(pais)[101] <- 'AEAT Costruccion ToT'
names(pais)[102] <- 'AEAT Construccion Edificios ToT'
names(pais)[103] <- 'AEAT Construccion Ingenieria ToT'
names(pais)[104] <- 'AEAT Construccion Especializada ToT'

names(pais)[105] <- 'AEAT Comercio por Mayor ToT'
names(pais)[106] <- 'AEAT Ventas y Rep Vehiculos ToT'
names(pais)[107] <- 'AEAT Comercio por Mayor ex Autos ToT'

names(pais)[108] <- 'AEAT Comercio por Menor ex autos  ToT'
names(pais)[109] <- 'AEAT Comercio por Menor no espec  ToT'
names(pais)[110] <- 'AEAT Comercio por Menor Combustible  ToT'
names(pais)[111] <- 'AEAT Resto Comercio por Menor ToT'

names(pais)[112] <- 'AEAT Transporte y Almac. ToT'
names(pais)[113] <- 'AEAT Hosteleria ToT'
names(pais)[114] <- 'AEAT Alojamiento ToT'
names(pais)[115] <- 'AEAT Serv. Comidas y Bebidas ToT'

names(pais)[116] <- 'AEAT Info y Cmunicaciones ToT'
names(pais)[117] <- 'AEAT Actividades Profesionales ToT'

######### Nuevas Inversion
names(pais)[118] <- 'BBVA Inversion Construccion YoY'
names(pais)[119] <- 'BBVA Inversion Intangibles YoY'
names(pais)[120] <- 'BBVA Inversion Transporte Eq YoY'
names(pais)[121] <- 'BBVA Inversion Otros Eq YoY'
names(pais)[122] <- 'BBVA Inversion B Equipo YoY'
names(pais)[123] <- 'BBVA Inversion Total YoY'
names(pais)[124] <- 'AEAT Intangibles YoY'




pais




#######################################################
########## Graficos Consumo & Spanish ##############################
filename <- file("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")
logo <- readPNG("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")

pais <- openxlsx::read.xlsx(file, sheet = country , startRow = 1)[,1:49]

xmax <- ymd(max_date)-10
xmin <- ymd(max_date)-90
ymin <- 0.025
ymax <- 0.04
names(pais)[1] <- 'Date'
names(pais)[25] <- 'Consumo Total (% Anual)'
names(pais)[47] <- 'Consumo Total (% Trimestral CVE)'







origin <-"1899-12-30"

start_date<- "2021-01-01"
pais$Date <- as.Date(pais$Date , origin = origin)

#pais$Date <- as.Date(pais$Date , origin = "1899-12-30")

pais <- select(pais,1,25, 47)
pais
fechas <- select(pais, Date)
fechas$variable <- 'Date'
fechas$value <- -2

pais <-  pais %>% reshape2::melt(id.vars='Date') %>%  na.omit()

pais$Value <- scales::percent(pais$value, accuracy = 0.01 )
pais$Value <- paste(pais$variable, pais$Value, sep = ": ")
fechas$newdate <- strptime(as.character(fechas$Date), "%Y-%m-%d")
fechas$newdate <- format(fechas$newdate, "%d-%m-%Y")
fechas$Value <- paste(fechas$variable, fechas$newdate, sep = ": ")
fechas <- select(fechas, -newdate)
pais <- rbind(pais, fechas)

colnames(pais)[1] <- 'Date'


#scale_y_continuous(
#  "mpg (US)", 
#  sec.axis = sec_axis(~ . * 1.20, name = "mpg (UK)")
#)


gg_demanda <- pais %>% group_by(variable,Date) %>% 
  ggplot( aes(x=Date, y= value,  colour = variable))+ 
  geom_line() +
  #geom_point(show.legend = TRUE) +
  geom_richtext(aes(x = max(Date)-90, y = min(value)+.1, label = format(Date, '%b-%Y') ), text.colour ="#666666", show.legend=F, 
                fill = NA, label.color = NA, size=10) + #Date inside plot
  theme_BBVA_grey +
  annotation_raster(logo, ymin = ymin, ymax = ymax, xmin = xmin, xmax =xmax) +
  scale_color_manual(values=c( "#0F4B7C","#2DCCCD","#FFFFFF"),
                     # scale_color_manual(values=c( "#0F4B7C","#2DCCCD","#9DC1F5","#D8BE75"),
                     breaks= c("Consumo Total (% Anual)",  "Consumo Total (% Trimestral CVE)")) +
  scale_y_continuous(trans = modulus_trans(0.003), breaks =  c(-0.08, -0.06,-0.04,-0.02,0,0.02,0.04),
                     labels = c('-8%', '-6%','-4%','-2%','0%','2%','4%'),
                     limits = c(-0.08, 0.04),
                     sec.axis=sec_axis(~.*1,breaks= c(-0.08,-0.06,-0.04,-0.02,0,0.02,0.04),labels = c( '-8%', '-6%','-4%','-2%','0%','2%','4%'))) + 
  
  
  scale_x_date(limits=c(as.Date("2022-03-01"), as.Date(date)), date_breaks = "2 months", date_labels = "%b-%y") +
  
  #geom_vline(xintercept = as.Date("2020-03-15"), colour="black",linetype = "dotted") +
  #geom_text(aes(x=as.Date("2020-03-13"), label="Covid in EU", y=1), colour="#666666", angle=90, vjust = 1.2, size=3)+
  
  #geom_vline(xintercept = as.Date("2021-03-02"), colour="black",linetype = "dotted") +
  #geom_text(aes(x=as.Date("2021-03-01"), label="Delta Variant in EU", y=05), colour="#666666", angle=90, vjust =0., size=3)+
  
  #geom_vline(xintercept = as.Date("2021-11-22"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2021-11-21"), label="Omicron", y=0.05), colour="#666666", angle=90, vjust = 1.2,size=3)+
  
  #geom_vline(xintercept = as.Date("2022-02-21"), colour="black",linetype = "dotted") +
  #geom_text(aes(x=as.Date("2022-02-20"), label="Rusia-Ucrania", y=0.045), colour="#666666", angle=90, vjust = 1.2,size=3)+
  transition_reveal(Date,keep_last = TRUE)  +
  exit_recolor() +
  #enter_fade() +
  #ease_aes("circular-out") +
  guides(col = guide_legend(nrow = 1, reverse = F)) +
  labs(title =  "España: Indicador BigData de Consumo Total (BBVA Research)",  
       caption= "Fuente: BBVA Research", 
       subtitle = "(Real deflactado por IPC, Acum 28D % Var. InterAnual & Acum 90D % Var. Trimestral CVE)")  # Subtitle Date
gg_demanda <- gg_demanda + labs(x="", y = '% YoY Real (deflactado por IPC )', positiony="left",color="") 

animate(gg_demanda, nframes = 200, duration = 7 ,fps = 32, width = 1200,
        height = 750, end_pause = 100)

anim_save("G1 Consumo Total anual y trimestral .gif")

filename <- file("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")
logo <- readPNG("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")




############ Grafico 2 Coicop BArs ##########
###################################

library(ggplot2)
library(gganimate)
library(tidyverse)
library(dplyr)


country<-"Spain"
file <- 'hoja sectorial Spain deflactado.xlsx'
coicops <- openxlsx::read.xlsx(file, sheet = country , startRow = 1)[,c(25,33,34,35,36,37,38,39,40,41,42,43,44)]



names(coicops) <- c('Total',
                    'Alimentación',
                    'Alcohol & Tabaco',
                    'Vestido y Calzado',
                    'Consumo energía del hogar',
                    'Equipamiento del hogar',
                    'Salud',
                    'Transporte',
                    'Comunicaciones',
                    'Ocio',
                    'Educación',
                    'Restaurantes y hoteles',
                    'Seguros & Serv. Financieros')


# Read the new consumption template file
# Get last available data
last_row <- tail(coicops, n =1)
last_data <- pivot_longer(last_row, cols=everything(), names_to = 'Coicop', values_to = "Value")
last_data <- last_data %>% mutate( ToHighlight = ifelse( Coicop == 'Total', "yes", "no" ) )
last_data <- last_data[order(last_data$Value),]


xmin <- tail(last_data$Coicop, n=1) # Coicop con valor mas positivo
xmax <- last_data$Coicop[11]
ymin <- min(last_data$Value) # Valor mas negativo de todos los coicops
ymax <- min(last_data$Value) + 0.05




# Barchart 
coicop_plot2 <- ggplot(last_data, aes(x=reorder(Coicop, Value), y=Value, fill=ToHighlight)) +
  geom_bar(stat='identity') +
  scale_fill_manual( values = c( "yes"="#072146", "no"="#19e5e6"), guide = "none")+
  coord_flip()+
  annotation_raster(logo/2, ymin = ymin, ymax = ymax, xmin = -8, xmax =-4) +
  
  scale_y_continuous(breaks =  c(-0.16,-0.12,-0.08,-0.04,0, 0.04,0.08,0.12,0.16,0.20,0.24),
                     labels = c('-16%','-12%','-8%','-4%','0%','4%','8%','12%','16%','20%','24%'),
                     limits = c(-0.16, 0.24)) +
  
  
  
  geom_text(
    aes(label = round(100*Value,1)),
    size = 3,
    hjust = 0.5- sign(last_data$Value)/1.25) +
  
  theme_BBVA_grey +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) + 
  
  labs(title =  "Consumo por Categorías COICOP (BBVA Research)",  
       # caption= "Source: BBVA Research",
       subtitle = "(Real deflactado por IPC Coicops, Acum 28D Tasa anual)")  +
  
  transition_states(
    states = reorder(Coicop, -Value), transition_length = 2, state_length = 1) +
  shadow_mark() +
  enter_fade(alpha=0) 


animate(plot = coicop_plot2,
        width = 1200,
        height = 750,
        end_pause = 100,
        duration = 9,
        fps = 20)

anim_save("G2 COICOP_BArs.gif")


#####################
#GRAFICO 3
#######################################################
########## Grafico Inversion ##############################
#filename <- file("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")
#logo <- readPNG("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")

filename <- file("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")
logo <- readPNG("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")

xmax <- ymd(max_date)
xmin <- ymd(max_date)-60
ymin <- 0.35
ymax <- 0.43




country<-"Spain"
file <- 'hoja sectorial Spain deflactado.xlsx'

pais <- openxlsx::read.xlsx(file, sheet = country , startRow = 1)[,1:45]






names(pais)[14] <- 'Construccion Vivienda'
names(pais)[15] <- 'Construccion Otros '
names(pais)[16] <- 'Construcción'
names(pais)[17] <- 'Transporte'
names(pais)[18] <- 'B. Equipo'
names(pais)[19] <- 'Maquinaria y Equipo'
names(pais)[20] <- 'Intangibles'
names(pais)[21] <- 'Total'
names(pais)[22] <- 'Digital'

origin <-"1899-12-30"
pais$Date <- as.Date(pais$Date , origin = origin)

#pais$Date <- as.Date(pais$Date , origin = "1899-12-30")

pais <- select(pais,1, 21, 16,19,20)
pais
fechas <- select(pais, Date)
fechas$variable <- 'Date'
fechas$value <- -2

pais <-  pais %>% reshape2::melt(id.vars='Date') %>%  na.omit()

pais$Value <- scales::percent(pais$value, accuracy = 0.01 )
pais$Value <- paste(pais$variable, pais$Value, sep = ": ")
fechas$newdate <- strptime(as.character(fechas$Date), "%Y-%m-%d")
fechas$newdate <- format(fechas$newdate, "%d-%m-%Y")
fechas$Value <- paste(fechas$variable, fechas$newdate, sep = ": ")
fechas <- select(fechas, -newdate)
pais <- rbind(pais, fechas)

colnames(pais)[1] <- 'Date'




gg_inversion <-    pais %>% group_by(variable, Date) %>% 
  ggplot( aes(x=Date, y= value,  colour = variable))+ 
  annotation_raster(logo, ymin = ymin, ymax = ymax, xmin = xmin, xmax =xmax) +
  geom_line() +
  geom_point(show.legend = FALSE) + # ends with a point
  geom_richtext(aes(x = max(Date)-90, y = min(value)+.1, label = format(Date, '%b-%Y') ), text.colour ="#666666", show.legend=F, 
                fill = NA, label.color = NA, size=10) + #Date inside plot
  theme_BBVA_grey +
  scale_color_manual(values=c( "#0F4B7C","#D8BE75" ,"#2DCCCD","#9DC1F5" ,"#9DC1F5"),
                     breaks= c("Total", "Construcción","Maquinaria y Equipo","Intangibles")) + 
  scale_y_continuous(trans = modulus_trans(0.003), breaks =  c( -0.1,0,0.1,0.2,0.3),
                     labels = c('-10%','0%','10%','20%','30%' ),
                     limits = c(-0.1, 0.3), 
                     sec.axis=sec_axis(~.*1,breaks= c(-0.1,0,0.1,0.2,0.3),labels = c('-10%','0%','10%','20%','30%' ))) +   
  
  scale_x_date(limits=c(as.Date("2022-07-01"), as.Date(date)), date_breaks = "2 months", date_labels = "%b-%y") +
  
  geom_vline(xintercept = as.Date("2020-03-15"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2020-03-13"), label="Covid in EU", y=1), colour="#666666", angle=90, vjust = 1.2, size=3)+
  
  geom_vline(xintercept = as.Date("2021-03-02"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2021-03-01"), label="Delta Variant in EU", y=1), colour="#666666", angle=90, vjust = 1.2, size=3)+
  
  geom_vline(xintercept = as.Date("2021-11-22"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2021-11-21"), label="Omicron", y=0.3), colour="#666666", angle=90, vjust = 1.2,size=3)+
  
  geom_vline(xintercept = as.Date("2022-02-21"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2022-02-20"), label="Rusia-Ucrania", y=0.3), colour="#666666", angle=90, vjust = 1.2,size=3)+
  transition_reveal(Date)  +
  exit_shrink() +
  enter_fade() +
  ease_aes("circular-out") +
  guides(col = guide_legend(nrow = 1, reverse = F)) +
  labs(title =  "España: Indicadores de Inversión BigData",  
       caption= "Fuente: BBVA Research", 
       subtitle = "(Real, Acumulado 28D Crecimiento Interanual - Date:{format(frame_along, '%d-%m-%Y')})") # Subtitle Date
gg_inversion <- gg_inversion + labs(x="", y = '% YoY Real (deflactado po Deflactores Mensualizados)', color="") 

animate(gg_inversion, nframes = 200, duration = 7 ,fps = 32, width = 1200,
        height = 750, end_pause = 100)

anim_save("G3 Inversion_España_Real.gif")
anim_save("G3 Inversion_España_Real_digital.gif")


###################################
#######################################################
########## Grafico 4 PIB Demanda Externa ##############################
country<-"Spain"
file <- 'hoja sectorial Spain deflactado.xlsx'

pais <- openxlsx::read.xlsx(file, sheet = country , startRow = 1)[,1:48]

filename <- file("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")
logo <- readPNG("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")

xmax <- ymd(max_date)
xmin <- ymd(max_date)-60
ymin <- 0.45
ymax <- 0.6

origin <-"1899-12-30"
start_date<- "2021-12-01"
pais$Date <- as.Date(pais$Date , origin = origin)

pais$Date <- as.Date(pais$Date , origin = "1899-12-30")


names(pais)[23] <- 'Importaciones'
names(pais)[24] <- 'Exportaciones'
names(pais)[25] <- 'Consumo Total'
pais




pais <- select(pais,1,23, 24)
pais
fechas <- select(pais, Date)
fechas$variable <- 'Date'
fechas$value <- -2

pais <-  pais %>% reshape2::melt(id.vars='Date') %>%  na.omit()

#pais$Value <- scales::percent(pais$value, accuracy = 0.01 )
pais$Value <- paste(pais$variable, pais$Value, sep = ": ")
fechas$newdate <- strptime(as.character(fechas$Date), "%Y-%m-%d")
fechas$newdate <- format(fechas$newdate, "%d-%m-%Y")
fechas$Value <- paste(fechas$variable, fechas$newdate, sep = ": ")
fechas <- select(fechas, -newdate)
pais <- rbind(pais, fechas)

colnames(pais)[1] <- 'Date'





gg_demanda <-    pais %>% group_by(variable, Date) %>% 
  ggplot( aes(x=Date, y= value,  colour = variable))+ 
  annotation_raster(logo, ymin = ymin, ymax = ymax, xmin = xmin, xmax =xmax) +
  geom_line() +
  geom_point(show.legend = FALSE) + # ends with a point
  geom_richtext(aes(x = max(Date)-90, y = min(value)+.1, label = format(Date, '%b-%Y') ), text.colour ="#666666", show.legend=F, 
                fill = NA, label.color = NA, size=10) + #Date inside plot
  theme_BBVA_grey +
  scale_color_manual(values=c( "#0F4B7C","#2DCCCD","#9DC1F5","#D8BE75","#FFFFFF"),
                     #                 breaks= c("Consumo Total", "Inversion Total", "Importaciones", "Exportaciones")) + 
                     breaks= c("Importaciones", "Exportaciones")) +
  scale_y_continuous(trans = modulus_trans(0.003), breaks =  c(-0.6,-0.4,-0.2,0.0,0.2,0.4,0.6,0.8,1.0),
                     labels = c('-60%','-40%','-20%','0%','20%','40%','60%','80%','100%'),
                     limits = c(-0.6, 1.0),
                     sec.axis=sec_axis(~.*1,breaks= c(-0.4,-0.2,0.0,0.2,0.4,0.6,0.8,1.0,1.2),labels = c('-40%','-20%','0%','20%','40%','60%','80%','100%','120%'))) +   
  
  scale_x_date(limits=c(as.Date("2022-07-01"), as.Date(date)), date_breaks = "2 months", date_labels = "%b-%y") +
  
  geom_vline(xintercept = as.Date("2020-03-15"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2020-03-13"), label="Covid in EU", y=1), colour="#666666", angle=90, vjust = 1.2, size=3)+
  
  geom_vline(xintercept = as.Date("2021-03-02"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2021-03-01"), label="Delta Variant in EU", y=1), colour="#666666", angle=90, vjust = 1.2, size=3)+
  
  geom_vline(xintercept = as.Date("2021-11-22"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2021-11-21"), label="Omicron", y=0.7), colour="#666666", angle=90, vjust = 1.2,size=3)+
  
  geom_vline(xintercept = as.Date("2022-02-21"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2022-02-20"), label="Rusia-Ucrania", y=0.7), colour="#666666", angle=90, vjust = 1.2,size=3)+
  transition_reveal(Date)  +
  exit_shrink() +
  enter_fade() +
  ease_aes("circular-out") +
  guides(col = guide_legend(nrow = 1, reverse = F)) +
  labs(title =  "España: Indicadores de Demanda Externa BigData",  
       caption= "Fuente: BBVA Research", 
       subtitle = "(Real, Acumulado 28D Crecimiento Interanual  - Date:{format(frame_along, '%d-%m-%Y')})")  # Subtitle Date
gg_demanda <- gg_demanda + labs(x="", y = '% YoY Real (deflactado por Deflactores Mensualizados)', color="") 

animate(gg_demanda, nframes = 200, duration = 7 ,fps = 32, width = 1200,
        height = 750, end_pause = 30)

anim_save("G4 Demanda Externa_España_Real.gif")



#######################################################
########## Graficos Consumo & Investment Trimestral Spain ##############################
filename <- file("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")
logo <- readPNG("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")

pais <- openxlsx::read.xlsx(file, sheet = country , startRow = 1)[,1:48]

xmax <- ymd(max_date)
xmin <- ymd(max_date)-60
ymin <- 0.33
ymax <- 0.4
names(pais)[1] <- 'Date'
names(pais)[47] <- 'Consumo Total (% Trimestral CVE)'
names(pais)[48] <- 'Inversion Total (% Trimestral CVE)'



xmax <- ymd(max_date)
xmin <- ymd(max_date)-60
ymin <- 0.06
ymax <- 0.08



origin <-"1899-12-30"
start_date<- "2021-12-01"
pais$Date <- as.Date(pais$Date , origin = origin)

#pais$Date <- as.Date(pais$Date , origin = "1899-12-30")

pais <- select(pais,1,47, 46)
pais
fechas <- select(pais, Date)
fechas$variable <- 'Date'
fechas$value <- -2

pais <-  pais %>% reshape2::melt(id.vars='Date') %>%  na.omit()

pais$Value <- scales::percent(pais$value, accuracy = 0.01 )
pais$Value <- paste(pais$variable, pais$Value, sep = ": ")
fechas$newdate <- strptime(as.character(fechas$Date), "%Y-%m-%d")
fechas$newdate <- format(fechas$newdate, "%d-%m-%Y")
fechas$Value <- paste(fechas$variable, fechas$newdate, sep = ": ")
fechas <- select(fechas, -newdate)
pais <- rbind(pais, fechas)

colnames(pais)[1] <- 'Date'


#scale_y_continuous(
#  "mpg (US)", 
#  sec.axis = sec_axis(~ . * 1.20, name = "mpg (UK)")
#)


gg_demanda <- pais %>% group_by(variable,Date) %>% 
  ggplot( aes(x=Date, y= value,  colour = variable))+ 
  geom_line() +
  geom_point(show.legend = FALSE) + # ends with a point
  geom_richtext(aes(x = max(Date)-90, y = min(value)+.1, label = format(Date, '%b-%Y') ), text.colour ="#666666", show.legend=F, 
                fill = NA, label.color = NA, size=10) + #Date inside plot
  theme_BBVA_grey +
  annotation_raster(logo, ymin = ymin, ymax = ymax, xmin = xmin, xmax =xmax) +
  scale_color_manual(values=c( "#0F4B7C","#2DCCCD","#FFFFFF"),
                     # scale_color_manual(values=c( "#0F4B7C","#2DCCCD","#9DC1F5","#D8BE75"),
                     breaks= c("Consumo Total (% Trimestral CVE)",  "Inversion Total (% Trimestral CVE)")) +
  scale_y_continuous(trans = modulus_trans(0.003), breaks =  c(-0.06,-0.04,-0.02,0,0.02,0.04,0.06,0.08),
                     labels = c('-6%','-4%','-2%','0%','2%','4%','6%','8%'),
                     limits = c(-0.06, 0.08),
                     sec.axis=sec_axis(~.*1,breaks= c(-0.06,-0.04,-0.02,0,0.02,0.04,0.06,0.08),labels = c('-6%','-4%','-2%','0%','2%','4%','6%','8%'))) + 
  
  
  scale_x_date(limits=c(as.Date("2021-08-01"), as.Date(date)), date_breaks = "2 months", date_labels = "%b-%y") +
  
  geom_vline(xintercept = as.Date("2020-03-15"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2020-03-13"), label="Covid in EU", y=1), colour="#666666", angle=90, vjust = 1.2, size=3)+
  
  geom_vline(xintercept = as.Date("2021-03-02"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2021-03-01"), label="Delta Variant in EU", y=05), colour="#666666", angle=90, vjust =0., size=3)+
  
  geom_vline(xintercept = as.Date("2021-11-22"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2021-11-21"), label="Omicron", y=0.05), colour="#666666", angle=90, vjust = 1.2,size=3)+
  
  geom_vline(xintercept = as.Date("2022-02-21"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2022-02-20"), label="Rusia-Ucrania", y=0.05), colour="#666666", angle=90, vjust = 1.2,size=3)+
  transition_reveal(Date)  +
  exit_shrink() +
  enter_fade() +
  ease_aes("circular-out") +
  guides(col = guide_legend(nrow = 1, reverse = F)) +
  labs(title =  "España: BigData Consumo & Inversion (BBVA Research)",  
       caption= "Fuente: BBVA Research", 
       subtitle = "(Real deflactado por IPC & Deflactor Mensualizado,  Acum 90D % Var. Trimestral CVE)")  # Subtitle Date
gg_demanda <- gg_demanda + labs(x="", y = '% YoY Real (deflactado por IPC y deflactor inversion )', positiony="left",color="") 

animate(gg_demanda, nframes = 250, duration = 7 ,fps = 35, width = 1200,
        height = 750, end_pause = 30)

anim_save(" Inversion.gif")


###################################################### Consumo y AEAT

filename <- file("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")
logo <- readPNG("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")

pais <- openxlsx::read.xlsx(file, sheet = country , startRow = 1)[,1:49]

xmax <- ymd(max_date)-10
xmin <- ymd(max_date)-70
ymin <- 0.36
ymax <- 0.05
names(pais)[1] <- 'Date'
names(pais)[49] <- 'AEAT Consumo (% ToT CVEC)'
names(pais)[47] <- 'Consumo BBVA (% ToT CVEC)'
names(pais)[48] <- 'AEAT Ventas Total (% ToT CVEC)'


origin <-"1899-12-30"

start_date<- "2023-01-01"
pais$Date <- as.Date(pais$Date , origin = origin)

#pais$Date <- as.Date(pais$Date , origin = "1899-12-30")

pais <- select(pais,1,48,47)
pais
fechas <- select(pais, Date)
fechas$variable <- 'Date'
fechas$value <- -2

pais <-  pais %>% reshape2::melt(id.vars='Date') %>%  na.omit()

pais$Value <- scales::percent(pais$value, accuracy = 0.01 )
pais$Value <- paste(pais$variable, pais$Value, sep = ": ")
fechas$newdate <- strptime(as.character(fechas$Date), "%Y-%m-%d")
fechas$newdate <- format(fechas$newdate, "%d-%m-%Y")
fechas$Value <- paste(fechas$variable, fechas$newdate, sep = ": ")
fechas <- select(fechas, -newdate)
pais <- rbind(pais, fechas)

colnames(pais)[1] <- 'Date'


#scale_y_continuous(
#  "mpg (US)", 
#  sec.axis = sec_axis(~ . * 1.20, name = "mpg (UK)")
#)


gg_demanda <- pais %>% group_by(variable,Date) %>% 
  ggplot( aes(x=Date, y= value,  colour = variable))+ 
  geom_line() +
#  geom_point(show.legend = TRUE) +
  geom_richtext(aes(x = max(Date)-90, y = min(value)+.1, label = format(Date, '%b-%Y') ), text.colour ="#666666", show.legend=F, 
                fill = NA, label.color = NA, size=10) + #Date inside plot
  theme_BBVA_grey +
  annotation_raster(logo, ymin = ymin, ymax = ymax, xmin = xmin, xmax =xmax) +
  scale_color_manual(values=c( "#0F4B7C","#2DCCCD","#FFFFFF"),
                     # scale_color_manual(values=c( "#0F4B7C","#2DCCCD","#9DC1F5","#D8BE75"),
                     breaks= c("AEAT Ventas Total (% ToT CVEC)",  "Consumo BBVA (% ToT CVEC)")) +
  scale_y_continuous(trans = modulus_trans(0.003), breaks =  c(-0.05,-0.04,-0.03,-0.02,-0.01,0.0,+0.01,+0.02,+0.03,+0.04,+0.05),
                     labels = c('-5%','-4%','-3%','-2%','-1%','0%','+1%','+2%','+3%','+4%','+5%'),
                     limits = c(-0.05, +0.05),
                     sec.axis=sec_axis(~.*1,breaks= c(-0.05,-0.04,-0.03,-0.02,-0.01,0.0,+0.01,+0.02,+0.03,+0.04,+0.05),labels = c('-5%','-4%','-3%','-2%','-1%','0%','+1%','+2%','+3%','+4%','+5%'))) + 
  

  
  
  scale_x_date(limits=c(as.Date("2022-08-01"), as.Date(date)), date_breaks = "1 months", date_labels = "%b-%y") +
  
  #geom_vline(xintercept = as.Date("2020-03-15"), colour="black",linetype = "dotted") +
  #geom_text(aes(x=as.Date("2020-03-13"), label="Covid in EU", y=1), colour="#666666", angle=90, vjust = 1.2, size=3)+
  
  #geom_vline(xintercept = as.Date("2021-03-02"), colour="black",linetype = "dotted") +
  #geom_text(aes(x=as.Date("2021-03-01"), label="Delta Variant in EU", y=05), colour="#666666", angle=90, vjust =0., size=3)+
  
  #geom_vline(xintercept = as.Date("2021-11-22"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2021-11-21"), label="Omicron", y=0.05), colour="#666666", angle=90, vjust = 1.2,size=3)+
  
  #geom_vline(xintercept = as.Date("2022-02-21"), colour="black",linetype = "dotted") +
  #geom_text(aes(x=as.Date("2022-02-20"), label="Rusia-Ucrania", y=0.045), colour="#666666", angle=90, vjust = 1.2,size=3)+
  transition_reveal(Date)  +
  #exit_shrink() +
  #enter_fade() +
  #ease_aes("circular-out") +
  guides(col = guide_legend(nrow = 1, reverse = F)) +
  labs(title =  "España: Consumo BigData BBVA y Ventas Totales AEAT ",  
       caption= "Fuente: BBVA Research & AEAT", 
       subtitle = "(Real deflactado por IPC y deflactor Ventas AEAT.Acum 91D % Var. Trimestral CVEC)")  # Subtitle Date
gg_demanda <- gg_demanda + labs(x="", y = '% ToT Real (deflactado por IPC & deflactor Ventas )', positiony="left",color="") 

animate(gg_demanda, nframes = 200, duration = 7 ,fps = 32, width = 1200,
        height = 750, end_pause = 40)

anim_save("G5bis Ventas AEAT y Consumo .gif")

filename <- file("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")
logo <- readPNG("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")

########## Consumo y Ventas de Consumo AEAT###########

filename <- file("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")
logo <- readPNG("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")

pais <- openxlsx::read.xlsx(file, sheet = country , startRow = 1)[,1:49]

xmax <- ymd(max_date)-10
xmin <- ymd(max_date)-70
ymin <- 0.36
ymax <- 0.05
names(pais)[1] <- 'Date'
names(pais)[47] <- 'Consumo Total (% ToT CVEC)'
names(pais)[48] <- 'AEAT Total  (% ToT CVEC)'
names(pais)[49] <- 'AEAT Consumo (% ToT CVEC)'


origin <-"1899-12-30"

start_date<- "2023-01-01"
pais$Date <- as.Date(pais$Date , origin = origin)

#pais$Date <- as.Date(pais$Date , origin = "1899-12-30")

pais <- select(pais,1,49,47)
pais
fechas <- select(pais, Date)
fechas$variable <- 'Date'
fechas$value <- -2

pais <-  pais %>% reshape2::melt(id.vars='Date') %>%  na.omit()

pais$Value <- scales::percent(pais$value, accuracy = 0.01 )
pais$Value <- paste(pais$variable, pais$Value, sep = ": ")
fechas$newdate <- strptime(as.character(fechas$Date), "%Y-%m-%d")
fechas$newdate <- format(fechas$newdate, "%d-%m-%Y")
fechas$Value <- paste(fechas$variable, fechas$newdate, sep = ": ")
fechas <- select(fechas, -newdate)
pais <- rbind(pais, fechas)

colnames(pais)[1] <- 'Date'


#scale_y_continuous(
#  "mpg (US)", 
#  sec.axis = sec_axis(~ . * 1.20, name = "mpg (UK)")
#)


gg_demanda <- pais %>% group_by(variable,Date) %>% 
  ggplot( aes(x=Date, y= value,  colour = variable))+ 
  geom_line() +
  #  geom_point(show.legend = TRUE) +
  geom_richtext(aes(x = max(Date)-90, y = min(value)+.1, label = format(Date, '%b-%Y') ), text.colour ="#666666", show.legend=F, 
                fill = NA, label.color = NA, size=10) + #Date inside plot
  theme_BBVA_grey +
  annotation_raster(logo, ymin = ymin, ymax = ymax, xmin = xmin, xmax =xmax) +
  scale_color_manual(values=c( "#0F4B7C","#2DCCCD","#FFFFFF"),
                     # scale_color_manual(values=c( "#0F4B7C","#2DCCCD","#9DC1F5","#D8BE75"),
                     breaks= c("AEAT Consumo (% ToT CVEC)",  "Consumo Total (% ToT CVEC)")) +
  scale_y_continuous(trans = modulus_trans(0.003), breaks =  c(-0.05,-0.04,-0.03,-0.02,-0.01,0.0,+0.01,+0.02,+0.03,+0.04),
                     labels = c('-5%','-4%','-3%','-2%','-1%','0%','+1%','+2%','+3%','+4%'),
                     limits = c(-0.05, +0.04),
                     sec.axis=sec_axis(~.*1,breaks= c(-0.05,-0.04,-0.03,-0.02,-0.01,0.0,+0.01,+0.02,+0.03,+0.04),labels = c('-5%','-4%','-3%','-2%','-1%','0%','+1%','+2%','+3%','+4%'))) + 
  
  
  
  
  scale_x_date(limits=c(as.Date("2022-08-01"), as.Date(date)), date_breaks = "1 months", date_labels = "%b-%y") +
  
  #geom_vline(xintercept = as.Date("2020-03-15"), colour="black",linetype = "dotted") +
  #geom_text(aes(x=as.Date("2020-03-13"), label="Covid in EU", y=1), colour="#666666", angle=90, vjust = 1.2, size=3)+
  
  #geom_vline(xintercept = as.Date("2021-03-02"), colour="black",linetype = "dotted") +
  #geom_text(aes(x=as.Date("2021-03-01"), label="Delta Variant in EU", y=05), colour="#666666", angle=90, vjust =0., size=3)+
  
  #geom_vline(xintercept = as.Date("2021-11-22"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2021-11-21"), label="Omicron", y=0.05), colour="#666666", angle=90, vjust = 1.2,size=3)+
  
  #geom_vline(xintercept = as.Date("2022-02-21"), colour="black",linetype = "dotted") +
  #geom_text(aes(x=as.Date("2022-02-20"), label="Rusia-Ucrania", y=0.045), colour="#666666", angle=90, vjust = 1.2,size=3)+
  transition_reveal(Date)  +
  #exit_shrink() +
  #enter_fade() +
  #ease_aes("circular-out") +
  guides(col = guide_legend(nrow = 1, reverse = F)) +
  labs(title =  "España: Consumo BigData BBVA y Ventas de Consumo AEAT ",  
       caption= "Fuente: BBVA Research & AEAT", 
       subtitle = "(Real deflactado por IPC y deflactor Ventas AEAT.Acum 91D % Var. Trimestral CVEC)")  # Subtitle Date
gg_demanda <- gg_demanda + labs(x="", y = '% ToT Real (deflactado por IPC & deflactor Ventas )', positiony="left",color="") 

animate(gg_demanda, nframes = 200, duration = 7 ,fps = 32, width = 1200,
        height = 750, end_pause = 40)

anim_save("G5bis Ventas AEAT B Consumo y Consumo .gif")

###################################################### Consumo y AEAT and AEAT  Bienes de Consumo

filename <- file("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")
logo <- readPNG("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")

pais <- openxlsx::read.xlsx(file, sheet = country , startRow = 1)[,1:49]

xmax <- ymd(max_date)-10
xmin <- ymd(max_date)-70
ymin <- 0.36
ymax <- 0.05
names(pais)[1] <- 'Date'
names(pais)[47] <- 'Consumo BBVA'
names(pais)[48] <- 'AEAT Ventas Total'
names(pais)[49] <- 'AEAT Ventas Comercio por Menor'


origin <-"1899-12-30"

start_date<- "2023-01-01"
pais$Date <- as.Date(pais$Date , origin = origin)

#pais$Date <- as.Date(pais$Date , origin = "1899-12-30")

pais <- select(pais,1,49,48,47)
pais
fechas <- select(pais, Date)
fechas$variable <- 'Date'
fechas$value <- -2

pais <-  pais %>% reshape2::melt(id.vars='Date') %>%  na.omit()

pais$Value <- scales::percent(pais$value, accuracy = 0.01 )
pais$Value <- paste(pais$variable, pais$Value, sep = ": ")
fechas$newdate <- strptime(as.character(fechas$Date), "%Y-%m-%d")
fechas$newdate <- format(fechas$newdate, "%d-%m-%Y")
fechas$Value <- paste(fechas$variable, fechas$newdate, sep = ": ")
fechas <- select(fechas, -newdate)
pais <- rbind(pais, fechas)

colnames(pais)[1] <- 'Date'


#scale_y_continuous(
#  "mpg (US)", 
#  sec.axis = sec_axis(~ . * 1.20, name = "mpg (UK)")
#)


gg_demanda <- pais %>% group_by(variable,Date) %>% 
  ggplot( aes(x=Date, y= value,  colour = variable))+ 
  geom_line() +
  #  geom_point(show.legend = TRUE) +
  geom_richtext(aes(x = max(Date)-90, y = min(value)+.1, label = format(Date, '%b-%Y') ), text.colour ="#666666", show.legend=F, 
                fill = NA, label.color = NA, size=10) + #Date inside plot
  theme_BBVA_grey +
  annotation_raster(logo, ymin = ymin, ymax = ymax, xmin = xmin, xmax =xmax) +
  scale_color_manual(values=c( "#0F4B7C","#9DC1F5","#2DCCCD","#FFFFFF"),
                     # scale_color_manual(values=c( "#0F4B7C","#2DCCCD","#9DC1F5","#D8BE75"),
                     breaks= c("AEAT Ventas Total","AEAT Ventas Comercio por Menor" , "Consumo BBVA")) +
  scale_y_continuous(trans = modulus_trans(0.003), breaks =  c(-0.05,-0.04,-0.03,-0.02,-0.01,0.0,+0.01,+0.02,+0.03,+0.04),
                     labels = c('-5%','-4%','-3%','-2%','-1%','0%','+1%','+2%','+3%','+4%'),
                     limits = c(-0.05, +0.04),
                     sec.axis=sec_axis(~.*1,breaks= c(-0.05,-0.04,-0.03,-0.02,-0.01,0.0,+0.01,+0.02,+0.03,+0.04),labels = c('-5%','-4%','-3%','-2%','-1%','0%','+1%','+2%','+3%','+4%'))) + 
  
  
  
  
  scale_x_date(limits=c(as.Date("2022-08-01"), as.Date(date)), date_breaks = "1 months", date_labels = "%b-%y") +
  
  #geom_vline(xintercept = as.Date("2020-03-15"), colour="black",linetype = "dotted") +
  #geom_text(aes(x=as.Date("2020-03-13"), label="Covid in EU", y=1), colour="#666666", angle=90, vjust = 1.2, size=3)+
  
  #geom_vline(xintercept = as.Date("2021-03-02"), colour="black",linetype = "dotted") +
  #geom_text(aes(x=as.Date("2021-03-01"), label="Delta Variant in EU", y=05), colour="#666666", angle=90, vjust =0., size=3)+
  
  #geom_vline(xintercept = as.Date("2021-11-22"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2021-11-21"), label="Omicron", y=0.05), colour="#666666", angle=90, vjust = 1.2,size=3)+
  
  #geom_vline(xintercept = as.Date("2022-02-21"), colour="black",linetype = "dotted") +
  #geom_text(aes(x=as.Date("2022-02-20"), label="Rusia-Ucrania", y=0.045), colour="#666666", angle=90, vjust = 1.2,size=3)+
  transition_reveal(Date)  +
  #exit_shrink() +
  #enter_fade() +
  #ease_aes("circular-out") +
  guides(col = guide_legend(nrow = 1, reverse = F)) +
  labs(title =  "Consumo BigData (BBVA),Ventas Totales y Comercio PM (AEAT)",size=10,  
       caption= "Fuente: BBVA Research & AEAT", 
       subtitle = "(Real deflactado por IPC y deflactor Ventas AEAT.Acum 91D % Var. Trimestral CVEC)")  # Subtitle Date
gg_demanda <- gg_demanda + labs(x="", y = '% ToT Real (deflactado por IPC & deflactor Ventas )', positiony="left",color="") 

animate(gg_demanda, nframes = 200, duration = 7 ,fps = 32, width = 1200,
        height = 750, end_pause = 40)

anim_save("G5bis Ventas AEAT B Consumo y Consumo .gif")

####################### Inversion  M y Equipo y Sectores AEAT#######

filename <- file("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")
logo <- readPNG("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")

pais <- openxlsx::read.xlsx(file, sheet = country , startRow = 1)[,1:60]

xmax <- ymd(max_date)-10
xmin <- ymd(max_date)-70
ymin <- 0.27
ymax <- 0.35
names(pais)[1] <- 'Date'
####### Datos de Inversion
names(pais)[14] <- 'Construccion Vivienda'
names(pais)[15] <- 'Construccion Otros '
names(pais)[16] <- 'Construccion '
names(pais)[17] <- 'Inversión Transporte'
names(pais)[18] <- 'Inversion B. Equipo'
names(pais)[19] <- 'Inversión Maquinaria y Eq'
names(pais)[20] <- 'Intangibles '
names(pais)[21] <- 'Inversion Total'
names(pais)[22] <- 'Inversion Digital'
names(pais)[50] <- 'AEAT Total'
names(pais)[51] <- 'AEAT Core'
names(pais)[52] <- 'AEAT Maquinaria y Equipo'
names(pais)[53] <- 'AEAT Fab. Vehiculos'
names(pais)[54] <- 'AEAT Informatica'
names(pais)[55] <- 'AEAT Costruccion'
names(pais)[56] <- 'AEAT Construccion Edificios'
names(pais)[57] <- 'AEAT Construccion Ingenieria'
names(pais)[58] <- 'AEAT Construccion Especializada'
names(pais)[59] <- 'AEAT Info y Comunicaciones'
names(pais)[60] <- 'AEAT Act. Profesionales '



origin <-"1899-12-30"

start_date<- "2023-01-01"
pais$Date <- as.Date(pais$Date , origin = origin)

#pais$Date <- as.Date(pais$Date , origin = "1899-12-30")

pais <- select(pais,1,52,19)
pais
fechas <- select(pais, Date)
fechas$variable <- 'Date'
fechas$value <- -2

pais <-  pais %>% reshape2::melt(id.vars='Date') %>%  na.omit()

pais$Value <- scales::percent(pais$value, accuracy = 0.01 )
pais$Value <- paste(pais$variable, pais$Value, sep = ": ")
fechas$newdate <- strptime(as.character(fechas$Date), "%Y-%m-%d")
fechas$newdate <- format(fechas$newdate, "%d-%m-%Y")
fechas$Value <- paste(fechas$variable, fechas$newdate, sep = ": ")
fechas <- select(fechas, -newdate)
pais <- rbind(pais, fechas)

colnames(pais)[1] <- 'Date'


#scale_y_continuous(
#  "mpg (US)", 
#  sec.axis = sec_axis(~ . * 1.20, name = "mpg (UK)")
#)


gg_demanda <- pais %>% group_by(variable,Date) %>% 
  ggplot( aes(x=Date, y= value,  colour = variable))+ 
  geom_line() +
  #  geom_point(show.legend = TRUE) +
  geom_richtext(aes(x = max(Date)-90, y = min(value)+.1, label = format(Date, '%b-%Y') ), text.colour ="#666666", show.legend=F, 
                fill = NA, label.color = NA, size=10) + #Date inside plot
  theme_BBVA_grey +
  annotation_raster(logo, ymin = ymin, ymax = ymax, xmin = xmin, xmax =xmax) +
  scale_color_manual(values=c( "#0F4B7C","#2DCCCD","#FFFFFF"),
                     # scale_color_manual(values=c( "#0F4B7C","#2DCCCD","#9DC1F5","#D8BE75"),
                     breaks= c("Inversión Maquinaria y Eq",  "AEAT Maquinaria y Equipo")) +
  
  scale_y_continuous(trans = modulus_trans(0.003), breaks =  c(-0.3,-0.2,-0.1,0.0,+0.1,+0.2,+0.3,+0.4),
                     labels = c('-30%','-20%','-10%','0%','+10%','+20%','+30%','+40%'),
                     limits = c(-0.3, +0.4),
                     sec.axis=sec_axis(~.*1,breaks= c(-0.3,-0.2,-0.1,0.0,+0.1,+0.2,+0.3,0.4),labels = c('-30%','-20%','-10%','0%','+10%','+20%','+30%','+40%'))) + 
  
  
  
  scale_x_date(limits=c(as.Date("2022-08-01"), as.Date(date)), date_breaks = "1 months", date_labels = "%b-%y") +
  
  #geom_vline(xintercept = as.Date("2020-03-15"), colour="black",linetype = "dotted") +
  #geom_text(aes(x=as.Date("2020-03-13"), label="Covid in EU", y=1), colour="#666666", angle=90, vjust = 1.2, size=3)+
  
  #geom_vline(xintercept = as.Date("2021-03-02"), colour="black",linetype = "dotted") +
  #geom_text(aes(x=as.Date("2021-03-01"), label="Delta Variant in EU", y=05), colour="#666666", angle=90, vjust =0., size=3)+
  
  #geom_vline(xintercept = as.Date("2021-11-22"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2021-11-21"), label="Omicron", y=0.05), colour="#666666", angle=90, vjust = 1.2,size=3)+
  
  #geom_vline(xintercept = as.Date("2022-02-21"), colour="black",linetype = "dotted") +
  #geom_text(aes(x=as.Date("2022-02-20"), label="Rusia-Ucrania", y=0.045), colour="#666666", angle=90, vjust = 1.2,size=3)+
  transition_reveal(Date)  +
  #exit_shrink() +
  #enter_fade() +
  #ease_aes("circular-out") +
  guides(col = guide_legend(nrow = 1, reverse = F)) +
  labs(title =  "España: Inversion BigData BBVA y Ventas  AEAT B. Equipo",  
       caption= "Fuente: BBVA Research & AEAT", 
       subtitle = "(Real deflactado por def inversión y deflactor Ventas AEAT.% Var YoY")  # Subtitle Date
gg_demanda <- gg_demanda + labs(x="", y = '% YoY Real (deflactado por def inversión & deflactor Ventas )', positiony="left",color="") 

animate(gg_demanda, nframes = 200, duration = 7 ,fps = 32, width = 1200,
        height = 750, end_pause = 100)

anim_save("G6 Ventas AEAT Bienes Equipo.gif")

########################################## Inversion y AEAT fab de vehiculos
filename <- file("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")
logo <- readPNG("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")

pais <- openxlsx::read.xlsx(file, sheet = country , startRow = 1)[,1:60]

xmax <- ymd(max_date)-10
xmin <- ymd(max_date)-70
ymin <- 0.9
ymax <- 1.0
names(pais)[1] <- 'Date'
####### Datos de Inversion
names(pais)[14] <- 'Construccion Vivienda'
names(pais)[15] <- 'Construccion Otros '
names(pais)[16] <- 'Construccion '
names(pais)[17] <- 'Inversión Transporte'
names(pais)[18] <- 'Inversion B. Equipo'
names(pais)[19] <- 'Inversión Maquinaria y Eq'
names(pais)[20] <- 'Intangibles '
names(pais)[21] <- 'Inversion Total'
names(pais)[22] <- 'Inversion Digital'
names(pais)[52] <- 'AEAT Maquinaria y Equipo'
names(pais)[53] <- 'AEAT Fab. Vehiculos'
names(pais)[54] <- 'AEAT Informatica'
names(pais)[55] <- 'AEAT Costruccion'
names(pais)[56] <- 'AEAT Construccion Edificios'
names(pais)[57] <- 'AEAT Construccion Ingenieria'
names(pais)[58] <- 'AEAT Construccion Especializada'
names(pais)[59] <- 'AEAT Comunicaciones'
names(pais)[60] <- 'AEAT Act. Profesionales '



origin <-"1899-12-30"

start_date<- "2023-01-01"
pais$Date <- as.Date(pais$Date , origin = origin)

#pais$Date <- as.Date(pais$Date , origin = "1899-12-30")

pais <- select(pais,1,53,17)
pais
fechas <- select(pais, Date)
fechas$variable <- 'Date'
fechas$value <- -2

pais <-  pais %>% reshape2::melt(id.vars='Date') %>%  na.omit()

pais$Value <- scales::percent(pais$value, accuracy = 0.01 )
pais$Value <- paste(pais$variable, pais$Value, sep = ": ")
fechas$newdate <- strptime(as.character(fechas$Date), "%Y-%m-%d")
fechas$newdate <- format(fechas$newdate, "%d-%m-%Y")
fechas$Value <- paste(fechas$variable, fechas$newdate, sep = ": ")
fechas <- select(fechas, -newdate)
pais <- rbind(pais, fechas)

colnames(pais)[1] <- 'Date'


#scale_y_continuous(
#  "mpg (US)", 
#  sec.axis = sec_axis(~ . * 1.20, name = "mpg (UK)")
#)


gg_demanda <- pais %>% group_by(variable,Date) %>% 
  ggplot( aes(x=Date, y= value,  colour = variable))+ 
  geom_line() +
  #  geom_point(show.legend = TRUE) +
  geom_richtext(aes(x = max(Date)-90, y = min(value)+.1, label = format(Date, '%b-%Y') ), text.colour ="#666666", show.legend=F, 
                fill = NA, label.color = NA, size=10) + #Date inside plot
  theme_BBVA_grey +
  annotation_raster(logo, ymin = ymin, ymax = ymax, xmin = xmin, xmax =xmax) +
  scale_color_manual(values=c( "#0F4B7C","#2DCCCD","#FFFFFF"),
                     # scale_color_manual(values=c( "#0F4B7C","#2DCCCD","#9DC1F5","#D8BE75"),
                     breaks= c("Inversión Transporte",  "AEAT Fab. Vehiculos")) +
  
  scale_y_continuous(trans = modulus_trans(0.003), breaks =  c(-0.4,-0.3,-0.2,-0.1,0.0,+0.1,+0.2,+0.3,+0.4,+0.5,1),
                     labels = c('-40%','-30%','-20%','-10%','0%','+10%','+20%','+30%','+40%','+50%','+100%'),
                     limits = c(-0.4, +1.0),
                     sec.axis=sec_axis(~.*1,breaks= c(-0.4,-0.3,-0.2,-0.1,0.0,+0.1,+0.2,+0.3,0.4,0.5,1),labels = c('-40%','-30%','-20%','-10%','0%','+10%','+20%','+30%','+40%','+50%','+100%'))) + 
  
  
  
  scale_x_date(limits=c(as.Date("2022-08-01"), as.Date(date)), date_breaks = "1 months", date_labels = "%b-%y") +
  
  #geom_vline(xintercept = as.Date("2020-03-15"), colour="black",linetype = "dotted") +
  #geom_text(aes(x=as.Date("2020-03-13"), label="Covid in EU", y=1), colour="#666666", angle=90, vjust = 1.2, size=3)+
  
  #geom_vline(xintercept = as.Date("2021-03-02"), colour="black",linetype = "dotted") +
  #geom_text(aes(x=as.Date("2021-03-01"), label="Delta Variant in EU", y=05), colour="#666666", angle=90, vjust =0., size=3)+
  
  #geom_vline(xintercept = as.Date("2021-11-22"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2021-11-21"), label="Omicron", y=0.05), colour="#666666", angle=90, vjust = 1.2,size=3)+
  
  #geom_vline(xintercept = as.Date("2022-02-21"), colour="black",linetype = "dotted") +
  #geom_text(aes(x=as.Date("2022-02-20"), label="Rusia-Ucrania", y=0.045), colour="#666666", angle=90, vjust = 1.2,size=3)+
  transition_reveal(Date)  +
  #exit_shrink() +
  #enter_fade() +
  #ease_aes("circular-out") +
  guides(col = guide_legend(nrow = 1, reverse = F)) +
  labs(title =  "España: Inversion BigData BBVA y  AEAT Ventas Automoviles ",  
       caption= "Fuente: BBVA Research & AEAT", 
       subtitle = "(Real deflactado por def inversión y deflactor Ventas AEAT.% Var YoY")  # Subtitle Date
gg_demanda <- gg_demanda + labs(x="", y = '% YoY Real (deflactado por def inversión & deflactor Ventas )', positiony="left",color="") 

animate(gg_demanda, nframes = 200, duration = 7 ,fps = 32, width = 1200,
        height = 750, end_pause = 100)

anim_save("G6 Ventas AEAT e inversion autos.gif")

############### Inversion en Construccion y AEAT##########

####################### Inversion Construccion Total y Sectores AEAT#######

filename <- file("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")
logo <- readPNG("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")

pais <- openxlsx::read.xlsx(file, sheet = country , startRow = 1)[,1:122]

xmax <- ymd(max_date)-10
xmin <- ymd(max_date)-70
ymin <- 0.25
ymax <- 0.3
names(pais)[1] <- 'Date'
####### Datos de Inversion
names(pais)[16] <- 'Inversion Construccion Old'
names(pais)[118] <- 'Inversion Construccion New'
names(pais)[67] <- 'AEAT Construccion'


origin <-"1899-12-30"

start_date<- "2023-01-01"
pais$Date <- as.Date(pais$Date , origin = origin)

#pais$Date <- as.Date(pais$Date , origin = "1899-12-30")

pais <- select(pais,1,67,118,16)
pais
fechas <- select(pais, Date)
fechas$variable <- 'Date'
fechas$value <- -2

pais <-  pais %>% reshape2::melt(id.vars='Date') %>%  na.omit()

pais$Value <- scales::percent(pais$value, accuracy = 0.01 )
pais$Value <- paste(pais$variable, pais$Value, sep = ": ")
fechas$newdate <- strptime(as.character(fechas$Date), "%Y-%m-%d")
fechas$newdate <- format(fechas$newdate, "%d-%m-%Y")
fechas$Value <- paste(fechas$variable, fechas$newdate, sep = ": ")
fechas <- select(fechas, -newdate)
pais <- rbind(pais, fechas)

colnames(pais)[1] <- 'Date'


#scale_y_continuous(
#  "mpg (US)", 
#  sec.axis = sec_axis(~ . * 1.20, name = "mpg (UK)")
#)


gg_demanda <- pais %>% group_by(variable,Date) %>% 
  ggplot( aes(x=Date, y= value,  colour = variable))+ 
  geom_line() +
  #  geom_point(show.legend = TRUE) +
  geom_richtext(aes(x = max(Date)-90, y = min(value)+.1, label = format(Date, '%b-%Y') ), text.colour ="#666666", show.legend=F, 
                fill = NA, label.color = NA, size=10) + #Date inside plot
  theme_BBVA_grey +
  annotation_raster(logo, ymin = ymin, ymax = ymax, xmin = xmin, xmax =xmax) +
  scale_color_manual(values=c( "#0F4B7C","#2DCCCD","#ED7D31","#FFFFFF"),
                     # scale_color_manual(values=c( "#0F4B7C","#2DCCCD","#9DC1F5","#D8BE75"),
                     breaks= c("Inversion Construccion Old","Inversion Construccion New",  "AEAT Construccion")) +
  
  scale_y_continuous(trans = modulus_trans(0.003), breaks =  c(-0.1,0.0,+0.1,+0.2,+0.3,0.4,0.5),
                     labels = c('-10%','0%','+10%','+20%','+30%','+40%','+50%'),
                     limits = c(-0.1, +0.5),
                     sec.axis=sec_axis(~.*1,breaks= c(-0.1,0.0,+0.1,+0.2,+0.3,0.4,0.5),labels = c('-10%','0%','+10%','+20%','+30%','+40%','+50%'))) + 
  
  
  
  scale_x_date(limits=c(as.Date("2021-01-01"), as.Date(date)), date_breaks = "3 months", date_labels = "%b-%y") +
  
  #geom_vline(xintercept = as.Date("2020-03-15"), colour="black",linetype = "dotted") +
  #geom_text(aes(x=as.Date("2020-03-13"), label="Covid in EU", y=1), colour="#666666", angle=90, vjust = 1.2, size=3)+
  
  #geom_vline(xintercept = as.Date("2021-03-02"), colour="black",linetype = "dotted") +
  #geom_text(aes(x=as.Date("2021-03-01"), label="Delta Variant in EU", y=05), colour="#666666", angle=90, vjust =0., size=3)+
  
  #geom_vline(xintercept = as.Date("2021-11-22"), colour="black",linetype = "dotted") +
  #geom_text(aes(x=as.Date("2021-11-21"), label="Omicron", y=0.05), colour="#666666", angle=90, vjust = 1.2,size=3)+
  
  #geom_vline(xintercept = as.Date("2022-02-21"), colour="black",linetype = "dotted") +
  #geom_text(aes(x=as.Date("2022-02-20"), label="Rusia-Ucrania", y=0.045), colour="#666666", angle=90, vjust = 1.2,size=3)+
  transition_reveal(Date)  +
  #exit_shrink() +
  #enter_fade() +
  #ease_aes("circular-out") +
  guides(col = guide_legend(nrow = 1, reverse = F)) +
  labs(title =  "España: Inversion BigData BBVA y Ventas AEAT Construcción ",  
       caption= "Fuente: BBVA Research & AEAT", 
       subtitle = "(Cum 28 dias .% Var YoY)")  # Subtitle Date
gg_demanda <- gg_demanda + labs(x="", y = 'Cum 28 dias .% Var YoY )', positiony="left",color="") 

animate(gg_demanda, nframes = 200, duration = 7 ,fps = 32, width = 1200,
        height = 750, end_pause = 100)

anim_save("G6 Ventas AEAT Construccion.gif")

#########################################Inversion Maquinaria y Equipo OTROS 


pais <- openxlsx::read.xlsx(file, sheet = country , startRow = 1)[,1:122]

xmax <- ymd(max_date)-10
xmin <- ymd(max_date)-70
ymin <- 0.25
ymax <- 0.3
names(pais)[1] <- 'Date'
####### Datos de Inversion

names(pais)[18] <- 'Inversion Equipo Old'
names(pais)[121] <- 'Inversion Equipo New'
names(pais)[64] <- 'AEAT Maquinaria'


origin <-"1899-12-30"

start_date<- "2023-01-01"
pais$Date <- as.Date(pais$Date , origin = origin)

#pais$Date <- as.Date(pais$Date , origin = "1899-12-30")

pais <- select(pais,1,18,121,64)
pais
fechas <- select(pais, Date)
fechas$variable <- 'Date'
fechas$value <- -2

pais <-  pais %>% reshape2::melt(id.vars='Date') %>%  na.omit()

pais$Value <- scales::percent(pais$value, accuracy = 0.01 )
pais$Value <- paste(pais$variable, pais$Value, sep = ": ")
fechas$newdate <- strptime(as.character(fechas$Date), "%Y-%m-%d")
fechas$newdate <- format(fechas$newdate, "%d-%m-%Y")
fechas$Value <- paste(fechas$variable, fechas$newdate, sep = ": ")
fechas <- select(fechas, -newdate)
pais <- rbind(pais, fechas)

colnames(pais)[1] <- 'Date'


#scale_y_continuous(
#  "mpg (US)", 
#  sec.axis = sec_axis(~ . * 1.20, name = "mpg (UK)")
#)


gg_demanda <- pais %>% group_by(variable,Date) %>% 
  ggplot( aes(x=Date, y= value,  colour = variable))+ 
  geom_line() +
  #  geom_point(show.legend = TRUE) +
  geom_richtext(aes(x = max(Date)-90, y = min(value)+.1, label = format(Date, '%b-%Y') ), text.colour ="#666666", show.legend=F, 
                fill = NA, label.color = NA, size=10) + #Date inside plot
  theme_BBVA_grey +
  annotation_raster(logo, ymin = ymin, ymax = ymax, xmin = xmin, xmax =xmax) +
  scale_color_manual(values=c( "#0F4B7C","#2DCCCD","#ED7D31","#FFFFFF"),
                     # scale_color_manual(values=c( "#0F4B7C","#2DCCCD","#9DC1F5","#D8BE75"),
                     breaks= c("Inversion Equipo Old","Inversion Equipo New",  "AEAT Maquinaria")) +
  
  scale_y_continuous(trans = modulus_trans(0.003), breaks =  c(-0.3,-0.1,0.0,+0.1,+0.2,+0.3,0.4,0.6),
                     labels = c('-30%','-10%','0%','+10%','+20%','+30%','+40%','+60%'),
                     limits = c(-0.3, +0.6),
                     sec.axis=sec_axis(~.*1,breaks= c(-0.3,-0.1,0.0,+0.1,+0.2,+0.3,0.4,0.6),labels = c('-30%','-10%','0%','+10%','+20%','+30%','+40%','+60%'))) + 
  
  
  
  scale_x_date(limits=c(as.Date("2021-01-01"), as.Date(date)), date_breaks = "3 months", date_labels = "%b-%y") +
  
  #geom_vline(xintercept = as.Date("2020-03-15"), colour="black",linetype = "dotted") +
  #geom_text(aes(x=as.Date("2020-03-13"), label="Covid in EU", y=1), colour="#666666", angle=90, vjust = 1.2, size=3)+
  
  #geom_vline(xintercept = as.Date("2021-03-02"), colour="black",linetype = "dotted") +
  #geom_text(aes(x=as.Date("2021-03-01"), label="Delta Variant in EU", y=05), colour="#666666", angle=90, vjust =0., size=3)+
  
  #geom_vline(xintercept = as.Date("2021-11-22"), colour="black",linetype = "dotted") +
  #geom_text(aes(x=as.Date("2021-11-21"), label="Omicron", y=0.05), colour="#666666", angle=90, vjust = 1.2,size=3)+
  
  #geom_vline(xintercept = as.Date("2022-02-21"), colour="black",linetype = "dotted") +
  #geom_text(aes(x=as.Date("2022-02-20"), label="Rusia-Ucrania", y=0.045), colour="#666666", angle=90, vjust = 1.2,size=3)+
  transition_reveal(Date)  +
  #exit_shrink() +
  #enter_fade() +
  #ease_aes("circular-out") +
  guides(col = guide_legend(nrow = 1, reverse = F)) +
  labs(title =  "España: Inversion BigData BBVA y Ventas AEAT MAquin- Otros",  
       caption= "Fuente: BBVA Research & AEAT", 
       subtitle = "(Cum 28 dias .% Var YoY")  # Subtitle Date
gg_demanda <- gg_demanda + labs(x="", y = 'Cum 28 dias .% Var YoY )', positiony="left",color="") 

animate(gg_demanda, nframes = 200, duration = 7 ,fps = 32, width = 1200,
        height = 750, end_pause = 100)

anim_save("G6 Ventas AEAT B. Equipo.gif")


#########################################Inversion Autos

filename <- file("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")
logo <- readPNG("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")

pais <- openxlsx::read.xlsx(file, sheet = country , startRow = 1)[,1:122]

xmax <- ymd(max_date)-10
xmin <- ymd(max_date)-70
ymin <- 0.25
ymax <- 0.3
names(pais)[1] <- 'Date'
####### Datos de Inversion

names(pais)[17] <- 'Inversion Autos Old'
names(pais)[120] <- 'Inversion Autos New'
names(pais)[62] <- 'AEAT Autos'


origin <-"1899-12-30"

start_date<- "2023-01-01"
pais$Date <- as.Date(pais$Date , origin = origin)

#pais$Date <- as.Date(pais$Date , origin = "1899-12-30")

pais <- select(pais,1,62,120,17)
pais
fechas <- select(pais, Date)
fechas$variable <- 'Date'
fechas$value <- -2

pais <-  pais %>% reshape2::melt(id.vars='Date') %>%  na.omit()

pais$Value <- scales::percent(pais$value, accuracy = 0.01 )
pais$Value <- paste(pais$variable, pais$Value, sep = ": ")
fechas$newdate <- strptime(as.character(fechas$Date), "%Y-%m-%d")
fechas$newdate <- format(fechas$newdate, "%d-%m-%Y")
fechas$Value <- paste(fechas$variable, fechas$newdate, sep = ": ")
fechas <- select(fechas, -newdate)
pais <- rbind(pais, fechas)

colnames(pais)[1] <- 'Date'


#scale_y_continuous(
#  "mpg (US)", 
#  sec.axis = sec_axis(~ . * 1.20, name = "mpg (UK)")
#)


gg_demanda <- pais %>% group_by(variable,Date) %>% 
  ggplot( aes(x=Date, y= value,  colour = variable))+ 
  geom_line() +
  #  geom_point(show.legend = TRUE) +
  geom_richtext(aes(x = max(Date)-90, y = min(value)+.1, label = format(Date, '%b-%Y') ), text.colour ="#666666", show.legend=F, 
                fill = NA, label.color = NA, size=10) + #Date inside plot
  theme_BBVA_grey +
  annotation_raster(logo, ymin = ymin, ymax = ymax, xmin = xmin, xmax =xmax) +
  scale_color_manual(values=c( "#0F4B7C","#2DCCCD","#ED7D31","#FFFFFF"),
                     # scale_color_manual(values=c( "#0F4B7C","#2DCCCD","#9DC1F5","#D8BE75"),
                     breaks= c("Inversion Autos Old","Inversion Autos New",  "AEAT Autos")) +
  
  scale_y_continuous(trans = modulus_trans(0.003), breaks =  c(-1,-0.1,0.0,+0.1,+0.2,+0.3,0.4,0.5,0.8,1),
                     labels = c('-100%','-10%','0%','+10%','+20%','+30%','+40%','+50%','+80%','+100%'),
                     limits = c(-1, +1),
                     sec.axis=sec_axis(~.*1,breaks= c(-1,-0.1,0.0,+0.1,+0.2,+0.3,0.4,0.5,0.8,1),labels = c('-100%','-10%','0%','+10%','+20%','+30%','+40%','+50%','+80%','+100%'))) + 
  
  
  
  scale_x_date(limits=c(as.Date("2021-01-01"), as.Date(date)), date_breaks = "3 months", date_labels = "%b-%y") +
  
  #geom_vline(xintercept = as.Date("2020-03-15"), colour="black",linetype = "dotted") +
  #geom_text(aes(x=as.Date("2020-03-13"), label="Covid in EU", y=1), colour="#666666", angle=90, vjust = 1.2, size=3)+
  
  #geom_vline(xintercept = as.Date("2021-03-02"), colour="black",linetype = "dotted") +
  #geom_text(aes(x=as.Date("2021-03-01"), label="Delta Variant in EU", y=05), colour="#666666", angle=90, vjust =0., size=3)+
  
  #geom_vline(xintercept = as.Date("2021-11-22"), colour="black",linetype = "dotted") +
  #geom_text(aes(x=as.Date("2021-11-21"), label="Omicron", y=0.05), colour="#666666", angle=90, vjust = 1.2,size=3)+
  
  #geom_vline(xintercept = as.Date("2022-02-21"), colour="black",linetype = "dotted") +
  #geom_text(aes(x=as.Date("2022-02-20"), label="Rusia-Ucrania", y=0.045), colour="#666666", angle=90, vjust = 1.2,size=3)+
  transition_reveal(Date)  +
  #exit_shrink() +
  #enter_fade() +
  #ease_aes("circular-out") +
  guides(col = guide_legend(nrow = 1, reverse = F)) +
  labs(title =  "España: Inversion BigData BBVA y AEAT Fab. Automoviles",  
       caption= "Fuente: BBVA Research & AEAT", 
       subtitle = "(Cum 28 dias .% Var YoY")  # Subtitle Date
gg_demanda <- gg_demanda + labs(x="", y = 'Cum 28 dias .% Var YoY )', positiony="left",color="") 

animate(gg_demanda, nframes = 200, duration = 7 ,fps = 32, width = 1200,
        height = 750, end_pause = 100)

anim_save("G6 Ventas AEAT Autos.gif")


###################### INVERSION EN AUTOS 2

#########################################Inversion Autos

filename <- file("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")
logo <- readPNG("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")

pais <- openxlsx::read.xlsx(file, sheet = country , startRow = 1)[,1:122]

xmax <- ymd(max_date)-10
xmin <- ymd(max_date)-70
ymin <- 0.25
ymax <- 0.3
names(pais)[1] <- 'Date'
####### Datos de Inversion

names(pais)[17] <- 'Inversion Autos Old'
names(pais)[120] <- 'Inversion Autos New'
names(pais)[22] <- 'AEAT VENTAS Autos'


origin <-"1899-12-30"

start_date<- "2023-01-01"
pais$Date <- as.Date(pais$Date , origin = origin)

#pais$Date <- as.Date(pais$Date , origin = "1899-12-30")

pais <- select(pais,1,17,120,22)
pais
fechas <- select(pais, Date)
fechas$variable <- 'Date'
fechas$value <- -2

pais <-  pais %>% reshape2::melt(id.vars='Date') %>%  na.omit()

pais$Value <- scales::percent(pais$value, accuracy = 0.01 )
pais$Value <- paste(pais$variable, pais$Value, sep = ": ")
fechas$newdate <- strptime(as.character(fechas$Date), "%Y-%m-%d")
fechas$newdate <- format(fechas$newdate, "%d-%m-%Y")
fechas$Value <- paste(fechas$variable, fechas$newdate, sep = ": ")
fechas <- select(fechas, -newdate)
pais <- rbind(pais, fechas)

colnames(pais)[1] <- 'Date'


#scale_y_continuous(
#  "mpg (US)", 
#  sec.axis = sec_axis(~ . * 1.20, name = "mpg (UK)")
#)


gg_demanda <- pais %>% group_by(variable,Date) %>% 
  ggplot( aes(x=Date, y= value,  colour = variable))+ 
  geom_line() +
  #  geom_point(show.legend = TRUE) +
  geom_richtext(aes(x = max(Date)-90, y = min(value)+.1, label = format(Date, '%b-%Y') ), text.colour ="#666666", show.legend=F, 
                fill = NA, label.color = NA, size=10) + #Date inside plot
  theme_BBVA_grey +
  annotation_raster(logo, ymin = ymin, ymax = ymax, xmin = xmin, xmax =xmax) +
  scale_color_manual(values=c( "#0F4B7C","#2DCCCD","#ED7D31","#FFFFFF"),
                     # scale_color_manual(values=c( "#0F4B7C","#2DCCCD","#9DC1F5","#D8BE75"),
                     breaks= c("Inversion Autos Old","Inversion Autos New",  "AEAT VENTAS Autos")) +
  
  scale_y_continuous(trans = modulus_trans(0.003), breaks =  c(-1,-0.1,0.0,+0.1,+0.2,+0.3,0.4,0.5,0.8,1),
                     labels = c('-100%','-10%','0%','+10%','+20%','+30%','+40%','+50%','+80%','+100%'),
                     limits = c(-1, +1),
                     sec.axis=sec_axis(~.*1,breaks= c(-1,-0.1,0.0,+0.1,+0.2,+0.3,0.4,0.5,0.8,1),labels = c('-100%','-10%','0%','+10%','+20%','+30%','+40%','+50%','+80%','+100%'))) + 
  
  
  
  scale_x_date(limits=c(as.Date("2021-01-01"), as.Date(date)), date_breaks = "3 months", date_labels = "%b-%y") +
  
  #geom_vline(xintercept = as.Date("2020-03-15"), colour="black",linetype = "dotted") +
  #geom_text(aes(x=as.Date("2020-03-13"), label="Covid in EU", y=1), colour="#666666", angle=90, vjust = 1.2, size=3)+
  
  #geom_vline(xintercept = as.Date("2021-03-02"), colour="black",linetype = "dotted") +
  #geom_text(aes(x=as.Date("2021-03-01"), label="Delta Variant in EU", y=05), colour="#666666", angle=90, vjust =0., size=3)+
  
  #geom_vline(xintercept = as.Date("2021-11-22"), colour="black",linetype = "dotted") +
  #geom_text(aes(x=as.Date("2021-11-21"), label="Omicron", y=0.05), colour="#666666", angle=90, vjust = 1.2,size=3)+
  
  #geom_vline(xintercept = as.Date("2022-02-21"), colour="black",linetype = "dotted") +
#geom_text(aes(x=as.Date("2022-02-20"), label="Rusia-Ucrania", y=0.045), colour="#666666", angle=90, vjust = 1.2,size=3)+
transition_reveal(Date)  +
  #exit_shrink() +
  #enter_fade() +
  #ease_aes("circular-out") +
  guides(col = guide_legend(nrow = 1, reverse = F)) +
  labs(title =  "España: Inversion BigData BBVA y AEAT VENTAS Automoviles",  
       caption= "Fuente: BBVA Research & AEAT", 
       subtitle = "(Cum 28 dias .% Var YoY")  # Subtitle Date
gg_demanda <- gg_demanda + labs(x="", y = 'Cum 28 dias .% Var YoY )', positiony="left",color="") 

animate(gg_demanda, nframes = 200, duration = 7 ,fps = 32, width = 1200,
        height = 750, end_pause = 100)

anim_save("G6 Ventas AEAT Autos.gif")


####################### Inversion en Intangibles 

filename <- file("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")
logo <- readPNG("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")

pais <- openxlsx::read.xlsx(file, sheet = country , startRow = 1)[,1:124]

xmax <- ymd(max_date)-10
xmin <- ymd(max_date)-70
ymin <- 0.25
ymax <- 0.3
names(pais)[1] <- 'Date'
####### Datos de Inversion

names(pais)[20] <- 'Inversion Intangibles Old'
names(pais)[119] <- 'Inversion Intangibles New'
names(pais)[124] <- 'AEAT Intangibles'


origin <-"1899-12-30"

start_date<- "2023-01-01"
pais$Date <- as.Date(pais$Date , origin = origin)

#pais$Date <- as.Date(pais$Date , origin = "1899-12-30")

pais <- select(pais,1,20,119,124)
pais
fechas <- select(pais, Date)
fechas$variable <- 'Date'
fechas$value <- -2

pais <-  pais %>% reshape2::melt(id.vars='Date') %>%  na.omit()

pais$Value <- scales::percent(pais$value, accuracy = 0.01 )
pais$Value <- paste(pais$variable, pais$Value, sep = ": ")
fechas$newdate <- strptime(as.character(fechas$Date), "%Y-%m-%d")
fechas$newdate <- format(fechas$newdate, "%d-%m-%Y")
fechas$Value <- paste(fechas$variable, fechas$newdate, sep = ": ")
fechas <- select(fechas, -newdate)
pais <- rbind(pais, fechas)

colnames(pais)[1] <- 'Date'


#scale_y_continuous(
#  "mpg (US)", 
#  sec.axis = sec_axis(~ . * 1.20, name = "mpg (UK)")
#)


gg_demanda <- pais %>% group_by(variable,Date) %>% 
  ggplot( aes(x=Date, y= value,  colour = variable))+ 
  geom_line() +
  #  geom_point(show.legend = TRUE) +
  geom_richtext(aes(x = max(Date)-90, y = min(value)+.1, label = format(Date, '%b-%Y') ), text.colour ="#666666", show.legend=F, 
                fill = NA, label.color = NA, size=10) + #Date inside plot
  theme_BBVA_grey +
  annotation_raster(logo, ymin = ymin, ymax = ymax, xmin = xmin, xmax =xmax) +
  scale_color_manual(values=c( "#0F4B7C","#2DCCCD","#ED7D31","#FFFFFF"),
                     # scale_color_manual(values=c( "#0F4B7C","#2DCCCD","#9DC1F5","#D8BE75"),
                     breaks= c("Inversion Intangibles Old","Inversion Intangibles New",  "AEAT Intangibles")) +
  
  scale_y_continuous(trans = modulus_trans(0.003), breaks =  c(-0.3,-0.1,0.0,+0.1,+0.2,+0.3,0.4,0.5),
                     labels = c('-30%','-10%','0%','+10%','+20%','+30%','+40%','+50%'),
                     limits = c(-0.3, +0.5),
                     sec.axis=sec_axis(~.*1,breaks= c(-0.3,-0.1,0.0,+0.1,+0.2,+0.3,0.4,0.5),labels = c('-30','-10%','0%','+10%','+20%','+30%','+40%','+50%'))) + 
  
  
  
  scale_x_date(limits=c(as.Date("2021-01-01"), as.Date(date)), date_breaks = "3 months", date_labels = "%b-%y") +
  
  #geom_vline(xintercept = as.Date("2020-03-15"), colour="black",linetype = "dotted") +
  #geom_text(aes(x=as.Date("2020-03-13"), label="Covid in EU", y=1), colour="#666666", angle=90, vjust = 1.2, size=3)+
  
  #geom_vline(xintercept = as.Date("2021-03-02"), colour="black",linetype = "dotted") +
  #geom_text(aes(x=as.Date("2021-03-01"), label="Delta Variant in EU", y=05), colour="#666666", angle=90, vjust =0., size=3)+
  
  #geom_vline(xintercept = as.Date("2021-11-22"), colour="black",linetype = "dotted") +
  #geom_text(aes(x=as.Date("2021-11-21"), label="Omicron", y=0.05), colour="#666666", angle=90, vjust = 1.2,size=3)+
  
  #geom_vline(xintercept = as.Date("2022-02-21"), colour="black",linetype = "dotted") +
#geom_text(aes(x=as.Date("2022-02-20"), label="Rusia-Ucrania", y=0.045), colour="#666666", angle=90, vjust = 1.2,size=3)+
transition_reveal(Date)  +
  #exit_shrink() +
  #enter_fade() +
  #ease_aes("circular-out") +
  guides(col = guide_legend(nrow = 1, reverse = F)) +
  labs(title =  "España: Inversion BigData BBVA y AEAT Intangibles",  
       caption= "Fuente: BBVA Research & AEAT", 
       subtitle = "(Cum 28 dias .% Var YoY)")  # Subtitle Date
gg_demanda <- gg_demanda + labs(x="", y = 'Cum 28 dias .% Var YoY )', positiony="left",color="") 

animate(gg_demanda, nframes = 200, duration = 7 ,fps = 32, width = 1200,
        height = 750, end_pause = 100)

anim_save("G6 Ventas AEAT Intangibles.gif")




####################### Inversion Cnstruccion Edificios  y AEAT #######

filename <- file("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")
logo <- readPNG("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")

pais <- openxlsx::read.xlsx(file, sheet = country , startRow = 1)[,1:60]

xmax <- ymd(max_date)-10
xmin <- ymd(max_date)-70
ymin <- 0.25
ymax <- 0.3
names(pais)[1] <- 'Date'
####### Datos de Inversion
names(pais)[14] <- 'Construccion Vivienda'
names(pais)[15] <- 'Construccion Otros '
names(pais)[16] <- 'Construccion'
names(pais)[17] <- 'Inversión Transporte'
names(pais)[18] <- 'Inversion B. Equipo'
names(pais)[19] <- 'Inversión Maquinaria y Eq'
names(pais)[20] <- 'Intangibles '
names(pais)[21] <- 'Inversion Total'
names(pais)[22] <- 'Inversion Digital'
names(pais)[52] <- 'AEAT Maquinaria y Equipo'
names(pais)[53] <- 'AEAT Fab. Vehiculos'
names(pais)[54] <- 'AEAT Informatica'
names(pais)[55] <- 'AEAT Construccion'
names(pais)[56] <- 'AEAT Construccion Edificios'
names(pais)[57] <- 'AEAT Construccion Ingenieria'
names(pais)[58] <- 'AEAT Construccion Especializada'
names(pais)[59] <- 'AEAT Comunicaciones'
names(pais)[60] <- 'AEAT Act. Profesionales '




origin <-"1899-12-30"

start_date<- "2023-01-01"
pais$Date <- as.Date(pais$Date , origin = origin)

#pais$Date <- as.Date(pais$Date , origin = "1899-12-30")

pais <- select(pais,1,56,14)
pais
fechas <- select(pais, Date)
fechas$variable <- 'Date'
fechas$value <- -2

pais <-  pais %>% reshape2::melt(id.vars='Date') %>%  na.omit()

pais$Value <- scales::percent(pais$value, accuracy = 0.01 )
pais$Value <- paste(pais$variable, pais$Value, sep = ": ")
fechas$newdate <- strptime(as.character(fechas$Date), "%Y-%m-%d")
fechas$newdate <- format(fechas$newdate, "%d-%m-%Y")
fechas$Value <- paste(fechas$variable, fechas$newdate, sep = ": ")
fechas <- select(fechas, -newdate)
pais <- rbind(pais, fechas)

colnames(pais)[1] <- 'Date'


#scale_y_continuous(
#  "mpg (US)", 
#  sec.axis = sec_axis(~ . * 1.20, name = "mpg (UK)")
#)


gg_demanda <- pais %>% group_by(variable,Date) %>% 
  ggplot( aes(x=Date, y= value,  colour = variable))+ 
  geom_line() +
  #  geom_point(show.legend = TRUE) +
  geom_richtext(aes(x = max(Date)-90, y = min(value)+.1, label = format(Date, '%b-%Y') ), text.colour ="#666666", show.legend=F, 
                fill = NA, label.color = NA, size=10) + #Date inside plot
  theme_BBVA_grey +
  annotation_raster(logo, ymin = ymin, ymax = ymax, xmin = xmin, xmax =xmax) +
  scale_color_manual(values=c( "#0F4B7C","#2DCCCD","#FFFFFF"),
                     # scale_color_manual(values=c( "#0F4B7C","#2DCCCD","#9DC1F5","#D8BE75"),
                     breaks= c("Construccion Vivienda",  "AEAT Construccion Edificios")) +
  
  scale_y_continuous(trans = modulus_trans(0.003), breaks =  c(-0.3,-0.2,-0.1,0.0,+0.1,+0.2,+0.3),
                     labels = c('-30%','-20%','-10%','0%','+10%','+20%','+30%'),
                     limits = c(-0.3, +0.3),
                     sec.axis=sec_axis(~.*1,breaks= c(-0.3,-0.2,-0.1,0.0,+0.1,+0.2,+0.3),labels = c('-30%','-20%','-10%','0%','+10%','+20%','+30%'))) + 
  
  
  
  scale_x_date(limits=c(as.Date("2022-08-01"), as.Date(date)), date_breaks = "1 months", date_labels = "%b-%y") +
  
  #geom_vline(xintercept = as.Date("2020-03-15"), colour="black",linetype = "dotted") +
  #geom_text(aes(x=as.Date("2020-03-13"), label="Covid in EU", y=1), colour="#666666", angle=90, vjust = 1.2, size=3)+
  
  #geom_vline(xintercept = as.Date("2021-03-02"), colour="black",linetype = "dotted") +
  #geom_text(aes(x=as.Date("2021-03-01"), label="Delta Variant in EU", y=05), colour="#666666", angle=90, vjust =0., size=3)+
  
  #geom_vline(xintercept = as.Date("2021-11-22"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2021-11-21"), label="Omicron", y=0.05), colour="#666666", angle=90, vjust = 1.2,size=3)+
  
  #geom_vline(xintercept = as.Date("2022-02-21"), colour="black",linetype = "dotted") +
  #geom_text(aes(x=as.Date("2022-02-20"), label="Rusia-Ucrania", y=0.045), colour="#666666", angle=90, vjust = 1.2,size=3)+
  transition_reveal(Date)  +
  #exit_shrink() +
  #enter_fade() +
  #ease_aes("circular-out") +
  guides(col = guide_legend(nrow = 1, reverse = F)) +
  labs(title =  "España: Inversion BigData BBVA y Ventas AEAT Edificios ",  
       caption= "Fuente: BBVA Research & AEAT", 
       subtitle = "(Real deflactado por def inversión y deflactor Ventas AEAT.% Var YoY")  # Subtitle Date
gg_demanda <- gg_demanda + labs(x="", y = '% YoY Real (deflactado por def inversión & deflactor Ventas )', positiony="left",color="") 

animate(gg_demanda, nframes = 200, duration = 7 ,fps = 32, width = 1200,
        height = 750, end_pause = 100)

anim_save("G6 Ventas AEAT Construccion edificioss.gif")

##################### Inversion e Ingenieria Civil


filename <- file("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")
logo <- readPNG("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")

pais <- openxlsx::read.xlsx(file, sheet = country , startRow = 1)[,1:60]

xmax <- ymd(max_date)-10
xmin <- ymd(max_date)-70
ymin <- 0.25
ymax <- 0.3
names(pais)[1] <- 'Date'
####### Datos de Inversion
names(pais)[14] <- 'Construccion Vivienda'
names(pais)[15] <- 'Construccion Otros'
names(pais)[16] <- 'Construccion'
names(pais)[17] <- 'Inversión Transporte'
names(pais)[18] <- 'Inversion B. Equipo'
names(pais)[19] <- 'Inversión Maquinaria y Eq'
names(pais)[20] <- 'Intangibles '
names(pais)[21] <- 'Inversion Total'
names(pais)[22] <- 'Inversion Digital'
names(pais)[52] <- 'AEAT Maquinaria y Equipo'
names(pais)[53] <- 'AEAT Fab. Vehiculos'
names(pais)[54] <- 'AEAT Informatica'
names(pais)[55] <- 'AEAT Construccion'
names(pais)[56] <- 'AEAT Construccion Edificios'
names(pais)[57] <- 'AEAT Construccion Ingenieria'
names(pais)[58] <- 'AEAT Construccion Especializada'
names(pais)[59] <- 'AEAT Comunicaciones'
names(pais)[60] <- 'AEAT Act. Profesionales '



origin <-"1899-12-30"

start_date<- "2023-01-01"
pais$Date <- as.Date(pais$Date , origin = origin)

#pais$Date <- as.Date(pais$Date , origin = "1899-12-30")

pais <- select(pais,1,57,15)
pais
fechas <- select(pais, Date)
fechas$variable <- 'Date'
fechas$value <- -2

pais <-  pais %>% reshape2::melt(id.vars='Date') %>%  na.omit()

pais$Value <- scales::percent(pais$value, accuracy = 0.01 )
pais$Value <- paste(pais$variable, pais$Value, sep = ": ")
fechas$newdate <- strptime(as.character(fechas$Date), "%Y-%m-%d")
fechas$newdate <- format(fechas$newdate, "%d-%m-%Y")
fechas$Value <- paste(fechas$variable, fechas$newdate, sep = ": ")
fechas <- select(fechas, -newdate)
pais <- rbind(pais, fechas)

colnames(pais)[1] <- 'Date'


#scale_y_continuous(
#  "mpg (US)", 
#  sec.axis = sec_axis(~ . * 1.20, name = "mpg (UK)")
#)


gg_demanda <- pais %>% group_by(variable,Date) %>% 
  ggplot( aes(x=Date, y= value,  colour = variable))+ 
  geom_line() +
  #  geom_point(show.legend = TRUE) +
  geom_richtext(aes(x = max(Date)-90, y = min(value)+.1, label = format(Date, '%b-%Y') ), text.colour ="#666666", show.legend=F, 
                fill = NA, label.color = NA, size=10) + #Date inside plot
  theme_BBVA_grey +
  annotation_raster(logo, ymin = ymin, ymax = ymax, xmin = xmin, xmax =xmax) +
  scale_color_manual(values=c( "#0F4B7C","#2DCCCD","#FFFFFF"),
                     # scale_color_manual(values=c( "#0F4B7C","#2DCCCD","#9DC1F5","#D8BE75"),
                     breaks= c("Construccion Otros",  "AEAT Construccion Ingenieria")) +
  
  scale_y_continuous(trans = modulus_trans(0.003), breaks =  c(-0.2,-0.1,0.0,+0.1,+0.2,+0.3,+0.4),
                     labels = c('-20%','-10%','0%','+10%','+20%','+30%','+40%'),
                     limits = c(-0.2, +0.4),
                     sec.axis=sec_axis(~.*1,breaks= c(-0.2,-0.1,0.0,+0.1,+0.2,+0.3,0.4),labels = c('-20%','-10%','0%','+10%','+20%','+30%','+40%'))) + 
  
  
  
  scale_x_date(limits=c(as.Date("2022-08-01"), as.Date(date)), date_breaks = "1 months", date_labels = "%b-%y") +
  
  #geom_vline(xintercept = as.Date("2020-03-15"), colour="black",linetype = "dotted") +
  #geom_text(aes(x=as.Date("2020-03-13"), label="Covid in EU", y=1), colour="#666666", angle=90, vjust = 1.2, size=3)+
  
  #geom_vline(xintercept = as.Date("2021-03-02"), colour="black",linetype = "dotted") +
  #geom_text(aes(x=as.Date("2021-03-01"), label="Delta Variant in EU", y=05), colour="#666666", angle=90, vjust =0., size=3)+
  
  #geom_vline(xintercept = as.Date("2021-11-22"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2021-11-21"), label="Omicron", y=0.05), colour="#666666", angle=90, vjust = 1.2,size=3)+
  
  #geom_vline(xintercept = as.Date("2022-02-21"), colour="black",linetype = "dotted") +
  #geom_text(aes(x=as.Date("2022-02-20"), label="Rusia-Ucrania", y=0.045), colour="#666666", angle=90, vjust = 1.2,size=3)+
  transition_reveal(Date)  +
  #exit_shrink() +
  #enter_fade() +
  #ease_aes("circular-out") +
  guides(col = guide_legend(nrow = 1, reverse = F)) +
  labs(title =  "España: Inversion BigData BBVA y Ventas AEAT Constr. Otros ",  
       caption= "Fuente: BBVA Research & AEAT", 
       subtitle = "(Real deflactado por def inversión y deflactor Ventas AEAT.% Var YoY")  # Subtitle Date
gg_demanda <- gg_demanda + labs(x="", y = '% YoY Real (deflactado por def inversión & deflactor Ventas )', positiony="left",color="") 

animate(gg_demanda, nframes = 200, duration = 7 ,fps = 32, width = 1200,
        height = 750, end_pause = 100)

anim_save("G6 Ventas AEAT Construccion Civil.gif")

###################################### Inversion en Intangibles


filename <- file("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")
logo <- readPNG("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")

pais <- openxlsx::read.xlsx(file, sheet = country , startRow = 1)[,1:60]

xmax <- ymd(max_date)-10
xmin <- ymd(max_date)-70
ymin <- 0.25
ymax <- 0.3
names(pais)[1] <- 'Date'
####### Datos de Inversion
names(pais)[14] <- 'Construccion Vivienda'
names(pais)[15] <- 'Construccion Otros'
names(pais)[16] <- 'Construccion'
names(pais)[17] <- 'Inversión Transporte'
names(pais)[18] <- 'Inversion B. Equipo'
names(pais)[19] <- 'Inversión Maquinaria y Eq'
names(pais)[20] <- 'Intangibles'
names(pais)[21] <- 'Inversion Total'
names(pais)[22] <- 'Inversion Digital'
names(pais)[52] <- 'AEAT Maquinaria y Equipo'
names(pais)[53] <- 'AEAT Fab. Vehiculos'
names(pais)[54] <- 'AEAT Informatica'
names(pais)[55] <- 'AEAT Construccion'
names(pais)[56] <- 'AEAT Construccion Edificios'
names(pais)[57] <- 'AEAT Construccion Ingenieria'
names(pais)[58] <- 'AEAT Construccion Especializada'
names(pais)[59] <- 'AEAT Comunicaciones'
names(pais)[60] <- 'AEAT Act. Profesionales'


origin <-"1899-12-30"

start_date<- "2023-01-01"
pais$Date <- as.Date(pais$Date , origin = origin)

#pais$Date <- as.Date(pais$Date , origin = "1899-12-30")

pais <- select(pais,1,60,20)
pais
fechas <- select(pais, Date)
fechas$variable <- 'Date'
fechas$value <- -2

pais <-  pais %>% reshape2::melt(id.vars='Date') %>%  na.omit()

pais$Value <- scales::percent(pais$value, accuracy = 0.01 )
pais$Value <- paste(pais$variable, pais$Value, sep = ": ")
fechas$newdate <- strptime(as.character(fechas$Date), "%Y-%m-%d")
fechas$newdate <- format(fechas$newdate, "%d-%m-%Y")
fechas$Value <- paste(fechas$variable, fechas$newdate, sep = ": ")
fechas <- select(fechas, -newdate)
pais <- rbind(pais, fechas)

colnames(pais)[1] <- 'Date'


#scale_y_continuous(
#  "mpg (US)", 
#  sec.axis = sec_axis(~ . * 1.20, name = "mpg (UK)")
#)


gg_demanda <- pais %>% group_by(variable,Date) %>% 
  ggplot( aes(x=Date, y= value,  colour = variable))+ 
  geom_line() +
  #  geom_point(show.legend = TRUE) +
  geom_richtext(aes(x = max(Date)-90, y = min(value)+.1, label = format(Date, '%b-%Y') ), text.colour ="#666666", show.legend=F, 
                fill = NA, label.color = NA, size=10) + #Date inside plot
  theme_BBVA_grey +
  annotation_raster(logo, ymin = ymin, ymax = ymax, xmin = xmin, xmax =xmax) +
  scale_color_manual(values=c( "#0F4B7C","#2DCCCD","#FFFFFF"),
                     # scale_color_manual(values=c( "#0F4B7C","#2DCCCD","#9DC1F5","#D8BE75"),
                     breaks= c("Intangibles",  "AEAT Act. Profesionales")) +
  
  scale_y_continuous(trans = modulus_trans(0.003), breaks =  c(-0.1,0.0,+0.1,+0.2,+0.3),
                     labels = c('-10%','0%','+10%','+20%','+30%'),
                     limits = c(-0.1, +0.3),
                     sec.axis=sec_axis(~.*1,breaks= c(-0.1,0.0,+0.1,+0.2,+0.3),labels = c('-10%','0%','+10%','+20%','+30%'))) + 
  
  
  
  scale_x_date(limits=c(as.Date("2022-08-01"), as.Date(date)), date_breaks = "1 months", date_labels = "%b-%y") +
  
  #geom_vline(xintercept = as.Date("2020-03-15"), colour="black",linetype = "dotted") +
  #geom_text(aes(x=as.Date("2020-03-13"), label="Covid in EU", y=1), colour="#666666", angle=90, vjust = 1.2, size=3)+
  
  #geom_vline(xintercept = as.Date("2021-03-02"), colour="black",linetype = "dotted") +
  #geom_text(aes(x=as.Date("2021-03-01"), label="Delta Variant in EU", y=05), colour="#666666", angle=90, vjust =0., size=3)+
  
  #geom_vline(xintercept = as.Date("2021-11-22"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2021-11-21"), label="Omicron", y=0.05), colour="#666666", angle=90, vjust = 1.2,size=3)+
  
  #geom_vline(xintercept = as.Date("2022-02-21"), colour="black",linetype = "dotted") +
  #geom_text(aes(x=as.Date("2022-02-20"), label="Rusia-Ucrania", y=0.045), colour="#666666", angle=90, vjust = 1.2,size=3)+
  transition_reveal(Date)  +
  #exit_shrink() +
  #enter_fade() +
  #ease_aes("circular-out") +
  guides(col = guide_legend(nrow = 1, reverse = F)) +
  labs(title =  "España: Inversion BigData BBVA y Ventas AEAT Act Profesionales",  
       caption= "Fuente: BBVA Research & AEAT", 
       subtitle = "(Real deflactado por def inversión y deflactor Ventas AEAT.% Var YoY")  # Subtitle Date
gg_demanda <- gg_demanda + labs(x="", y = '% YoY Real (deflactado por def inversión & deflactor Ventas )', positiony="left",color="") 

animate(gg_demanda, nframes = 200, duration = 7 ,fps = 32, width = 1200,
        height = 750, end_pause = 100)

anim_save("G6 Ventas AEAT intangibles.gif")

#######################################################
########## Graficos Consumo & English ##############################
filename <- file("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")
logo <- readPNG("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")
country<-"Spain"
file <- 'hoja sectorial Spain deflactado.xlsx'

pais <- openxlsx::read.xlsx(file, sheet = country , startRow = 1)[,1:48]

xmax <- ymd(max_date)-10
xmin <- ymd(max_date)-90
ymin <- 0.045
ymax <- 0.06

names(pais)[1] <- 'Date'
names(pais)[25] <- 'Total Consumption (% Yearly)'
names(pais)[47] <- 'Total Consumption (% Quarterly S.Adj)'



xmax <- ymd(max_date)
xmin <- ymd(max_date)-60
ymin <- 0.06
ymax <- 0.08



origin <-"1899-12-30"
start_date<- "2021-09-01"
pais$Date <- as.Date(pais$Date , origin = origin)

#pais$Date <- as.Date(pais$Date , origin = "1899-12-30")

pais <- select(pais,1,25, 47)
pais
fechas <- select(pais, Date)
fechas$variable <- 'Date'
fechas$value <- -2

pais <-  pais %>% reshape2::melt(id.vars='Date') %>%  na.omit()

pais$Value <- scales::percent(pais$value, accuracy = 0.01 )
pais$Value <- paste(pais$variable, pais$Value, sep = ": ")
fechas$newdate <- strptime(as.character(fechas$Date), "%Y-%m-%d")
fechas$newdate <- format(fechas$newdate, "%d-%m-%Y")
fechas$Value <- paste(fechas$variable, fechas$newdate, sep = ": ")
fechas <- select(fechas, -newdate)
pais <- rbind(pais, fechas)

colnames(pais)[1] <- 'Date'


#scale_y_continuous(
#  "mpg (US)", 
#  sec.axis = sec_axis(~ . * 1.20, name = "mpg (UK)")
#)


gg_demanda <- pais %>% group_by(variable,Date) %>% 
  ggplot( aes(x=Date, y= value,  colour = variable))+ 
  geom_line() +
  geom_point(show.legend = FALSE) + # ends with a point
  geom_richtext(aes(x = max(Date)-90, y = min(value)+.1, label = format(Date, '%b-%Y') ), text.colour ="#666666", show.legend=F, 
                fill = NA, label.color = NA, size=10) + #Date inside plot
  theme_BBVA_grey +
  annotation_raster(logo, ymin = ymin, ymax = ymax, xmin = xmin, xmax =xmax) +
  scale_color_manual(values=c( "#0F4B7C","#2DCCCD","#FFFFFF"),
                     # scale_color_manual(values=c( "#0F4B7C","#2DCCCD","#9DC1F5","#D8BE75"),
                     breaks= c("Total Consumption (% Yearly)",  "Total Consumption (% Quarterly S.Adj)")) +
  scale_y_continuous(trans = modulus_trans(0.003), breaks =  c( -0.08,-0.06,-0.04,-0.02,0,0.02,0.04,0.06,0.08),
                     labels = c('-8%', '-6%','-4%','-2%','0%','2%','4%','6%','8%'),
                     limits = c(-0.08, 0.08),
                     sec.axis=sec_axis(~.*1,breaks= c(-0.08,-0.06,-0.04,-0.02,0,0.02,0.04,0.06,0.08),labels = c('-8%', '-6%','-4%','-2%','0%','2%','4%','6%','8%'))) + 
  
  
  scale_x_date(limits=c(as.Date("2021-08-01"), as.Date(date)), date_breaks = "2 months", date_labels = "%b-%y") +
  
  geom_vline(xintercept = as.Date("2020-03-15"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2020-03-13"), label="Covid in EU", y=1), colour="#666666", angle=90, vjust = 1.2, size=3)+
  
  geom_vline(xintercept = as.Date("2021-03-02"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2021-03-01"), label="Delta Variant in EU", y=05), colour="#666666", angle=90, vjust =0., size=3)+
  
  geom_vline(xintercept = as.Date("2021-11-22"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2021-11-21"), label="Omicron", y=0.05), colour="#666666", angle=90, vjust = 1.2,size=3)+
  
  geom_vline(xintercept = as.Date("2022-02-21"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2022-02-20"), label="Russia-Ukrinea", y=0.05), colour="#666666", angle=90, vjust = 1.2,size=3)+
  transition_reveal(Date)  +
  exit_shrink() +
  enter_fade() +
  #ease_aes("circular-out") +
  guides(col = guide_legend(nrow = 1, reverse = F)) +
  labs(title =  "Spain: BigData Total Consumption  (BBVA Research)",  
       #caption= "source: BBVA Research", 
       subtitle = "(Real deflated by CPI, Cum 28D % Yearly & Cum 90D % QoQ Sadj)")  # Subtitle Date
gg_demanda <- gg_demanda + labs(x="", y = '% YoY Real (deflacted by CPI )', positiony="left",color="") 


animate(gg_demanda, nframes = 200, duration = 5 ,fps = 32, width = 1200,
        height = 750, end_pause = 30)

anim_save("G4_Consumo Total y Ventas AEAT .gif")


#######################################################
########## Grafico Consumo Tarjetas y Total ##############################
filename <- file("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")
logo <- readPNG("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")

country<-"Spain"
file <- 'hoja sectorial Spain deflactado.xlsx'

pais <- openxlsx::read.xlsx(file, sheet = country , startRow = 1)[,1:47]

xmax <- ymd(max_date)
xmin <- ymd(max_date)-60
ymin <- 0.33
ymax <- 0.4
names(pais)[1] <- 'Date'
names(pais)[2] <- 'Consumo Tarjetas'
names(pais)[25] <- 'Consumo Total'

pais <- select(pais,1,25, 2)
pais

origin <-"1899-12-30"
start_date<- "2021-09-01"
pais$Date <- as.Date(pais$Date , origin = origin)

#pais$Date <- as.Date(pais$Date , origin = "1899-12-30")

fechas <- select(pais, Date)
fechas$variable <- 'Date'
fechas$value <- -2

pais <-  pais %>% reshape2::melt(id.vars='Date') %>%  na.omit()

pais$Value <- scales::percent(pais$value, accuracy = 0.01 )
pais$Value <- paste(pais$variable, pais$Value, sep = ": ")
fechas$newdate <- strptime(as.character(fechas$Date), "%Y-%m-%d")
fechas$newdate <- format(fechas$newdate, "%d-%m-%Y")
fechas$Value <- paste(fechas$variable, fechas$newdate, sep = ": ")
fechas <- select(fechas, -newdate)
pais <- rbind(pais, fechas)

colnames(pais)[1] <- 'Date'


#scale_y_continuous(
#  "mpg (US)", 
#  sec.axis = sec_axis(~ . * 1.20, name = "mpg (UK)")
#)


gg_demanda <-    pais %>% group_by(variable, Date) %>% 
  ggplot( aes(x=Date, y= value,  colour = variable))+ 
  annotation_raster(logo, ymin = ymin, ymax = ymax, xmin = xmin, xmax =xmax) +
   geom_line() +
  geom_point(show.legend = FALSE) + # ends with a point
  geom_richtext(aes(x = max(Date)-90, y = min(value)+.1, label = format(Date, '%b-%Y') ), text.colour ="#666666", show.legend=F, 
                fill = NA, label.color = NA, size=10) + #Date inside plot
  theme_BBVA_grey +
  scale_color_manual(values=c( "#0F4B7C","#2DCCCD","#9DC1F5","#D8BE75"),
                      breaks= c("Consumo Total",  "Consumo Tarjetas")) +
  scale_y_continuous(trans = modulus_trans(0.003), breaks =  c( -0.1,-0.05,0,0.05,0.1),
                    labels = c('-10%','-5%','0%','5%','10%'),
                   limits = c(-0.1, 0.1),
                  sec.axis=sec_axis(~.*1,breaks= c(-0.1,0,0.1,0.2,0.3,0.5),labels = c('-10%','-5%','0%','5%','10%'))) + 
  scale_x_date(limits=c(as.Date("2021-08-01"), as.Date(date)), date_breaks = "1 months", date_labels = "%b-%y") +
  
  geom_vline(xintercept = as.Date("2020-03-15"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2020-03-13"), label="Covid in EU", y=1), colour="#666666", angle=90, vjust = 1.2, size=3)+
  
  geom_vline(xintercept = as.Date("2021-03-02"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2021-03-01"), label="Delta Variant in EU", y=1), colour="#666666", angle=90, vjust = 1.2, size=3)+
  
  geom_vline(xintercept = as.Date("2021-11-22"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2021-11-21"), label="Omicron", y=0.4), colour="#666666", angle=90, vjust = 1.2,size=3)+
  
  geom_vline(xintercept = as.Date("2022-02-21"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2022-02-20"), label="Rusia-Ucrania", y=0.4), colour="#666666", angle=90, vjust = 1.2,size=3)+
  transition_reveal(Date)  +
  exit_shrink() +
  enter_fade() +
  ease_aes("circular-out") +
  guides(col = guide_legend(nrow = 1, reverse = F)) +
  labs(title =  "España: Indicadores de Consumo BigData (Total y Tarjetas)",  
       caption= "Fuente: BBVA Research", 
       subtitle = "(Real, YoY 28D Moving avg - Date:{format(frame_along, '%d-%m-%Y')})")  # Subtitle Date
gg_demanda <- gg_demanda + labs(x="", y = '% YoY Real (deflactado por IPC)', positiony="left",color="") 

animate(gg_demanda, nframes = 200, duration = 7 ,fps = 12, width = 1200,
        height = 750, end_pause = 30)

anim_save("Consumo Total y Tarjetas.gif")
###################################
#######################################################
########## Grafico PIB Demanda Externa ##############################
country<-"Spain"
file <- 'hoja sectorial Spain deflactado.xlsx'

pais <- openxlsx::read.xlsx(file, sheet = country , startRow = 1)[,1:48]

filename <- file("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")
logo <- readPNG("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")

xmax <- ymd(max_date)
xmin <- ymd(max_date)-60
ymin <- 0.65
ymax <- 0.8

origin <-"1899-12-30"
start_date<- "2021-12-01"
pais$Date <- as.Date(pais$Date , origin = origin)

pais$Date <- as.Date(pais$Date , origin = "1899-12-30")


names(pais)[23] <- 'Importaciones'
names(pais)[24] <- 'Exportaciones'
names(pais)[25] <- 'Consumo Total'
pais




pais <- select(pais,1,23, 24)
pais
fechas <- select(pais, Date)
fechas$variable <- 'Date'
fechas$value <- -2

pais <-  pais %>% reshape2::melt(id.vars='Date') %>%  na.omit()

pais$Value <- scales::percent(pais$value, accuracy = 0.01 )
pais$Value <- paste(pais$variable, pais$Value, sep = ": ")
fechas$newdate <- strptime(as.character(fechas$Date), "%Y-%m-%d")
fechas$newdate <- format(fechas$newdate, "%d-%m-%Y")
fechas$Value <- paste(fechas$variable, fechas$newdate, sep = ": ")
fechas <- select(fechas, -newdate)
pais <- rbind(pais, fechas)

colnames(pais)[1] <- 'Date'





gg_demanda <-    pais %>% group_by(variable, Date) %>% 
  ggplot( aes(x=Date, y= value,  colour = variable))+ 
  annotation_raster(logo, ymin = ymin, ymax = ymax, xmin = xmin, xmax =xmax) +
  geom_line() +
  geom_point(show.legend = FALSE) + # ends with a point
  geom_richtext(aes(x = max(Date)-90, y = min(value)+.1, label = format(Date, '%b-%Y') ), text.colour ="#666666", show.legend=F, 
                fill = NA, label.color = NA, size=10) + #Date inside plot
  theme_BBVA_grey +
  scale_color_manual(values=c( "#0F4B7C","#2DCCCD","#9DC1F5","#D8BE75","#FFFFFF"),
    #                 breaks= c("Consumo Total", "Inversion Total", "Importaciones", "Exportaciones")) + 
    breaks= c("Importaciones", "Exportaciones")) +
  scale_y_continuous(trans = modulus_trans(0.003), breaks =  c(-0.2,0.0,0.2,0.4,0.6,0.8,1.0,1.2),
                     labels = c('-20%','0%','20%','40%','60%','80%','100%','120%'),
                     limits = c(-0.6, 1.2),
                     sec.axis=sec_axis(~.*1,breaks= c(-0.2,0.0,0.2,0.4,0.6,0.8,1.0,1.2),labels = c('-60%','-20%','0%','20%','40%','60%','80%','100%','120%'))) +   

  scale_x_date(limits=c(as.Date("2021-08-01"), as.Date(date)), date_breaks = "2 months", date_labels = "%b-%y") +
  
  geom_vline(xintercept = as.Date("2020-03-15"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2020-03-13"), label="Covid in EU", y=1), colour="#666666", angle=90, vjust = 1.2, size=3)+
  
  geom_vline(xintercept = as.Date("2021-03-02"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2021-03-01"), label="Delta Variant in EU", y=1), colour="#666666", angle=90, vjust = 1.2, size=3)+
  
  geom_vline(xintercept = as.Date("2021-11-22"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2021-11-21"), label="Omicron", y=0.7), colour="#666666", angle=90, vjust = 1.2,size=3)+
  
  geom_vline(xintercept = as.Date("2022-02-21"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2022-02-20"), label="Rusia-Ucrania", y=0.7), colour="#666666", angle=90, vjust = 1.2,size=3)+
  transition_reveal(Date)  +
  exit_shrink() +
  enter_fade() +
  ease_aes("circular-out") +
  guides(col = guide_legend(nrow = 1, reverse = F)) +
  labs(title =  "España: Indicadores de Demanda Externa BigData",  
       caption= "Fuente: BBVA Research", 
       subtitle = "(Real, Acumulado 28D Crecimiento Interanual  - Date:{format(frame_along, '%d-%m-%Y')})")  # Subtitle Date
gg_demanda <- gg_demanda + labs(x="", y = '% YoY Real (deflactado por Deflactores Mensualizados)', color="") 

animate(gg_demanda, nframes = 200, duration = 7 ,fps = 32, width = 1200,
        height = 750, end_pause = 30)

anim_save("G4 Demanda Externa_España_Real.gif")

#######################################################
########## Grafico Inversion ##############################
#filename <- file("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")
#logo <- readPNG("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")

filename <- file("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")
logo <- readPNG("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")

xmax <- ymd(max_date)
xmin <- ymd(max_date)-60
ymin <- 0.25
ymax <- 0.33




country<-"Spain"
file <- 'hoja sectorial Spain deflactado.xlsx'

pais <- openxlsx::read.xlsx(file, sheet = country , startRow = 1)[,1:45]






names(pais)[14] <- 'Construccion Vivienda'
names(pais)[15] <- 'Construccion Otros '
names(pais)[16] <- 'Construcción'
names(pais)[17] <- 'Transporte'
names(pais)[18] <- 'B. Equipo'
names(pais)[19] <- 'Maquinaria y Equipo'
names(pais)[20] <- 'Intangibles'
names(pais)[21] <- 'Total'
names(pais)[22] <- 'Digital'

origin <-"1899-12-30"
pais$Date <- as.Date(pais$Date , origin = origin)

#pais$Date <- as.Date(pais$Date , origin = "1899-12-30")

pais <- select(pais,1, 21, 16,19,20)
pais
fechas <- select(pais, Date)
fechas$variable <- 'Date'
fechas$value <- -2

pais <-  pais %>% reshape2::melt(id.vars='Date') %>%  na.omit()

pais$Value <- scales::percent(pais$value, accuracy = 0.01 )
pais$Value <- paste(pais$variable, pais$Value, sep = ": ")
fechas$newdate <- strptime(as.character(fechas$Date), "%Y-%m-%d")
fechas$newdate <- format(fechas$newdate, "%d-%m-%Y")
fechas$Value <- paste(fechas$variable, fechas$newdate, sep = ": ")
fechas <- select(fechas, -newdate)
pais <- rbind(pais, fechas)

colnames(pais)[1] <- 'Date'




gg_inversion <-    pais %>% group_by(variable, Date) %>% 
  ggplot( aes(x=Date, y= value,  colour = variable))+ 
 annotation_raster(logo, ymin = ymin, ymax = ymax, xmin = xmin, xmax =xmax) +
   geom_line() +
  geom_point(show.legend = FALSE) + # ends with a point
  geom_richtext(aes(x = max(Date)-90, y = min(value)+.1, label = format(Date, '%b-%Y') ), text.colour ="#666666", show.legend=F, 
                fill = NA, label.color = NA, size=10) + #Date inside plot
  theme_BBVA_grey +
    scale_color_manual(values=c( "#0F4B7C","#D8BE75" ,"#2DCCCD","#9DC1F5" ,"#9DC1F5"),
                     breaks= c("Total", "Construcción","Maquinaria y Equipo","Intangibles")) + 
  scale_y_continuous(trans = modulus_trans(0.003), breaks =  c( -0.2,-0.1,0,0.1,0.2,0.3,0.4),
                     labels = c('-20%','-10%','0%','10%','20%','30%','40%' ),
                     limits = c(-0.2, 0.4), 
  sec.axis=sec_axis(~.*1,breaks= c(-0.2,-0.1,0,0.1,0.2,0.3,0.4),labels = c('-20%','-10%','0%','10%','20%','30%','40%' ))) +   
  
  scale_x_date(limits=c(as.Date("2021-08-01"), as.Date(date)), date_breaks = "2 months", date_labels = "%b-%y") +
  
  geom_vline(xintercept = as.Date("2020-03-15"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2020-03-13"), label="Covid in EU", y=1), colour="#666666", angle=90, vjust = 1.2, size=3)+
  
  geom_vline(xintercept = as.Date("2021-03-02"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2021-03-01"), label="Delta Variant in EU", y=1), colour="#666666", angle=90, vjust = 1.2, size=3)+
  
  geom_vline(xintercept = as.Date("2021-11-22"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2021-11-21"), label="Omicron", y=0.3), colour="#666666", angle=90, vjust = 1.2,size=3)+
  
  geom_vline(xintercept = as.Date("2022-02-21"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2022-02-20"), label="Rusia-Ucrania", y=0.3), colour="#666666", angle=90, vjust = 1.2,size=3)+
  transition_reveal(Date)  +
  exit_shrink() +
  enter_fade() +
  ease_aes("circular-out") +
  guides(col = guide_legend(nrow = 1, reverse = F)) +
  labs(title =  "España: Indicadores de Inversión BigData",  
       caption= "Fuente: BBVA Research", 
       subtitle = "(Real, Acumulado 28D Crecimiento Interanual - Date:{format(frame_along, '%d-%m-%Y')})") # Subtitle Date
gg_inversion <- gg_inversion + labs(x="", y = '% YoY Real (deflactado po Deflactores Mensualizados)', color="") 

animate(gg_inversion, nframes = 200, duration = 7 ,fps = 32, width = 1200,
        height = 750, end_pause = 30)

anim_save("G3 Inversion_España_Real.gif")
anim_save("G3 Inversion_España_Real_digital.gif")
#
############ Graficos Consumo de Tarjetas

filename <- file("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")
logo <- readPNG("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")

xmax <- ymd(max_date)
xmin <- ymd(max_date)-60
ymin <- 0.6
ymax <- 0.72




country<-"Spain"
file <- 'hoja sectorial Spain deflactado.xlsx'

pais <- openxlsx::read.xlsx(file, sheet = country , startRow = 1)[,1:25]


names(pais)[1] <- 'Date'
names(pais)[2] <- 'Consumo Tarjetas'
names(pais)[11] <- 'Bienes'
names(pais)[12] <- 'Servicios'


pais <- select(pais,1,2, 11,12)
pais



pais$Date <- as.Date(pais$Date , origin = "1899-12-30")

fechas <- select(pais, Date)
fechas$variable <- 'Date'
fechas$value <- -2

pais <-  pais %>% reshape2::melt(id.vars='Date') %>%  na.omit()

pais$Value <- scales::percent(pais$value, accuracy = 0.01 )
pais$Value <- paste(pais$variable, pais$Value, sep = ": ")
fechas$newdate <- strptime(as.character(fechas$Date), "%Y-%m-%d")
fechas$newdate <- format(fechas$newdate, "%d-%m-%Y")
fechas$Value <- paste(fechas$variable, fechas$newdate, sep = ": ")
fechas <- select(fechas, -newdate)
pais <- rbind(pais, fechas)

colnames(pais)[1] <- 'Date'



gg_consumo_1 <-    pais %>% group_by(variable, Date) %>% 
  ggplot( aes(x=Date, y= value,  colour = variable)) + 
  annotation_raster(logo, ymin = ymin, ymax = ymax, xmin = xmin, xmax =xmax) +
  geom_line() +
  geom_point(show.legend = FALSE) + # ends with a point
  geom_richtext(aes(x = max(Date)-90, y = min(value)+.1, label = format(Date, '%b-%Y') ), text.colour ="#666666", show.legend=F, 
                fill = NA, label.color = NA, size=10) + #Date inside plot
  theme_BBVA_grey +
  scale_color_manual(values=c( "#0F4B7C","#D8BE75","#9DC1F5","#FFFFFF"),
                     breaks= c("Consumo Tarjetas", "Bienes", "Servicios")) + 
  scale_y_continuous(trans = modulus_trans(0.003), breaks =  c( -0.1,0,0.2,0.4,0.6,0.8,1),
                     labels = c('-10%','0%', '20%','40%', '60%','80%','100%' ),
                     limits = c(-0.1, 1.0)) +
  scale_x_date(limits=c(as.Date("2021-08-01"), as.Date(date)), date_breaks = "1 months", date_labels = "%b-%y") +
  
  
  geom_vline(xintercept = as.Date("2021-11-22"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2021-11-21"), label="Omicron", y=0.8), colour="#666666", angle=90, vjust = 0.8,size=3)+
  
  geom_vline(xintercept = as.Date("2022-02-21"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2022-02-20"), label="Rusia-Ucrania", y=0.7), colour="#666666", angle=90, vjust = 0.7,size=3)+
  transition_reveal(Date)  +
  exit_shrink() +
  enter_fade() +
  ease_aes("circular-out") +
  guides(col = guide_legend(nrow = 1, reverse = F)) +
  labs(title =  "España: Indicadores Consumo Tarjetas BigData",  
       caption= "Fuente: BBVA Research", 
       subtitle = "(Real, YoY 28D Moving avg - Date:{format(frame_along, '%d-%m-%Y')})")  # Subtitle Date
gg_consumo_1 <- gg_consumo_1 + labs(x="", y = '% YoY Real (Deflactado por IPC general)', color="") 


animate(gg_consumo_1, nframes = 200, duration = 7 ,fps = 10, width = 1200,
        height = 750, end_pause = 30)



anim_save("ConsumoBigData_Españareal_v1.gif")

#######################################################
########## Gráficos  Consumo de tarjetas##############################

filename <- file("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")
logo <- readPNG("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")

xmax <- ymd(max_date)
xmin <- ymd(max_date)-60
ymin <- 0.8
ymax <- 0.95




country<-"Spain"
file <- 'hoja sectorial Spain deflactado.xlsx'

pais <- openxlsx::read.xlsx(file, sheet = country , startRow = 1)[,1:25]


names(pais)[1] <- 'Date'
names(pais)[2] <- 'Consumo Tarjetas'
names(pais)[8] <- 'Restaurantes'
names(pais)[6] <- 'Ocio'
names(pais)[13] <-'Transporte'
pais <- select(pais,1,2, 8,7, 13)
pais



pais$Date <- as.Date(pais$Date , origin = "1899-12-30")

fechas <- select(pais, Date)
fechas$variable <- 'Date'
fechas$value <- -2

pais <-  pais %>% reshape2::melt(id.vars='Date') %>%  na.omit()

pais$Value <- scales::percent(pais$value, accuracy = 0.01 )
pais$Value <- paste(pais$variable, pais$Value, sep = ": ")
fechas$newdate <- strptime(as.character(fechas$Date), "%Y-%m-%d")
fechas$newdate <- format(fechas$newdate, "%d-%m-%Y")
fechas$Value <- paste(fechas$variable, fechas$newdate, sep = ": ")
fechas <- select(fechas, -newdate)
pais <- rbind(pais, fechas)

colnames(pais)[1] <- 'Date'



gg_consumo_2 <-    pais %>% group_by(variable, Date) %>% 
  ggplot( aes(x=Date, y= value,  colour = variable)) + 
  annotation_raster(logo, ymin = ymin, ymax = ymax, xmin = xmin, xmax =xmax) +
  geom_line() +
  geom_point(show.legend = FALSE) + # ends with a point
  geom_richtext(aes(x = max(Date)-90, y = min(value)+.1, label = format(Date, '%b-%Y') ), text.colour ="#666666", show.legend=F, 
                fill = NA, label.color = NA, size=10) + #Date inside plot
  theme_BBVA_grey +
  scale_color_manual(values=c( "#0F4B7C","#D8BE75","#9DC1F5","#2DCCCD","#FFFFFF"),
                     breaks= c("Consumo Tarjetas", "Ocio", "Restaurantes", "Transporte")) + 
  scale_y_continuous(trans = modulus_trans(0.003), breaks =  c( 0,0.2,0.4,0.6,0.8,1,1.2,1.4,1.6),
                     labels = c('0%','20%', '40%','60%','80%','100%', '120%', '140%','160%' ),
                     limits = c(0, 1.6)) +
  scale_x_date(limits=c(as.Date("2021-08-01"), as.Date(date)), date_breaks = "1 months", date_labels = "%b-%y") +
  
  
  geom_vline(xintercept = as.Date("2021-11-22"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2021-11-21"), label="Omicron", y=1.2), colour="#666666", angle=90, vjust = 1.2,size=3)+
  
  geom_vline(xintercept = as.Date("2022-02-21"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2022-02-20"), label="Rusia-Ucrania", y=1.2), colour="#666666", angle=90, vjust = 1.2,size=3)+
  transition_reveal(Date)  +
  exit_shrink() +
  enter_fade() +
  ease_aes("circular-out") +
  guides(col = guide_legend(nrow = 1, reverse = F)) +
  labs(title =  "España: Indicadores Consumo Tarjetas BigData Servicios",  
       caption= "Fuente: BBVA Research", 
       subtitle = "(Real, YoY 28D Moving avg - Date:{format(frame_along, '%d-%m-%Y')})")  # Subtitle Date
gg_consumo_2 <- gg_consumo_2 + labs(x="", y = '% YoY Real (Deflactado por IPC general)', color="") 

animate(gg_consumo_2, nframes = 200, duration = 7 ,fps = 10, width = 1200,
        height = 750, end_pause = 30)



anim_save("ConsumoBigData_Españareal_v2.gif")

#######################################################
########## Gráficos  Consumo Total  COICOP 1 ##############################

filename <- file("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")
logo <- readPNG("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")

xmax <- ymd(max_date)
xmin <- ymd(max_date)-60
ymin <- 0.13
ymax <- 0.18

country<-"Spain"
file <- 'hoja sectorial Spain deflactado.xlsx'

pais <- openxlsx::read.xlsx(file, sheet = country , startRow = 1)[,1:47]
#Medios de Pago
names(pais)[25] <- 'Total'
names(pais)[26] <- 'Tarjetas'
names(pais)[27] <- 'Domiciliaciones'
names(pais)[28] <- 'Transferencias'
names(pais)[29] <- 'Efectivo'
names(pais)[30] <- 'Alquileres'
names(pais)[31] <- 'Tarjetas Online'
names(pais)[32] <- 'Tarjetas Offline'
#Codigos COICOP
names(pais)[33] <- 'Alimentacion'
names(pais)[34] <- 'Bebidas Alc. y Tabaco'
names(pais)[35] <- 'Textil y Calzado'
names(pais)[36] <- 'Energia (Agua-Luz-Gas..)'
names(pais)[37] <- 'Salud'
names(pais)[38] <- 'Transporte'
names(pais)[39] <- 'Comunicaciones'
names(pais)[40] <- 'Cultura-Ocio'
names(pais)[41] <- 'Educacion'
names(pais)[42] <- 'Comunicaciones'
names(pais)[43] <- 'Restaurantes-Hoteles'
names(pais)[44] <- 'Otros bienes/Servicos'

names(pais)[45] <- 'Bienes de Primera Necesidad'
names(pais)[46] <- 'Bienes de Lujo'


pais <- select(pais,1,45, 46)
pais



pais$Date <- as.Date(pais$Date , origin = "1899-12-30")

fechas <- select(pais, Date)
fechas$variable <- 'Date'
fechas$value <- -2

pais <-  pais %>% reshape2::melt(id.vars='Date') %>%  na.omit()

pais$Value <- scales::percent(pais$value, accuracy = 0.01 )
pais$Value <- paste(pais$variable, pais$Value, sep = ": ")
fechas$newdate <- strptime(as.character(fechas$Date), "%Y-%m-%d")
fechas$newdate <- format(fechas$newdate, "%d-%m-%Y")
fechas$Value <- paste(fechas$variable, fechas$newdate, sep = ": ")
fechas <- select(fechas, -newdate)
pais <- rbind(pais, fechas)

colnames(pais)[1] <- 'Date'





gg_consumo_2 <-    pais %>% group_by(variable, Date) %>% 
  ggplot( aes(x=Date, y= value,  colour = variable)) + 
  annotation_raster(logo, ymin = ymin, ymax = ymax, xmin = xmin, xmax =xmax) +
  geom_line() +
  geom_point(show.legend = FALSE) + # ends with a point
  geom_richtext(aes(x = max(Date)-90, y = min(value)+.1, label = format(Date, '%b-%Y') ), text.colour ="#666666", show.legend=F, 
                fill = NA, label.color = NA, size=10) + #Date inside plot
  theme_BBVA_grey +
  scale_color_manual(values=c( "#0F4B7C","#0F4B7C","#9DC1F5"),
                     breaks= c("Total", "Bienes de Primera Necesidad", "Bienes de Lujo")) + 
  scale_y_continuous(trans = modulus_trans(0.003), breaks =  c(-0.2,-0.1,0, 0.1,0.2),
                     labels = c('-20%','-10%','0%','10%','20%' ),
                     limits = c(-0.2, 0.2),
  sec.axis=sec_axis(~.*1,breaks= c(-0.2,-0.1,0, 0.1,0.2),labels = c('-20%', '-10%','0%','10%', '20%'))) +   
  scale_x_date(limits=c(as.Date("2021-08-01"), as.Date(date)), date_breaks = "2 months", date_labels = "%b-%y") +
  
  
  geom_vline(xintercept = as.Date("2021-11-22"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2021-11-21"), label="Omicron", y=0.15), colour="#666666", angle=90, vjust = 1.2,size=3)+
  
  geom_vline(xintercept = as.Date("2022-02-21"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2022-02-20"), label="Rusia-Ucrania", y=0.15), colour="#666666", angle=90, vjust = 1.2,size=3)+
  transition_reveal(Date)  +
  exit_shrink() +
  enter_fade() +
  ease_aes("circular-out") +
  guides(col = guide_legend(nrow = 1, reverse = F)) +
  labs(title =  "España: Indic. Big Data por categorías Categorías (COICOP)",  
       caption= "Fuente: BBVA Research", 
       subtitle = "(Real, Acumulado 28D Crecimiento Interanual - Date:{format(frame_along, '%d-%m-%Y')})")  # Subtitle Date
gg_consumo_2 <- gg_consumo_2 + labs(x="", y = '% YoY Real (Deflactado por IPC COICOP)', color="") 

animate(gg_consumo_2, nframes = 200, duration = 7 ,fps = 35, width = 1200,
        height = 750, end_pause = 30)



anim_save("G5 ConsumoBigData_Españareal_necesidadylujo.gif")

#######################################################
########## Gráficos  Consumo COICOP 2 ##############################

filename <- file("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")
logo <- readPNG("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")

xmax <- ymd(max_date)
xmin <- ymd(max_date)-60
ymin <- 0.2
ymax <- 0.25

country<-"Spain"
file <- 'hoja sectorial Spain deflactado.xlsx'

pais <- openxlsx::read.xlsx(file, sheet = country , startRow = 1)[,1:46]
#Medios de Pago
names(pais)[25] <- 'Total'
names(pais)[26] <- 'Tarjetas'
names(pais)[27] <- 'Domiciliaciones'
names(pais)[28] <- 'Transferencias'
names(pais)[29] <- 'Efectivo'
names(pais)[30] <- 'Alquileres'
names(pais)[31] <- 'Tarjetas Online'
names(pais)[32] <- 'Tarjetas Offline'
#Codigos COICOP
names(pais)[33] <- 'Alimentacion y Bebidas'
names(pais)[34] <- 'Alcohol y Tabaco'
names(pais)[35] <- 'Textil y Calzado'
names(pais)[36] <- 'Energia (Agua-Luz-Gas..)'
names(pais)[37] <- 'Salud'
names(pais)[38] <- 'Transporte'
names(pais)[39] <- 'Comunicaciones'
names(pais)[40] <- 'Cultura-Ocio'
names(pais)[41] <- 'Educacion'
names(pais)[42] <- 'Comunicaciones'
names(pais)[43] <- 'Restaurantes-Hoteles'
names(pais)[44] <- 'Otros bienes/Servicos'

names(pais)[45] <- 'Bienes Duraderos'
names(pais)[46] <- 'Bienes No Duraderos'


pais <- select(pais,1,25, 33,34, 35)
pais



pais$Date <- as.Date(pais$Date , origin = "1899-12-30")

fechas <- select(pais, Date)
fechas$variable <- 'Date'
fechas$value <- -2

pais <-  pais %>% reshape2::melt(id.vars='Date') %>%  na.omit()

pais$Value <- scales::percent(pais$value, accuracy = 0.01 )
pais$Value <- paste(pais$variable, pais$Value, sep = ": ")
fechas$newdate <- strptime(as.character(fechas$Date), "%Y-%m-%d")
fechas$newdate <- format(fechas$newdate, "%d-%m-%Y")
fechas$Value <- paste(fechas$variable, fechas$newdate, sep = ": ")
fechas <- select(fechas, -newdate)
pais <- rbind(pais, fechas)

colnames(pais)[1] <- 'Date'

#max_date <- "2022-09-30"




gg_consumo_2 <-    pais %>% group_by(variable, Date) %>% 
  ggplot( aes(x=Date, y= value,  colour = variable)) + 
  annotation_raster(logo, ymin = ymin, ymax = ymax, xmin = xmin, xmax =xmax) +
  geom_line() +
  geom_point(show.legend = FALSE) + # ends with a point
  geom_richtext(aes(x = max(Date)-90, y = min(value)+.1, label = format(Date, '%b-%Y') ), text.colour ="#666666", show.legend=F, 
                fill = NA, label.color = NA, size=10) + #Date inside plot
  theme_BBVA_grey +
  scale_color_manual(values=c( "#0F4B7C","#D8BE75","#9DC1F5","#2DCCCD","#FFFFFF"),
                     breaks= c("Total", "Alimentacion y Bebidas", "Alcohol y Tabaco", "Textil y Calzado")) + 
  scale_y_continuous(trans = modulus_trans(0.003), breaks =  c( -0.3,-0.2,-0.1,0, 0.1,0.2),
                     labels = c('-30%', '-20%','-10%','0%','10%','20%' ),
                     limits = c(-0.3, 0.2),
                     sec.axis=sec_axis(~.*1,breaks= c(-0.3,-0.2,-0.1,0, 0.1,0.2),labels = c('-30%', '-20%','-10%','0%','10%','20%'))) +   
  scale_x_date(limits=c(as.Date("2021-08-01"), as.Date(date)), date_breaks = "1 months", date_labels = "%b-%y") +
  
  
  geom_vline(xintercept = as.Date("2021-11-22"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2021-11-21"), label="Omicron", y=0.8), colour="#666666", angle=90, vjust = 1.2,size=3)+
  
  geom_vline(xintercept = as.Date("2022-02-21"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2022-02-20"), label="Rusia-Ucrania", y=0.8), colour="#666666", angle=90, vjust = 1.2,size=3)+
  transition_reveal(Date)  +
  exit_shrink() +
  enter_fade() +
  ease_aes("circular-out") +
  guides(col = guide_legend(nrow = 1, reverse = F)) +
  labs(title =  "España: Indic. Big Data Consumo Total y Categorías (COICOP)",  
       caption= "Fuente: BBVA Research", 
       subtitle = "(Real, Acumulado 28D Crecimiento Interanual - Date:{format(frame_along, '%d-%m-%Y')})")  # Subtitle Date
gg_consumo_2 <- gg_consumo_2 + labs(x="", y = '% YoY Real (Deflactado por IPC COICOP)', color="") 

animate(gg_consumo_2, nframes = 200, duration = 7 ,fps = 10, width = 1200,
        height = 750, end_pause = 30)



anim_save("ConsumoBigData_Españareal_COICOP2.gif")

#######################################################
########## Gráficos  Consumo COICOP  ##############################

filename <- file("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")
logo <- readPNG("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")

#xmax <- ymd(max_date)
#xmin <- ymd(max_date)-60
ymin <- 0.2
ymax <- 0.27

pais <- openxlsx::read.xlsx(file, sheet = country , startRow = 1)[,1:48]


#Medios de Pago
names(pais)[25] <- 'Total'
names(pais)[26] <- 'Tarjetas'
names(pais)[27] <- 'Domiciliaciones'
names(pais)[28] <- 'Transferencias'
names(pais)[29] <- 'Efectivo'
names(pais)[30] <- 'Alquileres'
names(pais)[31] <- 'Tarjetas Online'
names(pais)[32] <- 'Tarjetas Offline'
#Codigos COICOP
names(pais)[33] <- 'Alimentacion y Bebidas'
names(pais)[34] <- 'Alcohol y Tabaco'
names(pais)[35] <- 'Textil y Calzado'
names(pais)[36] <- 'Energia (Agua-Luz-Gas..)'
names(pais)[37] <- 'Salud'
names(pais)[38] <- 'Transporte'
names(pais)[39] <- 'Comunicaciones'
names(pais)[40] <- 'Cultura-Ocio'
names(pais)[41] <- 'Educacion'
names(pais)[42] <- 'Comunicaciones'
names(pais)[43] <- 'Restaurantes-Hoteles'
names(pais)[44] <- 'Otros bienes/Servicos'

names(pais)[45] <- 'Bienes Primera Necesidad'
names(pais)[46] <- 'Bienes Lujo'


pais <- select(pais,1, 45, 46)
pais



pais$Date <- as.Date(pais$Date , origin = "1899-12-30")

fechas <- select(pais, Date)
fechas$variable <- 'Date'
fechas$value <- -2

pais <-  pais %>% reshape2::melt(id.vars='Date') %>%  na.omit()

pais$Value <- scales::percent(pais$value, accuracy = 0.01 )
pais$Value <- paste(pais$variable, pais$Value, sep = ": ")
fechas$newdate <- strptime(as.character(fechas$Date), "%Y-%m-%d")
fechas$newdate <- format(fechas$newdate, "%d-%m-%Y")
fechas$Value <- paste(fechas$variable, fechas$newdate, sep = ": ")
fechas <- select(fechas, -newdate)
pais <- rbind(pais, fechas)

colnames(pais)[1] <- 'Date'





gg_consumo_2 <-    pais %>% group_by(variable, Date) %>% 
  ggplot( aes(x=Date, y= value,  colour = variable)) + 
  annotation_raster(logo, ymin = ymin, ymax = ymax, xmin = xmin, xmax =xmax) +
  geom_line() +
  geom_point(show.legend = FALSE) + # ends with a point
  geom_richtext(aes(x = max(Date)-90, y = min(value)+.1, label = format(Date, '%b-%Y') ), text.colour ="#666666", show.legend=F, 
                fill = NA, label.color = NA, size=10) + #Date inside plot
  theme_BBVA_grey +
  
 
  scale_color_manual(values=c( "#2DCCCD","#9DC1F5","#FFFFFF"),
                     breaks= c( "Bienes Primera Necesidad", "Bienes Lujo")) + 
  scale_y_continuous(trans = modulus_trans(0.003), breaks =  c( -0.2,-0.1,0, 0.1,0.2,0.3),
                     labels = c('-20%','-10%','0%','10%','20%','30%' ),
                     limits = c(-0.2, 0.3),
                     sec.axis=sec_axis(~.*1,breaks= c(-0.2,-0.1,0, 0.1,0.2,0.3),labels = c ('-20%','-10%','0%','10%','20%','30%'))) +   
  scale_x_date(limits=c(as.Date("2021-08-01"), as.Date(date)), date_breaks = "2 months", date_labels = "%b-%y") +
  
  
  geom_vline(xintercept = as.Date("2021-11-22"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2021-11-21"), label="Omicron", y=0.25), colour="#666666", angle=90, vjust = 1.2,size=3)+
  
  geom_vline(xintercept = as.Date("2022-02-21"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2022-02-20"), label="Rusia-Ucrania", y=0.23), colour="#666666", angle=90, vjust = 1.2,size=3)+
  transition_reveal(Date)  +
  exit_shrink() +
  enter_fade() +
  ease_aes("circular-out") +
  guides(col = guide_legend(nrow = 1, reverse = F)) +
  labs(title =  "España: Consumo Total por tipo de Bienes (Necesidad y Lujo)",  
       caption= "Fuente: BBVA Research", 
       subtitle = "(Real, Acumulado 28D Crecimiento Interanual - Date:{format(frame_along, '%d-%m-%Y')})")  # Subtitle Date
gg_consumo_2 <- gg_consumo_2 + labs(x="", y = '% YoY Real (Deflactado por IPC COICOP)', color="") 

animate(gg_consumo_2, nframes = 250, duration = 7 ,fps = 35, width = 1200,
        height = 750, end_pause = 30)



anim_save("ConsumoBigData_Españareal_Tipo Bienes.gif")



############ GRAFICO BARRAS COICOP TOTAL 




############ GRQAFICO BARRAS COICOP TOTAL (ENGLISH)


#filename <- file("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")
#logo <- readPNG("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")





library(ggplot2)
library(gganimate)
library(tidyverse)
library(dplyr)


country<-"Spain"
file <- 'hoja sectorial Spain deflactado.xlsx'
coicops <- openxlsx::read.xlsx(file, sheet = country , startRow = 1)[,c(25,33,34,35,36,37,38,39,40,41,42,43,44)]



names(coicops) <- c('Total',
                    'Alimentos & Bebidas',
                    'Alcohol & Tabaco',
                    'Textil & Calzado',
                    'Consumo Energía',
                    'Mobiliario & Electrodomesticos',
                    'Salud',
                    'Transporte',
                    'Comunicaciones',
                    'Ocio',
                    'Educacion',
                    'Restaurante & Hoteles',
                    'Seguros & Ser. Financieros')


# Read the new consumption template file
# Get last available data
last_row <- tail(coicops, n =1)
last_data <- pivot_longer(last_row, cols=everything(), names_to = 'Coicop', values_to = "Value")
last_data <- last_data %>% mutate( ToHighlight = ifelse( Coicop == 'Total', "yes", "no" ) )
last_data <- last_data[order(last_data$Value),]


xmin <- tail(last_data$Coicop, n=1) # Coicop con valor mas positivo
xmax <- last_data$Coicop[11]
ymin <- min(last_data$Value) # Valor mas negativo de todos los coicops
ymax <- min(last_data$Value) + 0.05




# Barchart 
coicop_plot2 <- ggplot(last_data, aes(x=reorder(Coicop, Value), y=Value, fill=ToHighlight)) +
  geom_bar(stat='identity') +
  scale_fill_manual( values = c( "yes"="#072146", "no"="#19e5e6"), guide = "none")+
  coord_flip()+
  annotation_raster(logo, ymin = ymin, ymax = ymax, xmin = xmin, xmax =xmax) +
  
  scale_y_continuous(breaks =  c(-0.12,-0.08, -0.04,0, 0.04,0.08,0.12,0.16,0.18),
                     labels = c('-12%','-8%','-4%','0%','4%','8%','12%','16%','18%'),
                     limits = c(-0.12, 0.18)) +
  
  
  
  
  geom_text(
    aes(label = round(100*Value,1)),
    size = 3,
    hjust = 0.5- sign(last_data$Value)/1.25) +
  
  theme_BBVA_grey +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) + 
  
  labs(title =  "Consumo por Categorias COICOP (BBVA Research)",  
       # caption= "Fuente: BBVA Research",
       subtitle = "(Real deflactado por IPC Coicops, Cum 28D YoY )")  +
  
  transition_states(
    states = reorder(Coicop, -Value), transition_length = 2, state_length = 1)+
  shadow_mark() +
  enter_fade(alpha=0) 


animate(plot = coicop_plot2,
        width = 1200,
        height = 750,
        end_pause = 100,
        duration = 9,
        fps = 20)


anim_save("G2_COICOP_BArs.gif")

############## INVERSION  ESPAÑA TOTAL COOMPARATIVA CON INE TRIMESTRAL###########


############## INVERSION  ESPAÑA TOTAL COOMPARATIVA CON INE###########

#filename <- file("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")
#logo <- readPNG("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")

filename <- file("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")
logo <- readPNG("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")

xmax <- ymd(max_date)
xmin <- ymd(max_date)-60
ymin <- 0.3
ymax <- 0.4


library(ggplot2)
library(cowplot)
library(magick)

#change PC language to Spanish
#Sys.setlocale("LC_TIME", "es_ES.UTF-8")
Sys.setlocale("LC_TIME", "en_US.UTF-8")

# Load data - Investment by sector
spain <- openxlsx::read.xlsx('/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/Global Book Investment to be updated_FV_Current_Spain.xlsx',
                             sheet = 'Spain NOMINAL', detectDates=T, startRow = 3)


#column 1 - Date
#column 4 - SPAIN: Construction
#column 7 - SPAIN: Machinery
#column 8 - SPAIN: Other
#column 9 - SPAIN: Total Invest
#column 11 - COLOMBIA: Machinery
#column 13 - COLOMBIA: Construction
#column 15 - COLOMBIA: Other
#column 16 - COLOMBIA: Total Invest
#column 17 - TURKEY: Cons
#column 18 - TURKEY: Machinery
#column 19 - TURKEY: TOTAL
#column 20 - MEXICO: Cons
#column 21 - MEXICO: Machinery
#column 22 - MEXICO: Total
#column 26 - PERU: Construction 
#column 27 - PERU: Machinery
#column 28 - PERU: Total
spain <- select(spain, 1,6,16)
names(spain)[1] <- 'Date'
spain <- filter(spain, Date >= '2020-01-01', Date <= date)
names(spain)[2] <- 'Old'
names(spain)[3] <- 'New'

##lo transformo 
spain <-  spain %>% reshape2::melt(id.vars='Date') %>%  na.omit()



gg <-    spain %>% group_by(variable, Date) %>% 
  ggplot( aes(x=Date, y= value,  colour = variable)) +
  annotation_raster(logo, ymin = ymin, ymax = ymax, xmin = xmin, xmax =xmax) +

  geom_line() +
  geom_point(show.legend = FALSE) + # ends with a point
  geom_richtext(aes(x = max(Date)-90, y = min(value)+.1, label = format(Date, '%b-%Y') ), text.colour ="#666666", show.legend=F, 
                fill = NA, label.color = NA, size=11) + #Date inside plot
  theme_BBVA_grey + 
  scale_color_manual(values=c('#D8BE75',"#9DC1F5",'#2DCCCD','#0F4B7C',"#FFFFFF")) +
  scale_y_continuous(trans = modulus_trans(0.003), breaks =  c( -0.6, -0.25, 0,0.25, 0.95),
                     labels = c('-60%','-25%', '0%','25%', '95%'),
                     limits = c(-0.6, 0.95)) + 
  scale_x_date(limits=c(as.Date("2020-01-28"), as.Date(graph_date)),
               date_breaks = "3 months", date_labels = "%b-%y") +
  guides(col = guide_legend(nrow = 1, reverse = F)) +
  geom_segment(aes(x = as.Date("2020-01-28"), y = -40/100,xend =as.Date("2020-02-29") , yend = -40/100)) +
  geom_segment(aes(x = as.Date("2020-01-28"), y = -3.1/100,xend =as.Date("2020-03-31") , yend = -3.1/100)) +
  geom_segment(aes(x = as.Date("2020-04-01"), y = -22.9/100,xend =as.Date("2020-06-30") , yend = -22.9/100)) +
  geom_segment(aes(x = as.Date("2020-07-01"), y = -7.9/100,xend =as.Date("2020-09-30") , yend = -7.9/100)) +
  geom_segment(aes(x = as.Date("2020-10-01"), y = -8.35/100,xend =as.Date("2020-12-31") , yend = -8.35/100)) +
  geom_segment(aes(x = as.Date("2021-01-01"), y = -0.6/100,xend =as.Date("2021-03-31") , yend = -0.6/100)) +
  geom_segment(aes(x = as.Date("2021-04-01"), y = 22.6/100,xend =as.Date("2021-06-30") , yend = 22.6/100)) +
  geom_segment(aes(x = as.Date("2021-07-01"), y =  7.9/100,xend =as.Date("2021-09-30") , yend = 7.9/100)) +
  geom_segment(aes(x = as.Date("2021-10-01"), y =  17.4/100,xend =as.Date("2021-12-30") , yend = 17.4/100)) +
  geom_segment(aes(x = as.Date("2022-01-01"), y =  14.8/100,xend =as.Date("2022-03-31") , yend = 14.8/100)) +
  geom_segment(aes(x = as.Date("2022-04-01"), y =  19.0/100,xend =as.Date("2022-06-30") , yend = 19.0/100)) +
  #construccion
  geom_segment(aes(x = as.Date("2020-01-28"), y = 0.4/100,xend =as.Date("2020-03-31") , yend = 0.4/100)) +
  geom_segment(aes(x = as.Date("2020-04-01"), y = -19.6/100,xend =as.Date("2020-06-30") , yend = -19.6/100)) +
  geom_segment(aes(x = as.Date("2020-07-01"), y = -6.3/100,xend =as.Date("2020-09-30") , yend = -6.3/100)) +
  geom_segment(aes(x = as.Date("2020-10-01"), y = -9.4/100,xend =as.Date("2020-12-31") , yend = -9.4/100)) +
  geom_segment(aes(x = as.Date("2021-01-01"), y = -11.1/100,xend =as.Date("2021-03-31") , yend = -11.1/100)) +
  geom_segment(aes(x = as.Date("2021-04-01"), y = 14.7/100,xend =as.Date("2021-06-30") , yend = 14.7/100)) +
  geom_segment(aes(x = as.Date("2021-07-01"), y =  -0.4/100,xend =as.Date("2021-09-30") , yend = -0.4/100)) +
  geom_segment(aes(x = as.Date("2021-10-01"), y =  7.8/100,xend =as.Date("2021-12-30") , yend = 7.8/100)) +
  geom_segment(aes(x = as.Date("2022-01-01"), y =  12.8/100,xend =as.Date("2022-03-31") , yend = 12.8/100)) +
  geom_segment(aes(x = as.Date("2022-04-01"), y =  16.7/100,xend =as.Date("2022-06-30") , yend = 16.7/100)) +
  #vivienda
  geom_segment(aes(x = as.Date("2020-01-28"), y = 3.6/100,xend =as.Date("2020-03-31") , yend = 3.6/100)) +
  geom_segment(aes(x = as.Date("2020-04-01"), y = -18.3/100,xend =as.Date("2020-06-30") , yend = -18.3/100)) +
  geom_segment(aes(x = as.Date("2020-07-01"), y = -6.3/100,xend =as.Date("2020-09-30") , yend = -6.3/100)) +
  geom_segment(aes(x = as.Date("2020-10-01"), y = -10.7/100,xend =as.Date("2020-12-31") , yend = -10.7/100)) +
  geom_segment(aes(x = as.Date("2021-01-01"), y = -9.5/100,xend =as.Date("2021-03-31") , yend = -9.5/100)) +
  geom_segment(aes(x = as.Date("2021-04-01"), y = 15.8/100,xend =as.Date("2021-06-30") , yend = 15.8/100)) +
  geom_segment(aes(x = as.Date("2021-07-01"), y =  -8.0/100,xend =as.Date("2021-09-30") , yend = -8.0/100)) +
  geom_segment(aes(x = as.Date("2021-10-01"), y =  2.0/100,xend =as.Date("2021-12-30") , yend = 2.0/100)) +
  geom_segment(aes(x = as.Date("2022-01-01"), y =  7.9/100,xend =as.Date("2022-03-31") , yend = 7.9/100)) +
  geom_segment(aes(x = as.Date("2022-04-01"), y =  13.8/100,xend =as.Date("2022-06-30") , yend = 13.8/100)) +
  
  #otros cosntruction
  geom_segment(aes(x = as.Date("2020-01-28"), y = -3.5/100,xend =as.Date("2020-03-31") , yend = -3.5/100)) +
  geom_segment(aes(x = as.Date("2020-04-01"), y = -21.3/100,xend =as.Date("2020-06-30") , yend = -21.3/100)) +
  geom_segment(aes(x = as.Date("2020-07-01"), y = -6.2/100,xend =as.Date("2020-09-30") , yend = -6.2/100)) +
  geom_segment(aes(x = as.Date("2020-10-01"), y = -7.5/100,xend =as.Date("2020-12-31") , yend = -7.5/100)) +
  geom_segment(aes(x = as.Date("2021-01-01"), y = -13.2/100,xend =as.Date("2021-03-31") , yend = -13.2/100)) +
  geom_segment(aes(x = as.Date("2021-04-01"), y = 13.2/100,xend =as.Date("2021-06-30") , yend = 13.2/100)) +
  geom_segment(aes(x = as.Date("2021-07-01"), y =  8.7/100,xend =as.Date("2021-09-30") , yend = 8.7/100)) +
  geom_segment(aes(x = as.Date("2021-10-01"), y =  15.2/100,xend =as.Date("2021-12-30") , yend = 15.2/100)) +
  geom_segment(aes(x = as.Date("2022-01-01"), y =  19.3/100,xend =as.Date("2022-03-31") , yend = 19.3/100)) +
  geom_segment(aes(x = as.Date("2022-04-01"), y =  15.4/100,xend =as.Date("2022-06-30") , yend = 15.4/100)) +
  #Maquinaria
  geom_segment(aes(x = as.Date("2020-01-28"), y = -7.8/100,xend =as.Date("2020-03-31") , yend = -7.8/100)) +
  geom_segment(aes(x = as.Date("2020-04-01"), y = -33.4/100,xend =as.Date("2020-06-30") , yend = -33.4/100)) +
  geom_segment(aes(x = as.Date("2020-07-01"), y = -5.6/100,xend =as.Date("2020-09-30") , yend = -5.6/100)) +
  geom_segment(aes(x = as.Date("2020-10-01"), y = -2.4/100,xend =as.Date("2020-12-31") , yend = -2.4/100)) +
  geom_segment(aes(x = as.Date("2021-01-01"), y = 1.0/100,xend =as.Date("2021-03-31") , yend = 1.0/100)) +
  geom_segment(aes(x = as.Date("2021-04-01"), y = 41.3/100,xend =as.Date("2021-06-30") , yend = 41.3/100)) +
  geom_segment(aes(x = as.Date("2021-07-01"), y =  -0.6/100,xend =as.Date("2021-09-30") , yend = -0.6/100)) +
  geom_segment(aes(x = as.Date("2021-10-01"), y =  0.2/100,xend =as.Date("2021-12-30") , yend = 0.2/100)) +
  geom_segment(aes(x = as.Date("2022-01-01"), y =  13.1/100,xend =as.Date("2022-03-31") , yend = 13.1/100)) +
  geom_segment(aes(x = as.Date("2022-04-01"), y =  8.4/100,xend =as.Date("2022-06-30") , yend = 8.4/100)) +
  #Tranporte
  geom_segment(aes(x = as.Date("2020-01-28"), y = -7.4/100,xend =as.Date("2020-03-31") , yend = -7.4/100)) +
  geom_segment(aes(x = as.Date("2020-04-01"), y = -53.5/100,xend =as.Date("2020-06-30") , yend = -53.5/100)) +
  geom_segment(aes(x = as.Date("2020-07-01"), y = -18.8/100,xend =as.Date("2020-09-30") , yend = -18.8/100)) +
  geom_segment(aes(x = as.Date("2020-10-01"), y = -5.1/100,xend =as.Date("2020-12-31") , yend = -5.1/100)) +
  geom_segment(aes(x = as.Date("2021-01-01"), y = -15.2/100,xend =as.Date("2021-03-31") , yend = -15.2/100)) +
  geom_segment(aes(x = as.Date("2021-04-01"), y = 56.8/100,xend =as.Date("2021-06-30") , yend = 56.8/100)) +
  geom_segment(aes(x = as.Date("2021-07-01"), y =  -19.5/100,xend =as.Date("2021-09-30") , yend = -19.5/100)) +
  geom_segment(aes(x = as.Date("2021-10-01"), y =  -19.0/100,xend =as.Date("2021-12-30") , yend = -19.0/100)) +
  geom_segment(aes(x = as.Date("2022-01-01"), y =  -0.2/100,xend =as.Date("2022-03-31") , yend = -0.2/100)) +
  geom_segment(aes(x = as.Date("2022-04-01"), y =  23.6/100,xend =as.Date("2022-06-30") , yend = 23.46/100)) +
  #maquinas
  geom_segment(aes(x = as.Date("2020-01-28"), y = -8.1/100,xend =as.Date("2020-03-31") , yend = -8.1/100)) +
  geom_segment(aes(x = as.Date("2020-04-01"), y = -23.3/100,xend =as.Date("2020-06-30") , yend = -23.3/100)) +
  geom_segment(aes(x = as.Date("2020-07-01"), y = -9.8/100,xend =as.Date("2020-09-30") , yend = -9.8/100)) +
  geom_segment(aes(x = as.Date("2020-10-01"), y = -1.2/100,xend =as.Date("2020-12-31") , yend = -1.2/100)) +
  geom_segment(aes(x = as.Date("2021-01-01"), y = 9.1/100,xend =as.Date("2021-03-31") , yend = 9.1/100)) +
  geom_segment(aes(x = as.Date("2021-04-01"), y = 36.6/100,xend =as.Date("2021-06-30") , yend = 36.6/100)) +
  geom_segment(aes(x = as.Date("2021-07-01"), y =  6.9/100,xend =as.Date("2021-09-30") , yend = 6.9/100)) +
  geom_segment(aes(x = as.Date("2021-10-01"), y =  8.4/100,xend =as.Date("2021-12-30") , yend = 8.4/100)) +
  geom_segment(aes(x = as.Date("2022-01-01"), y =  18.3/100,xend =as.Date("2022-03-31") , yend = 18.3/100)) +
  geom_segment(aes(x = as.Date("2022-04-01"), y =  3.2/100,xend =as.Date("2022-06-30") , yend = 3.2/100)) +
  
  
  #otros
  geom_segment(aes(x = as.Date("2020-01-28"), y = 2.3/100,xend =as.Date("2020-03-31") , yend = 2.3/100)) +
  geom_segment(aes(x = as.Date("2020-04-01"), y = -5.5/100,xend =as.Date("2020-06-30") , yend = -5.5/100)) +
  geom_segment(aes(x = as.Date("2020-07-01"), y = -3.7/100,xend =as.Date("2020-09-30") , yend = -3.7/100)) +
  geom_segment(aes(x = as.Date("2020-10-01"), y = -2.6/100,xend =as.Date("2020-12-31") , yend = -2.6/100)) +
  geom_segment(aes(x = as.Date("2021-01-01"), y = -1.8/100,xend =as.Date("2021-03-31") , yend = -1.8/100)) +
  geom_segment(aes(x = as.Date("2021-04-01"), y = 9.0/100,xend =as.Date("2021-06-30") , yend = 9.0/100)) +
  geom_segment(aes(x = as.Date("2021-07-01"), y =  7.8/100,xend =as.Date("2021-09-30") , yend = 7.8/100)) +
  geom_segment(aes(x = as.Date("2021-10-01"), y =  8.8/100,xend =as.Date("2021-12-30") , yend = 8.8/100)) +
  geom_segment(aes(x = as.Date("2022-01-01"), y =  10.6/100,xend =as.Date("2022-03-31") , yend = 10.6/100)) +
  geom_segment(aes(x = as.Date("2022-04-01"), y =  5.7/100,xend =as.Date("2022-06-30") , yend = 5.7/100)) +
  
  geom_vline(xintercept = as.Date("2021-07-15"), colour="black",linetype = "dotted") +
  geom_vline(xintercept = as.Date("2021-08-17"), colour="black",linetype = "dotted") +
  geom_vline(xintercept = as.Date("2022-02-23"), colour="black",linetype = "dotted") +
  annotate("text", x = as.Date("2021-07-04"), y = 60/100, label = "(1)",size=3) +
  annotate("text", x = as.Date("2021-08-06"), y = 60/100, label = "(2)",size=3) +
  annotate("text", x = as.Date("2022-02-10"), y = 60/100, label = "(3)",size=3) +
  
  annotate("text", x = as.Date("2020-05-18"), y = 60/100, label = "(1) Aprobación NGEU",size=3) +
  annotate("text", x = as.Date("2020-05-30"), y = 52/100, label = "(2) Primer Tramo NGEU", size=3) +
  annotate("text", x = as.Date("2020-05-05"), y = 45/100, label = "(3) Rusia-Ucrania", size=3) +
  
  annotate("text", x = as.Date("2020-07-10"), y = -40/100, label = "Inversión Bruta Capital(%ia) (INE)", size=3) +
  geom_vline(xintercept = as.Date("2022-02-21"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2022-02-20"), label="Rusia-Ukraine", y=1.4), colour="#666666", angle=90, vjust = 1.2,size=3)+
  labs(title =  "España: Indice Maquinas Otros  Big Data (BBVA Research)",  
       #labs(title =  "España: Indicador Intangibles Big Data (BBVA Research)",  
       #caption= "Source: BBVA Research", 
       subtitle = "(Nominal, % Anual, cumulative 90D - Fecha:{format(frame_along, '%y-%m')})")+ 
  transition_reveal(Date)  +
  ease_aes("quintic-out") +
  exit_shrink() 

gg <- gg + labs(x="", y = '% IA nominal', color="")

animate(gg, nframes = 200, duration = 7 ,fps = 32, width = 1350,
        height = 750, end_pause = 30)


anim_save("investment_all_Spain_WITH INE.gif")

############## INVERSION  ESPAÑA Construccion COMPARATIVA CON INE###########


spain <- select(spain, 1,4,14)
names(spain)[1] <- 'Date'
spain <- filter(spain, Date >= '2020-01-01', Date <= date)
names(spain)[2] <- 'Construccion Old'
names(spain)[3] <- 'Construccion New'

Sys.Date <- '2018-01-01'

date <- '2023-03-30'
#fecha del eje X para ir sumando de 15 en 15
graph_date <- '2023-03-30'


##lo transformo 
spain <-  spain %>% reshape2::melt(id.vars='Date') %>%  na.omit()



gg <-    spain %>% group_by(variable, Date) %>% 
  ggplot( aes(x=Date, y= value,  colour = variable)) +
  annotation_raster(logo, ymin = ymin, ymax = ymax, xmin = xmin, xmax =xmax) +
  
  geom_line() +
  geom_point(show.legend = FALSE) + # ends with a point
  geom_richtext(aes(x = max(Date)-90, y = min(value)+.1, label = format(Date, '%b-%Y') ), text.colour ="#666666", show.legend=F, 
                fill = NA, label.color = NA, size=11) + #Date inside plot
  theme_BBVA_grey + 
  scale_color_manual(values=c('#D8BE75',"#9DC1F5",'#2DCCCD','#0F4B7C',"#FFFFFF")) +
  scale_y_continuous(trans = modulus_trans(0.003), breaks =  c( -0.4, -0.25, 0,0.25, 0.65),
                     labels = c('-40%','-25%', '0%','25%', '65%'),
                     limits = c(-0.4, 0.65)) + 
  scale_x_date(limits=c(as.Date("2020-01-28"), as.Date(graph_date)),
               date_breaks = "3 months", date_labels = "%b-%y") +
  guides(col = guide_legend(nrow = 1, reverse = F)) +
  #geom_segment(aes(x = as.Date("2020-01-28"), y = 0.4/100,xend =as.Date("2020-02-29") , yend = -40/100)) +
  geom_segment(aes(x = as.Date("2020-01-28"), y = 0.4/100,xend =as.Date("2020-03-31") , yend = 0.4/100)) +
  geom_segment(aes(x = as.Date("2020-04-01"), y = -19.6/100,xend =as.Date("2020-06-30") , yend = -19.6/100)) +
  geom_segment(aes(x = as.Date("2020-07-01"), y = -6.3/100,xend =as.Date("2020-09-30") , yend = -6.3/100)) +
  geom_segment(aes(x = as.Date("2020-10-01"), y = -9.4/100,xend =as.Date("2020-12-31") , yend = -9.4/100)) +
  geom_segment(aes(x = as.Date("2021-01-01"), y = -11.1/100,xend =as.Date("2021-03-31") , yend = -11.1/100)) +
  geom_segment(aes(x = as.Date("2021-04-01"), y = 14.7/100,xend =as.Date("2021-06-30") , yend = 14.7/100)) +
  geom_segment(aes(x = as.Date("2021-07-01"), y =  -0.4/100,xend =as.Date("2021-09-30") , yend = -0.4/100)) +
  geom_segment(aes(x = as.Date("2021-10-01"), y =  7.8/100,xend =as.Date("2021-12-30") , yend = 7.8/100)) +
  geom_segment(aes(x = as.Date("2022-01-01"), y =  12.8/100,xend =as.Date("2022-03-31") , yend = 12.8/100)) +
  geom_segment(aes(x = as.Date("2022-04-01"), y =  16.7/100,xend =as.Date("2022-06-30") , yend = 16.7/100)) +
  
  geom_vline(xintercept = as.Date("2021-07-15"), colour="black",linetype = "dotted") +
  geom_vline(xintercept = as.Date("2021-08-17"), colour="black",linetype = "dotted") +
  geom_vline(xintercept = as.Date("2022-02-23"), colour="black",linetype = "dotted") +
  annotate("text", x = as.Date("2021-07-04"), y = 60/100, label = "(1)",size=3) +
  annotate("text", x = as.Date("2021-08-06"), y = 60/100, label = "(2)",size=3) +
  annotate("text", x = as.Date("2022-02-10"), y = 60/100, label = "(3)",size=3) +
  
  annotate("text", x = as.Date("2020-05-18"), y = 60/100, label = "(1) Aprobación NGEU",size=3) +
  annotate("text", x = as.Date("2020-05-30"), y = 52/100, label = "(2) Primer Tramo NGEU", size=3) +
  annotate("text", x = as.Date("2020-05-05"), y = 45/100, label = "(3) Rusia-Ucrania", size=3) +
  
  annotate("text", x = as.Date("2020-07-10"), y = -40/100, label = "Inversión Bruta Capital(%ia) (INE)", size=3) +
  geom_vline(xintercept = as.Date("2022-02-21"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2022-02-20"), label="Rusia-Ukraine", y=1.4), colour="#666666", angle=90, vjust = 1.2,size=3)+
  labs(title =  "España: Indice de Inversión Construccion Big Data (BBVA Research)",  
       #labs(title =  "España: Indicador Inversión Big Data (BBVA Research)",  
       #caption= "Source: BBVA Research", 
       subtitle = "(Nominal, % Anual, cumulative 90D - Fecha:{format(frame_along, '%y-%m')})")+ 
  transition_reveal(Date)  +
  ease_aes("quintic-out") +
  exit_shrink() 

gg <- gg + labs(x="", y = '% IA nominal', color="")

animate(gg, nframes = 200, duration = 7 ,fps = 13, width = 1350,
        height = 750, end_pause = 30)


anim_save("investment_all_Spain_WITH INE.gif")

############## INVERSION  ESPAÑA Maquinaria y Equipo COMPARATIVA CON INE###########

#filename <- file("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")
#logo <- readPNG("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")

filename <- file("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")
logo <- readPNG("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")

xmax <- ymd(max_date)
xmin <- ymd(max_date)-60
ymin <- 0.3
ymax <- 0.4


library(ggplot2)
library(cowplot)
library(magick)

#change PC language to Spanish
#Sys.setlocale("LC_TIME", "es_ES.UTF-8")
Sys.setlocale("LC_TIME", "en_US.UTF-8")

# Load data - Investment by sector
spain <- openxlsx::read.xlsx('/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/Global Book Investment to be updated_FV.xlsx',
                             sheet = 'All countries NOMINAL', detectDates=T, startRow = 3)
#column 1 - Date
#column 4 - SPAIN: Construction
#column 7 - SPAIN: Machinery
#column 8 - SPAIN: Other
#column 9 - SPAIN: Total Invest
#column 11 - COLOMBIA: Machinery
#column 13 - COLOMBIA: Construction
#column 15 - COLOMBIA: Other
#column 16 - COLOMBIA: Total Invest
#column 17 - TURKEY: Cons
#column 18 - TURKEY: Machinery
#column 19 - TURKEY: TOTAL
#column 20 - MEXICO: Cons
#column 21 - MEXICO: Machinery
#column 22 - MEXICO: Total
#column 26 - PERU: Construction 
#column 27 - PERU: Machinery
#column 28 - PERU: Total
spain <- select(spain, 1,7)
names(spain)[1] <- 'Date'
spain <- filter(spain, Date >= '2020-01-01', Date <= date)
names(spain)[2] <- 'Maquinaria y Equipo'
##lo transformo 
spain <-  spain %>% reshape2::melt(id.vars='Date') %>%  na.omit()



gg <-    spain %>% group_by(variable, Date) %>% 
  ggplot( aes(x=Date, y= value,  colour = variable)) +
  annotation_raster(logo, ymin = ymin, ymax = ymax, xmin = xmin, xmax =xmax) +
  
  geom_line() +
  geom_point(show.legend = FALSE) + # ends with a point
  geom_richtext(aes(x = max(Date)-90, y = min(value)+.1, label = format(Date, '%b-%Y') ), text.colour ="#666666", show.legend=F, 
                fill = NA, label.color = NA, size=11) + #Date inside plot
  theme_BBVA_grey + 
  scale_color_manual(values=c('#D8BE75',"#9DC1F5",'#2DCCCD','#0F4B7C',"#FFFFFF")) +
  scale_y_continuous(trans = modulus_trans(0.003), breaks =  c( -0.4, -0.25, 0,0.25, 0.65),
                     labels = c('-40%','-25%', '0%','25%', '65%'),
                     limits = c(-0.4, 0.65)) + 
  scale_x_date(limits=c(as.Date("2020-01-28"), as.Date(graph_date)),
               date_breaks = "3 months", date_labels = "%b-%y") +
  guides(col = guide_legend(nrow = 1, reverse = F)) +
  #geom_segment(aes(x = as.Date("2020-01-28"), y = 0.4/100,xend =as.Date("2020-02-29") , yend = -40/100)) +
  geom_segment(aes(x = as.Date("2020-01-28"), y = -7.8/100,xend =as.Date("2020-03-31") , yend = -7.8/100)) +
  geom_segment(aes(x = as.Date("2020-04-01"), y = -33.4/100,xend =as.Date("2020-06-30") , yend = -33.4/100)) +
  geom_segment(aes(x = as.Date("2020-07-01"), y = -5.6/100,xend =as.Date("2020-09-30") , yend = -5.6/100)) +
  geom_segment(aes(x = as.Date("2020-10-01"), y = -2.4/100,xend =as.Date("2020-12-31") , yend = -2.4/100)) +
  geom_segment(aes(x = as.Date("2021-01-01"), y = 1.0/100,xend =as.Date("2021-03-31") , yend = 1.0/100)) +
  geom_segment(aes(x = as.Date("2021-04-01"), y = 41.3/100,xend =as.Date("2021-06-30") , yend = 41.3/100)) +
  geom_segment(aes(x = as.Date("2021-07-01"), y =  -0.6/100,xend =as.Date("2021-09-30") , yend = -0.6/100)) +
  geom_segment(aes(x = as.Date("2021-10-01"), y =  0.2/100,xend =as.Date("2021-12-30") , yend = 0.2/100)) +
  geom_segment(aes(x = as.Date("2022-01-01"), y =  13.1/100,xend =as.Date("2022-03-31") , yend = 13.1/100)) +
  geom_segment(aes(x = as.Date("2022-04-01"), y =  8.4/100,xend =as.Date("2022-06-30") , yend = 8.4/100)) +
  
  geom_vline(xintercept = as.Date("2021-07-15"), colour="black",linetype = "dotted") +
  geom_vline(xintercept = as.Date("2021-08-17"), colour="black",linetype = "dotted") +
  geom_vline(xintercept = as.Date("2022-02-23"), colour="black",linetype = "dotted") +
  annotate("text", x = as.Date("2021-07-04"), y = 60/100, label = "(1)",size=3) +
  annotate("text", x = as.Date("2021-08-06"), y = 60/100, label = "(2)",size=3) +
  annotate("text", x = as.Date("2022-02-10"), y = 60/100, label = "(3)",size=3) +
  
  annotate("text", x = as.Date("2020-05-18"), y = 60/100, label = "(1) Aprobación NGEU",size=3) +
  annotate("text", x = as.Date("2020-05-30"), y = 52/100, label = "(2) Primer Tramo NGEU", size=3) +
  annotate("text", x = as.Date("2020-05-05"), y = 45/100, label = "(3) Rusia-Ucrania", size=3) +
  
  annotate("text", x = as.Date("2020-07-10"), y = -40/100, label = "Inversión Bruta Capital(%ia) (INE)", size=3) +
  geom_vline(xintercept = as.Date("2022-02-21"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2022-02-20"), label="Rusia-Ukraine", y=1.4), colour="#666666", angle=90, vjust = 1.2,size=3)+
  labs(title =  "España: Indice de Inversión Maquinaria Big Data (BBVA Research)",  
       #labs(title =  "España: Indicador Inversión Big Data (BBVA Research)",  
       #caption= "Source: BBVA Research", 
       subtitle = "(Nominal, % Anual, cumulative 90D - Fecha:{format(frame_along, '%y-%m')})")+ 
  transition_reveal(Date)  +
  ease_aes("quintic-out") +
  exit_shrink() 

gg <- gg + labs(x="", y = '% IA nominal', color="")

animate(gg, nframes = 200, duration = 7 ,fps = 13, width = 1350,
        height = 750, end_pause = 30)


anim_save("investment_all_Spain_WITH INE.gif")


############## INVERSION  ESPAÑA Intangibles COMPARATIVA CON INE###########

#filename <- file("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")
#logo <- readPNG("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")

filename <- file("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")
logo <- readPNG("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")

xmax <- ymd(max_date)
xmin <- ymd(max_date)-60
ymin <- 0.3
ymax <- 0.4


library(ggplot2)
library(cowplot)
library(magick)

#change PC language to Spanish
#Sys.setlocale("LC_TIME", "es_ES.UTF-8")
Sys.setlocale("LC_TIME", "en_US.UTF-8")

# Load data - Investment by sector
spain <- openxlsx::read.xlsx('/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/Global Book Investment to be updated_FV.xlsx',
                             sheet = 'All countries NOMINAL', detectDates=T, startRow = 3)
#column 1 - Date
#column 4 - SPAIN: Construction
#column 7 - SPAIN: Machinery
#column 8 - SPAIN: Other
#column 9 - SPAIN: Total Invest
#column 11 - COLOMBIA: Machinery
#column 13 - COLOMBIA: Construction
#column 15 - COLOMBIA: Other
#column 16 - COLOMBIA: Total Invest
#column 17 - TURKEY: Cons
#column 18 - TURKEY: Machinery
#column 19 - TURKEY: TOTAL
#column 20 - MEXICO: Cons
#column 21 - MEXICO: Machinery
#column 22 - MEXICO: Total
#column 26 - PERU: Construction 
#column 27 - PERU: Machinery
#column 28 - PERU: Total
spain <- select(spain, 1,8)
names(spain)[1] <- 'Date'
spain <- filter(spain, Date >= '2020-01-01', Date <= date)
names(spain)[2] <- 'Intangibles'
##lo transformo 
spain <-  spain %>% reshape2::melt(id.vars='Date') %>%  na.omit()



gg <-    spain %>% group_by(variable, Date) %>% 
  ggplot( aes(x=Date, y= value,  colour = variable)) +
  annotation_raster(logo, ymin = ymin, ymax = ymax, xmin = xmin, xmax =xmax) +
  
  geom_line() +
  geom_point(show.legend = FALSE) + # ends with a point
  geom_richtext(aes(x = max(Date)-90, y = min(value)+.1, label = format(Date, '%b-%Y') ), text.colour ="#666666", show.legend=F, 
                fill = NA, label.color = NA, size=11) + #Date inside plot
  theme_BBVA_grey + 
  scale_color_manual(values=c('#D8BE75',"#9DC1F5",'#2DCCCD','#0F4B7C',"#FFFFFF")) +
  scale_y_continuous(trans = modulus_trans(0.003), breaks =  c( -0.4, -0.25, 0,0.25, 0.65),
                     labels = c('-40%','-25%', '0%','25%', '65%'),
                     limits = c(-0.4, 0.65)) + 
  scale_x_date(limits=c(as.Date("2020-01-28"), as.Date(graph_date)),
               date_breaks = "3 months", date_labels = "%b-%y") +
  guides(col = guide_legend(nrow = 1, reverse = F)) +
  #geom_segment(aes(x = as.Date("2020-01-28"), y = 0.4/100,xend =as.Date("2020-02-29") , yend = -40/100)) +
  geom_segment(aes(x = as.Date("2020-01-28"), y = 2.3/100,xend =as.Date("2020-03-31") , yend = 2.3/100)) +
  geom_segment(aes(x = as.Date("2020-04-01"), y = -5.5/100,xend =as.Date("2020-06-30") , yend = -5.5/100)) +
  geom_segment(aes(x = as.Date("2020-07-01"), y = -3.7/100,xend =as.Date("2020-09-30") , yend = -3.7/100)) +
  geom_segment(aes(x = as.Date("2020-10-01"), y = -2.6/100,xend =as.Date("2020-12-31") , yend = -2.6/100)) +
  geom_segment(aes(x = as.Date("2021-01-01"), y = -1.8/100,xend =as.Date("2021-03-31") , yend = -1.8/100)) +
  geom_segment(aes(x = as.Date("2021-04-01"), y = 9.0/100,xend =as.Date("2021-06-30") , yend = 9.0/100)) +
  geom_segment(aes(x = as.Date("2021-07-01"), y =  7.8/100,xend =as.Date("2021-09-30") , yend = 7.8/100)) +
  geom_segment(aes(x = as.Date("2021-10-01"), y =  8.8/100,xend =as.Date("2021-12-30") , yend = 8.8/100)) +
  geom_segment(aes(x = as.Date("2022-01-01"), y =  10.6/100,xend =as.Date("2022-03-31") , yend = 10.6/100)) +
  geom_segment(aes(x = as.Date("2022-04-01"), y =  5.7/100,xend =as.Date("2022-06-30") , yend = 5.7/100)) +
  
  geom_vline(xintercept = as.Date("2021-07-15"), colour="black",linetype = "dotted") +
  geom_vline(xintercept = as.Date("2021-08-17"), colour="black",linetype = "dotted") +
  geom_vline(xintercept = as.Date("2022-02-23"), colour="black",linetype = "dotted") +
  annotate("text", x = as.Date("2021-07-04"), y = 60/100, label = "(1)",size=3) +
  annotate("text", x = as.Date("2021-08-06"), y = 60/100, label = "(2)",size=3) +
  annotate("text", x = as.Date("2022-02-10"), y = 60/100, label = "(3)",size=3) +
  
  annotate("text", x = as.Date("2020-05-18"), y = 60/100, label = "(1) Aprobación NGEU",size=3) +
  annotate("text", x = as.Date("2020-05-30"), y = 52/100, label = "(2) Primer Tramo NGEU", size=3) +
  annotate("text", x = as.Date("2020-05-05"), y = 45/100, label = "(3) Rusia-Ucrania", size=3) +
  
  annotate("text", x = as.Date("2020-07-10"), y = -40/100, label = "Inversión Bruta Capital(%ia) (INE)", size=3) +
  geom_vline(xintercept = as.Date("2022-02-21"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2022-02-20"), label="Rusia-Ukraine", y=1.4), colour="#666666", angle=90, vjust = 1.2,size=3)+
  labs(title =  "España: Indice de Inversión Intangible  Big Data (BBVA Research)",  
       #labs(title =  "España: Indicador Inversión Big Data (BBVA Research)",  
       #caption= "Source: BBVA Research", 
       subtitle = "(Nominal, % Anual, cumulative 90D - Fecha:{format(frame_along, '%y-%m')})")+ 
  transition_reveal(Date)  +
  ease_aes("quintic-out") +
  exit_shrink() 

gg <- gg + labs(x="", y = '% IA nominal', color="")

animate(gg, nframes = 200, duration = 7 ,fps = 13, width = 1350,
        height = 750, end_pause = 30)


anim_save("investment_all_Spain_WITH INE.gif")


############## MACHINERY INVESTMENT SPAIN WITH INE QUARTERLY SADJ###########

#filename <- file("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")
#logo <- readPNG("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")

filename <- file("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")
logo <- readPNG("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")

xmax <- ymd(max_date)
xmin <- ymd(max_date)-120
ymin <- 0.5
ymax <- 0.62

Sys.Date <- '2018-01-01'

date <- '2023-03-30'
#fecha del eje X para ir sumando de 15 en 15
graph_date <- '2023-03-30'



library(ggplot2)
library(cowplot)
library(magick)

#change PC language to Spanish
#Sys.setlocale("LC_TIME", "es_ES.UTF-8")
Sys.setlocale("LC_TIME", "en_US.UTF-8")

# Load data - Investment by sector
spain <- openxlsx::read.xlsx('/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/Machinery Investment Sadj.xlsx',
                             sheet = 'Sheet1', detectDates=T, startRow = 3)
spain <- select(spain, 1,2)
names(spain)[1] <- 'Date'
spain <- filter(spain, Date >= '2018-01-01', Date <= date)
names(spain)[2] <- 'Machinery'
##lo transformo 
spain <-  spain %>% reshape2::melt(id.vars='Date') %>%  na.omit()



gg <-    spain %>% group_by(variable, Date) %>% 
  ggplot( aes(x=Date, y= value,  colour = variable)) +
  annotation_raster(logo, ymin = ymin, ymax = ymax, xmin = xmin, xmax =xmax) +
   geom_line() +
  #geom_smooth(se=FALSE,show.legend = NA) +
  geom_point(show.legend = FALSE) + # ends with a point
 # geom_richtext(aes(x = max(Date)-90, y = min(value)+.1, label = format(Date, '%b-%Y') ), text.colour ="#666666", show.legend=F, 
  #              fill = NA, label.color = NA, size=10) + #Date inside plot
  theme_BBVA_grey + 
  scale_color_manual(values=c('#0F4B7C',"#9DC1F5",'#2DCCCD','#0F4B7C',"#FFFFFF")) +
  scale_y_continuous(trans = modulus_trans(0.003), breaks =  c(-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4),
                     labels = c('-40%','-30%','-20%','-10%','0%','10%','20%','30%','40%' ),
                     limits = c(-0.4, 0.4), 
                     sec.axis=sec_axis(~.*1,breaks= c(-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4),labels = c('-40%','-30%','-20%','-10%','0%','10%','20%','30%','40%'  ))) +   
  
  scale_x_date(limits=c(as.Date("2018-01-28"), as.Date(graph_date)),
               date_breaks = "6 months", date_labels = "%b-%y") +
  guides(col = guide_legend(nrow = 1, reverse = F)) +
  
  geom_segment(aes(x = as.Date("2018-01-01"), y = -4.7/100,xend =as.Date("2018-03-31") , yend = -4.7/100)) +
  geom_segment(aes(x = as.Date("2018-04-01"), y = 9.5/100,xend =as.Date("2018-06-30") , yend = 9.5/100)) +
  geom_segment(aes(x = as.Date("2018-07-01"), y = -2.5/100,xend =as.Date("2018-09-30") , yend = -2.5/100)) +
  geom_segment(aes(x = as.Date("2018-10-01"), y = -2.0/100,xend =as.Date("2018-12-31") , yend = -2.0/100)) +
  

  geom_segment(aes(x = as.Date("2019-01-01"), y = 3.8/100,xend =as.Date("2019-03-31") , yend = 3.8/100)) +
  geom_segment(aes(x = as.Date("2019-04-01"), y = -2.2/100,xend =as.Date("2019-06-30") , yend = -2.2/100)) +
  geom_segment(aes(x = as.Date("2019-07-01"), y = 3.0/100,xend =as.Date("2019-09-30") , yend =  3.0/100)) +
  geom_segment(aes(x = as.Date("2019-10-01"), y = -3.9/100,xend =as.Date("2019-12-31") , yend = -3.9/100)) +
  
  geom_segment(aes(x = as.Date("2020-01-01"), y = -5.3/100,xend =as.Date("2020-03-31") , yend = -5.3/100)) +
  geom_segment(aes(x = as.Date("2020-04-01"), y = -29.4/100,xend =as.Date("2020-06-30") , yend = -29.4/100)) +
  geom_segment(aes(x = as.Date("2020-07-01"), y =  44.3/100,xend =as.Date("2020-09-30") , yend = 44.3/100)) +
  geom_segment(aes(x = as.Date("2020-10-01"), y = -0.3/100,xend =as.Date("2020-12-31") , yend = -0.3/100)) +
  
  geom_segment(aes(x = as.Date("2021-01-01"), y = -1.3/100,xend =as.Date("2021-03-31") , yend = -1.3/100)) +
  geom_segment(aes(x = as.Date("2021-04-01"), y = 0.0/100,xend =as.Date("2021-06-30") , yend = 0.0/100)) +
  geom_segment(aes(x = as.Date("2021-07-01"), y =  -1.4/100,xend =as.Date("2021-09-30") , yend = -1.4/100)) +
  geom_segment(aes(x = as.Date("2021-10-01"), y =  -0.7/100,xend =as.Date("2021-12-30") , yend = -0.7/100)) +
 
  geom_segment(aes(x = as.Date("2022-01-01"), y =  9.1/100,xend =as.Date("2022-03-31") , yend = 9.1/100)) +
  geom_segment(aes(x = as.Date("2022-04-01"), y =  -2.3/100,xend =as.Date("2022-06-30") , yend = -2.3/100)) +
  geom_segment(aes(x = as.Date("2022-07-01"), y =  -0.1/100,xend =as.Date("2022-09-30") , yend = -0.1/100)) +
  geom_segment(aes(x = as.Date("2022-10-01"), y =  -6.1/100,xend =as.Date("2022-12-30") , yend = -6.1/100)) +
  
 # geom_vline(xintercept = as.Date("2021-07-15"), colour="black",linetype = "dotted") +
  #geom_vline(xintercept = as.Date("2021-08-17"), colour="black",linetype = "dotted") +
  #geom_vline(xintercept = as.Date("2022-02-23"), colour="black",linetype = "dotted") +
  #annotate("text", x = as.Date("2021-07-04"), y = 60/100, label = "(1)",size=3) +
  #annotate("text", x = as.Date("2021-08-06"), y = 60/100, label = "(2)",size=3) +
  #annotate("text", x = as.Date("2022-02-10"), y = 60/100, label = "(3)",size=3) +
  
  #annotate("text", x = as.Date("2020-05-01"), y = 60/100, label = "(1) Approval NGEU",size=3) +
  #annotate("text", x = as.Date("2020-05-20"), y = 52/100, label = "(2) First Tranche NGEU", size=3) +
  #annotate("text", x = as.Date("2020-04-28"), y = 45/100, label = "(3) Russia-Ukraine", size=3) +
  
  annotate("text", x = as.Date("2020-09-10"), y = -50/100, label = "Gross Capital Formation (%qoq) (INE)", size=3) +
  #geom_vline(xintercept = as.Date("2022-02-21"), colour="black",linetype = "dotted") +
  #geom_text(aes(x=as.Date("2022-04-01"), label="Russia-Ukraine", y=1.4), colour="#666666", angle=90, vjust = 1.2,size=3)+
  labs(title =  "Spain: Big Data Machinery  Investment Real (BBVA Research)",  
       #labs(title =  "España: Indicador Inversión Big Data (BBVA Research)",  
       #caption= "Source: BBVA Research", 
       subtitle = "(real, % YoY, cumulative 28D - Fecha:{format(frame_along, '%y-%m')})")+ 
  transition_reveal(Date)  +
  ease_aes("quintic-out") +
  exit_shrink() 

gg <- gg + labs(x="", y = '% YoY real', color="")

animate(gg, nframes = 200, duration = 7 ,fps = 32, width = 1350,
        height = 750, end_pause = 30)


anim_save("investment Machinery trimestral real Prueba.gif")

############################
############## INTANGIBLE REAL INVESTMENT SPAIN WITH INE QUARTERLY SADJ###########

#filename <- file("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")
#logo <- readPNG("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")

#filename <- file("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")
#logo <- readPNG("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")

xmax <- ymd(max_date)
xmin <- ymd(max_date)-120
ymin <- 0.5
ymax <- 0.62

Sys.Date <- '2018-01-01'

date <- '2023-03-30'
#fecha del eje X para ir sumando de 15 en 15
graph_date <- '2023-03-30'



library(ggplot2)
library(cowplot)
library(magick)

#change PC language to Spanish
#Sys.setlocale("LC_TIME", "es_ES.UTF-8")
Sys.setlocale("LC_TIME", "en_US.UTF-8")

# Load data - Investment by sector
spain <- openxlsx::read.xlsx('/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/Intangible Investment Sadj.xlsx',
                             sheet = 'Sheet1', detectDates=T, startRow = 3)
spain <- select(spain, 1,2)
names(spain)[1] <- 'Date'
spain <- filter(spain, Date >= '2018-01-01', Date <= date)
names(spain)[2] <- 'Intangible'
##lo transformo 
spain <-  spain %>% reshape2::melt(id.vars='Date') %>%  na.omit()



gg <-    spain %>% group_by(variable, Date) %>% 
  ggplot( aes(x=Date, y= value,  colour = variable)) +
  annotation_raster(logo, ymin = ymin, ymax = ymax, xmin = xmin, xmax =xmax) +
  geom_line() +
  #geom_smooth(se=FALSE,show.legend = NA) +
  geom_point(show.legend = FALSE) + # ends with a point
  # geom_richtext(aes(x = max(Date)-90, y = min(value)+.1, label = format(Date, '%b-%Y') ), text.colour ="#666666", show.legend=F, 
  #              fill = NA, label.color = NA, size=10) + #Date inside plot
  theme_BBVA_grey + 
  scale_color_manual(values=c('#0F4B7C',"#9DC1F5",'#2DCCCD','#0F4B7C',"#FFFFFF")) +
  scale_y_continuous(trans = modulus_trans(0.003), breaks =  c(-0.1,-0.05,0,0.05,0.1),
                     labels = c('-10%','-5%','0%','5%','10%' ),
                     limits = c(-0.1, 0.1), 
                     sec.axis=sec_axis(~.*1,breaks= c(-0.1,-0.05,0,0.05,0.1),labels = c('-10%','-5%','0%','5%','10%'  ))) +   
  
  scale_x_date(limits=c(as.Date("2018-01-28"), as.Date(graph_date)),
               date_breaks = "6 months", date_labels = "%b-%y") +
  guides(col = guide_legend(nrow = 1, reverse = F)) +
  
   geom_segment(aes(x = as.Date("2018-01-01"), y = 2.2/100,xend =as.Date("2018-03-31") , yend = 2.2/100)) +
   geom_segment(aes(x = as.Date("2018-04-01"), y = -0.8/100,xend =as.Date("2018-06-30") , yend = -0.8/100)) +
   geom_segment(aes(x = as.Date("2018-07-01"), y = -0.06/100,xend =as.Date("2018-09-30") , yend = -0.06/100)) +
   geom_segment(aes(x = as.Date("2018-10-01"), y = -1.4621/100,xend =as.Date("2018-12-31") , yend = -1.4621/100)) +
  # 
  # 
   geom_segment(aes(x = as.Date("2019-01-01"), y = 0.1887/100,xend =as.Date("2019-03-31") , yend = 0.1887/100)) +
   geom_segment(aes(x = as.Date("2019-04-01"), y = 1.1885/100,xend =as.Date("2019-06-30") , yend = 1.1885/100)) +
   geom_segment(aes(x = as.Date("2019-07-01"), y = 0.9250/100,xend =as.Date("2019-09-30") , yend =  0.9250/100)) +
   geom_segment(aes(x = as.Date("2019-10-01"), y = 1.7375/100,xend =as.Date("2019-12-31") , yend = 1.7375/100)) +
  # 
   geom_segment(aes(x = as.Date("2020-01-01"), y = -0.0910/100,xend =as.Date("2020-03-31") , yend = -0.0910/100)) +
   geom_segment(aes(x = as.Date("2020-04-01"), y = -8.2482/100,xend =as.Date("2020-06-30") , yend = -8.2482/100)) +
   geom_segment(aes(x = as.Date("2020-07-01"), y =  2.8764/100,xend =as.Date("2020-09-30") , yend = 2.8764/100)) +
   geom_segment(aes(x = as.Date("2020-10-01"), y = 1.9867/100,xend =as.Date("2020-12-31") , yend = 1.9867/100)) +
  # 
   geom_segment(aes(x = as.Date("2021-01-01"), y = 2.1065/100,xend =as.Date("2021-03-31") , yend = 2.1065/100)) +
   geom_segment(aes(x = as.Date("2021-04-01"), y = 0.8975/100,xend =as.Date("2021-06-30") , yend = 0.8975/100)) +
   geom_segment(aes(x = as.Date("2021-07-01"), y =  1.5733/100,xend =as.Date("2021-09-30") , yend = 1.5733/100)) +
   geom_segment(aes(x = as.Date("2021-10-01"), y =  1.5832/100,xend =as.Date("2021-12-30") , yend = 1.5832/100)) +
  # 
   geom_segment(aes(x = as.Date("2022-01-01"), y =  2.9013/100,xend =as.Date("2022-03-31") , yend = 2.9013/100)) +
   geom_segment(aes(x = as.Date("2022-04-01"), y =  0.6156/100,xend =as.Date("2022-06-30") , yend = 0.6156/100)) +
   geom_segment(aes(x = as.Date("2022-07-01"), y =  1.4368/100,xend =as.Date("2022-09-30") , yend = 1.4368/100)) +
   geom_segment(aes(x = as.Date("2022-10-01"), y =  -3.3786/100,xend =as.Date("2022-12-30") , yend = -3.3786/100)) +
  # 
  # geom_vline(xintercept = as.Date("2021-07-15"), colour="black",linetype = "dotted") +
  #geom_vline(xintercept = as.Date("2021-08-17"), colour="black",linetype = "dotted") +
  #geom_vline(xintercept = as.Date("2022-02-23"), colour="black",linetype = "dotted") +
  #annotate("text", x = as.Date("2021-07-04"), y = 60/100, label = "(1)",size=3) +
  #annotate("text", x = as.Date("2021-08-06"), y = 60/100, label = "(2)",size=3) +
  #annotate("text", x = as.Date("2022-02-10"), y = 60/100, label = "(3)",size=3) +
  
  #annotate("text", x = as.Date("2020-05-01"), y = 60/100, label = "(1) Approval NGEU",size=3) +
  #annotate("text", x = as.Date("2020-05-20"), y = 52/100, label = "(2) First Tranche NGEU", size=3) +
  #annotate("text", x = as.Date("2020-04-28"), y = 45/100, label = "(3) Russia-Ukraine", size=3) +

annotate("text", x = as.Date("2020-09-10"), y = -50/100, label = "Gross Capital Formation (%qoq) (INE)", size=3) +
  #geom_vline(xintercept = as.Date("2022-02-21"), colour="black",linetype = "dotted") +
  #geom_text(aes(x=as.Date("2022-04-01"), label="Russia-Ukraine", y=1.4), colour="#666666", angle=90, vjust = 1.2,size=3)+
  labs(title =  "Spain: Big Data Intangible Investment Real (BBVA Research)",  
       #labs(title =  "España: Indicador Inversión Big Data (BBVA Research)",  
       #caption= "Source: BBVA Research", 
       subtitle = "( cumulative 91D,real, QoQ Seasonally Adj  - Fecha:{format(frame_along, '%y-%m')})")+ 
  transition_reveal(Date)  +
  ease_aes("quintic-out") +
  exit_shrink() 

gg <- gg + labs(x="", y = '% QoQ real Sadj', color="")

animate(gg, nframes = 200, duration = 7 ,fps = 32, width = 1350,
        height = 750, end_pause = 30)


anim_save("investment INTANGIBLE trimestral real Prueba.gif")

############################
############## INTANGIBLE NOMINAL INVESTMENT SPAIN WITH INE QUARTERLY SADJ###########

#filename <- file("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")
#logo <- readPNG("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")

#filename <- file("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")
#logo <- readPNG("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")

xmax <- ymd(max_date)
xmin <- ymd(max_date)-120
ymin <- 0.5
ymax <- 0.62

Sys.Date <- '2018-01-01'

date <- '2023-03-30'
#fecha del eje X para ir sumando de 15 en 15
graph_date <- '2023-03-30'



library(ggplot2)
library(cowplot)
library(magick)

#change PC language to Spanish
#Sys.setlocale("LC_TIME", "es_ES.UTF-8")
Sys.setlocale("LC_TIME", "en_US.UTF-8")

# Load data - Investment by sector
spain <- openxlsx::read.xlsx('/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/Intangible Investment Sadj Nominal.xlsx',
                             sheet = 'Sheet1', detectDates=T, startRow = 3)
spain <- select(spain, 1,2)
names(spain)[1] <- 'Date'
spain <- filter(spain, Date >= '2018-01-01', Date <= date)
names(spain)[2] <- 'Intangible Nominal'
##lo transformo 
spain <-  spain %>% reshape2::melt(id.vars='Date') %>%  na.omit()



gg <-    spain %>% group_by(variable, Date) %>% 
  ggplot( aes(x=Date, y= value,  colour = variable)) +
  annotation_raster(logo, ymin = ymin, ymax = ymax, xmin = xmin, xmax =xmax) +
  geom_line() +
  #geom_smooth(se=FALSE,show.legend = NA) +
  geom_point(show.legend = FALSE) + # ends with a point
  # geom_richtext(aes(x = max(Date)-90, y = min(value)+.1, label = format(Date, '%b-%Y') ), text.colour ="#666666", show.legend=F, 
  #              fill = NA, label.color = NA, size=10) + #Date inside plot
  theme_BBVA_grey + 
  scale_color_manual(values=c('#0F4B7C',"#9DC1F5",'#2DCCCD','#0F4B7C',"#FFFFFF")) +
  scale_y_continuous(trans = modulus_trans(0.003), breaks =  c(-0.1,-0.05,0,0.05,0.1),
                     labels = c('-10%','-5%','0%','5%','10%' ),
                     limits = c(-0.1, 0.1), 
                     sec.axis=sec_axis(~.*1,breaks= c(-0.1,-0.05,0,0.05,0.1),labels = c('-10%','-5%','0%','5%','10%'  ))) +   
  
  scale_x_date(limits=c(as.Date("2018-01-28"), as.Date(graph_date)),
               date_breaks = "6 months", date_labels = "%b-%y") +
  guides(col = guide_legend(nrow = 1, reverse = F)) +
  

  geom_segment(aes(x = as.Date("2018-01-01"), y = 1.8/100,xend =as.Date("2018-03-31") , yend = 1.8/100)) +
  geom_segment(aes(x = as.Date("2018-04-01"), y = -0.2/100,xend =as.Date("2018-06-30") , yend = -0.2/100)) +
  geom_segment(aes(x = as.Date("2018-07-01"), y = 0.0/100,xend =as.Date("2018-09-30") , yend = -0.0/100)) +
  geom_segment(aes(x = as.Date("2018-10-01"), y = -0.4/100,xend =as.Date("2018-12-31") , yend = -0.4/100)) +
  # 
  # 
  
  geom_segment(aes(x = as.Date("2019-01-01"), y = 0.8/100,xend =as.Date("2019-03-31") , yend = 0.8/100)) +
  geom_segment(aes(x = as.Date("2019-04-01"), y = 1.0/100,xend =as.Date("2019-06-30") , yend = 1.0/100)) +
  geom_segment(aes(x = as.Date("2019-07-01"), y = 0.5/100,xend =as.Date("2019-09-30") , yend =  0.5/100)) +
  geom_segment(aes(x = as.Date("2019-10-01"), y = 1.4/100,xend =as.Date("2019-12-31") , yend = 1.4/100)) +
  # 

  geom_segment(aes(x = as.Date("2020-01-01"), y = 0.0/100,xend =as.Date("2020-03-31") , yend = 0.0/100)) +
  geom_segment(aes(x = as.Date("2020-04-01"), y = -7.1/100,xend =as.Date("2020-06-30") , yend = -7.1/100)) +
  geom_segment(aes(x = as.Date("2020-07-01"), y =  2.1/100,xend =as.Date("2020-09-30") , yend = 2.1/100)) +
  geom_segment(aes(x = as.Date("2020-10-01"), y = 1.8/100,xend =as.Date("2020-12-31") , yend = 1.8/100)) +
  # 

  geom_segment(aes(x = as.Date("2021-01-01"), y = 2.5/100,xend =as.Date("2021-03-31") , yend = 2.5/100)) +
  geom_segment(aes(x = as.Date("2021-04-01"), y = 2.2/100,xend =as.Date("2021-06-30") , yend = 2.2/100)) +
  geom_segment(aes(x = as.Date("2021-07-01"), y =  1.8/100,xend =as.Date("2021-09-30") , yend = 1.8/100)) +
  geom_segment(aes(x = as.Date("2021-10-01"), y =  2.0/100,xend =as.Date("2021-12-30") , yend = 2.0/100)) +
  # 

  geom_segment(aes(x = as.Date("2022-01-01"), y =  2.5/100,xend =as.Date("2022-03-31") , yend = 2.5/100)) +
  geom_segment(aes(x = as.Date("2022-04-01"), y =  0.8/100,xend =as.Date("2022-06-30") , yend = 0.8/100)) +
  geom_segment(aes(x = as.Date("2022-07-01"), y =  1.7/100,xend =as.Date("2022-09-30") , yend = 1.7/100)) +
  geom_segment(aes(x = as.Date("2022-10-01"), y =  -1.0/100,xend =as.Date("2022-12-30") , yend = -1.0/100)) +
  # 
  # geom_vline(xintercept = as.Date("2021-07-15"), colour="black",linetype = "dotted") +
  #geom_vline(xintercept = as.Date("2021-08-17"), colour="black",linetype = "dotted") +
  #geom_vline(xintercept = as.Date("2022-02-23"), colour="black",linetype = "dotted") +
  #annotate("text", x = as.Date("2021-07-04"), y = 60/100, label = "(1)",size=3) +
  #annotate("text", x = as.Date("2021-08-06"), y = 60/100, label = "(2)",size=3) +
  #annotate("text", x = as.Date("2022-02-10"), y = 60/100, label = "(3)",size=3) +
  
  #annotate("text", x = as.Date("2020-05-01"), y = 60/100, label = "(1) Approval NGEU",size=3) +
  #annotate("text", x = as.Date("2020-05-20"), y = 52/100, label = "(2) First Tranche NGEU", size=3) +
  #annotate("text", x = as.Date("2020-04-28"), y = 45/100, label = "(3) Russia-Ukraine", size=3) +

annotate("text", x = as.Date("2020-09-10"), y = -50/100, label = "Gross Capital Formation (%qoq) (INE)", size=3) +
  #geom_vline(xintercept = as.Date("2022-02-21"), colour="black",linetype = "dotted") +
  #geom_text(aes(x=as.Date("2022-04-01"), label="Russia-Ukraine", y=1.4), colour="#666666", angle=90, vjust = 1.2,size=3)+
  labs(title =  "Spain: Big Data Intangible Investment NOMINAL (BBVA Research)",  
       #labs(title =  "España: Indicador Inversión Big Data (BBVA Research)",  
       #caption= "Source: BBVA Research", 
       subtitle = "( cumulative 91D,Nominal, QoQ Seasonally Adj  - Fecha:{format(frame_along, '%y-%m')})")+ 
  transition_reveal(Date)  +
  ease_aes("quintic-out") +
  exit_shrink() 

gg <- gg + labs(x="", y = '% QoQ nominal Sadj', color="")

animate(gg, nframes = 200, duration = 7 ,fps = 32, width = 1350,
        height = 750, end_pause = 30)


anim_save("investment INTANGIBLE trimestral NOMINAL Prueba.gif")



############## Consumption in SPAIN WITH INE QUARTERLY###########

#filename <- file("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")
#logo <- readPNG("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")

filename <- file("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")
logo <- readPNG("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")

xmax <- ymd(max_date)
xmin <- ymd(max_date)-120
ymin <- 0.5
ymax <- 0.62


library(ggplot2)
library(cowplot)
library(magick)

#change PC language to Spanish
#Sys.setlocale("LC_TIME", "es_ES.UTF-8")
Sys.setlocale("LC_TIME", "en_US.UTF-8")

# Load data - Investment by sector

country<-"Spain"
file <- 'hoja sectorial Spain deflactado_long.xlsx'

pais <- openxlsx::read.xlsx(file, sheet = country , startRow = 1)[,1:48]

names(pais)[1] <- 'Date'
names(pais)[2] <- 'Consumo Tarjetas'
names(pais)[3] <- 'Efectivo'
names(pais)[4] <- 'On-Line'
names(pais)[5] <- 'Físico'
names(pais)[6] <- 'Alimentación'
names(pais)[7] <- 'Ocio'
names(pais)[8] <- 'Restaurantes'
names(pais)[9] <- 'Hoteles'
names(pais)[10] <- 'Salud'
names(pais)[11] <- 'Bienes'
names(pais)[12] <- 'Servicios'
names(pais)[13] <- 'Consumo Transportes'
####### Datos de Inversion
names(pais)[14] <- 'Construccion Vivienda'
names(pais)[15] <- 'Construccion Otros '
names(pais)[16] <- 'Construccion '
names(pais)[17] <- 'Inversión Transporte'
names(pais)[18] <- 'Inversion B. Equipo'
names(pais)[19] <- 'Inversión Maquinaria y Eq'
names(pais)[20] <- 'Intangibles '
names(pais)[21] <- 'Inversion Total'
names(pais)[22] <- 'Inversion Digital'
#######3 Datos de Sector Exterior
names(pais)[23] <- 'Importaciones'
names(pais)[24] <- 'Exportaciones'
names(pais)[25] <- 'Consumo Total (%YoY)'
names(pais)[26] <- 'Tarjetas'
names(pais)[27] <- 'Domiciliaciones'
names(pais)[28] <- 'Transferencias'
names(pais)[29] <- 'Efectivo'
names(pais)[30] <- 'Alquileres'
names(pais)[31] <- 'Tarjetas Online'
names(pais)[32] <- 'Tarjetas Offline'
names(pais)[47] <- ' "Consumo Total (% Trimestral CVE)")'
names(pais)[48] <- 'Inversion Total (%ToT)'



origin <-"1899-12-30"


start_date<- "2021-09-01"
pais$Date <- as.Date(pais$Date , origin = origin)

#pais$Date <- as.Date(pais$Date , origin = "1899-12-30")

pais <- select(pais,1, 47)
pais
fechas <- select(pais, Date)
fechas$variable <- 'Date'
fechas$value <- -2

pais <-  pais %>% reshape2::melt(id.vars='Date') %>%  na.omit()

pais$Value <- scales::percent(pais$value, accuracy = 0.01 )
pais$Value <- paste(pais$variable, pais$Value, sep = ": ")
fechas$newdate <- strptime(as.character(fechas$Date), "%Y-%m-%d")
fechas$newdate <- format(fechas$newdate, "%d-%m-%Y")
fechas$Value <- paste(fechas$variable, fechas$newdate, sep = ": ")
fechas <- select(fechas, -newdate)
pais <- rbind(pais, fechas)

colnames(pais)[1] <- 'Date'

gg_demanda <- pais %>% group_by(variable,Date) %>% 
  ggplot( aes(x=Date, y= value,  colour = variable))+ 
  geom_line() +
  geom_point(show.legend = FALSE) +
  geom_richtext(aes(x = max(Date)-90, y = min(value)+.1, label = format(Date, '%b-%Y') ), text.colour ="#666666", show.legend=F, 
                fill = NA, label.color = NA, size=10) + #Date inside plot
  theme_BBVA_grey +
  annotation_raster(logo, ymin = ymin, ymax = ymax, xmin = xmin, xmax =xmax) +
  scale_color_manual(values=c( "#0F4B7C","#2DCCCD","#9DC1F5"),
                     # scale_color_manual(values=c( "#0F4B7C","#2DCCCD","#9DC1F5","#D8BE75"),
                     breaks= c(  "Consumo Total (% Trimestral CVE)")) +
  scale_y_continuous(trans = modulus_trans(0.003), breaks =  c( -0.25,-0.20,-0.15,-0.1,-0.05,0.0,0.05,0.1,0.15,0.2,0.25),
                     labels = c('-25%','-20%','-15%','-10%','-5%','0%','5%','10%','15%','20%','25%'),
                     limits = c(-0.25, 0.25),
                     sec.axis=sec_axis(~.*1,breaks= c(-0.25,-0.20,-0.15,-0.1,-0.05,0.0,0.05,0.1,0.15,0.2,0.25),labels = c('-25%','-20%','-15%','-10%','-5%','0%','5%','10%','15%','20%','25%'))) + 
  
  
  scale_x_date(limits=c(as.Date("2020-10-01"), as.Date(date)), date_breaks = "3 months", date_labels = "%b-%y") +
  
  #geom_vline(xintercept = as.Date("2020-03-15"), colour="black",linetype = "dotted") +
  #geom_text(aes(x=as.Date("2020-03-13"), label="Covid in EU", y=1), colour="#666666", angle=90, vjust = 1.2, size=3)+
  
#  geom_vline(xintercept = as.Date("2021-03-02"), colour="black",linetype = "dotted") +
 # geom_text(aes(x=as.Date("2021-03-01"), label="Delta Variant in EU", y=05), colour="#666666", angle=90, vjust =0., size=3)+
  
  #geom_vline(xintercept = as.Date("2021-11-22"), colour="black",linetype = "dotted") +
  #geom_text(aes(x=as.Date("2021-11-21"), label="Omicron", y=0.05), colour="#666666", angle=90, vjust = 1.2,size=3)+
  
  #geom_vline(xintercept = as.Date("2022-02-21"), colour="black",linetype = "dotted") +
  #geom_text(aes(x=as.Date("2022-02-20"), label="Rusia-Ucrania", y=0.05), colour="#666666", angle=90, vjust = 1.2,size=3)+
  transition_reveal(Date)  +
  #exit_shrink() +
  #enter_fade() +
  #ease_aes("circular-out") +
  guides(col = guide_legend(nrow = 1, reverse = F)) +
  #geom_segment(aes(x = as.Date("2020-01-28"), y = -50/100,xend =as.Date("2020-02-29") , yend = -50/100)) +
  
  #geom_point(aes(x = as.Date("2019-03-31"), y = 0.21/100,xend =as.Date("2019-06-31") , yend = 0.21/100)) +
  #geom_point(aes(x = as.Date("2019-06-30"), y = 0.21/100,xend =as.Date("2019-06-30") , yend = 0.21/100)) +
  #geom_point(aes(x = as.Date("2019-09-30"), y = 0.21/100,xend =as.Date("2019-09-30") , yend = 0.21/100)) +
  #geom_point(aes(x = as.Date("2019-12-31"), y = 0.21/100,xend =as.Date("2019-12-31") , yend = 0.21/100)) +
  

  #geom_segment(aes(x = as.Date("2019-03-01"), y = -20/100,xend =as.Date("2019-05-30") , yend = -20/100),colour='#F7893B') +
  
  #  annotate("text", x = as.Date("2019-01-15"), y = -20/100, label = "Dato (INE)", size=2) +

#Previous


#OLD Revision

geom_segment(aes(x = as.Date("2019-04-01"), y = -0.87/100,xend =as.Date("2019-06-30") , yend = -0.87/100),colour='#F7893B') +
  geom_segment(aes(x = as.Date("2019-07-01"), y = +3.66/100,xend =as.Date("2019-09-30") , yend = +3.66/100),colour='#F7893B') +
  geom_segment(aes(x = as.Date("2019-10-01"), y = -0.6/100,xend =as.Date("2019-12-31") , yend = -0.6/100),colour='#F7893B') +
  geom_segment(aes(x = as.Date("2020-01-01"), y = -6.3/100,xend =as.Date("2020-03-31") , yend = -6.3/100),colour='#F7893B') +
  geom_segment(aes(x = as.Date("2020-04-01"), y = -21.2/100,xend =as.Date("2020-06-30") , yend = -21.2/100),colour='#F7893B') +
  geom_segment(aes(x = as.Date("2020-07-01"), y = +21.6/100,xend =as.Date("2020-09-30") , yend = +21.6/100),colour='#F7893B') +
  geom_segment(aes(x = as.Date("2020-10-01"), y = -0.2/100,xend =as.Date("2020-12-31") , yend = -0.2/100),colour='#F7893B') +
  
  geom_segment(aes(x = as.Date("2021-01-01"), y = -0.14/100,xend =as.Date("2021-03-31") , yend = -0.14/100),colour='#F7893B') +
  geom_segment(aes(x = as.Date("2021-04-01"), y = 2.18/100,xend =as.Date("2021-06-30") , yend = 2.18/100),colour='#F7893B') +
  geom_segment(aes(x = as.Date("2021-07-01"), y =  2.12/100,xend =as.Date("2021-09-30") , yend = 2.12/100),colour='#F7893B') +
  geom_segment(aes(x = as.Date("2021-10-01"), y =  0.32/100,xend =as.Date("2021-12-30") , yend = 0.32/100),colour='#F7893B') +
  
  geom_segment(aes(x = as.Date("2022-01-01"), y =  -0.8/100,xend =as.Date("2022-03-31") , yend = -0.8/100),colour='#F7893B') +
  geom_segment(aes(x = as.Date("2022-04-01"), y =  1.7/100,xend =as.Date("2022-06-30") , yend = 1.7/100),colour='#F7893B') +
  geom_segment(aes(x = as.Date("2022-07-01"), y =  0.12/100,xend =as.Date("2022-09-30") , yend = 0.12/100),colour='#F7893B') +
  
  # NEW 
  geom_segment(aes(x = as.Date("2019-04-01"), y = -0.87/100,xend =as.Date("2019-06-30") , yend = -0.87/100),colour='#F7893B') +
  geom_segment(aes(x = as.Date("2019-07-01"), y = +3.66/100,xend =as.Date("2019-09-30") , yend = +3.66/100),colour='#F7893B') +
  geom_segment(aes(x = as.Date("2019-10-01"), y = -0.6/100,xend =as.Date("2019-12-31") , yend = -0.6/100),colour='#F7893B') +
  geom_segment(aes(x = as.Date("2020-01-01"), y = -6.3/100,xend =as.Date("2020-03-31") , yend = -6.3/100),colour='#F7893B') +
  geom_segment(aes(x = as.Date("2020-04-01"), y = -21.2/100,xend =as.Date("2020-06-30") , yend = -21.2/100),colour='#F7893B') +
  geom_segment(aes(x = as.Date("2020-07-01"), y = +21.6/100,xend =as.Date("2020-09-30") , yend = +21.6/100),colour='#F7893B') +
  geom_segment(aes(x = as.Date("2020-10-01"), y = -0.2/100,xend =as.Date("2020-12-31") , yend = -0.2/100),colour='#F7893B') +
  
  geom_segment(aes(x = as.Date("2021-01-01"), y = -0.14/100,xend =as.Date("2021-03-31") , yend = -0.14/100),colour='#F7893B') +
  geom_segment(aes(x = as.Date("2021-04-01"), y = 2.18/100,xend =as.Date("2021-06-30") , yend = 2.18/100),colour='#F7893B') +
  geom_segment(aes(x = as.Date("2021-07-01"), y =  2.12/100,xend =as.Date("2021-09-30") , yend = 2.12/100),colour='#F7893B') +
  geom_segment(aes(x = as.Date("2021-10-01"), y =  0.32/100,xend =as.Date("2021-12-30") , yend = 0.32/100),colour='#F7893B') +
  
  geom_segment(aes(x = as.Date("2022-01-01"), y =  -0.8/100,xend =as.Date("2022-03-31") , yend = -0.8/100),colour='#F7893B') +
  geom_segment(aes(x = as.Date("2022-04-01"), y =  1.7/100,xend =as.Date("2022-06-30") , yend = 1.7/100),colour='#F7893B') +
  geom_segment(aes(x = as.Date("2022-07-01"), y =  0.12/100,xend =as.Date("2022-09-30") , yend = 0.12/100),colour='#F7893B') +
  
  
  
  labs(title =  "España: Indicador BigData Consumo vs INE (BBVA Research)",  
       caption= "Fuente: BBVA Research", 
       subtitle = "(Real deflactado por IPC, Acum 90D % Var. Trimestral CVE)")  # Subtitle Date
gg_demanda <- gg_demanda + labs(x="", y = '% TrimestralReal (deflactado por IPC) & Sadj', positiony="left",color="") 

animate(gg_demanda, nframes = 200, duration = 6 ,fps = 32, width = 1200,
        height = 750, end_pause = 30)

anim_save("Consumo Prueba INE .gif")


############### Consumption BBVA vs INE in English############# 
############## Consumption in SPAIN WITH INE QUARTERLY###########

#filename <- file("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")
#logo <- readPNG("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")

filename <- file("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")
logo <- readPNG("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")

xmax <- ymd(max_date)
xmin <- ymd(max_date)-120
ymin <- 0.16
ymax <- 0.20


library(ggplot2)
library(cowplot)
library(magick)

#change PC language to Spanish
#Sys.setlocale("LC_TIME", "es_ES.UTF-8")
Sys.setlocale("LC_TIME", "en_US.UTF-8")

# Load data - Investment by sector

country<-"Spain"
file <- 'hoja sectorial Spain deflactado_long.xlsx'

pais <- openxlsx::read.xlsx(file, sheet = country , startRow = 1)[,1:48]

names(pais)[1] <- 'Date'
names(pais)[2] <- 'Consumo Tarjetas'
names(pais)[3] <- 'Efectivo'
names(pais)[4] <- 'On-Line'
names(pais)[5] <- 'Físico'
names(pais)[6] <- 'Alimentación'
names(pais)[7] <- 'Ocio'
names(pais)[8] <- 'Restaurantes'
names(pais)[9] <- 'Hoteles'
names(pais)[10] <- 'Salud'
names(pais)[11] <- 'Bienes'
names(pais)[12] <- 'Servicios'
names(pais)[13] <- 'Consumo Transportes'
####### Datos de Inversion
names(pais)[14] <- 'Construccion Vivienda'
names(pais)[15] <- 'Construccion Otros '
names(pais)[16] <- 'Construccion '
names(pais)[17] <- 'Inversión Transporte'
names(pais)[18] <- 'Inversion B. Equipo'
names(pais)[19] <- 'Inversión Maquinaria y Eq'
names(pais)[20] <- 'Intangibles '
names(pais)[21] <- 'Inversion Total'
names(pais)[22] <- 'Inversion Digital'
#######3 Datos de Sector Exterior
names(pais)[23] <- 'Importaciones'
names(pais)[24] <- 'Exportaciones'
names(pais)[25] <- 'Consumo Total (%YoY)'
names(pais)[26] <- 'Tarjetas'
names(pais)[27] <- 'Domiciliaciones'
names(pais)[28] <- 'Transferencias'
names(pais)[29] <- 'Efectivo'
names(pais)[30] <- 'Alquileres'
names(pais)[31] <- 'Tarjetas Online'
names(pais)[32] <- 'Tarjetas Offline'
names(pais)[47] <- 'Total Consumption (% QoQ Sadj)'
names(pais)[48] <- 'Inversion Total (%ToT)'



origin <-"1899-12-30"


start_date<- "2021-09-01"
pais$Date <- as.Date(pais$Date , origin = origin)

#pais$Date <- as.Date(pais$Date , origin = "1899-12-30")

pais <- select(pais,1, 47)
pais
fechas <- select(pais, Date)
fechas$variable <- 'Date'
fechas$value <- -2

pais <-  pais %>% reshape2::melt(id.vars='Date') %>%  na.omit()

pais$Value <- scales::percent(pais$value, accuracy = 0.01 )
pais$Value <- paste(pais$variable, pais$Value, sep = ": ")
fechas$newdate <- strptime(as.character(fechas$Date), "%Y-%m-%d")
fechas$newdate <- format(fechas$newdate, "%d-%m-%Y")
fechas$Value <- paste(fechas$variable, fechas$newdate, sep = ": ")
fechas <- select(fechas, -newdate)
pais <- rbind(pais, fechas)

colnames(pais)[1] <- 'Date'

gg_demanda <- pais %>% group_by(variable,Date) %>% 
  ggplot( aes(x=Date, y= value,  colour = variable))+ 
  geom_line() +
  geom_point(show.legend = FALSE) +
  geom_richtext(aes(x = max(Date)-90, y = min(value)+.1, label = format(Date, '%b-%Y') ), text.colour ="#666666", show.legend=F, 
                fill = NA, label.color = NA, size=10) + #Date inside plot
  theme_BBVA_grey +
  annotation_raster(logo, ymin = ymin, ymax = ymax, xmin = xmin, xmax =xmax) +
  scale_color_manual(values=c( "#0F4B7C","#2DCCCD","#9DC1F5"),
                     # scale_color_manual(values=c( "#0F4B7C","#2DCCCD","#9DC1F5","#D8BE75"),
                     breaks= c(  "Total Consumption (% QoQ Sadj)")) +
  scale_y_continuous(trans = modulus_trans(0.003), breaks =  c( -0.25,-0.20,-0.15,-0.1,-0.05,0.0,0.05,0.1,0.15,0.2,0.25),
                     labels = c('-25%','-20%','-15%','-10%','-5%','0%','5%','10%','15%','20%','25%'),
                     limits = c(-0.25, 0.25),
                     sec.axis=sec_axis(~.*1,breaks= c(-0.25,-0.20,-0.15,-0.1,-0.05,0.0,0.05,0.1,0.15,0.2,0.25),labels = c('-25%','-20%','-15%','-10%','-5%','0%','5%','10%','15%','20%','25%'))) + 
  
  
  scale_x_date(limits=c(as.Date("2019-01-01"), as.Date(date)), date_breaks = "3 months", date_labels = "%b-%y") +
  
  #geom_vline(xintercept = as.Date("2020-03-15"), colour="black",linetype = "dotted") +
  #geom_text(aes(x=as.Date("2020-03-13"), label="Covid in EU", y=1), colour="#666666", angle=90, vjust = 1.2, size=3)+
  
  #  geom_vline(xintercept = as.Date("2021-03-02"), colour="black",linetype = "dotted") +
  # geom_text(aes(x=as.Date("2021-03-01"), label="Delta Variant in EU", y=05), colour="#666666", angle=90, vjust =0., size=3)+
  
  #geom_vline(xintercept = as.Date("2021-11-22"), colour="black",linetype = "dotted") +
  #geom_text(aes(x=as.Date("2021-11-21"), label="Omicron", y=0.05), colour="#666666", angle=90, vjust = 1.2,size=3)+
  
  #geom_vline(xintercept = as.Date("2022-02-21"), colour="black",linetype = "dotted") +
#geom_text(aes(x=as.Date("2022-02-20"), label="Rusia-Ucrania", y=0.05), colour="#666666", angle=90, vjust = 1.2,size=3)+
transition_reveal(Date)  +
  #exit_shrink() +
  #enter_fade() +
  #ease_aes("circular-out") +
  guides(col = guide_legend(nrow = 1, reverse = F)) +
  #geom_segment(aes(x = as.Date("2020-01-28"), y = -50/100,xend =as.Date("2020-02-29") , yend = -50/100)) +
  
  #geom_point(aes(x = as.Date("2019-03-31"), y = 0.21/100,xend =as.Date("2019-06-31") , yend = 0.21/100)) +
  #geom_point(aes(x = as.Date("2019-06-30"), y = 0.21/100,xend =as.Date("2019-06-30") , yend = 0.21/100)) +
  #geom_point(aes(x = as.Date("2019-09-30"), y = 0.21/100,xend =as.Date("2019-09-30") , yend = 0.21/100)) +
  #geom_point(aes(x = as.Date("2019-12-31"), y = 0.21/100,xend =as.Date("2019-12-31") , yend = 0.21/100)) +
  
  
  #geom_segment(aes(x = as.Date("2019-03-01"), y = -20/100,xend =as.Date("2019-05-30") , yend = -20/100),colour='#F7893B') +
  
  #  annotate("text", x = as.Date("2019-01-15"), y = -20/100, label = "Dato (INE)", size=2) +

#Previous


#New Revision

geom_segment(aes(x = as.Date("2019-04-01"), y = -0.87/100,xend =as.Date("2019-06-30") , yend = -0.87/100),colour='#F7893B') +
  geom_segment(aes(x = as.Date("2019-07-01"), y = +3.66/100,xend =as.Date("2019-09-30") , yend = +3.66/100),colour='#F7893B') +
  geom_segment(aes(x = as.Date("2019-10-01"), y = -0.6/100,xend =as.Date("2019-12-31") , yend = -0.6/100),colour='#F7893B') +
  geom_segment(aes(x = as.Date("2020-01-01"), y = -6.3/100,xend =as.Date("2020-03-31") , yend = -6.3/100),colour='#F7893B') +
  geom_segment(aes(x = as.Date("2020-04-01"), y = -21.2/100,xend =as.Date("2020-06-30") , yend = -21.2/100),colour='#F7893B') +
  geom_segment(aes(x = as.Date("2020-07-01"), y = +21.6/100,xend =as.Date("2020-09-30") , yend = +21.6/100),colour='#F7893B') +
  geom_segment(aes(x = as.Date("2020-10-01"), y = -0.2/100,xend =as.Date("2020-12-31") , yend = -0.2/100),colour='#F7893B') +
  
  geom_segment(aes(x = as.Date("2021-01-01"), y = -0.14/100,xend =as.Date("2021-03-31") , yend = -0.14/100),colour='#F7893B') +
  geom_segment(aes(x = as.Date("2021-04-01"), y = 2.18/100,xend =as.Date("2021-06-30") , yend = 2.18/100),colour='#F7893B') +
  geom_segment(aes(x = as.Date("2021-07-01"), y =  2.12/100,xend =as.Date("2021-09-30") , yend = 2.12/100),colour='#F7893B') +
  geom_segment(aes(x = as.Date("2021-10-01"), y =  0.32/100,xend =as.Date("2021-12-30") , yend = 0.32/100),colour='#F7893B') +
  
  geom_segment(aes(x = as.Date("2022-01-01"), y =  -0.8/100,xend =as.Date("2022-03-31") , yend = -0.8/100),colour='#F7893B') +
  geom_segment(aes(x = as.Date("2022-04-01"), y =  1.7/100,xend =as.Date("2022-06-30") , yend = 1.7/100),colour='#F7893B') +
  geom_segment(aes(x = as.Date("2022-07-01"), y =  0.12/100,xend =as.Date("2022-09-30") , yend = 0.12/100),colour='#F7893B') +
  
  #Old Data   
  
  
 # geom_segment(aes(x = as.Date("2022-01-01"), y =  -1.2/100,xend =as.Date("2022-03-31") , yend = -1.2/100),colour='#D8BE75') +
  #geom_segment(aes(x = as.Date("2022-04-01"), y =  1.2/100,xend =as.Date("2022-06-30") , yend = 1.2/100),colour='#D8BE75') +
  #geom_segment(aes(x = as.Date("2022-07-01"), y =  1.1/100,xend =as.Date("2022-09-30") , yend = 1.1/100),colour='#D8BE75') +
  
  
  labs(title =  "Spain: BBVA BigData Consumption vs Official (BBVA Research)",  
       caption= "Source: BBVA Research", 
       subtitle = "(Real deflated por CPI, Cum 90D % QoQ  Sadj)")  # Subtitle Date
gg_demanda <- gg_demanda + labs(x="", y = '% Quarterly Real  (deflated by CPI) & Sadj', positiony="left",color="") 

animate(gg_demanda, nframes = 200, duration = 6 ,fps = 32, width = 1200,
        height = 750, end_pause = 30)

anim_save("Consumo Prueba INE English.gif")






############## INVESTMENT SPAIN WITH INE QUARTERLY###########

#filename <- file("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")
#logo <- readPNG("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")

filename <- file("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")
logo <- readPNG("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")

xmax <- ymd(max_date)
xmin <- ymd(max_date)-120
ymin <- 0.5
ymax <- 0.62


library(ggplot2)
library(cowplot)
library(magick)

#change PC language to Spanish
#Sys.setlocale("LC_TIME", "es_ES.UTF-8")
Sys.setlocale("LC_TIME", "en_US.UTF-8")

# Load data - Investment by sector

country<-"Spain"
file <- 'hoja sectorial Spain deflactado_long.xlsx'

pais <- openxlsx::read.xlsx(file, sheet = country , startRow = 1)[,1:48]

names(pais)[1] <- 'Date'
names(pais)[2] <- 'Consumo Tarjetas'
names(pais)[3] <- 'Efectivo'
names(pais)[4] <- 'On-Line'
names(pais)[5] <- 'Físico'
names(pais)[6] <- 'Alimentación'
names(pais)[7] <- 'Ocio'
names(pais)[8] <- 'Restaurantes'
names(pais)[9] <- 'Hoteles'
names(pais)[10] <- 'Salud'
names(pais)[11] <- 'Bienes'
names(pais)[12] <- 'Servicios'
names(pais)[13] <- 'Consumo Transportes'
####### Datos de Inversion
names(pais)[14] <- 'Construccion Vivienda'
names(pais)[15] <- 'Construccion Otros '
names(pais)[16] <- 'Construccion '
names(pais)[17] <- 'Inversión Transporte'
names(pais)[18] <- 'Inversion B. Equipo'
names(pais)[19] <- 'Inversión Maquinaria y Eq'
names(pais)[20] <- 'Intangibles '
names(pais)[21] <- 'Inversion Total'
names(pais)[22] <- 'Inversion Digital'
#######3 Datos de Sector Exterior
names(pais)[23] <- 'Importaciones'
names(pais)[24] <- 'Exportaciones'
names(pais)[25] <- 'Consumo Total (%YoY)'
names(pais)[26] <- 'Tarjetas'
names(pais)[27] <- 'Domiciliaciones'
names(pais)[28] <- 'Transferencias'
names(pais)[29] <- 'Efectivo'
names(pais)[30] <- 'Alquileres'
names(pais)[31] <- 'Tarjetas Online'
names(pais)[32] <- 'Tarjetas Offline'
names(pais)[47] <- 'Consumo Total (%ToT)'
names(pais)[48] <- 'Inversion Total (%ToT)'



origin <-"1899-12-30"

start_date<- "2021-09-01"
pais$Date <- as.Date(pais$Date , origin = origin)

#pais$Date <- as.Date(pais$Date , origin = "1899-12-30")

pais <- select(pais,1, 48)
pais
fechas <- select(pais, Date)
fechas$variable <- 'Date'
fechas$value <- -2

pais <-  pais %>% reshape2::melt(id.vars='Date') %>%  na.omit()

pais$Value <- scales::percent(pais$value, accuracy = 0.01 )
pais$Value <- paste(pais$variable, pais$Value, sep = ": ")
fechas$newdate <- strptime(as.character(fechas$Date), "%Y-%m-%d")
fechas$newdate <- format(fechas$newdate, "%d-%m-%Y")
fechas$Value <- paste(fechas$variable, fechas$newdate, sep = ": ")
fechas <- select(fechas, -newdate)
pais <- rbind(pais, fechas)

colnames(pais)[1] <- 'Date'

gg_demanda <- pais %>% group_by(variable,Date) %>% 
  ggplot( aes(x=Date, y= value,  colour = variable))+ 
  geom_line() +
  geom_point(show.legend = FALSE) +
  geom_richtext(aes(x = max(Date)-90, y = min(value)+.1, label = format(Date, '%b-%Y') ), text.colour ="#666666", show.legend=F, 
                fill = NA, label.color = NA, size=10) + #Date inside plot
  theme_BBVA_grey +
  annotation_raster(logo, ymin = ymin, ymax = ymax, xmin = xmin, xmax =xmax) +
  scale_color_manual(values=c( "#0F4B7C","#2DCCCD","#9DC1F5","#D8BE75"),
                     # scale_color_manual(values=c( "#0F4B7C","#2DCCCD","#9DC1F5","#D8BE75"),
                     breaks= c(  "Inversion Total (%ToT)")) +
  scale_y_continuous(trans = modulus_trans(0.003), breaks =  c( -0.22,-0.06,-0.04,-0.02,0,0.02,0.04,0.06,0.22),
                     labels = c('-22%', '-6%','-4%','-2%','0%','2%','4%','6%','22%'),
                     limits = c(-0.22, 0.22),
                     sec.axis=sec_axis(~.*1,breaks= c(-0.22,-0.06,-0.04,-0.02,0,0.02,0.04,0.06,0.22),labels = c('-22%', '-6%','-4%','-2%','0%','2%','4%','6%','22%'))) + 
  
  
  scale_x_date(limits=c(as.Date("2019-01-01"), as.Date(date)), date_breaks = "3 months", date_labels = "%b-%y") +
  
  geom_vline(xintercept = as.Date("2020-03-15"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2020-03-13"), label="Covid in EU", y=1), colour="#666666", angle=90, vjust = 1.2, size=3)+
  
  geom_vline(xintercept = as.Date("2021-03-02"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2021-03-01"), label="Delta Variant in EU", y=05), colour="#666666", angle=90, vjust =0., size=3)+
  
  geom_vline(xintercept = as.Date("2021-11-22"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2021-11-21"), label="Omicron", y=0.05), colour="#666666", angle=90, vjust = 1.2,size=3)+
  
  geom_vline(xintercept = as.Date("2022-02-21"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2022-02-20"), label="Rusia-Ucrania", y=0.05), colour="#666666", angle=90, vjust = 1.2,size=3)+
  transition_reveal(Date)  +
  #exit_shrink() +
  #enter_fade() +
  #ease_aes("circular-out") +
  guides(col = guide_legend(nrow = 1, reverse = F)) +
  #geom_segment(aes(x = as.Date("2020-01-28"), y = -50/100,xend =as.Date("2020-02-29") , yend = -50/100)) +
  geom_segment(aes(x = as.Date("2019-01-01"), y = 1.37/100,xend =as.Date("2019-03-31") , yend = 1.37/100)) +
  geom_segment(aes(x = as.Date("2019-04-01"), y = 0.75/100,xend =as.Date("2019-06-30") , yend = 0.75/100)) +
  geom_segment(aes(x = as.Date("2019-07-01"), y = -5.01/100,xend =as.Date("2019-09-30") , yend = -5.01/100)) +
  geom_segment(aes(x = as.Date("2019-10-01"), y = 6.07/100,xend =as.Date("2019-12-31") , yend = 6.07/100)) +
  geom_segment(aes(x = as.Date("2020-01-01"), y = -5.2/100,xend =as.Date("2020-03-31") , yend = -5.2/100)) +
  geom_segment(aes(x = as.Date("2020-04-01"), y = -20/100,xend =as.Date("2020-06-30") , yend = -20/100)) +
  geom_segment(aes(x = as.Date("2020-07-01"), y = +19.9/100,xend =as.Date("2020-09-30") , yend = +19.9/100)) +
  geom_segment(aes(x = as.Date("2020-10-01"), y = -1.3/100,xend =as.Date("2020-12-31") , yend = -1.3/100)) +
  geom_segment(aes(x = as.Date("2021-01-01"), y = -4.2/100,xend =as.Date("2021-03-31") , yend = -4.2/100)) +
  geom_segment(aes(x = as.Date("2021-04-01"), y = 5.8/100,xend =as.Date("2021-06-30") , yend = 5.8/100)) +
  geom_segment(aes(x = as.Date("2021-07-01"), y =  4.1/100,xend =as.Date("2021-09-30") , yend = 4.1/100)) +
  geom_segment(aes(x = as.Date("2021-10-01"), y =  6.1/100,xend =as.Date("2021-12-30") , yend = 6.1/100)) +
  geom_segment(aes(x = as.Date("2022-01-01"), y =  -5.0/100,xend =as.Date("2022-03-31") , yend = -5.0/100)) +
  geom_segment(aes(x = as.Date("2022-04-01"), y =  0.8/100,xend =as.Date("2022-06-30") , yend = 0.8/100)) +
  geom_segment(aes(x = as.Date("2022-07-01"), y =  1.4/100,xend =as.Date("2022-09-30") , yend = 1.4/100)) +
  
  
  labs(title =  "España: Indicador BigData Inversion Real va INE (BBVA Research)",  
       caption= "Fuente: BBVA Research", 
       subtitle = "(Real deflactado por Deflactor Mesualizado, Acum 90D % Var. Trimestral CVE)")  # Subtitle Date
gg_demanda <- gg_demanda + labs(x="", y = '% QoQ Real (deflactado por deflactor mensualizado )', positiony="left",color="") 

animate(gg_demanda, nframes = 200, duration = 3 ,fps = 32, width = 1200,
        height = 750, end_pause = 30)

anim_save("Demanda Interna_Consumo Total anual y trimestral .gif")






anim_save("investment_all_Spain_WITHIN_dec.gif")

############## INVESTMENT NOMINAL SPAIN WITH INE QUARTERLY###########

#filename <- file("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")
#logo <- readPNG("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")

filename <- file("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")
logo <- readPNG("/Users/alvaroortizvidal-abarca/Library/Mobile Documents/com~apple~CloudDocs/BBVA_R/logo_Research.png")

xmax <- ymd(max_date)
xmin <- ymd(max_date)-120
ymin <- 0.5
ymax <- 0.62


library(ggplot2)
library(cowplot)
library(magick)

#change PC language to Spanish
#Sys.setlocale("LC_TIME", "es_ES.UTF-8")
Sys.setlocale("LC_TIME", "en_US.UTF-8")

# Load data - Investment by sector

country<-"Spain"
file <- 'hoja sectorial Spain deflactado_long.xlsx'

pais <- openxlsx::read.xlsx(file, sheet = country , startRow = 1)[,1:50]

names(pais)[1] <- 'Date'
names(pais)[2] <- 'Consumo Tarjetas'
names(pais)[3] <- 'Efectivo'
names(pais)[4] <- 'On-Line'
names(pais)[5] <- 'Físico'
names(pais)[6] <- 'Alimentación'
names(pais)[7] <- 'Ocio'
names(pais)[8] <- 'Restaurantes'
names(pais)[9] <- 'Hoteles'
names(pais)[10] <- 'Salud'
names(pais)[11] <- 'Bienes'
names(pais)[12] <- 'Servicios'
names(pais)[13] <- 'Consumo Transportes'
####### Datos de Inversion
names(pais)[14] <- 'Construccion Vivienda'
names(pais)[15] <- 'Construccion Otros '
names(pais)[16] <- 'Construccion '
names(pais)[17] <- 'Inversión Transporte'
names(pais)[18] <- 'Inversion B. Equipo'
names(pais)[19] <- 'Inversión Maquinaria y Eq'
names(pais)[20] <- 'Intangibles '
names(pais)[21] <- 'Inversion Total'
names(pais)[22] <- 'Inversion Digital'
#######3 Datos de Sector Exterior
names(pais)[23] <- 'Importaciones'
names(pais)[24] <- 'Exportaciones'
names(pais)[25] <- 'Consumo Total (%YoY)'
names(pais)[26] <- 'Tarjetas'
names(pais)[27] <- 'Domiciliaciones'
names(pais)[28] <- 'Transferencias'
names(pais)[29] <- 'Efectivo'
names(pais)[30] <- 'Alquileres'
names(pais)[31] <- 'Tarjetas Online'
names(pais)[32] <- 'Tarjetas Offline'
names(pais)[47] <- 'Consumo Total (%ToT)'
names(pais)[48] <- 'Inversion Total (%ToT)'
names(pais)[49] <- 'Inversion Total Nom (%ToT)'
names(pais)[50] <- 'Inversion Total Nom New (%ToT)'



origin <-"1899-12-30"

start_date<- "2021-09-01"
pais$Date <- as.Date(pais$Date , origin = origin)

#pais$Date <- as.Date(pais$Date , origin = "1899-12-30")

pais <- select(pais,1, 50)
pais
fechas <- select(pais, Date)
fechas$variable <- 'Date'
fechas$value <- -2

pais <-  pais %>% reshape2::melt(id.vars='Date') %>%  na.omit()

pais$Value <- scales::percent(pais$value, accuracy = 0.01 )
pais$Value <- paste(pais$variable, pais$Value, sep = ": ")
fechas$newdate <- strptime(as.character(fechas$Date), "%Y-%m-%d")
fechas$newdate <- format(fechas$newdate, "%d-%m-%Y")
fechas$Value <- paste(fechas$variable, fechas$newdate, sep = ": ")
fechas <- select(fechas, -newdate)
pais <- rbind(pais, fechas)

colnames(pais)[1] <- 'Date'

gg_demanda <- pais %>% group_by(variable,Date) %>% 
  ggplot( aes(x=Date, y= value,  colour = variable))+ 
  geom_line() +
  geom_point(show.legend = FALSE) +
  geom_richtext(aes(x = max(Date)-90, y = min(value)+.1, label = format(Date, '%b-%Y') ), text.colour ="#666666", show.legend=F, 
                fill = NA, label.color = NA, size=10) + #Date inside plot
  theme_BBVA_grey +
  annotation_raster(logo, ymin = ymin, ymax = ymax, xmin = xmin, xmax =xmax) +
    scale_color_manual(values=c( "#0F4B7C","#2DCCCD","#9DC1F5", "#D8BE75"),
                     # scale_color_manual(values=c( "#0F4B7C","#2DCCCD","#9DC1F5","#D8BE75"),
                     breaks= c(  "Inversion Total Nom New (%ToT)")) +

  
  
  scale_y_continuous(trans = modulus_trans(0.003), breaks =  c( -0.22,-0.06,-0.04,-0.02,0,0.02,0.04,0.06,0.22),
                     labels = c('-22%', '-6%','-4%','-2%','0%','2%','4%','6%','22%'),
                     limits = c(-0.22, 0.22),
                     sec.axis=sec_axis(~.*1,breaks= c(-0.22,-0.06,-0.04,-0.02,0,0.02,0.04,0.06,0.22),labels = c('-22%', '-6%','-4%','-2%','0%','2%','4%','6%','22%'))) + 
  
  
  scale_x_date(limits=c(as.Date("2019-01-01"), as.Date(date)), date_breaks = "3 months", date_labels = "%b-%y") +
  
  geom_vline(xintercept = as.Date("2020-03-15"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2020-03-13"), label="Covid in EU", y=1), colour="#666666", angle=90, vjust = 1.2, size=3)+
  
  geom_vline(xintercept = as.Date("2021-03-02"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2021-03-01"), label="Delta Variant in EU", y=05), colour="#666666", angle=90, vjust =0., size=3)+
  
  geom_vline(xintercept = as.Date("2021-11-22"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2021-11-21"), label="Omicron", y=0.05), colour="#666666", angle=90, vjust = 1.2,size=3)+
  
  geom_vline(xintercept = as.Date("2022-02-21"), colour="black",linetype = "dotted") +
  geom_text(aes(x=as.Date("2022-02-20"), label="Rusia-Ucrania", y=0.05), colour="#666666", angle=90, vjust = 1.2,size=3)+
  transition_reveal(Date)  +
  #exit_shrink() +
  #enter_fade() +
  #ease_aes("circular-out") +
  guides(col = guide_legend(nrow = 1, reverse = F)) +
  #geom_segment(aes(x = as.Date("2020-01-28"), y = -50/100,xend =as.Date("2020-02-29") , yend = -50/100)) +
  geom_segment(aes(x = as.Date("2019-01-01"), y = 2.8/100,xend =as.Date("2019-03-31") , yend = 2.8/100)) +
  geom_segment(aes(x = as.Date("2019-04-01"), y = -0.9/100,xend =as.Date("2019-06-30") , yend = -0.9/100)) +
  geom_segment(aes(x = as.Date("2019-07-01"), y = 0.6/100,xend =as.Date("2019-09-30") , yend = 0.6/100)) +
  geom_segment(aes(x = as.Date("2019-10-01"), y = -1.6/100,xend =as.Date("2019-12-31") , yend = -1.6/100)) +
  
  geom_segment(aes(x = as.Date("2020-01-01"), y = -4.4/100,xend =as.Date("2020-03-31") , yend = -4.4/100)) +
  geom_segment(aes(x = as.Date("2020-04-01"), y = -20.2/100,xend =as.Date("2020-06-30") , yend = -20.2/100)) +
  geom_segment(aes(x = as.Date("2020-07-01"), y = +21.5/100,xend =as.Date("2020-09-30") , yend = +21.5/100)) +
  geom_segment(aes(x = as.Date("2020-10-01"), y = -1.8/100,xend =as.Date("2020-12-31") , yend = -1.8/100)) +
  
  geom_segment(aes(x = as.Date("2021-01-01"), y = -0.7/100,xend =as.Date("2021-03-31") , yend = -0.7/100)) +
  geom_segment(aes(x = as.Date("2021-04-01"), y = 1.9/100,xend =as.Date("2021-06-30") , yend = 1.9/100)) +
  geom_segment(aes(x = as.Date("2021-07-01"), y =  8.1/100,xend =as.Date("2021-09-30") , yend = 8.1/100)) +
  geom_segment(aes(x = as.Date("2021-10-01"), y =  10.7/100,xend =as.Date("2021-12-30") , yend = 10.7/100)) +
  
  geom_segment(aes(x = as.Date("2022-01-01"), y =  -7.4/100,xend =as.Date("2022-03-31") , yend = -7.4/100),colour="#F7893B") +
  geom_segment(aes(x = as.Date("2022-04-01"), y =  0.6/100,xend =as.Date("2022-06-30") , yend = 0.6/100)) +
  geom_segment(aes(x = as.Date("2022-07-01"), y =  6.1/100,xend =as.Date("2022-09-30") , yend = 6.1/100)) +
  
  
  labs(title =  "España: Indicador BigData Inversion Real va INE (BBVA Research)",  
       caption= "Fuente: BBVA Research", 
       subtitle = "(Real deflactado por Deflactor Mesualizado, Acum 90D % Var. Trimestral CVE)")  # Subtitle Date
gg_demanda <- gg_demanda + labs(x="", y = '% QoQ Real (deflactado por deflactor mensualizado )', positiony="left",color="") 

animate(gg_demanda, nframes = 200, duration = 3 ,fps = 32, width = 1200,
        height = 750, end_pause = 30)

anim_save("Demanda Interna_Consumo Total anual y trimestral .gif")






anim_save("investment_all_Spain_WITHIN_dec.gif")



