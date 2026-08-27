library(leaflet)
library(shiny)
library(geojsonio)
library(sf)
library(readxl)
library(dplyr)
library(stringr)
library(classInt)
library(htmlwidgets)

setwd("G:/Stab/Analys/Data/Ortsstruktur")

# Läs in geopackage
karta13 <- st_read("ortstruktur.gpkg")

#Läs in data
struktur <- read.csv("Nettopendling2.csv", sep = ";")

#Slå ihop geopackage och data KOPPLA ANTINGEN PÅ KOD ELLER ORTSNAMN
punkter <- left_join(karta13, struktur, by = c("tatortskod" = "Tätortskod"))

#Ändra koordinatsystem för leaflet
punkter <- st_transform(punkter, 4326)

# Lägg till Kommungränser
kommuner <- st_read("kommungrans_vastmanland_v2.gpkg")  
kommuner_wgs84 <- st_transform(kommuner, 4326)

#Lägg till tätortsgränser 
tatortsgrans <- st_read("tatorter_vastmanland_v2.gpkg")
tatortsgrans_wgs84 <- st_transform(tatortsgrans, 4326)

# Omvandla till punkter istället för polygoner (centroider)
#punkter <- karta_wgs84  # kopiera först
st_geometry(punkter) <- st_centroid(st_geometry(punkter))  # ersätt geometrin med punkter

# Gör till faktor för att kunna klassificera efter innehåll i kolumn för Ortsklass
punkter$klass <- as.factor(punkter$Kopia.av.Ortsstruktur.karta_Ortsklass)

# Definiera storlek på punkterna manuellt
storlekar <- c(
  "Storregional nodstad" = 25,
  "Centralort" = 16,
  "Arbetsort" = 12,
  "Serviceort" = 10,
  "Bostadsort med viss service" = 8,
  "Bostadsort" = 6
)

# Lägg till storlek för klassificeringen
punkter$storlek <- storlekar[as.character(punkter$klass)] 

# Skapa egen färgpalett för punkterna
farger <- c(
  "Positiv" = "#538a4c",
  "Negativ, hög" = "#8dd284",
  "Negativ, medel" = "#f1da98",
  "Negativ, låg" = "#ae6976"
)

# Lägg till färg för klassificeringen (ändra koppling)
punkter$farg <- farger[as.character(punkter$Nettopendl_karta2)]


# Skapa Leaflet-karta
Nettopendling2 <- leaflet(punkter) %>%
  addTiles(
    urlTemplate = "https://basemaps.cartocdn.com/rastertiles/light_all/{z}/{x}/{y}.png?key=cb1_25io_1_6f57800601aa825b08fd3aed",
    attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors &copy; <a href="https://carto.com/attributions">CARTO</a>'
  ) %>%
  addPolygons(data = kommuner_wgs84,
              fill = FALSE,
              color = "grey",
              weight = 0.7,
              opacity = 1) %>%
  addPolygons(data = tatortsgrans_wgs84,
              fill = FALSE,
              color = "grey",
              weight = 1,
              opacity = 0.8) %>%
  addCircleMarkers(
    radius = ~storlek,
    color = ~farg,
    stroke = TRUE,
    weight = 3,
    fillOpacity = 0.8,
    popup = ~paste0(tatort,"<br>", "Ortsklass: ", klass, "<br>", "Nettopendling: ", punkter$Dagbef.nattbef)
  ) %>%
  addLegend(
    "bottomright",
    colors = unname(farger),
    labels = names(farger),
    title = "Nettopendling per tätort år 2023",
    opacity = 1
  ) %>%
  addControl(
    html = "<span style='font-size:11px;'>Nettopendling motsvarar här måttet dagbefolkning/nattbefolkning x 100.<br> 
    Ett värde över 100 anger positiv nettopendling (fler inpendlare än utpendlare). <br> Orter med negativ nettopendling <br>
    (fler utpendlare än inpendlare) <br> har grupperats i tre grupper - <br>ju lägre värde desto större negativ nettopendling.</span>",
    position = "bottomleft"
)

# Spara kartan som HTML
saveWidget(Nettopendling2, "Nettopendling.html", selfcontained = TRUE)




