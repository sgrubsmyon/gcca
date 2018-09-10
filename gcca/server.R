library(rdwd)
library(data.table)
library(lubridate)
library(ggplot2)

station_id <- 2667

kl_2667 <- dataDWD(selectDWD(id = station_id, res = "daily", var = "kl", per = "recent"))
kl_2667 <- as.data.table(
  dataDWD(selectDWD(id = station_id, res = "daily", var = "kl", per = "historical"))  
)

# Übersicht über Variablen:
#   (aus Metadaten der Station 2667, in der historical .zip-Datei, Datei "Metadaten_Parameter_klima_tag_02667.html")
# FM: Tagesmittel der Windgeschwindigkeit [m/s]
# FX: Maximum der Windspitze [m/s]
# NM: Tagesmittel des Bedeckungsgrades [Achtel]
# PM: Tagesmittel des Luftdrucks [hpa]
# RSK: tgl. Niederschlagshoehe [mm]
# RSKF: tgl. Niederschlagsform (=Niederschlagshoehe_ind) [numerischer Code]
# SDK: tgl. Sonnenscheindauer [h]
# SHK_TAG: Schneehoehe Tageswert [cm]
# TGK: Minimum der Lufttemperatur am Erdboden in 5cm Hoehe [°C]
# TMK: Tagesmittel der Temperatur [°C]
# TNK: Tagesminimum der Lufttemperatur in 2m Hoehe [°C]
# TXK: Tagesmaximum der Lufttemperatur in 2m Höhe [°C]
# UPM: Tagesmittel der Relativen Feuchte [%]
# VPM: Tagesmittel des Dampfdruckes [hpa]

# QN_3?
# QN_4?
# eor?

# ref_window <- c(1958, 1967)
# ana_window <- c(2008, 2017)
# 
# txk_ref <- kl_2667[year(MESS_DATUM) >= ref_window[1] & year(MESS_DATUM) < ref_window[2]][["TXK"]]
# txk_ana <- kl_2667[year(MESS_DATUM) >= ana_window[1] & year(MESS_DATUM) < ana_window[2]][["TXK"]]
# ks.test(txk_ana, txk_ref)
# 
# tmk_ref <- kl_2667[year(MESS_DATUM) >= ref_window[1] & year(MESS_DATUM) < ref_window[2]][["TMK"]]
# tmk_ana <- kl_2667[year(MESS_DATUM) >= ana_window[1] & year(MESS_DATUM) < ana_window[2]][["TMK"]]
# ks.test(tmk_ana, tmk_ref)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  ana_duration <- reactive(dyears(input$ana_duration)) # 10 years
  
  output$widgets <- renderUI({
    ref_min_date <- min(kl_2667$MESS_DATUM)
    ref_max_date <- min_date + ana_duration()
    ana_max_date <- max(kl_2667$MESS_DATUM)
    ana_min_date <- ana_max_date - ana_duration()
    sl_ref <- sliderInput("ref_dates", label = h3("Reference Time Window"), timeFormat = "%Y",
                          min = ref_min_date, max = ana_max_date,
                          value = c(ref_min_date, ref_max_date), step = ana_duration())
    sl_ref$attribs$class <- paste(sl_ref$attribs$class, "fixed-width")
    sl_ana <- sliderInput("ana_dates", label = h3("Analysis Time Window"), timeFormat = "%Y",
                          min = ref_min_date, max = ana_max_date,
                          value = c(ana_min_date, ana_max_date), step = ana_duration())
    sl_ana$attribs$class <- paste(sl_ana$attribs$class, "fixed-width")
    tagList(sl_ref, sl_ana)
  })
  
  refData <- reactive({
    kl_2667[MESS_DATUM >= input$ref_dates[1] & MESS_DATUM < input$ref_dates[2]]
  })
  
  anaData <- reactive({
    kl_2667[MESS_DATUM >= input$ana_dates[1] & MESS_DATUM < input$ana_dates[2]]
  })
  
   output$distPlot <- renderPlot({
     temp <- data.frame(
       maxtemp = c(refData()$TXK, anaData()$TXK),
       window = c(rep("ref", nrow(refData())), rep("ana", nrow(anaData())))
     )
     ggplot(temp, aes(maxtemp, colour = window)) +
       geom_freqpoly(binwidth = 5) +
       xlim(c(-10, 40))
   })
   
})
