library(rdwd)
library(data.table)
library(lubridate)
library(ggplot2)

station_id <- 2667

kl_2667 <- dataDWD(selectDWD(id = station_id, res = "daily", var = "kl", per = "recent"))
kl_2667 <- as.data.table(
  dataDWD(selectDWD(id = station_id, res = "daily", var = "kl", per = "historical"))  
)

# Set default ggplot font size to a larger value
theme_set(theme_gray(base_size = 20))

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

labels <- list(
  TXK = "Diurnal max. of air temperature at 2m above ground [°C]",
  TNK = "Diurnal min. of air temperature at 2m above ground [°C]",
  TMK = "Diurnal average of air temperature [°C]",
  FX  = "Max. wind speed [m/s]",
  FM  = "Average wind speed [m/s]"
)

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
  
  kstest <- function(quantity = "TXK") {
    reactive({
      ks.test(anaData()[[quantity]], refData()[[quantity]])
    })
  }
  
  # output$stats <- renderUI({
  #   req(input$ana_dates)
  #   tagList(
  #     h3(sprintf("Comparing [%s - %s] with [%s - %s]",
  #               year(input$ana_dates[1]), year(input$ana_dates[2]),
  #               year(input$ref_dates[1]), year(input$ref_dates[2]))
  #     ),
  #     br(),
  #     p(
  #       "Statistical significance: ",
  #       strong(paste("p =", kstest()$p.value)),
  #       paste0("(", kstest()$method, ", ", kstest()$alternative, ")")
  #     ),
  #     # Mean ks.test p.value of two samples drawn from the same random distribution is quite close to 0.5:
  #     #   mean(sapply(1:1000, function(i) ks.test(rnorm(3500, 15, 9), rnorm(3500, 15, 9))$p.value))
  #     # So, assume that p.value = 0.5 "means" ~100% probability that distributions
  #     # are identical.
  #     h2(sprintf("Probability of no climate change between ref and ana: ~%s%%",
  #                format(min(kstest()$p.value * 2 * 100, 100), scientific = FALSE)))
  #   )
  # })
  
  output$time_windows <- renderUI({
    req(input$ana_dates)
    h3(sprintf("Comparing [%s - %s] with [%s - %s]",
               year(input$ana_dates[1]), year(input$ana_dates[2]),
               year(input$ref_dates[1]), year(input$ref_dates[2])))
  })
  
  output$distPlot <- renderPlot({
    temp <- data.table(
      quantity = c(refData()$TXK, anaData()$TXK),
      Window = c(rep("Reference", nrow(refData())), rep("Analysis", nrow(anaData())))
    )
    # temp <- rbind(
    #   temp,
    #   data.table(
    #     maxtemp = rnorm(3500, 15, 9),
    #     window = "rnorm 15 8"
    #   )
    # )
    # temp <- rbind(
    #   temp,
    #   data.table(
    #     maxtemp = rnorm(3500, 13.5, 9),
    #     window = "rnorm 13.5 8"
    #   )
    # )
    ggplot(temp, aes(quantity, colour = Window)) +
      geom_freqpoly(binwidth = 5) +
      xlab(labels$TXK) +
      xlim(c(-10, 40))
  })
  
})
