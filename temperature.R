library(rdwd)
library(data.table)
library(lubridate)

# bonn <- nearbyStations(lat = 50.73736, lon = 7.08442, 20)
# saveRDS(bonn, file = "bonn_stations.rds")
bonn <- readRDS("bonn_stations.rds")
station_id <- 2667

# kl_2667 <- dataDWD(selectDWD(id = station_id, res = "daily", var = "kl", per = "recent"))
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

# plot(kl_2667[, c("MESS_DATUM", "TMK")], type = "l", las = 1)
# plot(kl_2667[, c("MESS_DATUM", "TXK")], type = "l", las = 1)

sliced_average <- function(dt, col_name, timebreaks) {
  sapply(seq_len(length(timebreaks) - 1), function(i) {
    low <- timebreaks[i]
    high <- timebreaks[i + 1]
    mean(
      dt[year(MESS_DATUM) >= low & year(MESS_DATUM) < high][[col_name]]
    )
  })
}
# 
# timebreaks <- seq(1957, 2017, 5)
# times <- timebreaks[2:length(timebreaks)] - 0.5 * diff(timebreaks)
# 
# plot(times, sliced_average(kl_2667, "TXK", timebreaks))
# plot(times, sliced_average(kl_2667, "TMK", timebreaks))

ref_window <- c(1958, 1967)
ana_window <- c(2008, 2017)

txk_ref <- kl_2667[year(MESS_DATUM) >= ref_window[1] & year(MESS_DATUM) < ref_window[2]][["TXK"]]
txk_ana <- kl_2667[year(MESS_DATUM) >= ana_window[1] & year(MESS_DATUM) < ana_window[2]][["TXK"]]
ks.test(txk_ana, txk_ref)

tmk_ref <- kl_2667[year(MESS_DATUM) >= ref_window[1] & year(MESS_DATUM) < ref_window[2]][["TMK"]]
tmk_ana <- kl_2667[year(MESS_DATUM) >= ana_window[1] & year(MESS_DATUM) < ana_window[2]][["TMK"]]
ks.test(tmk_ana, tmk_ref)
