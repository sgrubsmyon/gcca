library(rdwd)

# bonn <- nearbyStations(lat = 50.73736, lon = 7.08442, 20)
# saveRDS(bonn, file = "bonn_stations.rds")
bonn <- readRDS("bonn_stations.rds")
station_id <- 2667

# data <- dataDWD(selectDWD(id = station_id, res = "daily", var = "kl", per = "recent"))
data <- dataDWD(selectDWD(id = station_id, res = "daily", var = "kl", per = "historical"))
plot(data[, c("MESS_DATUM", "TMK")], type = "l", las = 1)
