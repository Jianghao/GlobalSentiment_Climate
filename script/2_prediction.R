setwd("~/Dropbox/GlobalSentiment/")
library(rasterVis)
library(ncdf4)
library(jtools)
library(ggplot2)
library(dplyr)
library(sf)
source("scripts/utils/theme.R")

# -------------------------------------------------------
# function: rasterPredict
# -------------------------------------------------------
rasterPredict <- function(rs, var = "", model) {
  rs.pred <- rs[[var]]
  names(rs.pred) <- "xmid"
  rs.pred <- raster::predict(rs.pred, model, progress = "")
  names(rs.pred) <- var
  return(rs.pred)
}

result <- "01_tem_bin_main"
load(paste0("output/results/reg_coef_Rdata/", result, ".Rdata")) # coef, tem
model <- lm(coef * 100 ~ splines::ns(xmid, df = 5), data = coef)

# using jtools to layout the figure
model.fit.quantity <- effect_plot(model, pred = xmid, interval = TRUE) +
  geom_hline(yintercept = 0, color="grey50", linetype = "dashed") +
  geom_point(data = coef, aes(x = xmid, y = coef * 100), colour = "#2E9FDF", size = 2) +
  geom_errorbar(data = coef, aes(x = xmid, y = coef * 100, ymin = ci.l * 100, ymax = ci.h * 100),
                width = 0.2, size = 1, alpha = 1, fill = "#2E9FDF", colour = "#2E9FDF") +
  ylab("Sentiment change (%)") +
  xlab(expression(paste("Max. Temperature (", degree, C, ")"))) +
  xlim(c(-10, 40)) +
  theme_Publication()
model.fit.quantity
ggsave(filename = paste0("output/figures/", result, "_effect_plot_v2.pdf"),
       plot = model.fit.quantity, width = 7, height = 5)

# Climate zone raster -----------------------------------------------------

cz.grid <- read.csv("data/climatic_zone/Koeppen-Geiger-ASCII.csv")
names(cz.grid) <- c("Lat", "Lon", "Cls")
cz.grid <- cz.grid %>%
  dplyr::mutate(cz1 = substr(Cls, 1, 1)) %>%
  dplyr::mutate(cz1_num = ifelse(cz1 == "A", 1, 
                                 ifelse(cz1 == "B", 2, 
                                        ifelse(cz1 == "C", 3,
                                               ifelse(cz1 == "D", 4, 5)))))
rs.extent <- raster::raster(ncols = 720, nrows = 360)
rcz <- raster::rasterize(x = cbind(cz.grid$Lon, cz.grid$Lat), 
                         rs.extent, 
                         field = cz.grid$cz1_num)
raster::plot(rcz)



## population
# popu <- raster("data/environmental/population/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec.tif")
# popu_0.05 <- raster::aggregate(x = popu, fact = 6,  fun = sum)
# popu_0.5  <- raster::aggregate(x = popu, fact = 60, fun = sum)
# names(popu_0.05) <- names(popu_0.5) <- "pop"
# writeRaster(popu_0.05, filename="data/environmental/population/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_0.05_degree.tif", datatype='INT4S', overwrite=TRUE)
# writeRaster(popu_0.5, filename="data/environmental/population/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_0.5_degree.tif", datatype='INT4S', overwrite=TRUE)

library(raster)
popu <- raster::raster("data/environmental/origin/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_min.tif")
# popu[popu < 100] <- NA
raster::plot(popu)
popu <- popu %>% 
  crop(extent(-180, 180, -60, 90))

# future prediction for each year ------------------------------------------

product <- "tasmax"

for(rcp in c("rcp26")){ # "rcp26", "rcp45", "rcp60", "rcp85"
  for(y in 2000:2100){
    cat(as.character(Sys.time()), ":", rcp, ":", y, "\n")
    nc.name <- paste0(getwd(), "/data/CMIP5/", product, "_Amon_modmean_", rcp, "_000.nc")
    # read ncdf data
    r <- raster::brick(nc.name)
    nr <- names(r)
    # format ncdf dates
    nr.df <- data.frame(date = gsub("X", "", nr)) %>%
      mutate(date = as.character(date)) %>%
      mutate(year = as.integer(substr(date, 1, 4))) %>%
      mutate(month = as.integer(substr(date, 6, 7))) %>%
      mutate(day = as.integer(substr(date, 9, 10)))

    index <- which(nr.df$year %in% c(y))
    r.sel <- r[[nr[index]]]
    r.sel <- disaggregate(r.sel, fact = c(5, 5), method="bilinear")

    names(r.sel) <- c(
      paste0("Y", y, "_M", c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"))
    )

    r.df <- rasterToPoints(r.sel) %>%
      as.data.frame() %>%
      mutate(lon = ifelse(x > 180, x-360, x), lat = y) %>%
      dplyr::select(-x, -y)

    r.rs <- rasterize(cbind(r.df$lon, r.df$lat), rs.extent,
                      field = dplyr::select(r.df, -lon, -lat)) %>%
      mask(rcz)

    r.pred <- r.rs
    for(m in 1:12) {
      cat(m, "..")
      month <- ifelse(m < 10, paste0(0, m), m)
      r.pred[[m]] <- rasterPredict(r.rs[[m]] - 273.15, var = paste0("Y", y, "_M", month), model)
    }
    cat("\n")
    r.pred <- readAll(r.pred)
    saveRDS(r.pred, file = paste0("data/CMIP5/predict/", product, "_", y, "_modmean_", rcp, "_000.Rds"),
            compress = T)
  }
}




# Shift the original data by -360 degree ----------------------------------
# only run it for once

# if (FALSE) {
#   for(rcp in c("rcp26", "rcp45", "rcp60", "rcp85")) {
#     cat(as.character(Sys.time()), ":", rcp, "\n")
#     nc.name <- paste0(getwd(), "/data/CMIP5/", product, "_Amon_modmean_", rcp, "_000.nc")
#     r <- raster::brick(nc.name)
#     index <- which(substr(names(r), 2, 5) %in% 1861:2100)
#     r.sel <- r[[index]]
#
#     ## ~ 3 mins
#     r.df <- rasterToPoints(r.sel) %>%
#       as.data.frame() %>%
#       mutate(lon = ifelse(x > 180, x-360, x), lat = y) %>%
#       dplyr::select(-x, -y)
#
#     ## ~ 12 mins
#     r.rs <- rasterize(cbind(r.df$lon, r.df$lat), raster(ncols = 144, nrows = 72),
#                       field = dplyr::select(r.df, -lon, -lat))
#     r.rs <- readAll(r.rs)
#     saveRDS(r.rs, file = paste0("data/CMIP5/", product, "_Amon_modmean_", rcp, "_000_reproj.Rds"),
#             compress = T)
#   }
# }


# Visualize the historical temperature variations -------------------------

# tmax.rcp85 <- readRDS("data/CMIP5/tasmax_Amon_modmean_rcp60_000_reproj.Rds")
# tmax.rcp85 <- tmax.rcp85[[which(substr(names(tmax.rcp85), 2, 5) %in% c(2019, 2050, 2100))]]%>%
#   disaggregate(fact = c(5, 5), method="bilinear") %>%
#   mask(rcz) %>%
#   crop(extent(-180, 180, -60, 90))
# tmax.rcp85 <- tmax.rcp85 - 273.15
# tmax.rcp85.2019 <- tmax.rcp85[[ which(substr(names(tmax.rcp85), 2, 5) %in% 2019) ]]
# tmax.rcp85.2050 <- tmax.rcp85[[ which(substr(names(tmax.rcp85), 2, 5) %in% 2050) ]]
# tmax.rcp85.2100 <- tmax.rcp85[[ which(substr(names(tmax.rcp85), 2, 5) %in% 2100) ]]
#
# tmax.rcp85.2019.y <- mean(tmax.rcp85.2019)
# tmax.rcp85.2050.y <- mean(tmax.rcp85.2050)
# tmax.rcp85.2100.y <- mean(tmax.rcp85.2100)
#
# tmax.rcp85.s <- stack(tmax.rcp85.2019.y, tmax.rcp85.2050.y, tmax.rcp85.2100.y)
# names(tmax.rcp85.s) <- paste0("Y", c(2019, 2050, 2100))
#
# plot(tmax.rcp85.s)



# Plot the 12 months tmax for years 2019, 2050, 2100 ----------------------

# df.2019 <- as.data.frame( tmax.rcp85.2019) %>% colMeans(na.rm = T)
# df.2050 <- as.data.frame( tmax.rcp85.2050) %>% colMeans(na.rm = T)
# df.2100 <- as.data.frame( tmax.rcp85.2100) %>% colMeans(na.rm = T)
#
# df <- cbind(df.2019, df.2050, df.2100) %>% as.data.frame() %>%
#   mutate(month = seq(1, 12, 1))
# names(df) <- c("Y2019", "Y2050", "Y2100", "month")
#
# df <- df %>% tidyr::gather("group", "value") %>%
#   filter(group != "month") %>%
#   mutate(month = rep(seq(1, 12, 1), 3))
#
# ggplot(data = df, aes(x = month, y=value, colour = group)) +
#   geom_line(alpha = 0.8, lwd = 1) +
#   xlab("Month") +
#   ylab(expression(paste("Monthly mean max. temperature (", degree, C, ")"))) +
#   scale_fill_npg() +
#   theme_Publication() +
#   scale_x_continuous(limits=c(0, 12), breaks = seq(1,12,1)) +
#   scale_y_continuous(breaks = seq(0,30,5)) +
#   theme(
#     legend.title = element_blank(),
#     legend.text = element_text(size = 12),
#     legend.key.size= unit(0.8, "cm"),
#     legend.direction = "vertical",
#     legend.position = c(0.15, 0.85)
#     # legend.position = "right"
#   )
#
# ggplot2::ggsave(paste0("output/figures/06_pred_dataV_rcp60_byMonth.pdf"), width = 6, height = 5)


# Visualize the historical temperature variations -------------------------

## load data and basic process
listofdf <- NULL
rcp.names <- c("rcp26", "rcp45", "rcp60", "rcp85")
rcp.names <- c("rcp60")
by.year <- FALSE
by.season <- TRUE
by.month <- FALSE
for(i in 1:length(rcp.names)){
  rcp.name <- rcp.names[i]
  cat(as.character(Sys.time()), i, rcp.name, "\n")
  tmax.rcp <- readRDS(paste0("data/CMIP5/tasmax_Amon_modmean_", rcp.name, "_000_reproj.Rds"))
  tmax.rcp <- tmax.rcp[[which(substr(names(tmax.rcp), 2, 5) %in% c(2000:2100))]]

  ## transform date into an index: by year
  if(by.year) index <- substr(names(tmax.rcp), 2, 5)
  if(by.season) index <- paste0(substr(names(tmax.rcp), 2, 5), "_", c("Winter", "Winter", "Spring", "Spring","Spring", "Summer", "Summer", "Summer", "Fall", "Fall", "Fall", "Winter"))
  if(by.month) index <-  paste0(substr(names(tmax.rcp), 2, 5), "_", c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
  tmax.year <- stackApply(tmax.rcp, index, mean, na.rm=TRUE) %>%
    disaggregate(fact = c(5, 5)) %>%
    mask(rcz) %>%
    crop(extent(-180, 180, -60, 90))
  tmax.year <- tmax.year - 273.15

  if(by.year) output.f <- "byYear"
  if(by.season) output.f <- "bySeason"
  if(by.month) output.f <- "byMonth"

  ## MC the model
  mc <- TRUE
  reps <- 100
  for(k in 1:reps){
    cat(as.character(Sys.time()), rcp.name, k, "\n")
    if(mc) {
      # simulation of model
      coef.simu <- coef %>%
        rowwise() %>%
        mutate(coef = rnorm(1, coef, se))
    } else {
      coef.simu = coef
    }

    model <- lm(coef * 100 ~ splines::ns(xmid, df = 5), data = coef.simu)


    ## predict the yearly mean
    list <- tmax.year
    index.name <- names(tmax.year)
    for(j in 1:length(index.name)) {
      index.j <- index.name[j]
      list[[j]] <- rasterPredict(tmax.year, var = index.j, model)
    }

    tmax.pred <- stack(list)

    saveRDS(readAll(tmax.pred), file = paste0("data/CMIP5/prediction/", output.f, "/tasmax_", rcp.name, "_sim_", sprintf("%04.0f", k), ".Rds"), compress = T)
  }

  # %>%
  #   mask(popu)
  #
  # tmax.pred.w <- tmax.pred * popu
  # names(tmax.pred.w) <- names(tmax.pred)
  #
  # plot(tmax.pred[["index_2100_Summer"]] - tmax.pred[["index_2019_Summer"]])
  # plot(tmax.pred.w[["index_2100_Summer"]] - tmax.pred.w[["index_2019_Summer"]])
  # cellStats(tmax.pred[["index_2100_Summer"]] - tmax.pred[["index_2019_Summer"]], mean)
  # cellStats(tmax.pred.w[["index_2100_Summer"]] - tmax.pred.w[["index_2019_Summer"]], sum) / sum(values(popu), na.rm = T)
  #
  # plot(tmax.pred.w[["index_2100_Winter"]] - tmax.pred[["index_2019_Winter"]])
  # cellStats(tmax.pred[["index_2100_Winter"]] - tmax.pred[["index_2019_Winter"]], mean)
  # cellStats(tmax.pred.w[["index_2100_Winter"]] - tmax.pred.w[["index_2019_Winter"]], sum) / sum(values(popu), na.rm = T)
  # cellStats(tmax.pred[["index_2100_Fall"]] - tmax.pred[["index_2019_Fall"]], mean)
  # cellStats(tmax.pred[["index_2100_Spring"]] - tmax.pred[["index_2019_Spring"]], mean)
  # cellStats(tmax.pred.w[["index_2100_Spring"]] - tmax.pred.w[["index_2019_Spring"]], sum) / sum(values(popu), na.rm = T)

  # temp
  # tmax.year.df <- data.frame(year = seq(2000,2100, 1),
  #                            tasmax = cellStats(tmax.year, "mean", na.rm = T), # here is tmax
  #                            sd = cellStats(tmax.year, "sd", na.rm = T),
  #                            rcp = rcp.name
  # )
  #
  # listofdf[[i]] <- tmax.year.df
}

tmax.year.df <- bind_rows(listofdf)

anomaly <- tmax.year.df %>%
  filter(year == 2019) %>%
  group_by(year) %>%
  summarise(tmax_ano = mean(tasmax))

ggplot(data = tmax.year.df, aes(x = year, y=tasmax - 14, colour = rcp, fill = rcp)) +
  geom_hline(yintercept = 0, color="grey50", linetype = "dashed") +
  geom_vline(xintercept = 2019, color="grey50", linetype = "dashed") +
  geom_line(alpha = 0.5, lwd = 0.5, show.legend = T) +
  geom_smooth(method = 'loess', show.legend = T, alpha = 0.5) +
  xlab("Year") +
  ylab(expression(paste("Change in annual max. temperature (", degree, C, ")"))) +
  scale_fill_npg() +
  theme_Publication() +
  scale_x_continuous(limits=c(2000, 2100), breaks = seq(2000,2100,10)) +
  scale_y_continuous(breaks = seq(-1,5,1)) +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.key.size= unit(0.8, "cm"),
    legend.direction = "vertical",
    legend.position = "right"
  )

ggplot2::ggsave(paste0("output/figures/06_pred_dataV_byYear.pdf"), width = 8, height = 5)



# # Visualize the prediction TS ---------------------------------------------
# # Version 1
pred <- readRDS("data/CMIP5/prediction/bySeason/tasmax_rcp60.Rds")

index = names(pred)
pred <- pred * popu
pop.total <- cellStats(popu, sum)

pred.df <- data.frame(predV = cellStats(pred, sum))
pred.df <- pred.df %>%
  mutate(predV = predV/pop.total,
         index = index) %>%
  tidyr::separate(index, into = c("index", "year", "season"), sep = "_") %>%
  mutate(year = as.numeric(year))

Spring2019 <- pred.df[(pred.df$year == 2019 & pred.df$season == "Spring"), "predV"]
Summer2019 <- pred.df[(pred.df$year == 2019 & pred.df$season == "Summer"), "predV"]
Fall2019 <- pred.df[(pred.df$year == 2019 & pred.df$season == "Fall"), "predV"]
Winter2019 <- pred.df[(pred.df$year == 2019 & pred.df$season == "Winter"), "predV"]


pred.df <- pred.df %>%
  mutate(predSeason = ifelse(season == "Spring", predV - Spring2019,
                             ifelse(season == "Summer", predV - Summer2019,
                                    ifelse(season == "Fall", predV - Fall2019,
                                           ifelse(season == "Winter", predV - Winter2019, NA)))))

pred.df.tmp <- pred.df %>%
  mutate(cil = predSeason - abs(predSeason)/5,
         ciu = predSeason + abs(predSeason)/5)

ggplot(data = pred.df.tmp, aes(x = year, y=predSeason, colour = season, fill = season)) +
  geom_hline(yintercept = 0, color="grey50", linetype = "dashed") +
  geom_vline(xintercept = 2019, color="grey50", linetype = "dashed") +
  geom_line(alpha = 1, lwd = 1, show.legend = T) +
  # geom_smooth(method = "loess", show.legend = T, alpha = 0.5) +
  geom_ribbon(aes(ymin = cil, ymax = ciu), alpha = 0.30, colour = NA) +
  xlab("Year") +
  ylab(expression(paste("Change in annual max. temperature (", degree, C, ")"))) +
  scale_fill_npg() +
  theme_Publication() +
  scale_x_continuous(limits=c(2019, 2100), breaks = seq(2020,2100,10)) +
  scale_y_continuous(breaks = seq(-6,2,1)) +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.key.size= unit(0.8, "cm"),
    legend.direction = "vertical",
    legend.position = "right"
  )

ggplot2::ggsave(paste0("output/figures/06_pred_future_bySeason_rcp60.pdf"), width = 8, height = 5)

# Visualize the prediction TS ---------------------------------------------
# Version 2 2023-10
listofdf <- NULL

sim.list <- dir("data/CMIP5/prediction/bySeason/", pattern = glob2rx("tasmax_rcp60_sim_*.Rds"), full.names = T)

for(i in 1:length(sim.list)){
  cat(as.character(Sys.time()), i, "\n")
  pred <- readRDS(sim.list[i])
  index = names(pred)
  pred <- pred * popu
  pop.total <- cellStats(popu, sum)
  
  pred.df <- data.frame(predV = cellStats(pred, sum)) 
  pred.df <- pred.df %>%
    mutate(predV = predV/pop.total,
           index = index) %>%
    tidyr::separate(index, into = c("index", "year", "season"), sep = "_") %>%
    mutate(year = as.numeric(year))
  
  Spring2019 <- pred.df[(pred.df$year == 2019 & pred.df$season == "Spring"), "predV"]
  Summer2019 <- pred.df[(pred.df$year == 2019 & pred.df$season == "Summer"), "predV"]
  Fall2019 <- pred.df[(pred.df$year == 2019 & pred.df$season == "Fall"), "predV"]
  Winter2019 <- pred.df[(pred.df$year == 2019 & pred.df$season == "Winter"), "predV"]
  
  pred.df <- pred.df %>%
    mutate(predSeason = ifelse(season == "Spring", predV - Spring2019, 
                               ifelse(season == "Summer", predV - Summer2019,
                                      ifelse(season == "Fall", predV - Fall2019,
                                             ifelse(season == "Winter", predV - Winter2019, NA)))))
  saveRDS(pred.df, file = paste0(i, ".Rds"), compress = T)
}

# Visual
df.list <- dir(getwd(), pattern = glob2rx("*.Rds"), full.names = T)
for(i in 1:length(df.list)) {
  cat(as.character(Sys.time()), i, "\n")
  listofdf[[i]] <- readRDS(df.list[i]) %>%
    mutate(id = i)
}
pred.df <- bind_rows(listofdf)

pred.df.sum <- pred.df %>%
  dplyr::group_by(year, season) %>%
  dplyr::summarise(median =  median(predSeason), 
            cil = quantile(predSeason,  probs = c(0.025)),
            ciu = quantile(predSeason,  probs = c(0.975))
  )

ggplot(data = pred.df.sum, aes(x = year, y=median, colour = season, fill = season)) +
  geom_hline(yintercept = 0, color="grey50", linetype = "dashed") +
  geom_vline(xintercept = 2019, color="grey50", linetype = "dashed") +
  geom_line(alpha = 1, lwd = 1, show.legend = T) +
  # geom_smooth(method = "loess", show.legend = T, alpha = 0.5) +
  geom_ribbon(aes(ymin = cil, ymax = ciu), alpha = 0.25, colour = NA) +
  xlab("Year") +
  ylab(expression(paste("Precentage point change in sentiment"))) +
  scale_fill_npg() +
  theme_Publication() +
  scale_x_continuous(limits=c(2000, 2100), breaks = seq(2000,2100,10)) +
  scale_y_continuous(breaks = seq(-6,2,1)) +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.key.size= unit(0.8, "cm"),
    legend.direction = "vertical",
    legend.position = "right"
  )

ggplot2::ggsave(paste0("output/figures/06_pred_future_bySeason_rcp60.pdf"), width = 8, height = 5)


# Visualize the prediction TS ---------------------------------------------

pred <- readRDS("data/CMIP5/prediction/bySeason/tasmax_rcp60.Rds")
pred.2019 <- pred[[which(substr(names(pred), 7, 10) %in% c("2019"))]]
pred.2100 <- pred[[which(substr(names(pred), 7, 10) %in% c("2100"))]]
pred.diff <- pred.2100 - pred.2019
names(pred.diff) <- c("Winter", "Spring", "Summer", "Fall")
plot(pred.diff)


min <- min(values(pred.diff), na.rm = T) %>% as.integer() - 1
max <- max(values(pred.diff), na.rm = T) %>% as.integer() + 1
at <- unique(c(seq(min, 0, abs(min)/30), seq(0, max, max/30)))

world <- st_read('data/spatial/ne_110m_admin_0_countries/ne_110m_admin_0_countries.shp', stringsAsFactors = F) %>% sf::as_Spatial()

pdf(paste0("output/figures/06_pred_future_bySeason_map_2100_rcp85.pdf"), width = 12, height = 8)
levelplot(pred.diff, cuts = 50,
          at = at,
          par.settings = rasterTheme(region = brewer.pal(9, "RdYlGn")),
          scales = list(draw=FALSE),
          margin = FALSE,
          xlab = NULL, 
          ylab = NULL,
          names.attr = names(pred.diff), 
          colorkey = list(labels = list(cex = 1.25, col = "grey15"),
                          height = 0.6,
                          space= "bottom"),
          par.strip.text = list(cex=1.25, lines=1, col="grey15", face="bold")) +
  latticeExtra::layer(sp.polygons(world, fill='transparent', col = "grey40", alpha = 0.6, lwd = 0.7))
dev.off()


## by month
pred <- readRDS("data/CMIP5/prediction/byMonth/tasmax_rcp85.Rds")
pred.2019 <- pred[[which(substr(names(pred), 7, 10) %in% c("2019"))]]
pred.2100 <- pred[[which(substr(names(pred), 7, 10) %in% c("2100"))]]
pred.diff <- (pred.2100 - pred.2019) * popu
names(pred.diff) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

pred.diff.df <- data.frame(predV = cellStats(pred.diff, sum)) %>%
  mutate(predV = predV / pop.total)



plot(1:12, pred.diff.df$predV)
## plot the typical year


# pt <- levelplot(tmax.year[[c("index_2019", "index_2050", "index_2100")]], 
#                 cuts = 100,
#                 par.settings = rasterTheme(region = brewer.pal(11, "RdPu")),
#                 scales = list(draw=FALSE),
#                 margin = FALSE,
#                 xlab = NULL, 
#                 ylab = NULL,
#                 main = main, 
#                 colorkey = list(labels = list(cex = 1.25, col = "grey15"),
#                                 height = colorkey.height,
#                                 space=colorkey.space),
#                 par.strip.text = list(cex=1.25, lines=1, col="grey15", face="bold"))