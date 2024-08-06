library(arm)
library(dplyr)
library(lubridate)
library(stargazer)
library(magrittr)
library(rstudioapi)
library(jtools)
library(foreign)
library(broom)
library(haven)
library(ggjoy)
library(cowplot)
library(ggplot2)
library(weights)
library(data.table)
library(scales)
library(tidyverse)
library(ggridges)
library(ggthemes)

get_env_data <- function(level='admin1', path_to_env='data/environmental/origin/'){

  if (level=='admin1'){
    # env_2015 <- data.table::fread(paste0(path_to_env, 'var_join_adm1_2015.csv.gz')) %>% as.data.frame()
    # env_2016 <- data.table::fread(paste0(path_to_env, 'var_join_adm1_2016.csv.gz')) %>% as.data.frame()
    # env_2017 <- data.table::fread(paste0(path_to_env, 'var_join_adm1_2017.csv.gz')) %>% as.data.frame()
    # env_2018 <- data.table::fread(paste0(path_to_env, 'var_join_adm1_2018.csv.gz')) %>% as.data.frame()
    env_2019 <- data.table::fread(paste0(path_to_env, 'var_join_adm1_2019.csv.gz')) %>% as.data.frame()
    env_2019[ ,c('n', 'mean', 'median', 'sd', 'quan10', 'quan25', 'quan75', 'quan90', 'TEMP_gsod')] <- list(NULL)
    # env_1 <- rbind.fill(env_2015, env_2016, env_2017, env_2018, env_2019)

    # Temporary:
    env <- env_2019
    env <- env %>%
      dplyr::rename(geo_id = OBJECTID, date = tweet_date)
    env$admin1 <- NULL
  }

  if(level=='admin2'){
    env <- data.table::fread(paste0(path_to_env, 'var_join_adm2_2019.csv.gz')) %>% as.data.frame()
    env <- env[, !names(env) %in% c("n", "mean", "median", "sd", "quan10", "quan25", "quan75", "quan90", "admin1", "NAME_1", "NAME_2")]
    env <- env %>%
      dplyr::rename(date = tweet_date, geo_id = admin2_id, country = ISO)
  }

  return(env)
}

get_all_data <- function(sent_df, env_df, lang=NULL){

  if(length(unique(sent_df$id_0))>1){
    country='global'
  }else{
    country=sent_df$id_0[1]
  }

  if("id_2" %in% colnames(sent_df)){
    geo_level='admin2'
  }else{
    geo_level='admin1'
  }

  sent_df$date <- make_date(sent_df$year, sent_df$month, sent_df$day)

  if(geo_level=="admin1"){
    geo_xwalk <- read.dbf('data/spatial/adm1.dbf')
    geo_xwalk <- geo_xwalk[c("OBJECTID", "ID_0", "ID_1", "ISO", "NAME_0", "NAME_1")]
    geo_xwalk <- geo_xwalk %>%
      dplyr::rename(country = ISO, geo_id = OBJECTID, admin1 = NAME_1, country_name = NAME_0)
  } else if(geo_level=="admin2"){
    geo_xwalk <- read.dbf('data/spatial/adm2.dbf')
    geo_xwalk <- geo_xwalk[c("OBJECTID", "ID_0", "ID_1", "ID_2", "ISO", "NAME_0", "NAME_1", "NAME_2")]
    geo_xwalk <- geo_xwalk %>%
      dplyr::rename(country = ISO, geo_id = OBJECTID, admin1 = NAME_1, admin2 = NAME_2, country_name = NAME_0)
  }
  names(geo_xwalk) <- tolower(names(geo_xwalk))

  if(country!='global'){
    geo_xwalk <- geo_xwalk[geo_xwalk$id_0==country,]
    env_df <- env_df[env_df$country==geo_xwalk$country[1],]
  }

  if(geo_level=="admin1"){
    dta <- merge(geo_xwalk, sent_df, by=c("id_0", "id_1"))
  } else if(geo_level=="admin2"){
    dta <- merge(geo_xwalk, sent_df, by=c("id_0", "id_1", "id_2"))
  }
  dta <- merge(dta, env_df, by=c("country", "geo_id", "date"))
  dta <- dta[is.na(dta$Tmax_mean)==FALSE,]

  if(geo_level=="admin1"){

    pop <- data.table::fread("data/geographic_features/population/Pop_Admin1_Nico.csv")
    pop$name_0 <- NULL
    pop$name_1 <- NULL

    income <- data.table::fread('data/geographic_features/income_level/income_level.tsv', sep = '\t') %>% as.data.frame()
    gdp <- data.table::fread('data/geographic_features/gdp_per_capita/admin1_gdp_per_capita.tsv', sep = '\t') %>% as.data.frame()

    dta <- merge(dta, pop, by=c('country', 'id_1'), all.x = TRUE)
    dta <- merge(dta, gdp, by=c('id_0', 'id_1'), all.x = TRUE)
    dta <- merge(dta, income, by=c('country'), all.x = TRUE)

    ## Read in Climate Zone data:
    cz <- read.csv("data/climatic_zone/climateType_adm1.csv") %>%
      dplyr::rename(geo_id = ADM1_id)

    cz_xwalk <- rjson::fromJSON(file='data/climatic_zone/xwalk.json')
    cz$cz3 <- cz_xwalk[cz$rs]
    cz$cz1 <- ifelse(cz$cz3=='Ocean', 'Ocean', substr(cz$cz3, 1, 1))
    cz$cz3 <- NULL
    cz$rs <- NULL

    dta <- merge(dta, cz, by=c('geo_id'), all.x = TRUE)
  }

  if ("ind_count" %in% colnames(dta)){
    cat(paste0("\n5% quantile of Individual Count: ", quantile(dta$ind_count, 0.05)))
    if(quantile(dta$ind_count, 0.05)>500 & geo_level=='admin1'){
      cat(" --- WARNING: Consider using admin2 level.")
    }
  }

  dta <- dta %>%
    dplyr::rename(tem = TLML_mean,  # TLML = surface air temperature
                  tmin = Tmin_mean, # Tmin = surface min air temperature
                  tmax = Tmax_mean, # Tmax = surface max air temperature
                  pm25 = SurfacePM25_mean,
                  win_s = SPEEDLML_mean, # surface wind speed
                  cldtot = CLDTOT_mean, # total cloud area fraction
                  rhu = QLML_mean, # surface specific humidity
                  pr = PR_mean) %>% # bias corrected total precipitation
    dplyr::mutate(tmax_2 = tmax * tmax,
                  DOW  =  lubridate::wday(date),
                  month=  lubridate::month(date),
                  week = week(date),
                  trange = tmax - tmin)

  return(dta)
}

felmCoeff <- function(felm, var = "cuttem", breaks = 5, omit = c(20, 25)){
  df <- summary(felm)$coefficients %>%
    as.data.frame()
  names(df) <- c("coef", "se", "t", "p")
  if(var != "all") {
    dt <- df[grepl(var, rownames(df)), ] # extract variable name (e.g., tmax.cut, ppt.cut, ...), limit to matches
    dt$rhs <- str_split_fixed(rownames(dt), "   ", n=2)[,1]
    dt <- as.data.table(dt)
    dt[p <= 0.05, star := "*"]
    dt[p <= 0.01, star := "**"]
    dt[p <= 0.001, star := "***"]
    dt[, rhs := gsub(paste0(var), "", rhs)]
    dt[, rhs := gsub("\\(|]", "", rhs)]
    dt[, rhs := gsub(".neg.", "-", rhs)]
    dt[, rhs := gsub(".pos.", "", rhs)]
    dt[, rhs := gsub(".diff", "", rhs)]
    dt[grepl(",", rhs), rhs := tstrsplit(rhs, ',')[2]] # if a factor variable, grab second column (upper limit)
    dt[, rhs := as.numeric(rhs)]
    dt <- dt[!is.na(rhs)]
    dt[, xmin := rhs - breaks]
    dt[, xmax := rhs]
    dt[, rhs := NULL]
    setnames(dt, c("coef", "se", "t", "p", "star", "xmin", "xmax"))
    dt[, xmin.lead := data.table::shift(xmin, type="lead")]
    dt[, xmax.lag := data.table::shift(xmax, type="lag")]
    dt[xmin==-Inf, xmin := xmin.lead - breaks]
    dt[xmax==-Inf, xmax := xmin.lead]
    dt[xmin==Inf, xmin := xmax.lag]
    dt[xmax==Inf, xmax := xmin + breaks]
    dt[, c('xmin.lead','xmax.lag') := NULL]
    # identify omitted category, add 0.
    dt <- rbind(dt, data.table(coef=0, se=0, t=0, p = 0, star = "", xmin=omit[1], xmax=omit[2]))
    dt[, ':='(xmid=(xmin+xmax)/2, ci.l=coef - se*1.96, ci.h=coef + se*1.96)] # add some stuff for plotting
    dt[, ':='(semin=coef - se, semax=coef + se)] # add some stuff for plotting
    dt[, range := paste(xmin, xmax, sep="-")] # create range variable for x labels
    dt[xmax == min(xmax), range := paste0("<=",min(xmax))] # set bottom of range
    dt[xmin == max(xmin), range := paste0(">",min(xmin))] # set top of range
    df <- as.data.frame(dt)
  }
  return(df)
}

binned_plot <- function(main.df, dsty.df,
                        fit.df, fit.plot = FALSE, fit.color = "black",
                        quantile.plot = FALSE, quantile = 0.95,
                        theme.color = "#2E9FDF",
                        ylabel = "Percentage change in park visitation",
                        xlabel = expression(paste("Hourly Temperature (", degree, C, ")")),
                        xlim = range(c(main.df$xmin, main.df$xmax)),
                        ylim = range(c(main.df$ci.l*100 - 2, main.df$ci.h*100 + 2)),
                        density.theme.color = "grey50",
                        density.theme.fill = "grey75",
                        density.theme.position = "bottom",
                        density.theme.high = 0.15,
                        label = NULL,
                        label.position = 0.01) {
  # format coef
  main.df <- main.df %>%
    mutate(star = as.character(star)) %>%
    mutate(star = ifelse(is.na(star), "", star)) %>%
    mutate(star_f = ifelse(nchar(star) > 0, "sig", "ns")) %>%
    mutate(star_f = factor(star_f, levels =  c("sig", "ns")))
  # main plot
  pmain <- ggplot(main.df, aes(x = xmid, y = coef*100)) +
    geom_hline(yintercept = 0, color="grey50", linetype = "dashed") +
    geom_line(size = 1.0, colour = theme.color) +
    geom_ribbon(aes(ymin = ci.l*100, ymax = ci.h*100), alpha = 0.30, fill = theme.color) +
    geom_point(aes(fill = star_f), colour = theme.color, size = 3, shape = 21, show.legend = FALSE) +
    annotate("text", x = -Inf, y = label.position, vjust = 0, hjust = -0.2,
             colour = theme.color, size = 6, label = label) +
    # geom_vline(xintercept = main.df$temp95[1], color = "red", linetype = "dashed", size=1)+
    ylim(ylim) +
    xlim(xlim) +
    ylab(ylabel) +
    xlab(xlabel) +
    theme_Publication()+
    scale_fill_manual(values = c(theme.color, "#FFFFFF"))

  if(fit.plot)
    pmain <- pmain +
      geom_line(data=fit.df, aes(x=xx,y=pred*100), color= fit.color, size=1, alpha=0.8, linetype =2)

  if(quantile.plot)
    pmain <- pmain +
      geom_vline(xintercept = quantile(dsty.df$tem, c(0.95)), color = "red", linetype = "dashed", size=1)

  # density plot
  xdsty <- axis_canvas(pmain, axis = "x", data=dsty.df, aes(x=tem)) +
    geom_histogram(binwidth = 1, alpha=0.5, size=0.5, colour = density.theme.fill, fill = density.theme.fill) +
    # geom_histogram(binwidth = 1, alpha=0.3, size=0.5, colour = "grey75", fill = NA) +
    # geom_density(aes(y=1 * ..count..), alpha=0.3, size=0.5, colour = density.theme.color, fill = density.theme.fill) +
    # geom_line(stat="density", alpha=1, size=0.5, colour = density.theme.color) +
    geom_vline(xintercept = quantile(dsty.df$tem, c(0.95)), color = "red", linetype = "dashed", size=1)+
    expand_limits(y = - nrow(dsty.df)/100)
  # mosaic two parts
  p.mosaic <- insert_xaxis_grob(pmain, xdsty, grid::unit(density.theme.high, "null"), position = density.theme.position)
  return(p.mosaic)
}

get_countries <- function(sentiment_global){
  countries <- sentiment_global[sentiment_global$country!=0,]
  countries <- unique(countries$country)
  cat(paste0(length(countries), " countries with data"))
  return(countries)
}

read_in_sentiment_data <- function(country, geo_level, by_lang=FALSE, lang=NULL){
  
  if(!is.null(lang)){
    sentiment <- read.csv(
      paste0("data/sentiment/bert_", tolower(country), "_", geo_level, "_day", if(by_lang) {'_by_lang'} else {''}, "_gscc.tsv"),
      sep='\t', stringsAsFactors = F
    )
    sentiment <- sentiment[sentiment$lang==lang,]
  }else if(geo_level=='admin1'){
    sentiment <- read.csv(
      paste0("data/sentiment/bert_global_admin1_day_gscc.tsv"),
      sep='\t', stringsAsFactors = F
    )
    if(country!='global'){
      sentiment <- sentiment[sentiment$country==country,]
    }
  }else{
    sentiment <- read.csv(
      paste0("data/sentiment/bert_", tolower(country), "_", geo_level, "_day", if(by_lang) {'_by_lang'} else {''}, "_gscc.tsv"),
      sep='\t', stringsAsFactors = F
    )
  }
  return(sentiment)
}
