# input grid population
# gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec.tif

library(dplyr)

df = read.csv("C:\\Users\\hello\\Desktop/temp.csv")

names(df)[1] = "country"
countries = df$country %>% unique()

df = df %>% 
  group_by(country, id_1) %>%
  summarise(population = sum(population, na.rm = T))

df_o = read.csv("E:\\Dropbox\\GlobalSentiment\\data\\geographic_features\\population/Pop_Admin1.csv")

countries_o = df_o$country %>% unique()

sel = countries[!(countries %in% countries_o)]

df.sel = filter(df, country %in% sel)
df.f = rbind(df_o, df.sel)

df.f <- df.f %>% 
  group_by(country, id_1) %>%
  summarise(population = sum(population, na.rm = T))

write.csv(df.f, "E:\\Dropbox\\GlobalSentiment\\data\\geographic_features\\population/Pop_Admin1_final.csv")

