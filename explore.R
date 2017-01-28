library(tm)
library(stringr)
library(timevis)
library(animation)
library(mapdata)
# 
# Some ideas worth exploring:
#   
# • How many people got killed and injured per year in last 12 years?
# 
# • How many attacks involved killing of actual terrorists from Al-Qaeeda and Taliban?
# 
# • How many attacks involved women and children?
# 
# • Visualize drone attacks on timeline
# 
# • Find out any correlation with number of drone attacks with specific date and time, for example, do we have more drone attacks in September?
# 
# • Find out any correlation with drone attacks and major global events (US funding to Pakistan and/or Afghanistan, Friendly talks with terrorist outfits by local or foreign government?)
# 
# • The number of drone attacks in Bush Vs Obama tenure?
# 
# • The number of drone attacks versus the global increase/decrease in terrorism?
# 
# • Correlation between number of drone strikes and suicide bombings in Pakistan

df = read.csv("PakistanDroneAttacks.csv", stringsAsFactors = TRUE)
df = df[-c(nrow(df)),]

df$S. = NULL
df$City = as.factor(tolower(as.character(df$City)))

df$year = as.factor(str_sub(df$Date, start = -4))

min_death_agg = aggregate(Total.Died.Min ~ year, data = df, FUN = sum)
barplot(height = min_death_agg$Total.Died.Min, horiz = FALSE, names.arg = min_death_agg$year, las = 2, space = 1, main = "Count of People Killed - Minimum Estimate", xlab = "Year", ylab = "Count", border = "blue")

max_death_agg = aggregate(Total.Died.Mix ~ year, data = df, FUN = sum)
barplot(height = max_death_agg$Total.Died.Mix, horiz = FALSE, names.arg = max_death_agg$year, las = 2, space = 1, main = "Count of People Killed - Minimum Estimate", xlab = "Year", ylab = "Count", border = "blue")

strikes = subset(df, (!(is.na(df$Al.Qaeda)) | !(is.na(df$Taliban))))
100*(nrow(strikes)/nrow(df))

100*(nrow(subset(df, df$Women.Children == "Y"))/nrow(df))

table(df$City)

date <- gsub(".*y, ", "",df$Date)
date <- gsub(",", "",date)

months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
mnum <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

for (i in (seq_along(months))) date <- gsub(months[i], mnum[i], date)
df$Date <- as.Date(date, "%m %d %Y")
 
# tv_df = data.frame(id = 1:nrow(df), content = df$City, start = date, end = rep(NA, nrow(df)))
# timevis(tv_df)

i = 2004
saveGIF(while (i <= 2016) {
  print(i)
  country_year = subset(df, as.numeric(as.character(df$year)) == i)
  print(nrow(country_year))
  cities <- data.frame(lon=country_year$Longitude, lat=country_year$Latitude)
  
  map("world2Hires", "pakistan")
  points(cities$lon, cities$lat, col="red", pch=20)
  title(paste("Pakistan -- Drone Strikes in ", i, sep=""))
  
  i = i+1
  
}, movie.name = "pakistan.gif", interval = 3.5, convert = "convert", ani.width = 1000, ani.height = 800)

