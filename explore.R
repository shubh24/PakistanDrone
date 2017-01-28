library(tm)
library(stringr)
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
