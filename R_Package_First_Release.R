##########################################################################
#
# Author: Tomaz Kastrun
# Blog: http://tomaztsql.wordpress.com
# Git: https://github.com/tomaztk/First_Release_Dates_R_Packages
# Twitter: @tomaz_tsql
#
# Desc: Getting R Packages updates dates, frequency and initial dates 
#       of package releases
#
# Update:
#         24.January 2022
##########################################################################


### Get Packages Last updates ####

library(rvest)
#install.packages("rvest")
library(ggplot2)

url = 'https://cran.r-project.org/web/packages/available_packages_by_date.html'

CRANpage <- read_html(url)
tbls <- html_nodes(CRANpage, "table") # since HTML is in table; no need to scrape td/tr elements
table1 <- html_table(tbls[1], fill = TRUE)
dd <- data.frame(table1[1])

#house cleaning
dd$Date <- as.Date(dd$Date)


### simple graph
#General view
ggplot(dd, aes(x=Date))  +
  geom_dotplot(binwidth =12) + 
  labs(x = "Dates", y = "Number of packages updates by Year of last update") +
  scale_x_date(date_breaks= "2 years", date_labels = "%Y/%m", limits = as.Date(c("2005-01-01", "2022-01-25"))) 



#create stats for years
library(dplyr)
library(lubridate)


# by years (buckets)
dd %>%
  mutate( PYear= year(Date)) %>%
  select (PYear) %>%
  group_by(PYear) %>%
  summarise(
    nof = n()
  ) %>%
  ggplot(aes(x=PYear)) + geom_bar(aes(weight=nof))

# updates by year
dd_y <- dd %>%
  mutate( PYear= year(Date)) %>%
  select (PYear) %>%
  group_by(PYear) %>%
  summarise(
    nof = n()
  )

#total
sum(dd_y$nof)

#running cumulative
cumsum(dd_y$nof)
#or even better
dd_y %>% 
  mutate(cumsum = cumsum(nof)
         ,percY = nof/cumsum(nof)
         ,percC = cumsum(nof)/sum(nof))


#simple correlation
cor(dd_y)[1,2]

dd_ym <- dd %>%
  mutate( PYear= year(Date)
          ,month_name = month(Date, label = FALSE)) %>%
  select (PYear,month_name) %>%
  group_by(PYear,month_name) %>%
  summarise(
    nof = n()
  )


cor(dd_ym)[1,2]

#check distribution over months
dd_ym2010 <- dd_ym %>%
  filter(PYear > 2010 & PYear < 2023)

boxplot(dd_ym2010$nof~dd_ym2010$month_name, main="R Packages update over months", xlab = "Month", ylab="Number of Packages")

cor(dd_ym2010)[2,3]


###########################
### Get initial Dates #####
###########################

#str(dd)
rm(packageNames)
packageNames <- dd$Package

# rm(df_first)
df_first <- data.frame(name=c("TK_NA"),firstRelease=c(as.Date("1900-12-31")), nofUpdates=c(0))

for (i in 1:length(packageNames)){
  print(i)
  url1 <- 'https://cran.r-project.org/src/contrib/Archive/'
  #name1 <- 'airportr'
  name1 <- packageNames[i]
  url2 <- paste0(url1,name1,'/')
  
  ifErrorPass <- tryCatch(read_html(url2), error=function(e) e)    
  if(inherits(ifErrorPass, "error")) next # if package does not have archive!!!
  
  cp <- read_html(url2)
  t2 <- html_nodes(cp, "table") 
  t2 <- html_table(t2[1], fill = TRUE)
  rm(list = Filter(exists, c("dd2")))
  dd2 <- data.frame(t2[1])
  dat <- dd2$Last.modified
  dat <- as.Date(dat, format = '%Y-%m-%d')
  firstRelease <- dat[order(format(as.Date(dat),"%Y%m%d"))[1]]
  numberOfUpdates <- length(dat)  
  df_first <- rbind(df_first,data.frame(name=name1,firstRelease=as.Date(firstRelease, format='%Y-%m-%d'),nofUpdates=numberOfUpdates))
}

#clean
myData = df_first[df_first$firstRelease > '1900-12-31',]


# add missing packages that did not fall into archive folder on CRAN

myDataNonArchive <- dd$Package[!dd$Package %in% myData$name]
myDataNonArchive2 <- cbind(dd[dd$Package %in% myDataNonArchive,c(2,1)],1)

names(myData) <- c("Name","firstRelease","nofUpdates")
names(myDataNonArchive2) <- c("Name","firstRelease","nofUpdates")

finalArchive <- data.frame(rbind(myData, myDataNonArchive2))

#packages based on first release date
#ggplot(finalArchive, aes(x=year(finalArchive$firstRelease))) + geom_dotplot(binwidth = 0.007)


hist(year(finalArchive$firstRelease),
     main = paste("Histogram of First year of R Package Release")
     ,xlab="Year",ylab="Number of Packages"
     ,col="lightblue", border="Black"
     ,xlim = c(1995, 2025), las=1, ylim=c(0,10000))

#stats:
finalArchiveG<- finalArchive %>%
  group_by(year(finalArchive$firstRelease)) %>%
  summarise(
    nof_packages = n()
    ,numberOfUpdates = sum(nofUpdates))

finalArchiveG
