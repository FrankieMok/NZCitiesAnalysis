## NZCitiesAnalysis
	## Research on the relationship between public transport services 
	## and infrastructure expenditures of urban development in New Zealand.


## Getting ready, clear memory and console
rm(list=ls())
cat("\14")

# all used libraries / packages
library(data.table)
library(readxl)
library(ggplot2)
library(BBmisc)
library(reshape2)
library(treemapify)
library(sp)
library(spData)
library(tmap)

## Download the dataset from data.gov.nz with excel format then import sheet - Breakdown
target = "http://www.nzta.govt.nz/assets/userfiles/transport-data/FundRoadMOR.xls"
dest = 'FundRoadMOR.xls'

download.file(url = target, destfile = dest, mode='wb')
dataset <- read_excel("FundRoadMOR.xls", sheet = "Breakdown")

# Briefly Check the dataset
str(dataset)
head(dataset)
tail(dataset)

# Update the header Name
colnames(dataset) <- c("Region", "EG", "FA", "y07", "y08", "y09", "y10", "y11", "y12", "y13", "y14", "y15", "y16")
   ## Update the others from chr to num 
for (i in 4:13) {
  dataset[[i]] <- as.numeric(dataset[[i]])
}
# update the data class and group by levels
## factor the levels
dataset$Region <- factor(dataset$Region)
dataset$EG <- factor(dataset$EG)
dataset$FA <- factor(dataset$FA)

# Briefly check the dataset again
str(dataset)
head(dataset)
tail(dataset)
rowSums(is.na(dataset)==T)
is.null(dataset)

plot(dataset$y07)

##  grouping the dataset and confirm ation
dataset.nz <-subset(dataset, dataset$Region == "NZ")
dataset.region <- dataset[grep('Region',dataset$Region),]
dataset.others <- dataset[!grepl('Region',dataset$Region),]
dataset.others <- dataset.others[!grepl('NZ',dataset.others$Region),]
dataset.city <- dataset[grep('City',dataset$Region),]

 # To confirm the information are correct, all different groups has the same total value.
colSums(dataset.nz[4:13],na.rm=T)
colSums(dataset.region[4:13],na.rm=T)
colSums(dataset.others[4:13],na.rm=T)



## Update the unit from dollar to million dollar
dataset.nz[,4:13] <- dataset.nz[,4:13]/1000000
dataset.region[,4:13] <- dataset.region[,4:13]/1000000
dataset.others[,4:13] <- dataset.others[,4:13]/1000000
dataset.city[,4:13] <- dataset.city[,4:13]/1000000
attach(dataset.region) 



## Plot the value in 
datasetn.nz <- na.omit(dataset.region)
datasetn.region <- na.omit(dataset.region)
datasetn.others <- na.omit(dataset.others)
datasetn.region.m <- normalize(datasetn.region, method = 'standardize', range = c(0,1))
EG.levles <- levels(dataset.region$EG)
Region.levels <-factor(datasetn.region$Region)

# NZTA vs Local from 2007 ~ 2017
dataset.nz.nzta <- subset(dataset.nz, dataset.nz$FA == "NZTA")
dataset.nz.local <- subset(dataset.nz, dataset.nz$FA == "Local")

dataset.nz.fa <- as.data.frame(colSums(dataset.nz.nzta[,4:13],na.rm=T))
dataset.nz.fa <- cbind(dataset.nz.fa, as.data.frame(colSums(dataset.nz.local[,4:13],na.rm=T)))
dataset.nz.fa <- cbind(c(1:10),dataset.nz.fa)
colnames(dataset.nz.fa) <- c("yrs", "nzta", "local")

dataset.na.fa.long <- melt(dataset.nz.fa, id = "yrs")

ggplot(data=dataset.na.fa.long,
       aes(x=yrs, y=value, colour=variable)) +
  geom_line() + xlab("Years") + ylab(" NZD (million dollars)") +
  theme(axis.text.x = element_text(size = 12, face = "plain"),
        axis.text.y = element_text(size = 12, face = "plain"),
        legend.title=element_text(size=16), 
        legend.text=element_text(size=16)) +
  scale_x_discrete(name =" Years", limits=c("2007","","2009","","2011","", "2013","", "2015","", ""))


# 2016/2017's Expenditure Group distrubtion in each region   
ggplot(datasetn.region, aes(x = EG, y = y16, fill = Region)) +
  geom_bar(stat="identity") + theme(legend.position="bottom",legend.text=element_text(size=10)) + 
  ggtitle("Area distribution in Expenditure Group 2016/17") +
  theme(axis.text.x = element_text(size = 16, face = "plain"),
        axis.text.y = element_text(size = 16, face = "plain"),
        legend.title=element_text(size=12), 
        legend.text=element_text(size=12)) 

# Expenditure Group distrubtion from 2007 ~ 2017
datasetn.region.EG.yrs <- melt(datasetn.region, id.vars='EG', measure.vars= c('y07','y08','y09', 'y10', 'y11', 'y12', 'y13', 'y14', 'y15', 'y16'))
ggplot(datasetn.region.EG.yrs, aes(x = EG,y = value)) + 
  geom_bar(aes(fill = variable),stat = "identity",position = "dodge") + 
  scale_y_log10() +
  theme(axis.text.x = element_text(size = 16, face = "plain"),
        axis.text.y = element_text(size = 16, face = "plain"),
        legend.title=element_text(size=12), 
        legend.text=element_text(size=12)) 


# Region distrubtion from 2007 ~ 2017
datasetn.region.m.region.yrs <- melt(datasetn.region.m, id.vars='Region', measure.vars= c('y07','y08','y09', 'y10', 'y11', 'y12', 'y13', 'y14', 'y15', 'y16'))
ggplot(datasetn.region.m.region.yrs, aes(x = Region,y = value)) + 
  geom_boxplot(aes(x= Region, y= value, color=variable)) +
  theme(axis.text.x = element_text(size = 14, angle=90, hjust=1, face = "plain"),
        axis.text.y = element_text(size = 14, face = "plain"),
        legend.title=element_text(size=12), 
        legend.text=element_text(size=12))

Region.levels <- factor(datasetn.region$Region)
region.value <- NULL
# Calculate the Expenditure in different region at 2016/17
for (i in 1:14) {
  region.value[i] <- colSums(subset(datasetn.region, datasetn.region$Region == levels(Region.levels)[i])[,13])
}

nzmap <- nz
nzmap$Expenditure[1] <- region.value[8]
nzmap$Expenditure[2] <- region.value[1]
nzmap$Expenditure[3] <- region.value[12]
nzmap$Expenditure[4] <- region.value[2]
nzmap$Expenditure[5] <- region.value[4]
nzmap$Expenditure[6] <- region.value[5]
nzmap$Expenditure[7] <- region.value[11]
nzmap$Expenditure[8] <- region.value[6]
nzmap$Expenditure[9] <- region.value[13]
nzmap$Expenditure[10] <- region.value[14]
nzmap$Expenditure[11] <- region.value[3]
nzmap$Expenditure[12] <- region.value[9]
nzmap$Expenditure[13] <- region.value[10]
nzmap$Expenditure[14] <- region.value[7]
nzmap$Expenditure[15] <- region.value[7]
nzmap$Expenditure[16] <- region.value[7]

ggplot() + geom_sf(data = nzmap, aes(fill = Expenditure)) +
  geom_sf(data = nz_height) +
  scale_x_continuous(breaks = c(170, 175)) + 
   ggtitle(" 2016/17 Region distrubtion ")


# Environment & Drainage in Expenditure Group from 2007 ~ 2017
datasetn.region.m.ed <- subset(datasetn.region.m, datasetn.region.m$EG == EG.levles[3])
datasetn.region.m.ed.box <- melt(datasetn.region.m.ed, id.vars='Region', measure.vars= c('y07','y08','y09', 'y10', 'y11', 'y12', 'y13', 'y14', 'y15', 'y16'))
ggplot(datasetn.region.m.ed.box) + geom_boxplot(aes(x= Region, y= value, color=variable)) +
  theme(axis.text.x = element_text(size = 14, angle=90, hjust=1, face = "plain"),
        axis.text.y = element_text(size = 14, face = "plain"),
        legend.title=element_text(size=12), 
        legend.text=element_text(size=12))

# Network & Property Management in Expenditure Group from 2007 ~ 2017
datasetn.region.m.np <- subset(datasetn.region.m, datasetn.region.m$EG == EG.levles[4])
datasetn.region.m.np.box <- melt(datasetn.region.m.np, id.vars='Region', measure.vars= c('y07','y08','y09', 'y10', 'y11', 'y12', 'y13', 'y14', 'y15', 'y16'))
ggplot(datasetn.region.m.np.box) + geom_boxplot(aes(x= Region, y= value, color=variable)) +
  theme(axis.text.x = element_text(size = 14, angle=90, hjust=1, face = "plain"),
        axis.text.y = element_text(size = 14, face = "plain"),
        legend.title=element_text(size=12), 
        legend.text=element_text(size=12))

city.levels <- factor(datasetn.others$Region)

# prepare the Treemap to show the main city information
city.value <- data.frame(id = integer(0), yrs = numeric(0))
# Calculate the Expenditure in different region at 2016/17
for (i in 1:80) {
  city.value <- rbind(city.value,data.frame(id = levels(city.levels)[i], yrs = colSums(subset(datasetn.others, datasetn.others$Region == levels(city.levels)[i])[,13])))
}
ggplot(city.value, ggplot2::aes(area = yrs, fill = id, label = id)) +
  geom_treemap() + theme(legend.position="") +
  geom_treemap_text(colour = "white", place = "topleft", reflow = T)


# Total Expenditure per year
totexp <- as.data.frame(colSums(dataset.region[,4:13], na.rm = T))
colnames(totexp) <- "Expenditure"
totexp <- cbind(yrs = c(1:10),totexp)
tot.lm <- lm(Expenditure~.,totexp)
totexp <- cbind(totexp, predict = c("p","p","p","p","p","p","p","p","p","p"))
levels(totexp$predict) <- c("p","n")
totexp <- rbind(totexp, c(11, (tot.lm$fitted.values[10]+tot.lm$coefficients[2]), "n"))
totexp <- rbind(totexp, c(12, (tot.lm$fitted.values[10]+tot.lm$coefficients[2]*2), "n"))
plot(Expenditure ~yrs, totexp, main="Total Expenditure per year", col=totexp$predict, pch=0, xaxt="n")
axis(1, at = seq(1, 12, by = 1), labels = c("2007","","","","2011","","","","2015","","","2017"), font = 2)
abline(tot.lm,totexp)
