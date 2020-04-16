library(readr)
library(dplyr)
library(ggplot2)
library(ggridges)
library(highcharter)
library(plyr)
library(lubridate)
library(fmsb)
library(gridExtra)
library(cmna)
library(tidyselect)
library(factoextra)
library(psych)
library(gvlma)
library(MASS)
library(NbClust)
library(GGally)
library(car)

top10s <- read.csv("C:\\Users\\Apurva Sarode\\Desktop\\Spotify_mva.csv",header = TRUE)
View(top10s)
Data <- top10s

#----------------------------DATA CLEANING -------------------------------- #

#finding missing data
dim(Data)
any(Data$bpm==0)
any(Data$pop==0)
Data = filter(Data, bpm != 0)
Data = filter(Data, pop != 0)
dim(Data)

#reordering columns
Data <- Data[,c(1,2,3,4,5,6,12,7,8,9,10,11,13,14,15)]
View(Data)

#speechiness also include podcast and speeches
#For songs speechiness will be low and inaccurate, hence removing spch
Data$spch <- NULL
#Liveness includes lives shows which are also inaccurate to test songs
#in a recording studio, Hence removing live
Data$live <- NULL

#Renaming columns to a more readable format
colnames(Data)[4] <- "Genre"
colnames(Data)[7] <- "Duration"
colnames(Data)[8] <- "Energy"
colnames(Data)[9] <- "Dancebility"
colnames(Data)[10] <- "Loudness"
colnames(Data)[11] <- "Valence"
colnames(Data)[12] <- "Acoustiveness"
colnames(Data)[13] <- "Popularity"


#Normalizing Loudness
x  = Data$Loudness
normalized = (x-min(x))/(max(x)-min(x))
loud = normalized * 100
rounded_loud = round(loud, digits=0)
Data$Loudness = rounded_loud

#Creating the Dependannt Variable Rating based on the popularity given by Spotify
y = Data$Popularity
shapiro.test(y)
qqnorm(y)
qqline(y, col=2)

#we dont see normal distribution hence we cannot split the data in quartiles equally
#Instead we divide by average
mean(y)
max(y)
Rating <- cut(y, breaks = c(0,67,99),
              labels = c("Below Average", "Above Average"), 
              right = FALSE, include.lowest = TRUE)
Data['Rating'] <- Rating
View(Data)

summary(Data)

#-----------------------------Exploratory Data Analysis-------------------------------------------#
#Exploring Genres
gen = count(Data$Genre)
gen_dsc = gen[order(-gen$freq),]
gen10 = gen_dsc[1:10,]
barplot(gen10$freq, names.arg = gen10$x,main = 'Top 10 Genres',xlab = 'Genre',ylab = 'No. of songs')

years1 = Data[Data$year == c(2010),4:5]
gen1 = count(years1$Genre)
gen1 = gen1[order(-gen1$freq),]

years2 = Data[Data$year == c(2011),4:5]
gen2 = count(years2$Genre)
gen2 = gen2[order(-gen2$freq),]

years3 = Data[Data$year == c(2012),4:5]
gen3 = count(years3$Genre)
gen3 = gen3[order(-gen3$freq),]

years4 = Data[Data$year == c(2013),4:5]
gen4 = count(years4$Genre)
gen4 = gen4[order(-gen4$freq),]

years5 = Data[Data$year == c(2014),4:5]
gen5 = count(years5$Genre)
gen5 = gen5[order(-gen5$freq),]

years6 = Data[Data$year == c(2015),4:5]
gen6 = count(years6$Genre)
gen6 = gen6[order(-gen6$freq),]

years7 = Data[Data$year == c(2016),4:5]
gen7 = count(years7$Genre)
gen7 = gen7[order(-gen7$freq),]

years8 = Data[Data$year == c(2017),4:5]
gen8 = count(years8$Genre)
gen8 = gen8[order(-gen8$freq),]

years9 = Data[Data$year == c(2018),4:5]
gen9 = count(years9$Genre)
gen9 = gen9[order(-gen9$freq),]

years10 = Data[Data$year == c(2019),4:5]
gen10 = count(years10$Genre)
gen10 = gen10[order(-gen10$freq),]

plot1 <- ggplot(gen1, aes(x="", y=gen1$freq, fill=gen1$x)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void() + scale_fill_discrete(name = "Top Genre of 2010")

plot2 <- ggplot(gen2, aes(x="", y=gen2$freq, fill=gen2$x)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void() + scale_fill_discrete(name = "Top Genre of 2011")

plot3 <- ggplot(gen3, aes(x="", y=gen3$freq, fill=gen3$x)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void() + scale_fill_discrete(name = "Top Genre of 2012")

plot4 <- ggplot(gen4, aes(x="", y=gen4$freq, fill=gen4$x)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void() + scale_fill_discrete(name = "Top Genre of 2013")

plot5 <- ggplot(gen5, aes(x="", y=gen5$freq, fill=gen5$x)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void() + scale_fill_discrete(name = "Top Genre of 2014")

plot6 <- ggplot(gen6, aes(x="", y=gen6$freq, fill=gen6$x)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void() + scale_fill_discrete(name = "Top Genre of 2015")

plot7 <- ggplot(gen7, aes(x="", y=gen7$freq, fill=gen7$x)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void() + scale_fill_discrete(name = "Top Genre of 2016")

plot8 <- ggplot(gen8, aes(x="", y=gen8$freq, fill=gen8$x)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void() + scale_fill_discrete(name = "Top Genre of 2017")

plot9 <- ggplot(gen9, aes(x="", y=gen9$freq, fill=gen9$x)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void() + scale_fill_discrete(name = "Top Genre of 2018")

plot10 <- ggplot(gen10, aes(x="", y=gen10$freq, fill=gen10$x)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void() + scale_fill_discrete(name = "Top Genre of 2019")

grid.arrange(plot1, plot2,plot3,plot4,plot5 ,ncol=5)

grid.arrange(plot6,plot7,plot8,plot9,plot10 ,ncol=5)


#Checking if there is a optimal duration for a song
dur_data = Data[,c(1,2,7,13,14)]
durmin = round(dur_data$Duration/60, digits=1)
dur_data$Duration<- durmin #minute(period)
dur_data
ggplot(dur_data, aes(x=dur_data$Duration, y=dur_data$Popularity,color=dur_data$Rating)) +
  geom_point()+ labs(y = 'Popularity', x = "Duration in mins", title = "Duration vs popularity and Rating")

#--------------------
props = Data[,c(8:13)]

nrow(props)
colMeans(props)
var(props)
density_data <- data.frame(
  name=c("Energy","Dancebility","Loudness",
         "Valence","Acoustiveness","Popularity"),
  value=c( rnorm(598, 70, 256), rnorm(598, 64, 172), rnorm(598, 73, 171),
           rnorm(598, 52, 504), rnorm(598, 14, 433), rnorm(598,67,175))
)

ggplot(density_data, aes(x = value, y = name, fill = name)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")

ggplot(density_data, aes(x=value, fill=name)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity',bins=15) +
  labs(fill="") + facet_wrap(~name)

#------------------------------------- Statistical Tests -------------------------#
Data_num = Data[,6:13]
Corr_mat = cor(Data_num)
Corr_mat
heatmap(Corr_mat)

# T-Test on dataset columns Duration and Popularity
t.test(Data$Duration,Data$Popularity, var.equal = TRUE, paired=FALSE)
# p-value is <2.2e-16 which is very less and hence we reject the null hypothesis



with(Data,t.test(Energy,Valence))
with(Data,t.test(Valence,Dancebility))
with(Data,t.test(Energy,Acoustiveness))
# (Energy, Valence), (Valence,Dancebility) and Energy,Acoustiveness have very low p-value
# as seen from heat map earlier it has significant correlation and hence we reject the null
# hypothesis for these audio properties.

with(Data,t.test(Popularity,Duration))
with(Data,t.test(Popularity,Energy))
with(Data,t.test(Popularity,Dancebility))
with(Data,t.test(Popularity,Loudness))
with(Data,t.test(Popularity,Valence))
with(Data,t.test(Popularity,Acoustiveness))
# Checking the relation between dependent variable Popularity and different audio properties as independent variables
# we found out that the p-value is very low for all the t-test conducted between Popularity
# and independent variable and hence we reject the null hypothesis stating there is significant 
# relationship between dependent and independent variables.

#------------------------------------ PCA --------------------------------------------#
pca_data = Data
props = pca_data[,c(8:12)]   #taking out only numeric variables

#Correlation between different Variables
#We see that the correlation coefficient for popularity with other 
#variables is very low. Thus we will see if our audio properties are correlated  
corr = cor(props)
corr

pca <- prcomp(props, scale=TRUE, center=TRUE)
summary(pca)
pca$rotation
pca$x

fviz_screeplot(pca, type='bar',main='Scree plot')

pca$rotation <- -pca$rotation
fviz_pca_biplot(pca, col.var = "Blue", habillage=pca_data$Rating)

#--------------------------------------K Means Clustering------------------------------------#
Data_clust = props
#scaling the data and finding generalized euclidean distance
scale_data = scale(Data_clust)
scale_data
dist_data = dist(scale_data,method ="euclidean")
dist_data

#As we have a column of rating which classifies the songs from 1-5. We can assume that K = 5
(kmeans5 <- kmeans(scale_data,5,nstart = 20))
kmeans5
kmeans5$centers


kmeans5$cluster <- as.factor(kmeans5$cluster)
ggplot(Data_clust, aes(Acoustiveness,Loudness+Energy,color = kmeans5$cluster)) + geom_point()

#To validate our assumption we took the help of the nbclust function t find optimal no. of clusters
nb_clust = NbClust(Data_num, distance="euclidean", method = 'kmeans')
nb_clust

# for 2 clusters
(kmeans2 <- kmeans(scale_data,2,nstart = 10))
kmeans2
kmeans2$cluster <- as.factor(kmeans2$cluster)

kmeans2$centers
kmeans2$withinss
kmeans2$size
ggplot(Data_clust, aes(Loudness+Energy,Acoustiveness,color = kmeans2$cluster)) + geom_point()
ggplot(Data_clust, aes(Loudness+Energy,Dancebility+Valence,color = kmeans2$cluster)) + geom_point()
ggplot(Data_clust, aes(Acoustiveness,Dancebility+Valence,color = kmeans2$cluster)) + geom_point()

#--------------------------------------- Factor Analysis -----------------------------------#
fa <- principal(props, nfactors=2, rotate="varimax")
fa
fa$loadings
fa$communality
fa$scores

fa.parallel(props)
fa.plot(fa)
fa.diagram(fa) #Visualization of the factors between relationships

#-------------------------------Linear Regression------------------------------------------
# split data into train and test
set.seed(101)
sample_n(Data,10)

# Lets take a sample of 75/25 like before. Dplyr preserves class.
training_sample <- sample(c(TRUE, FALSE), nrow(Data), replace = T, prob = c(0.75,0.25))
train <- Data[training_sample, ]
test <- Data[!training_sample, ]

fit<- lm(Popularity~Duration+Energy+Dancebility+Loudness+Valence+Acoustiveness,data = train)
summary(fit)
coefficients(fit)
confint(fit,level=0.95)
# Predicted Values
fitted(fit)
residuals(fit)
#Anova Table
anova(fit)
vcov(fit)

step <- stepAIC(fit, direction="both")

step$anova # display results
fit6 <- lm(Popularity~Energy+Loudness+Duration+Dancebility,data = train)
summary(fit6)

attach(Data)
fc= predict.lm(fit6,data.frame(Duration=189,Energy=32,Loudness = 62,Dancebility=64))
fc

#----------------------------------------------------------------------------------------
d_g = Data[Data$Genre == 'pop' & Data$year == c(2019),c(4,7:13)]
ft_g = lm(Popularity~Energy+Dancebility+Loudness+Valence+Acoustiveness, data= d_g)
summary(ft_g)

fc_g= predict.lm(ft_g,data.frame(Acoustiveness=32,Loudness = 62,Dancebility=64,Valence=59,Energy=70))
fc_g

#diagnostic plots
plot(ft_g)
# Assessing Outliers
outlierTest(ft_g)
qqPlot(ft_g, main="QQ Plot")

# added variable plots
avPlots(ft_g)
# distribution of studentized residuals
sresid <- studres(ft_g)
hist(sresid, freq=FALSE,
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit)



