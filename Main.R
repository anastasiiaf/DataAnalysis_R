########################################################################################
#####                           IMPORTANT INFORMATION                              #####
########################################################################################
# In my program I differentiate between task 1 and 2: data for them stored in separate 
# data frames. When calling function clean.data(final_df, task), user needs to specify 
# the task number:
#  1 - prepare data for task 1
#  2 - prepare data for task 2
# I tried to make my program suitable for both tasks: one can use instruments with 
# type 1 and conduct k-means, hierarchical k-means clustering algorithms or  
# Hidden Markov model. Moreover, in task 1 (unsupervised machine learning) 
# I decided to use threemodels,thus, user has to specify a model type
# (functions check whether user`s inputs are right for certain function):
# 1 - all instruments are independent variables (90 for types 64 and 1024, 
#     and 101 for type 1)
# 2 - average of all returns  and standard deviation (between instruments) of securities
#     on a particular date, 2 independent variables
# 3 - group instruments by type 64 and 1024, take their average return and standard 
#     deviation (only valid for task 1 due to grouping), 4 independent variables

# I used k-means,  hierarchical k-means clustering and tried to infer transition 
# probabilities from Hidden Markov Model
# Program consists of three R scripts:
# "Main.R"  - from here I call all my functions to make analysis on data 
# "Data preparation.R" - cleaning data and taking log-returns from prices
# "Functions.R"   - contains all customazed functions I used for task 1 and task 2
# NOTE: Please, use csv documents I sent: I manipulated with date format in 'daily_data'
# (I just splitted date in day, month and year, and then assembled it again), and in
# 'factset_lse' I already separated data by ";" in Excel

########################################################################################
#####                     DATA EXTRACTION AND PREPARATION                          #####
########################################################################################

# NOTE: this part of code until TASK 1 header takes appr 10 min to process

library(lubridate)
ex <- read.csv('daily_data.csv', stringsAsFactors=FALSE)
# delete all rows where close price is NA
ex <- ex[!is.na(ex$last), ] 
df <- data.frame(ex)
df$date <- dmy(df$date)
ticker <- df[[1,2]]
type.instr <- df[[1,3]]
interim.df <- data.frame()

# create column which contains all dates from 01/01/2014 until 18/05/2017
date.initial <- seq(as.Date("2014-01-01"), as.Date("2017-05-18"), by="days")
final.df <- data.frame(date.initial)

# transform the initial data table: rows - instruments, columns - all dates 
# from 01/01/2014 to 18/05/2017
kol <- 1
for (i in 1:nrow(df)){
#select rows with the same security  
  if (df[i,'ticker'] == ticker & i < nrow(df)){   
    interim.df <- rbind(interim.df, df[i, c( 'date', 'last')]) 
  }
# allocate security daily closing price data to the corresponding date
# additionally, to each instrument name I addad its type: _64, _1024 or _1
  else  {  
    kol <- kol+1
    final.df <- merge.data.frame(final.df, interim.df[order(interim.df$date),], by.x='date.initial',by.y='date', all=T)
    colnames(final.df)[kol] <- paste(ticker, type.instr, sep="_")
    interim.df <- interim.df[NULL, ]
    ticker <- df[[i,'ticker']]
    type.instr <- df[[i,'type']]
  }
}

source("Data preparation.R")
source("Functions.R")

task1 <- clean.data(final.df, 1)
task2 <- clean.data(final.df, 2)

########################################################################################
#####                TASK 1: CAN WE PREDICT GLOBAL MARKETS?                        #####
########################################################################################
# prepare task data for clustering analysis
data.set <- kmeans.dataset(task1, 1, 2) 

# split dataset into 2 samples: 85% for trainind and 15% for testing
number.of.rows <- round(nrow(data.set)*0.85, 0)
training.set <- data.set[1:number.of.rows,]
testing.set <- data.set[(number.of.rows+1):nrow(data.set),]

kmeans.optimal.cluster(training.set, 2)
# Please, set the optimal number of clusters
  opt.clusters <- 6
  
# NOTE: kmeans.results, kmeans.results.hc and HMM.results variables return
# dataset + corresponding states
kmeans.results <- kmeans.algorithm(training.set,testing.set, opt.clusters, 2)
dendogram <- kmeans.HC.optimal.cluster(training.set, 2)
kmeans.results.hc <- kmeans.HC.algorithm(training.set,dendogram, opt.clusters)

HMM.results <- HMM.analysis(training.set, opt.clusters, 2)
# Function stocks.in.clusters() shows in charts average returns of 10 instruments
# in top N states (starting with largest  cluster). Instruments are selected
# according to the rule: take the largest cluster, from it take 5 instruments 
# with max average returns and 5 instruments with min average returns. Then, 
# the same instruments are selected from subsequent largest clusters.
# NOTE: the number of states to show user may select - the last argument in function
stocks.to.graph.set <- task1[1:round(nrow(task1)*0.85, 0),]
stocks.in.clusters(stocks.to.graph.set, kmeans.results.hc, opt.clusters, 2)

########################################################################################
#####                          TASK 2: PCA.FACTOR MODEL                            #####
########################################################################################

# Get PCA loadings 
factor.loadings <- data.frame(pca.task(task2))
factor.loadings[,1] <-  as.character(factor.loadings[, 1])
name.split <- c()
 for(i in 1:nrow(factor.loadings))
 {
   name.split <- unlist(strsplit(factor.loadings[i,1], "_", fixed = T))
   factor.loadings[i, 1] <- paste(name.split[1])
   print(name.split[1])
 }
print(factor.loadings)
component.names.pca <- factor.loadings[,1]
# Load data from 'factset_lse.csv'
factset.table <- read.csv('factset_lse.csv', stringsAsFactors=FALSE)


# Transform initial dataset (with all instruments) in order to get prices on the last day
main.table <- data.frame(final.df[nrow(final.df), ])
l <- vector()
l <- colnames(main.table)
l <- l[-1]
main.table <- main.table[-1]
main.table[is.na(main.table)] <- 0
main.table <- data.frame(as.numeric(t(main.table)))
names(main.table) <- "Price on the last day"

main.instrument.name <- vector()
for(i in 1:length(l))
{
  name.split <- unlist(strsplit(l[i], "_", fixed = T))
  main.instrument.name[i] <- paste(name.split[1])
}
main.table <- cbind(main.instrument.name, main.table)

# Obtain EPS, Dividends for corresponding instruments from 'factset_lse.csv'  
ticker.names.factset <- factset.table$ticker
index.of.instrument <- vector()
dividend <- vector()
eps <- vector()
price <- vector()
price.index <- 0
for (i in 1:nrow(factor.loadings)){
  if ( length(which(ticker.names.factset %in% component.names.pca[i])) == 0) 
  {index.of.instrument[i] <- 0
    dividend[i] <- 0
    eps[i] <- 0
    price[i] <- 0
    } else 
   {index.of.instrument[i] <- which(ticker.names.factset %in% component.names.pca[i])
   dividend[i] <- factset.table$dividend_reported[index.of.instrument[i]] 
   eps[i] <- factset.table$eps[index.of.instrument[i]] 
   price.index <- which(main.table[,1] %in% component.names.pca[i])
   price[i] <- main.table$`Price on the last day`[price.index]
   if (dividend[i] == 0 | eps[i]==0 | price[i]==0)  # if any metric is 0 - make all others equal to 0 (division by 0 problem)
     {dividend[i] <- 0
      eps[i] <- 0
      price[i] <- 0
    }
   }
  }
# this is a combined table with instruments` name, Component loadings and all needed metrics
fac <- cbind.data.frame(component.names.pca,factor.loadings$Comp.1,factor.loadings$Comp.2, factor.loadings$Comp.3,  price, dividend, eps)
colnames(fac)[1:4] <- paste(c("Instrument", "Comp.1", "Comp.2", "Comp.3"))

# calculate average ratios for each component
fac$PE.Comp.1 <-fac$Comp.1*fac$price/fac$eps
fac$Div.Yield.Comp.1 <-fac$Comp.1*fac$dividend/fac$price
fac$PE.Comp.2 <-fac$Comp.2*fac$price/fac$eps
fac$Div.Yield.Comp.2 <-fac$Comp.2*fac$dividend/fac$price
fac$PE.Comp.3 <-fac$Comp.3*fac$price/fac$eps
fac$Div.Yield.Comp.3 <-fac$Comp.3*fac$dividend/fac$price
   
fac[is.na(fac)] <- 0  
print(fac)
write.csv(fac, file='Results.csv')
PE <- vector()
DY <- vector()
mean.PE <- c(sum(fac$PE.Comp.1), sum(fac$PE.Comp.2), sum(fac$PE.Comp.3))
mean.PE <- mean(mean.PE)
PE <- c(sum(fac$PE.Comp.1), sum(fac$PE.Comp.2), sum(fac$PE.Comp.3), mean.PE)
mean.DY <- mean(sum(fac$Div.Yield.Comp.1), sum(fac$Div.Yield.Comp.2), sum(fac$Div.Yield.Comp.3))
mean.DY <- mean(mean.DY)
DY <- c(sum(fac$Div.Yield.Comp.1),sum(fac$Div.Yield.Comp.2), sum(fac$Div.Yield.Comp.3), mean.DY)
# print results in a table
results <- rbind(PE, DY)
colnames(results) <- c("Comp.1", "Comp.2", "Comp.3", "Average")
print(round(results,5))
