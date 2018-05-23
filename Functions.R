################################################################################################################
#####                                UNCOMMENT IF NEED THESE PACKAGES                                      #####
################################################################################################################
#install.packages("matrixStats")
#install.packages("cluster")
#install.packages("depmixS4")
#install.packages("ggplot2")
#install.packages("reshape2")
#install.packages("dplyr")
#install.packages("plyr")
#install.packages("caret")
#install.packages("e1071")
#install.packages("stringr")

library(matrixStats)
library(cluster)
library(depmixS4)
library(ggplot2)
library(reshape2)
library(dplyr)
library(plyr)
library(caret)
library(e1071)
library(stringi)



# prepare data for clustering analysis
kmeans.dataset <- function(task_data, task_type, model_type)
{
  x <- data.frame()
 if (model_type == 1 & (task_type == 1 | task_type == 2)) {
  x <- task_data[1:ncol(task_data)]
  } else if (model_type == 2 & (task_type == 1 | task_type == 2) ) {
  aver.return <-(rowMeans(task_data[2:ncol(task_data)]))*100
  stddev.return <-  ((apply(task_data[2:ncol(task_data)],1,sd)))*100
  x <- cbind(task_data[1], aver.return, stddev.return)
  }  else if (model_type == 3 & task_type == 1) {
  col64 <- c()
  col1024 <- c()
  i64 <- 0
  i1024 <- 0
  for (j in 2:ncol(task_data)) {
    type <- unlist(strsplit(colnames(task_data)[j], "_", fixed = T))
      if (type[length(type)] == 64)   
      {i64 <- i64+1
      col64[i64] <- j }
      else if (type[length(type)] == 1024) 
      { i1024 <- i1024+1
        col1024[i1024] <- j }}
  subset64 <- data.frame()
  subset1024 <- data.frame()
  subset64 <- task_data[,col64]
  subset1024 <- task_data[,col1024]
  aver.return64 <- (rowMeans(subset64[1:ncol(subset64)]))*100
  aver.return1024 <- (rowMeans(subset1024[1:ncol(subset1024)]))*100
  stddev.return64 <- ((apply(subset64[1:ncol(subset64)],1,sd)))*100
  stddev.return1024 <- ((apply(subset1024[1:ncol(subset1024)],1,sd)))*100
  
  x <- cbind(task_data[1], aver.return64, stddev.return64, aver.return1024, stddev.return1024)
}   else {print ("Error! Please, check the model and task types")}
  
return(x)  
}


# this function utilizes an elbow method  - identifies an optimal cluster number
kmeans.optimal.cluster <- function(task_data, model_type)
{ x <- task_data[2:ncol(task_data)]
  set.seed(101)
  wcss <- vector()
  for (i in 1:10){
    wcss[i] <- sum(kmeans(x, i)$withinss)
  }
  plot (1:10, wcss, type= "b", main=paste("Optimal number of clusters: Elbow method, Model type ",  model_type), xlab = "Number of Clusters", ylab = "WCSS")
 }


# k-means clustering algorithm with specified optimal number of clusters
kmeans.algorithm <- function(task_data,test_data, number_of_clusters, model_type)
{ x <- task_data[2:ncol(task_data)]
  set.seed(101)
  kmeans <- kmeans(x, number_of_clusters, iter.max = 300, nstart = 10)
  print("Cluster size:  ")
  print(kmeans$size)
  print("Centers:  ")
  print(kmeans$centers)
  print("-------------------------------------------------------------------------------------------")
  print("Within cluster sum of squares by cluster:  ")
  print(kmeans$withinss)
  cat("between_SS/total_SS=  ", kmeans$betweenss/kmeans$totss*100)
  print(" %")
  result.kmeans <- cbind.data.frame(task_data, kmeans$cluster)
  colnames(result.kmeans)[length(result.kmeans)] <- "state"
  
  if (model_type == 1) { print("Error! Impossible to plot clusters for this type of model!") 
    } else if (model_type == 2)
    {clusplot(result.kmeans[,2:3], result.kmeans$state, color=TRUE, shade=TRUE, labels=0,lines=0, plotchar = FALSE, span = TRUE, main = paste("Clusters: Model type ", model_type), xlab = "aver. return", ylab = "std.dev" )
    } else if (model_type == 3)
    {clusplot(result.kmeans[,2:3], result.kmeans$state, color=TRUE, shade=TRUE, labels=0,lines=0, plotchar = FALSE, span = TRUE, main = paste("Clusters: Model type ", model_type), xlab = "aver. return, type 64", ylab = "std.dev, type 64" )
     clusplot(result.kmeans[,4:5], result.kmeans$state, color=TRUE, shade=TRUE, labels=0,lines=0, plotchar = FALSE, span = TRUE, main = paste("Clusters: Model type ", model_type), xlab = "aver. return, type 1024", ylab = "std.dev, type 1024" )
     clusplot(result.kmeans[,c(2,4)], result.kmeans$state, color=TRUE, shade=TRUE, labels=0,lines=0, plotchar = FALSE, span = TRUE, main = paste("Clusters: Model type ", model_type), xlab = "aver. return, type 64", ylab = "aver. return, type 1024" )
     }
  
  # returns transition probabilities matrix
  k <- transition.probability(kmeans$cluster, number_of_clusters)
  
  #prediction(k, kmeans, test_data, number_of_clusters, model_type)
  
  
  # Optimal number of clusters using IC
  AIC <- vector()
  BIC <- vector()
 
  for (i in 1:25)
  {set.seed(101)
  kmeans <- kmeans(x, i, iter.max = 300, nstart = 10)
  IC <- kmeansIC(kmeans)
  AIC[i] <- IC[1]
  BIC[i] <- IC[2]
  }
  plot(1:25, AIC, type="l", lwd=2, xlab="Number of clusters",ylab="IC", main = paste("Optimal number of clusters: IC, Model", model_type), col="blue")
  lines(1:25, BIC, lwd=2, col="red")
  legend("topleft", legend=c("AIC","BIC"), lwd=c(2,2), col=c("blue","red"))
  
  return(result.kmeans)
  }

# Hierarchical k-means clustering - dendrogram
kmeans.HC.optimal.cluster <-function(task_data, model_type)
{ x <- task_data[2:ncol(task_data)]
  set.seed(101)
  dendrogram <- hclust(dist(x, method = 'euclidean'), method = 'ward.D')
  print(dendrogram)
  plot(dendrogram, main = paste('Hierarchical clustering: Model type ', model_type), xlab = '%', ylab = 'Euclidean distances')
  return(dendrogram)
}


# Hierarchical k-means clustering 
kmeans.HC.algorithm <-function(task_data, dendrogram, number_of_clusters)
{
  vector.hc <- cutree(dendrogram, number_of_clusters)
  result.hc <- cbind.data.frame(task_data, vector.hc)
  colnames(result.hc)[length(result.hc)] <- "state"
  transition.probability(vector.hc, number_of_clusters)
  
  return (result.hc)
}


# Transition probability matrix
transition.probability <- function(states_vector, number_of_clusters)
{
  today <- data.frame(states_vector)
  tomorrow <- rbind(NA, head(today, -1))
  autocorrelation <- head(cbind(today, tomorrow),-1)
  print("-------------------------------------------------------------------------------------------")
  print("Frequency table:")
  print(xtabs(~autocorrelation[,1]+(autocorrelation[,2])))
  y <- apply(xtabs(~autocorrelation[,1]+(autocorrelation[,2])),1,sum)
  w <- xtabs(~autocorrelation[,1]+(autocorrelation[,2]))
  z <- w
  for(i in 1:number_of_clusters){
    z[i,]=(w[i,]/y[i])
  }
  print("-------------------------------------------------------------------------------------------")
  print("Transition probabilities: frequentist approach")
  print(round(z,2))
  return(round(z,2))
}


# with prediction() function I tried to utilize Markov model (NOT Hidden) and predict returns in the future:
# initial probabilities * matrix(transition probabilities) * cluster centers. Returns converged to
# 0 after 20 steps, so I decided not to include it.
prediction <- function(transition_matrix, kmeans_table, test_data, number_of_clusters, model_type)
{
  initial.prob <- c(0.05, 0.3, 0.2, 0.2, 0.05, 0.2)
  mean.state <- data.frame(kmeans_table$centers)
  print(initial.prob)
  pr <- vector()
  newprob <- vector()
  for (i in 1:nrow(test_data))
  {
    if (i == 1) {dmatrix <- transition_matrix}
    else {dmatrix <- dmatrix %*% transition_matrix}
    newprob <- initial.prob %*% dmatrix
    if (model_type == 2) {pr[i] <- sum(newprob * mean.state[,1])}
    else {pr[i] <- sum(newprob * mean.state[ ,1])+sum(newprob * mean.state[ ,3])}
      }
  print(pr)
  
  plot(test_data$date.initial, test_data$aver.return, type="l", lwd=2, col="blue", xaxs="i", yaxs="i")
  lines(test_data$date.initial,pr, lwd=2, col="red")
  legend("topleft", legend=c("Real data","Predicted data"), lwd=c(2,2), col=c("blue","red"))
  
}


# Hidden Markov Model. Unfortunately, I could not test it....
HMM.analysis <- function(task_data, number_of_clusters, model_type)
{
  if (model_type == 1) { print("Error! Impossible to make HMM analysis with this type of model!") 
  } else if (model_type == 2)
  { x <- task_data[,2:3]
  mod <- depmix(response = list(aver.return ~ 1, stddev.return ~ 1), data = x, nstates = number_of_clusters, family = list(gaussian(), gaussian()))
  } else if (model_type == 3)
  {x <- task_data[,2:5]
  mod <- depmix(response = list(aver.return64 ~ 1, stddev.return64 ~ 1, aver.return1024 ~ 1, stddev.return1024 ~ 1), data = x, nstates = number_of_clusters, family = list(gaussian(), gaussian(), gaussian(), gaussian()))
  } else { print("Error! Impossible to plot clusters for this type of model!")}
  set.seed(1)
  fm <- fit(mod)
  print(fm)
  summary(fm)
  
  
  HMMposterior <- posterior(fm)
  colnames(HMMposterior)[2:ncol(HMMposterior)] <- paste("P",1:number_of_clusters, sep="-")
  df.plot <- cbind(task_data,HMMposterior[, 2:ncol(HMMposterior)])
  df.plot <- melt(df.plot,id = "date.initial")
  pl <- qplot(date.initial ,value, data=df.plot, geom="line",
        #main = paste("States", paste(names(states), states, collapse=": "), collapse="; "),
        main = paste("Hidden Markov Model: Model ", model_type),
        ylab = "State Probabilities") + 
    facet_grid(variable ~ ., scales="free_y") + theme_bw()
  print(pl)
  
  df.plot <- cbind(task_data,HMMposterior[, 1])
  if (model_type == 2) { 
    colnames(df.plot)[4] <- paste("state")
    df.plot2 <- melt(df.plot[, 1:4],id = "date.initial")
  } else if (model_type == 3) {
    colnames(df.plot)[6] <- paste("state")
    df.plot2 <- melt(df.plot[, 1:6],id = "date.initial")
  }
 
  pl2 <- qplot(date.initial ,value, data=df.plot2, geom="line",
        main = paste("HMM: Returns, Standard deviations and Estimated states, Model", model_type),
        ylab = "          Percent") + 
    facet_grid(variable ~ ., scales="free_y") + theme_bw()
  print(pl2)
 
return(df.plot)
  }



stocks.in.clusters <- function(task_data, result_dataset, number_of_clusters, number_of_clusters_to_graph)
{
  if (number_of_clusters_to_graph > number_of_clusters) {print("Error! Please, check the number of states you want to visualize")
    } else {
  cluster.size <- vector()
  for (i in 1:number_of_clusters){
    cluster.size[i] <- nrow(result_dataset[result_dataset$state == i, ])
    }
  cluster.no <- vector()
  po <- sort(cluster.size, decreasing = T)
  
  for (i in 1:number_of_clusters){
    cluster.no[i] <- which(cluster.size %in% po[i])
    }
  
  result_dataset <- cbind(task_data, result_dataset$state)
  colnames( result_dataset)[ncol(result_dataset)] <- paste("state")
  
  pl <- data.frame()
  for(i in 1:number_of_clusters_to_graph){
    g1 <- subset.data.frame(result_dataset, subset = result_dataset$state == cluster.no[i])
    gv1 <- (colMeans(g1[1:nrow(g1)-1,2:(ncol(g1)-1)]))*100        # calculate mean return per instrument in cluster1
     
    if (i == 1) {
      state <- c(tail(sort(gv1), 5), tail(sort(gv1, decreasing = T), 5))
      pl <- cbind.data.frame(sort(names(state)),state)
# we take the same instruments as the biggest cluster: first 5 with max aver return and 5 with min aver return
      state1 <- state   
      colnames(pl)[i] <- paste("Instrument")
      colnames(pl)[i+1] <-paste("State",cluster.no[i])
      }
    else { 
      state <- gv1[names(gv1) %in% names(state1)]
      pl <- cbind.data.frame(pl, state[sort(names(state1))])
      colnames(pl)[i+1] <- paste("State",cluster.no[i])
      }
  }
  pl <-  melt(pl, id="Instrument")
  g <- ggplot(pl, aes(x = Instrument, y = value))
  print(g + geom_bar(stat="identity") +labs(x = "Instrument_type", y = "Average percent in corresponding State")+ggtitle(paste("Top", number_of_clusters_to_graph ,"most populated clusters in Model"))+ facet_grid(variable~.))
   }
  }


# PCA. Here I print both actual component loading and relative contribution of instruments to component
pca.task <- function(task_data)
  {
  t <- task_data[,2:(ncol(task_data))]
  pca <- princomp(t, cor = T, scores = T)
  plot(pca, type = "l")
  factors <- data.frame()
  factors <- pca$loadings[,1:3]
  aload <- abs(factors) 
  relative.loadings <- (sweep(aload, 2, colSums(aload), "/"))*100
  v <- c(t(task_data)[,1])
  v <- v[-1]
  loadings <- cbind.data.frame(names(v), factors)
  print("Component loadings (used in task2 for ratios calculation:")
  print(loadings)
  relative.loadings <- cbind.data.frame(names(v), relative.loadings)
  colnames(relative.loadings)[1] <- paste("Instrument")
  colSums(sweep(aload, 2, colSums(aload), "/"))
  print("Relative contribution of component loadings, %:")
  print(relative.loadings)
  
  return(loadings)
}



kmeansIC = function(fit){
  r <- data.frame()
  m <- ncol(fit$centers)
  n <- length(fit$cluster)
  k <- nrow(fit$centers)
  D <- fit$tot.withinss
  AIC <- D + 2*m*k
  BIC <- D + log(n)*m*k
  r <- cbind.data.frame(AIC, BIC)
  return(r)
}
