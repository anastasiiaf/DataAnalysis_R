
clean.data <- function(final_df, task)
{ if (task == 1 | task == 2)
  {task.df <- data.frame(date.initial)
 
  # select only 64 and 1024 instruments for task #1 and only instruments 1 if task #2 is selected
  i <- 1
  cols.task <- c()
  cols.task[1] <- 1
  
  for (j in 2:ncol(final_df)) {
    type <- unlist(strsplit(colnames(final_df)[j], "_", fixed = T))
    if (task == 1){
        if (type[length(type)] == 64 | type[length(type)] == 1024)   
          {i <- i+1
          cols.task[i] <- j }
          }
    else if (task == 2) {
        if (type[length(type)] == 1) {
          i <- i+1
          cols.task[i] <- j }}
  }
  task.df <- final_df[,cols.task]
  
  # delete rows where NA data is more than 80%
  task.df <- task.df[!rowSums(is.na(task.df)) >= (ncol(task.df)-1)*0.8,]
  
  # find the starting row
  flag <- T
  while (flag == T){
    if (rowSums(is.na(task.df[1,])) >= (ncol(task.df)-1)*0.1)
    {    task.df <- task.df[-1, ] }
    else {flag <- F}
  }
  
  # replace NA prices with prices from previous available day
  for (i in 1:nrow(task.df)){
    for (j in 2:ncol(task.df)) {
      
      if (is.na(task.df[i,j])== T)
      { if (i==1) {task.df[i,j] <- task.df[i+1,j]} # here is the problem - in task 1 one stock does not have data up to 2016, because of it i cannot calculate return
        else
        {task.df[i,j] <- task.df[i-1,j]}}
    }
  }
  
  # calculate log returns
  log.returns <- data.frame(task.df)
  for (i in 2:ncol(task.df)){
    for (j in 2:nrow(task.df)) {
      log.returns[j,i] <- log(task.df[j,i]/task.df[j-1,i])
    }
  }
  
  #delete first row
  log.returns <- log.returns[-1, ]
  
  # check again NA data and convert it to 0
  log.returns[is.na(log.returns)] <- 0
  
  return(log.returns)
}
  else {print("Error! Please, check a task type")}
}
  
  
  
  
  

 