library(ggplot2)
library(grid)

explore <- function(data, plotswitch = "off", threshold = 0, vector = NULL) {
  #This function is main function give all subfunctions result
  #Must run subfunctions first before run this function
  
  #Parameter:
  #data: a dataframe
  #plotswitch: whether to plot
  #threshold: A threshold cut of value between 0 and 1 for correlations
  #vector: bin numbers of histograms. (use the default bin size if vector is not provided)
  
  #Returns:
  #All results subfunctions return
  
  Freq_table <- freq_table (data) #frequency table
  
  Summary_num <- summary_num (data) #statistics table
  
  R_squared <- r_squared (data) #r-sqaured values
  
  Pearson_coe <- pearson_coe (data, threshold) #pearson correlation coefficient
  
  plot_density_count(data,plotswitch,vector) #plot density and count histograms
  
  plot_gray (data, plotswitch) #plot gray bar
  
  new_result=list(Freq_table, Summary_num, R_squared, Pearson_coe) #combine data results
  
  return (new_result)
  
}




#1
freq_table <- function(data){
  #This function can create a frequency table for every categorical and logical variable in
  #a dataframe
  
  #Parameter: a dataframe
  
  #Returns: a frequency table
  data_cat <- c(data[sapply(data,is.factor)],data[sapply(data,is.logical)])
    #select categorical and logical variable
  return (sapply(data_cat,table)) #make a table
}

##2
#a)
summary_num <- function(data){
  #This function create a summary statistics table for each numerical variable
  
  #Parameter: a dataframe
  
  #Returns: a statistics table
  data_num=data[sapply(data,is.numeric)] #select numeric data
  return (summary(data_num)) #summary statistics
}

#b).
r_squared <- function(data) {
  #This function create a data frame that contains each pair of column names in
  #the first column (name the column "Variable Pairs") and the
  #associated r-square value in the second column (name the
  #column "R-Square").
  
  #Parameter: a dataframe
  
  #Returns: a new dataframe that contains each pair of column names and 
  #corresponding r-square value
  data_num <- data[sapply(data, is.numeric)] # Select numeric data
  colname <- colnames(data_num) # extract column names
  pairwise_rsquared <- c() # new empty r-squre data
  pairwise_names <- c() # new empty pairnames
  for (i in 1:(length(colname)-1)) {
    for (j in (i+1):length(colname)) { #two column names
      num_rsqaured <- summary(lm(data_num[,i]~data_num[,j]))$r.squared
        # get r-squared data using linear model r.squared
      pairwise_names <- c(pairwise_names, paste(colname[i], colname[j], sep="-"))
        # add pairnames to pairwise_names
      pairwise_rsquared <- c(pairwise_rsquared, num_rsqaured)
        # add r-squared data to pairwise_r_squared
    }
  }
  data_rsquared <- data.frame(pairwise_names, pairwise_rsquared)
  colnames(data_rsquared) <- c("Variable Pairs", "R-squared")
  return (data_rsquared)
}  

#c).
pearson_coe <- function(data, threshold = 0) {
  #This function Cor_pearson() is going to return A data frame that contains each pair of 
  #column names in the first column and correlation coefficient (Pearson) for all coefficients whose 
  #absolute value is greater than the correlation threshold (do not repeat any pairs) in the 
  #seconnd colum
  
  #Parameter: a dataframe
  
  #Returns: a new dataframe that contains each pair of column names and corresponding 
  #pearson correlation coefficient
  data_num <- data[sapply(data, is.numeric)] # select numeric data
  comb_names <- combn(colnames(data_num), 2) # combinations of all names
  pairwise_names <- paste(comb_names[1,], comb_names[2, ], sep = "-") 
    # add "-" in names e.g. x-y
  temp <- cor(data_num, method = "pearson")
    # derive pearson correlation coefficient data using cor function
  cor_data <- temp[which(lower.tri(temp))]  
    # use data in lower triangular of matrix to aviod double-use same data
  dfm_new <- data.frame(pairwise_names, cor_data)
    # create a new dataframe data_coe
  dfm_new <- subset(dfm_new, abs(cor_data) > threshold)
    # select absolute value of correlation greater than threshold
  colnames(dfm_new) <- c("Variable Pairs", "Pearson Exceeds Threshold")
  return(dfm_new)
}

#3.
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  #This function multiplot can be used to draw multiple graphs in one page.
  #This function will be used in function plot_density_count.
  plots <- c(list(...), plotlist)
    # Create a list 'plots' using ... and plotlist
  numPlots = length(plots)
  if (is.null(layout)) {
    # If layout is NULL, then use 'cols' to determine layout
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
                      # ncol=number of columns in plots
                      # nrow=number of rows needed, calculated from # of cols
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
      # Plot each in the correct location
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
        #the position that contain this subplot
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


plot_density_count <- function(data,plotswitch='off',vector=NULL){
  #This function plot a pair of blue histograms with a vertical red line at the 
  #mean (one using counts and the other density) for every numerical variable at 
  #each number of bins integer specified in the bin vector parameter.
  
  #Parameter:
  #data: a dataframe
  #plotswitch: a character decide whether to plot
  #vector: bin numbers of historgram (use the default bin size if vector is not provided)
  
  #Returns: histogranms
  num=data[sapply(data,is.numeric)]
  if(plotswitch == "on"){
    if(!is.null(vector)){ # if vector is NULL
      for(j in 1:length(vector)){ 
        for(i in 1:ncol(num)){
          mean <- mean(num[,i]) 
            # caculate the mean of each numeric column
          p1 <- ggplot(num,aes(x=num[i]),color = "blue")+ 
            #draw the histogram of count
            geom_histogram(fill="blue",bins=vector[j])+
            ggtitle(paste(colnames(num[i]),vector[j],sep=" bins="))+
            xlab(colnames(num[i]))+
            geom_vline(xintercept = mean,col="red")  
            #geom_vline can add red line on it
          
          p2 <- ggplot(num,aes(x=num[i],..density..))+
            #draw the density histogram
            geom_histogram(fill="blue",bins=vector[j])+
            ggtitle(paste(colnames(num[i]),vector[j],sep=" bins="))+
            xlab(colnames(num[i]))+
            geom_vline(xintercept = mean,col="red") 
          
          grid.newpage()
          #new page
          pushViewport(viewport(layout = grid.layout(2, 2, heights = unit(c(1, 8), "null"))))
          title <- paste(colnames(num[i]),vector[j],sep=" bin=")
          grid.text(title, vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
          print(p1, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
          print(p2, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))#print p1 and p2 two histograms
          
        }
      }
    }else{ #if vector isn't NULL
      for(i in 1:ncol(num)){
        mean <- mean(num[,i]) 
          #caculate the mean of each numeric column
        p1 <- ggplot(num,aes(x=num[i]),color = "blue")+  
          geom_histogram(fill="blue")+
          #draw the histogram of count
          ggtitle(paste(colnames(num[i]),"default bins",sep=" bins="))+
          xlab(colnames(num[i]))+
          geom_vline(xintercept = mean,col="red")
        p2 <- ggplot(num,aes(x=num[i],..density..))+
          #draw the density histogram
          geom_histogram(fill="blue")+
          ggtitle(paste(colnames(num[i]),"default bins",sep=" bins="))+
          xlab(colnames(num[i]))+
          geom_vline(xintercept = mean,col="red")
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(2, 2, heights = unit(c(1, 8), "null"))))
        title <- paste(colnames(num[i]),"default bins",sep=" bins=")
        grid.text(title, vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
        print(p1, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
        print(p2, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))#print p1 and p2 two histograms
        
      }
      
    }
    
  }else{
    if(plotswitch == "grid"){#  plotswitch can also = 'grid'
      for(j in 1:length(vector)){
        grid.newpage()
        his_count <-list()   
        his_density <- list()  
          #create two empty list
        for(i in 1:ncol(num)){
          his_count[[i]] <- ggplot(num, aes_string(colnames(num[i])), color = "blue") + 
            geom_histogram(fill="blue", bins = vector[j])+ 
            labs(title= paste(vector[j], "bins")) 
            #draw histograms of count and add them to list his_count
        }
        multiplot(plotlist = his_count, cols = 2)  
          #draw all histogram with same bins in one page
        for(i in 1:ncol(num)){
          his_density[[i]] <- ggplot(num, aes_string(colnames(num[i])), color = "blue") + 
            geom_histogram(aes(y= ..density..), fill="blue", bins = vector[j])+ 
            labs(title= paste(vector[j], "bins")) 
            #draw histograms of density and add them to list his_density 
        }
        multiplot(plotlist = his_density, cols = 2)  
          #similar to above, draw all histogram of density with same bins in one page
      }
    }
  }
}


#4.
is.binary <- function(v) {
  #This function will be used in the function plot_gray.
  #This function can tell whether the vector is a binary vector
  
  #Parameter: a vector
  
  #Returns: TRUE if the vector is binary, FALSE else
  x <- unique(v)                    
    #x contains all unique values in v
  length(x) - sum(is.na(x)) == 2L         
    #check to see if x only contains 2 distinct values
}


plot_gray <- function(data, plotswitch='off') {
  #This function plot a gray bar graph for every categorical and binary variable.
  
  #Parameter: 
  #data: a dataframe
  #plotswitch: whether or not to plot
  
  #Returns: a gray bar graph for every categorical and binary variable.
  dfm_cb <- data[,sapply(data,is.factor)|sapply(data,is.logical)|sapply(data,is.binary)]
  #select categorical and binary variable
  if(plotswitch=="on"|plotswitch=="grid"){
    for(i in 1:ncol(dfm_cb)){
      p <- ggplot(dfm_cb,aes(x=dfm_cb[,i]),colour="gray")+
        geom_bar()+ xlab(colnames(dfm_cb[i]))
        #plot gray bar for every categorial and binary variable
      print(p)
    }
  }
}


#hw 8
# 5
explore_2 <- function(data_frame,switch="off", threshold=0.5, vector=NULL){
  # This function work with defensive conditions
  
  # parameter: dataframe(if no, change the input to dataframe. Details see below codes)
  # return: explore function of fine parameter as inputs
  
  # omit the whole row if there are any nas
  data_frame <- na.omit(data_frame)
  
  # change the parameter to dataframe if it is not a dataframe
  if(!is.data.frame(data_frame)){                 
    data_frame <- as.data.frame(data_frame)
  }
  
  # if the second parameter is not 'on', 'off' or 'grid', ask users to re-input
  while(switch != "off" && switch != "on" && switch != "grid"){  
    print("invalid input for switch")
    switch <- readline(prompt="Enter your option(off / on / grid): ")  #re-enter the input
  }
  
  # if the second parameter is not in [0,1], ask users to re-input
  while(!is.numeric(threshold) || threshold < 0 || threshold >1 ){    #check to see if threshold is a valid input
    print("correlation threshold must be numeric and in range [0,1]")
    threshold <- as.numeric(readline(prompt="Enter your correlation threshold: "))   #re-enter the input
  }
  
  # check if bin vector is all numeric and all not less than 0, if so, ask users to re-input
  if(!is.null(vector)){
    if(!is.numeric(vector)||(is.numeric(vector) && (TRUE %in% (vector <= 0)))){ 
      print("the bins vector must be numeric vector and not less than 0, please enter new bins one by one and press 'return' to finish")
      vector <- c()
      bin <- 1
      #input "return"  to finish loop
      while(bin != ""){ 
        #re-enter the bin vector
        bin <- readline(prompt="Enter the number of bins: ")->bin1
        bin1 <- as.numeric(bin1)
        vector <- c(vector, bin1)
      }
      vector <- na.omit(vector) #cancel the NA
    }
    
    # if the bin vector is not integer, round it
    if (!is.integer(vector)) {        
      vector <- round(vector)
    }
  }
  return(explore(data_frame,switch,threshold,vector))
}

# check
explore_2(diamonds,"osjfs",1.5)
