library(reshape2)
library("ggplot2")

# plot correlation matrix of a given data frame df. 
# assign a name name_corr to the plot.
plot_correlation <- function(df, name_corr){
  melted_df <- melt(cor(df))
  ggplot(data = melted_df, aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile(color = "white") + 
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name=name_corr) + theme_minimal()+ 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 12, hjust = 1))+
    coord_fixed()
}


# df_melted: melted data frame
# corr_degree: degree of correlation over which two covariates are considered
#              highly correlated according to the Pearson index
# target: is the target variable, so the one to predict
# return a data frame containing pairs of covariates with high correlation and 
# the absolute value of their correlation coefficient.
find_competitive_covariates <- function(df_melted, corr_degree, target){
  correlated_covariates = data.frame("Var1"=c(), "Var2"=c(), "value"=c())
  pairs_seen = data.frame("Var1"=c(), "Var2"=c())
  # finding highly correlated covariates 
  for(i in 1: nrow(df_melted)){
    new_row = df_melted[i,]
    if(abs(new_row$value) >= corr_degree
       & new_row$Var1 != new_row$Var2
       & new_row$Var1 != target 
       & new_row$Var2 != target){
      new_pair = data.frame("Var1"=c(new_row$Var1), "Var2"=c(new_row$Var2))
      # checks if this pair has been already visited
      if(nrow(merge(data.frame("Var1"=c(new_row$Var2), "Var2"=c(new_row$Var1)), pairs_seen)) == 0){
        pairs_seen <- rbind(pairs_seen, new_pair)
        new_row$value <- abs(new_row$value)
        correlated_covariates <- rbind(correlated_covariates, new_row)
      }
    }
  }
  correlated_covariates = correlated_covariates[order(correlated_covariates$value, decreasing=TRUE),]
  return(correlated_covariates)
}


# find and remove the outliers of data frame df.
# in this function it is assumed that the target value is always the last column.
# Outliers of the target are kept.
remove_outliers <- function(df){
  upper_bounds = vector()
  lower_bounds = vector()
  for(i in 1: ncol(df)){
    q3 = quantile(df[, i], 0.75)
    q1 = quantile(df[, i], 0.25)
    interquantile_range = q3 - q1
    upper_bounds = append(upper_bounds, q3 + 1.5 * interquantile_range)
    lower_bounds = append(lower_bounds, q1 - 1.5 * interquantile_range)
  }
  outliers_index = vector()
  for(i in 1: nrow(df)){
    is_outlier = FALSE
    j = 1
    # quality is the last column and should not be considered
    while(j <= ncol(df) - 1 & !is_outlier){
      if( df[i, j] > upper_bounds[j]){
        outliers_index = append(outliers_index, i)
        is_outlier = !is_outlier
      }
      j = j + 1
    }
  }
  return(df[-outliers_index,])
}

# given a data frame df, this function returns a new data frame replacing its 
# column "quality" with the columns (y1, y2, .., y10) that represent the one
# hot encoding of the values "quality.
to_one_hot <- function(df){
  one_hot_df <- data.frame(
    "y1"=rep(0, nrow(df)),
    "y2"=rep(0, nrow(df)),
    "y3"=rep(0, nrow(df)),
    "y4"=rep(0, nrow(df)),
    "y5"=rep(0, nrow(df)),
    "y6"=rep(0, nrow(df)),
    "y7"=rep(0, nrow(df)),
    "y8"=rep(0, nrow(df)),
    "y9"=rep(0, nrow(df)),
    "y10"=rep(0, nrow(df))
  )
  for (i in 1: nrow(df)){
    qlty_value = df[i,]$quality
    one_hot_df[i, qlty_value] = 1
  }
  new_df <-cbind(df, one_hot_df)
  new_df <-new_df[,!names(new_df) %in% c("quality")]
  return(new_df)
}


# find and keep the outliers of data frame df.
keep_outliers <- function(df){
  upper_bounds = vector()
  lower_bounds = vector()
  for(i in 1: ncol(df)){
    q3 = quantile(df[, i], 0.75)
    q1 = quantile(df[, i], 0.25)
    interquantile_range = q3 - q1
    upper_bounds = append(upper_bounds, q3 + 1.5 * interquantile_range)
    lower_bounds = append(lower_bounds, q1 - 1.5 * interquantile_range)
  }
  outliers_index = vector()
  for(i in 1: nrow(df)){
    is_outlier = FALSE
    j = 1
    # quality is the last column and should not be considered
    while(j <= ncol(df) - 1 & !is_outlier){
      if( df[i, j] > upper_bounds[j]){
        outliers_index = append(outliers_index, i)
        is_outlier = !is_outlier
      }
      j = j + 1
    }
  }
  return(df[+outliers_index,])
}

get_box_plots_wine <- function(wine){
  color_outlier = "#ea526f"
  color_norm = '#25ced1'
  outlier_size = 2
  color_edges_bx = '#25ced1'
  bx_plt_alpha = 0.3
  
  p1 <- ggplot(wine, aes(x=fixed.acidity)) + 
    geom_boxplot(outlier.colour=color_outlier, outlier.shape=16, outlier.size=outlier_size, notch=FALSE, fill=color_norm, color=color_edges_bx, alpha=bx_plt_alpha) +
    coord_flip()
  
  p2 <- ggplot(wine, aes(x=volatile.acidity)) + 
    geom_boxplot(outlier.colour=color_outlier, outlier.shape=16, outlier.size=outlier_size, notch=FALSE, fill=color_norm, color=color_edges_bx, alpha=bx_plt_alpha) +
    coord_flip()
  
  p3 <- ggplot(wine, aes(x=citric.acid)) + 
    geom_boxplot(outlier.colour=color_outlier, outlier.shape=16, outlier.size=outlier_size, notch=FALSE, fill=color_norm, color=color_edges_bx, alpha=bx_plt_alpha) +
    coord_flip()
  
  p4 <- ggplot(wine, aes(x=residual.sugar)) + 
    geom_boxplot(outlier.colour=color_outlier, outlier.shape=16, outlier.size=outlier_size, notch=FALSE, fill=color_norm, color=color_edges_bx, alpha=bx_plt_alpha) +
    coord_flip()
  
  p5 <- ggplot(wine, aes(x=chlorides)) + 
    geom_boxplot(outlier.colour=color_outlier, outlier.shape=16, outlier.size=outlier_size, notch=FALSE, fill=color_norm, color=color_edges_bx, alpha=bx_plt_alpha) +
    coord_flip()
  
  p6 <- ggplot(wine, aes(x=free.sulfur.dioxide)) + 
    geom_boxplot(outlier.colour=color_outlier, outlier.shape=16, outlier.size=outlier_size, notch=FALSE, fill=color_norm, color=color_edges_bx, alpha=bx_plt_alpha) +
    coord_flip()
  
  p7 <- ggplot(wine, aes(x=total.sulfur.dioxide)) + 
    geom_boxplot(outlier.colour=color_outlier, outlier.shape=16, outlier.size=outlier_size, notch=FALSE, fill=color_norm, color=color_edges_bx, alpha=bx_plt_alpha) +
    coord_flip()
  
  p8 <- ggplot(wine, aes(x=density)) + 
    geom_boxplot(outlier.colour=color_outlier, outlier.shape=16, outlier.size=outlier_size, notch=FALSE, fill=color_norm, color=color_edges_bx, alpha=bx_plt_alpha) +
    coord_flip()
  
  p9 <- ggplot(wine, aes(x=pH)) + 
    geom_boxplot(outlier.colour=color_outlier, outlier.shape=16, outlier.size=outlier_size, notch=FALSE, fill=color_norm, color=color_edges_bx, alpha=bx_plt_alpha) +
    coord_flip()
  
  p10 <- ggplot(wine, aes(x=sulphates)) + 
    geom_boxplot(outlier.colour=color_outlier, outlier.shape=16, outlier.size=outlier_size, notch=FALSE, fill=color_norm, color=color_edges_bx, alpha=bx_plt_alpha) +
    coord_flip()
  
  p11 <- ggplot(wine, aes(x=alcohol)) + 
    geom_boxplot(outlier.colour=color_outlier, outlier.shape=16, outlier.size=outlier_size, notch=FALSE, fill=color_norm, color=color_edges_bx, alpha=bx_plt_alpha) +
    coord_flip()
  
  p12 <- ggplot(wine, aes(x=quality)) + 
    geom_boxplot(outlier.colour=color_outlier, outlier.shape=16, outlier.size=outlier_size, notch=FALSE, fill=color_norm, color=color_edges_bx, alpha=bx_plt_alpha) +
    coord_flip()
  
  box_plts = p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + p10 + p11 + p12 + plot_layout(ncol = 3)
  return(box_plts)
}

