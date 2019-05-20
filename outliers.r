library("ggplot2")
library("plotly")

#Univariate approach

df <- data.frame(x = c(1:100),y =rnorm(100, sd = 20))
newdf <- data.frame(x=c(700,600,660), y=c(190, 210,218))
df <- rbind(df,newdf)

#outlier values
outlier_values = boxplot.stats(df$x,coef=2,do.out=TRUE)$out   #coef = 2 -- This determines how far the plot ‘whiskers’ extend out from the box (either 1.5 or 2), if 0 no outlier returned
outlier_range = boxplot(df$x,coef =2)
 max = outlier_range$stats[5]
 min = outlier_range$stats[1]

#removing outliers by restricting the data within the range
df = subset(df,x>=min &x<=max)

