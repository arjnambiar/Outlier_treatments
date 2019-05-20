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
df1 = subset(df,x>=min &x<=max)

#bivariate approach

#cook's distance
#Data points with large residuals (outliers) and/or high leverage may distort the outcome and accuracy of a regression. 
#Cook's distance measures the effect of deleting a given observation. 
#Points with a large Cook's distance are considered to merit closer examination in the analysis.

mod <- lm(x ~ y, data=df)
cooksd <- cooks.distance(mod)
plot(cooksd, pch="*", cex=2, main="Influential Obs")
abline(h = 4*mean(cooksd, na.rm=T), col="red")  #cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])
outliers = df[influential, ]
df1 = df[-influential, ] #final data after removing outliers

#reference -http://r-statistics.co/Outlier-Treatment-With-R.html
