data(iris)
library(dplyr)
df1 <- as.data.frame(iris)
df <- createDataFrame(iris)
class(df) ##  SparkDataFrame
class(df1) ## data.frame
head(select(df, df$Sepal_Length, df$Species))
head(select(df1, df1$Sepal_Length, df1$Species)) # unable to find an inherited method for function ‘select’ for signature ‘"data.frame", "NULL"’

head(filter(df1, df1$Sepal_Length>5.5))
head(filter(df, df$Sepal_Length>5.5))
head(filter(df, df$Sepal_Length>5.5))

# nesting operations 
library(magrittr) for pipe operation so someone's head doesn't explode.
select(df, df$Sepal_Length, df$Species) %>% filter(df$Sepal_Length > 5.5) %>% head()

head(summarize(groupBy(df, df$Species), 
mean=mean(df$Sepal_Length), count=n(df$Sepal_Length)))

df2 = head(summarize(groupBy(df, df$Species),
mean=mean(df$Sepal_Length), count=n(df$Sepal_Length)))
head(arrange(df2, asc(df2$Species))) # arrange doens't work on character objects? 
