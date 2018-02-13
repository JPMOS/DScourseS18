system("wget -O nfl.json 'http://api.fantasy.nfl.com/v1/players/stats?statType=seasonStats&season=2010&week=1&format=json'")
library(jsonlite)
system("cat nfl.json")
mydf <- fromJSON("nfl.json")
class(mydf) #It's a list
class(mydf$players)
head(mydf)

