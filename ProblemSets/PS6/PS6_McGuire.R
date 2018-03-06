#####################################################################################################
#                             LOAD STUFF
#####################################################################################################

library(jsonlite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(VIM) # deal with NA
library(forcats)
library(lubridate)
library(scales) # This is for getting percent of total plot I think. 
library(forcats) # For reordering bar graphs using fct_infreq()

library(pscl) # R^2 for logistic regression because I'm lazy. 

setwd("~/Dropbox")
# beedata <- fromJSON("grad_data_modified.json")
beedata <- fromJSON("new_beedata_modified.json") # has new variables


#####################################################################################################
#                             CLEANING & FEATURE ENGINEERING
#####################################################################################################


# extract elements of payment from pledge and sum them so we can see how much money was spent on each goal.
amount_paid <- rapply(beedata$pledge, classes = 'matrix', how = 'list', f = function(x) x[, 2, drop = FALSE]) 
amount_paid_num <- lapply(amount_paid, FUN = as.numeric) %>% lapply(sum, na.rm = TRUE)

# Turns empty lists into "freebie" as there was no payment. 
beedata$pledge <- lapply(beedata$pledge, function(x) if(length(x) < 1) {"freebie"} else x) 


beedata2 <- beedata %>% 
  # Encode null and N/A as NA
  mutate(endstate = na_if(beedata$endstate, "N/A"),
         enddate = na_if(beedata$enddate, "null"), 
         autodata = as.factor(beedata$autodata),
         gtype = as.factor(gtype),
         # track amount paid per goal
         amount_paid = as.vector(unlist(amount_paid_num))) %>% 
  
  # Change NA to factor level that's descriptive. 
  mutate(autodata = fct_explicit_na(autodata, na_level = "manual"),
         #enddate = fct_explicit_na(enddate, na_level = "ongoing"), # not working.
         endstate = fct_explicit_na(endstate, na_level = "ongoing")) %>%
  
  mutate(createdat = ymd_hms(createdat),
         enddate = ymd_hms(enddate),
         goal_duration = as.integer((enddate - createdat) / 86400),
         
         dayofmonth_created = day(createdat),
         dayofweek_created = wday(createdat),
         dayofyear_created = yday(createdat),
         month_created = month(createdat),
         changesToSlope = as.vector(unlist(lapply(beedata$slope_history, length))), #consider dividing by duration
         DaysSinceCreation = as.integer(ymd_hms("2018-02-10 20:00:00") - createdat)
  ) %>% dplyr::select(-archived, -slug, -title)



for (i in seq_along(beedata2$enddate)) {
  if (beedata2$endstate[i] != "ongoing") {
    beedata2$DaysSinceCreation[i] <- NA
  }     
} 

#trial7 <- lapply(beedata2$endstate, function(x) if(x != "ongoing") {beedata2$DaysSinceApply[x] <- NA} else x)
#trial7 == beedata2$DaysSinceCreation

beedata2 <- beedata2 %>% 
    mutate(DurationPLUSOngoing = rowSums(beedata2[,c("goal_duration", "DaysSinceCreation")], na.rm=TRUE)) %>%
    filter(!(endstate == "won" & is.na(enddate))) %>% 
    mutate(is_autodata = as.logical(ifelse(autodata == "manual", 0 , 1)),
          level_paid = cut(amount_paid, breaks = c(-Inf ,0, 5, 30, Inf), 
                          labels = c("none", "a_little", "some", "a_lot")),
          year_goaldate = year(goaldate),
          level_goaldate = cut(year_goaldate, breaks = c(-Inf, 2017, 2019, Inf),
                              labels = c("present_term", "near_term", "long_term"))
  ) %>% dplyr::select(-DaysSinceCreation, -year_goaldate)


#####################################################################################################
#                            EDA & VISUALIZATIONS
#####################################################################################################


# THE FREQUENCY OF GOALDATES. MOSTLY VERY SOON AND VERY DISTANT (THE DEFAULT).
year(beedata$goaldate) %>% table()

# NA PLOT
plot_NA_perc <- function(df) {
  missing_values2 <- df %>% summarize_all(funs(sum(is.na(.))/n()))
  missing_values2 <- gather(missing_values2, key="feature", value="missing_pct")
  missing_values2 <- filter(missing_values2, missing_pct > 0)
  missing_values2 %>% 
    ggplot(aes(x=reorder(feature,-missing_pct),y=missing_pct)) +
    geom_bar(stat="identity",fill="red")+
    coord_flip()+theme_bw()
}
plot_NA_perc(beedata2)


p3 <- beedata2 %>% ggplot(aes(x = goal_duration, fill = endstate)) + geom_histogram()
ggsave(filename = "goal_duration_histogram.png", plot = p3)



beedata2 %>% #filter(level_goaldate == "long_term") %>%
  filter((DurationPLUSOngoing) > 21) %>%
  ggplot(aes(x = dayofweek_created, y = goal_duration, color = endstate, alpha = 0.1)) + 
  geom_point() +
  geom_jitter() + 
  geom_violin(alpha = 0.3) +
  facet_grid(~level_goaldate) +
  ylab("Goal Duration (in days)")

p2 <- beedata2 %>% #filter(level_goaldate == "long_term") %>%
        filter((DurationPLUSOngoing) > 21) %>%
        ggplot(aes(x = month_created, fill = endstate, alpha = 0.1)) + 
        geom_density() +  
        facet_grid(~level_goaldate) +
        theme(axis.text.x= element_text(angle = -90, hjust = 0))

ggsave(filename = "goal_density_byMonth.png", plot = p2)



# show histograms bracketed by level of payment. 
beedata2 %>% ggplot(aes(x = goal_duration, fill = endstate)) + geom_histogram() + facet_grid(~level_paid)

p1 <-  beedata2 %>% filter(level_paid != "none") %>%
          ggplot(aes(x = goal_duration, fill = endstate, alpha = 0.5)) + 
          geom_density() + 
          theme(axis.text.x= element_text(angle = -90, hjust = 0)) +
          facet_grid(~level_paid)
ggsave(filename = "goal_duration_density.png", plot = p1)

ggplot(beedata2, aes(x = DurationPLUSOngoing, fill = endstate, alpha = 0.5)) +
  geom_density() 
