library(tidyverse)
library(tidyquant)
library(ggthemes)
library(plotly)
library(viridis)
library(hrbrthemes)
library(gcookbook)

##############################################################################################################
# LOAD DATA
##############################################################################################################

inspection = read.csv("inspection.csv", sep = ",", na.strings=c(""," ","NA"))

#converting to a tibble
insp_df = as_tibble(inspection)

##############################################################################################################
# DATA MANIPULATION
##############################################################################################################

#for any Score >=0 & <= 13, changing the Grade to A
insp_df = insp_df %>% mutate(GRADE = ifelse(SCORE >= 0 & SCORE <= 13,"A",GRADE))

#for any Score >=14 & <= 27, changing the Grade to B
insp_df = insp_df %>% mutate(GRADE = ifelse(SCORE >= 14 & SCORE <= 27,"B",GRADE))

#for any Score >= 28, changing the Grade to C
insp_df = insp_df %>% mutate(GRADE = ifelse(SCORE >= 28,"C",GRADE))

#filtering out Scores that are NA and , "Not Yet Graded" and "P".
insp_df = insp_df %>%
  filter(!is.na(insp_df$SCORE)) 

#filtering out Scores that are < 0
insp_df = insp_df %>%
  filter(!insp_df$SCORE < 0)

#filtering out Grades that are "P"
insp_df = insp_df %>%
  filter(!insp_df$GRADE == "P")

insp_df = insp_df %>%
  filter(!insp_df$GRADE == "Z")

insp_df = insp_df %>%
filter(!insp_df$GRADE == "Not Yet Graded")

#separating the inspection date into year, month and day
insp_df = insp_df %>% 
      separate(col = INSPECTION.DATE, into = c("month","day","year"), sep = "/")


#keeping only the relevant columns
insp_df$GRADE.DATE <- NULL
insp_df$RECORD.DATE <- NULL
insp_df$CRITICAL.FLAG <- NULL
insp_df$BUILDING <- NULL
insp_df$STREET <- NULL
insp_df$PHONE <- NULL
insp_df$INSPECTION.TYPE <- NULL

insp = insp_df

##############################################################################################################
# QUESTION 1  Which area is safest to eat in?
##############################################################################################################

#ggplot(insp, aes(x = BORO, y = GRADE, fill = GRADE)) +
#geom_col() +
#guides(fill = guide_legend(reverse = TRUE)) +
#labs(title= "Distribution of grades in each borough", x= "Borough", y = "Grade Count")

#subsetting 
boro = insp %>%
  group_by(BORO, GRADE) %>%
  summarize(count = n())

#plotting the data as a grouped bar chart
ggplot(boro, aes(fill = GRADE, y=count, x=BORO)) + 
  geom_bar(position="dodge", stat="identity")  +
  labs(title= "Which is the safest area to eat in NYC?", x= "Borough", y = "Grade Count")

##############################################################################################################
# QUESTION 2  Which restaurant has the worst scores?  BAR CHART
##############################################################################################################

scores = insp %>%
  group_by(DBA) %>%
  summarize(total = sum(SCORE)) %>%
  arrange(desc(total)) %>%
  head(15)

#plotting the data
ggplot(scores, aes(x = DBA, y = total)) +
  geom_bar(stat = 'identity', aes(fill = total)) +
  scale_fill_gradient(
    low = "lightgreen",
    high = "green3", guide = "legend") +
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=45, vjust=1, color = "black")) +
  labs(title="Restaurants with the worst scores", x = "Restaurant", y = "Score")


##############################################################################################################
# QUESTION 3  What cuisine has always had the worst grades?
##############################################################################################################

# Create a new theme
theme_bluewhite <- function (base_size = 11, base_family = "") {
  theme_bw() %+replace% 
    theme(
      panel.grid.major  = element_line(color = "white"),
      panel.background = element_rect(fill = "lightblue"),
      panel.border = element_rect(color = "lightblue", fill = NA),
      axis.line = element_line(color = "lightblue"),
      axis.ticks = element_line(color = "lightblue"),
      axis.text = element_text(color = "steelblue")
    )
}

#comparing by score
worst_scores = insp %>%
  group_by(CUISINE.DESCRIPTION) %>%
  summarize(score = sum(SCORE)) %>%
  arrange(desc(score)) %>%
  head(10)

#plotting the data, comparison by score
ggplot(worst_scores, aes(y = reorder(CUISINE.DESCRIPTION, score), x = score)) +
  geom_bar(stat = "identity") +
  labs(title = "Cuisines with the worst scores", x = "Score", y = "Cuisine") +
  theme_bluewhite()

##############################################################################################################
# QUESTION 4  How do the inspection grades change over time for each borough?
##############################################################################################################
 
yearly_grades = insp %>%
  group_by(year, GRADE) %>%
  summarize(count = n())

#plotting the data
ggplot(data = yearly_grades, aes(x = year, y = count, group = GRADE, colour = GRADE)) +
  geom_line(size = 2) +
  geom_point(size = 2, shape = 21, fill = "white")+
  labs(title = "Change in grades over the years", x = "Year", y = "Count") + 
  theme_bw()   

##############################################################################################################
# QUESTION 5  Which borough has the most number of restaurants? FOLLOW UP FROM QUESTION 1
##############################################################################################################

most_restaurants = insp %>%
  group_by(BORO) %>%
  summarize(count = n())

#plotting the data
pie(most_restaurants$count, labels = most_restaurants$BORO)

##############################################################################################################
# QUESTION 6 Where should I go if I want to eat the best Chinese? FOLLOW UP FROM QUESTION 3
##############################################################################################################

best_chinese = insp %>%
  filter(CUISINE.DESCRIPTION == "Chinese")%>%
  group_by(BORO) %>%
  summarize(score = sum(SCORE))


ggplot(best_chinese, aes(x = score, y = BORO, color = BORO)) +
  geom_point(size = 3) +
  labs(title = "Comparing Chinese rastaurants by borough", x = "Score", y = "Borough")
