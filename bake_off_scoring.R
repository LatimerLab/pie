setwd("~/Research Projects/Pie bake-off")
library(lme4)
library(dplyr)


#Load the data exported from the google sheet (example exported data is in main repo folder). Google sheet is at: https://docs.google.com/spreadsheets/d/1uug4lYwZ3aecvPhbIARX3scJbaQbg0SJ2DAiEoFTAsI/edit?usp=sharing
d = read.csv("Latimer Lab Pie Bake-off 2019 - Sheet1.csv",stringsAsFactors=FALSE,header=TRUE)

# Add a fake pie # 0, which will serve as the intercept term so the scores of all other pies can be evaluated against one another.
d = bind_rows(d,d[nrow(d),])
d[nrow(d),"Pie_Number"] = 0

# Make sure pies and judges are factors
d = d %>%
  mutate(Pie_Number = as.factor(Pie_Number),
         Judge_Number = as.factor(Judge_Number))

# Fit models
m_taste = lmer(Taste_Score ~ Pie_Number + (1|Judge_Number), data=d)
m_appearance = lmer(Appearance_Score ~ Pie_Number + (1|Judge_Number), data=d)
m_creativity = lmer(Creativity_Score ~ Pie_Number + (1|Judge_Number), data=d)
m_botanicality = lmer(Botanicality_Score ~ Pie_Number + (1|Judge_Number), data=d)
m_statisticality = lmer(Statisticality_Score ~ Pie_Number + (1|Judge_Number), data=d)

# Get fixed effects (pie scores)
fixef_taste = fixef(m_taste)
fixef_appearance = fixef(m_appearance)
fixef_creativity = fixef(m_creativity)
fixef_botanicality = fixef(m_botanicality)
fixef_statisticality = fixef(m_statisticality)

# Sort them from best to worst
taste_ranking = rev(sort(fixef_taste))
appearance_ranking = rev(sort(fixef_appearance))
creativity_ranking = rev(sort(fixef_creativity))
botanicality_ranking = rev(sort(fixef_botanicality))
statisticality_ranking = rev(sort(fixef_statisticality))

# Compute overall score as come of individual category scores
overall = bind_rows(fixef_taste,fixef_appearance,fixef_creativity,fixef_botanicality,fixef_statisticality)
sum = colSums(overall)
overall_ranking = rev(sort(sum))

# Print rankings
taste_ranking
appearance_ranking
creativity_ranking
botanicality_ranking
statisticality_ranking
overall_ranking


#### Compute straight average scores (as a backup method or for comparison against mixed-model scoring)
d_avg = d %>%
  group_by(Pie_Number) %>%
  summarize(Taste = mean(Taste_Score))

