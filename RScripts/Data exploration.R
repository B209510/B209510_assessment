# Loading required libraries
library(tidyverse)
library(NHSRdatasets)
library(here)
library(knitr)
library(scales)
library(lubridate)
library(caret)

#Load the LOS_model data.
data(LOS_model)

LOS_data <- LOS_model

# Initial exploration of dataset

class(LOS_data)

LOS_data

glimpse(LOS_data)

head(LOS_data)
tail(LOS_data,10)

LOS_data %>% 
  map(is.na) %>% 
  map(sum)

summary(LOS_data)


vignette("LOS_model")
# Adding index
LOS_data <- rowid_to_column(LOS_data,"index")

# Saving raw data
write.csv(LOS_data,here("Rawdata","LOS_data.csv"))

# Data manipulation: Death variable will need to be converted to a factor

LOS_data$Death <- as.factor(LOS_data$Death)
class(LOS_data$Death)
levels(LOS_data$Death)

LOS_data <- LOS_data %>% 
  mutate(Outcome=fct_collapse(Death,
                              "Discharge"="0",
                              "Death"="1"))

LOS_data
class(LOS_data$Outcome)
levels(LOS_data$Outcome)


# Exploring dataset regarding outcome

LOS_data %>% 
  ggplot(aes(x=Age,y=LOS,color=Outcome))+
  geom_point()+
  facet_wrap(~Outcome)+
  ggtitle("Lenght of stay and age variables regarding outcome")+
  ylab("Lenght of stay in days")+
  scale_x_continuous(limits = c(0,99),expand = c(0,0))+
  scale_y_continuous(limits = c(0,20),expand=c(0,0))+
  scale_fill_gradient("Legend",low = "lightgreen",high = "red")+
  theme_bw()+
  ggeasy::easy_center_title()




# Subsetting dataframe with the variables of interest

subset_LOS <- LOS_data %>% 
  select(index,LOS,Age,Outcome)

# Summary statistics of subseted dataframe
summary(subset_LOS)

# Saving sunbset raw data
write_csv(subset_LOS, here("Rawdata", "subsetLOS.csv"))


############## May need to divide into test and training data
subset_LOS
nrow(subset_LOS)

prop<-(1-(15/nrow(subset_LOS)))

print(prop)

set.seed(333)

trainIndex <- createDataPartition(subset_LOS$index, p = prop, 
                                  list = FALSE, 
                                  times = 1)
head(trainIndex)

LOStrain <- subset_LOS[ trainIndex,]
nrow(LOStrain)

write_csv(LOStrain, here("Data", "Los_subset_train.csv"))

LOStest  <- subset_LOS[-trainIndex,]
nrow(LOStest)

LostestMarker <- LOStest[1,]

LostestMarker

write_csv(LostestMarker, here("Data", "subset_Los_test_marker.csv"))

LosTest  <- LOStest[2:nrow(LOStest),]

write_csv(LosTest, here("Data", "LosTest.csv"))

################## Exploratory analysis using subset_LOS - may need changed to ?train data

# LOS - Outcome

subset_LOS %>% 
  group_by(Outcome) %>% 
  summarise(mean_LOS=mean(LOS),
            median_LOS=median(LOS),
            sd_LOS=sd(LOS),
            iqr_LOS=IQR(LOS)) %>% 
  kable(caption = "Summary statistics for LOS variable regarding outcome")


subset_LOS %>% 
  ggplot(aes(x=LOS,fill=..x..))+
  geom_histogram(binwidth = 0.5)+
  xlab("Lenght of stay in days")+
  ylab("Count")+
  ggtitle("Histogram of LOS distribution")+
  scale_x_continuous(limits = c(0,20),expand = c(0,0))+
  scale_y_continuous(limits = c(0,50),expand=c(0,0))+
  scale_fill_gradient("Legend",low = "lightgreen",high = "red")+
  theme_bw()+
  ggeasy::easy_center_title()


subset_LOS %>% 
  ggplot(aes(x=LOS,fill=..x..))+
  geom_histogram(binwidth = 0.5)+
  facet_wrap(~Outcome)+
  xlab("Lenght of stay in days")+
  ylab("Count")+
  ggtitle("Histograms of LOS per Outcome")+
  scale_x_continuous(limits = c(0,20),expand = c(0,0))+
  scale_y_continuous(limits = c(0,50),expand=c(0,0))+
  scale_fill_gradient("Legend",low = "lightgreen",high = "red")+
  theme_bw()+
  ggeasy::easy_center_title()

subset_LOS %>% 
  ggplot(aes(x=Outcome,y=LOS))+
  stat_boxplot(geom = "errorbar")+
  geom_boxplot(aes(fill=Outcome))+
  scale_fill_manual(values = c("cadetblue4","coral2"))+
  xlab("Outcome")+
  labs(title=" LOS comparison between outcome",caption = "Note: plus signal indicative of mean values")+
  stat_summary(fun.y=mean,shape=3,col="white",geom="point",size=2)+
  ylab("Lenght of stay in days")+
  ylim(0,20)+
  theme_bw()+
  theme(legend.position = "none",text = element_text(size=14))+
  ggeasy::easy_center_title()

subset_LOS %>% 
  wilcox.test(LOS~Outcome,data=.,conf.int=T) 


# Age -> Outcome

subset_LOS %>% 
  group_by(Outcome) %>% 
  summarise(mean_Age=mean(Age),
            median_Age=median(Age),
            sd_Age=sd(Age),
            iqr_Age=IQR(Age)) %>% 
  kable(caption = "Summary statistics for Age variable regarding outcome")

subset_LOS %>% 
  ggplot(aes(x=Age))+
  geom_histogram(alpha=0.2,fill="blue",binwidth = 1.5)+
  xlab("Age in years")+
  ylab("Count")+
  ggtitle("Histogram of Age distribution")+
  scale_x_continuous(limits = c(0,99),expand = c(0,0))+
  scale_y_continuous(limits = c(0,15),expand=c(0,0))+
  theme_bw()+
  ggeasy::easy_center_title()


subset_LOS %>% 
  ggplot(aes(x=Age))+
  geom_histogram(alpha=0.2,fill="blue",binwidth = 1.5)+
  facet_wrap(~Outcome)+
  scale_x_continuous(limits = c(0,99),expand = c(0,0))+
  scale_y_continuous(limits = c(0,15),expand=c(0,0))+
  xlab("Age in years")+
  ylab("Count")+
  ggtitle("Histograms of Age per Outcome")+
  theme_bw()+
  ggeasy::easy_center_title()

subset_LOS %>% 
  ggplot(aes(x=Outcome,y=Age))+
  stat_boxplot(geom = "errorbar")+
  geom_boxplot(aes(fill=Outcome))+
  scale_fill_manual(values = c("cadetblue4","coral2"))+
  xlab("Outcome")+
  labs(title=" Age comparison between outcome",caption = "Note: plus signal indicative of mean values")+
  stat_summary(fun.y=mean,shape=3,col="white",geom="point",size=2)+
  ylab("Age in years")+
  ylim(0,100)+
  theme_bw()+
  theme(legend.position = "none",text = element_text(size=14))+
  ggeasy::easy_center_title()

subset_LOS %>% 
  wilcox.test(Age~Outcome,data=.,conf.int=T) 


# Age vs LOS

subset_LOS %>% 
  ggplot(aes(x=Age,y=LOS))+
  geom_point(color="red")+
  xlab("Age in years")+
  labs(title=" LOS comparison regarding Age")+
  ylab("Lenght of stay in days")+
  theme_bw()+
  theme(legend.position = "none",text = element_text(size=14))+
  ggeasy::easy_center_title()

a <-  cor.test(subset_LOS$Age, subset_LOS$LOS, method = "spearman")
a
