

#Install and add to library all of the packages below to be used in the 
# analysis later.

library(dplyr)
library(tidyr)
library(stringi)
library(ggplot2)
library(twitteR)
library(httr)
library(jsonlite)
library(tidyverse)
library(GGally)
library(mvtnorm)
library(class)
library(caret)
library(e1071)

#Create table for states and state abbreviations from MAPS package to create a
#dataset to match our states

lookup = data.frame(abb=state.abb, State = state.name)

#Pull only the State and City information out of the dataset to create less clutter.
#Change the name of State to "abb" to create a foreign key.
brews = Breweries[,c(3:4)]
colnames(brews)[2] = "abb"


#Merge the lookup table and our state tables to create a master copy. This will
#allow us to create our heatmap later.
#Change name to State to be able to count the total number of breweries per State.
brews2 = merge(brews,lookup,"abb")


#Use the count function to create a sum total of number of breweries per State.
#Print the overall dataset brewery map to see if it added correctly.
brewerymaps = count(brews2,State)
brewerymaps


#Change brewery to Breweries name
colnames(brewerymaps)[2] = "Breweries"

#Create a region column that is all lowercase to match all states
brewerymaps$region <-tolower(brewerymaps$State)

#Remove the first column so we only have state and Region
brewerymaps = brewerymaps[-1]

#Create a state map based on States.
states<- map_data("state")

#Merge dataframes brewerymaps and States in order to create 
# a heat map for breweries by state
map.df<- merge(states, brewerymaps, by = "region", all.x=T)
map.df<- map.df[order(map.df$order),]

p<-ggplot(map.df,aes(x=long,y=lat,group=group)) +
  geom_polygon(aes(fill=Breweries))+
  geom_path() + xlab("Latitude of State") + ylab("Longitude of State") +
  scale_fill_gradientn(colours = rev(heat.colors(10)), na.value ="grey90") +
  ggtitle("Breweries by State") + coord_map() 
p



#Change name to Brew_ID to create a foreign key
#Then combine both beers and breweries csv files
colnames(Beers)[5] = "Brew_ID"
combined <- merge(Beers,Breweries, by ="Brew_ID", all = TRUE)
colnames(combined)[2] = "Beer_Name"
colnames(combined)[8] = "Brewery_Name"


#Print first 6 rows and last 6 rows
head(combined)
tail(combined)

combined <- combined[complete.cases(combined$ABV),]
combinedABV <- combined[order(combined$ABV),]
tail(combinedABV)

combined <- combined[complete.cases(combined$IBU),]
combinedIBU <- combined[order(combined$IBU),]
tail(combinedIBU)



#Filter the data for non-NA ABV values. Group by state and create
#a median for every state. Then graph these medians together without a legend.
median_ABV_plot = combinedABV %>%
  filter(!is.na(ABV)) %>%
  group_by(State) %>%
  summarize(median=median(ABV),count=n())%>%
  ggplot(aes(x=State, y = median, fill = factor(if_else(State == "KY","Highest", "All Others")))) + 
  geom_bar(stat="identity", show.legend = FALSE) + 
  scale_fill_manual(name = "State", values=c("grey50","red")) + 
  ggtitle("Median Alcohol by Volume of Beer by State") + ylab("Median ABV") + 
  geom_hline(yintercept = mean(combinedABV$ABV, na.rm = TRUE), color="blue")
  median_ABV_plot

#Filter the data for non-NA IBU values. Group by state and create
#a median for every state. Then graph these medians together without a legend.
  median_IBU_plot = combined %>%
    filter(!is.na(IBU)) %>%
    group_by(State) %>%
    summarize(median=median(IBU),count=n())%>%
    ggplot(aes(x=State, y = median, fill = factor(if_else(State == "ME","Highest", "All Others")))) + 
    geom_bar(stat="identity", show.legend = FALSE) + scale_fill_manual(name = "State", values=c("grey50","red")) + 
    ggtitle("Median International Bitterness Units of Beer by State") +
    ylab("Median IBU") + 
    geom_hline(yintercept = mean(combined$IBU, na.rm = TRUE), color="blue")
  median_IBU_plot
  



#Make histogram of ABV values to show distribution
combined %>% ggplot(aes(x= ABV)) + geom_histogram(color = "Black") +
  ggtitle("Distribution of Alcohol by Volume in Beers")



#Create a scatterplot with a fitted regession line to show the relationship
# between ABV and IBU
combined %>%filter(!is.na(IBU))%>%
  filter(!is.na(ABV))%>%
  ggplot(aes(x=ABV, y = IBU)) + geom_point() +geom_smooth(method = "lm")



#Create a dataset with column called Style. If IPA is in name, label IPA
# If not an IPA but still an Ale, label Ale, if neither of these, label Other
#Then filter out others.
totalAle = combined %>%mutate(Style = if_else(str_detect(combined$Style, "IPA"), "IPA", if_else(str_detect(combined$Style, "Ale"), "Ale", "Other"))) %>%
  filter(!is.na(ABV)) %>%
  filter(!is.na(IBU)) %>%
  filter(Style == "IPA" | Style == "Ale") %>%
  select(ABV, IBU,State, Style)



#which k level is best?
## Create a test and train dataset based on the combined dataset with no missing values
### Loop for many k and one training / test partition

splitPerc = .7
trainIndices = sample(1:dim(totalAle)[1],round(splitPerc*dim(totalAle)[1]))
train = totalAle[trainIndices,]
test = totalAle[-trainIndices,]
train
test
accs = data.frame(accuracy = numeric(30), k = numeric(30))
for(i in 1:30)
{
  classifications = knn(train[,c(1:2)],test[,c(1:2)],train$Style, prob = TRUE, k = i)
  table(test$Style,classifications)
  CM = confusionMatrix(table(test$Style,classifications))
  accs$accuracy[i] = CM$overall[1]
  accs$k[i] = i
}
plot(accs$k,accs$accuracy, type = "l", xlab = "k")

#loop for many ks and the average of many training / test 
iterations = 500
numks = 30
masterAcc = matrix(nrow = iterations, ncol = numks)

for(j in 1:iterations)
{
  accs = data.frame(accuracy = numeric(30), k = numeric(30))
  trainIndices = sample(1:dim(totalAle)[1],round(splitPerc * dim(totalAle)[1]))
  train = totalAle[trainIndices,]
  test = totalAle[-trainIndices,]
  for(i in 1:numks)
  {
    classifications = knn(train[,c(1:2)],test[,c(1:2)],train$Style, prob = TRUE, k = i)
    table(classifications,test$Style)
    CM = confusionMatrix(table(classifications,test$Style))
    masterAcc[j,i] = CM$overall[1]
  }
}

MeanAcc = colMeans(masterAcc)
plot(seq(1,numks,1),MeanAcc, type = "l")


classifications = knn(train[,c(1:2)],test[,c(1:2)], train$Style,
                      prob = TRUE, k =5)
table(classifications,test$Style)

CM = confusionMatrix(table(classifications,test$Style))

fourfoldplot(CM$table,color = c("cyan", "pink"),
             conf.level = 0, margin = 1, main = "Predicition of IPAs and Ales")






model = naiveBayes(Style~ABV+IBU, data = train)
table(predict(model,test[,c(1:2)]), test$Style)
CM = confusionMatrix(table(predict(model,test[,c(1:2)]), test$Style))
CM



#Create a dataset that includes just the states Missouri, Texas, California, and Colorado.
#Then plot the distribution of the beer styles in each state and the percentages of beer styles in each state.
Oktoberfest = combined %>%mutate(Style = if_else(str_detect(combined$Style, "IPA"), "IPA", if_else(str_detect(combined$Style, "Ale"), "Ale", "Other"))) %>%
  filter(State == 'MO' | State == "TX" | State == "CO" | State == "CA") %>%
  filter(!is.na(Style)) %>%
  select(State,Style)

#Distribution
Oktoberfest %>% ggplot(aes(x = State, fill = Style)) +
  geom_bar(position = "dodge") + ggtitle("Distribution of Beer Types per Specific States")+ ylab("Count of Beer Type")
#Percentages
Oktoberfest %>% ggplot(aes(x = State, fill = Style)) +
  geom_bar(position = "fill") + ggtitle("Percentage of Beer Types per Specific States") + ylab("Percentage of Beer Type")


#Create a dataset to label the different types of ales and then filter by our four states.
#Then plot the distribution of the beer styles in each state and the percentages of ale types in each state.
Oktoberfest3 = combined %>%
  mutate(Styles = if_else(str_detect(combined$Style, "American Amber / Red Ale"), "Red Ale", if_else(str_detect(combined$Style, "American Blonde Ale"), "Blonde Ale", if_else(str_detect(combined$Style, "American Brown Ale"), "Brown Ale", if_else(str_detect(combined$Style, "American Double / Imperial IPA"), "IPA", if_else(str_detect(combined$Style, "American IPA"), "IPA", if_else(str_detect(combined$Style, "APA"), "APA", if_else(str_detect(combined$Style, "American Pale Wheat Ale"), "Pale Wheat Ale", if_else(str_detect(combined$Style, "American Porter"), "American Porter", if_else(str_detect(combined$Style, "Fruit / Vegetable Beer"), "Fruit / Vegetable Beer", "Other" )))))))))) %>%
  filter(!is.na(Styles)) %>%
  filter(Styles != "Other") %>%
  filter(State == "MO" | State == "TX" | State == "CO" | State == "CA") %>%
  select(Styles, State)

#Distribution
Oktoberfest3 %>% ggplot(aes(x = State, fill = Styles)) +
  geom_bar(position = "dodge") + ggtitle("Distribution of Ale Types per Specific States")+ ylab("Count of Beer Type")
#Percentages
Oktoberfest3 %>% ggplot(aes(x = State, fill = Styles)) +
  geom_bar(position = "fill") + ggtitle("Percentage of Ale Types per Specific States") + ylab("Percentage of Beer Type")
