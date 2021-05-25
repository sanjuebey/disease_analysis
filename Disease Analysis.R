#To check the directory and to read the file
getwd()
setwd("C:/sje_ds/Personal")
disease_df <- read.csv("disease_assig.csv")

str(disease_df)
summary(disease_df)
class(disease_df)
nrow(disease_df)
ncol(disease_df)
head(disease_df)
tail(disease_df)

#Let's convert the age column to type numeric
disease <- disease_df
disease$Age <- as.numeric(disease$Age)
class(disease$Age)

#Let's clean the data of column sex
mode(disease_df$sex)
class(disease_df$sex)
levels(disease_df$sex)
which(disease_df$sex=="-1")
table(disease_df$sex)

col_sex <- as.character(disease_df$sex)
col_sex[22] <- "Male"
col_sex
disease_df$sex <- as.factor(col_sex)
class(disease_df$sex)
levels(disease_df$sex)

#Let's omit the null values of column chocolate
table(disease_df$chocolate)
disease_df$chocolate[which(is.na(disease_df$chocolate))] <- "yes"
disease_df$chocolate

#Let's clean the data of column fruit salad
disease_df$fruitsalad
disease_df$fruitsalad[46] <- "yes"
colsalad <- as.character(disease_df$fruitsalad)
colsalad <- as.factor(colsalad)
disease_df$fruitsalad <- colsalad
disease_df$fruitsalad


disease_df$timesupper
mean(disease_df$timesupper, na.rm = TRUE)
#1981.818 is not possible in time,therefore we convert it into time which is equivalent to 2020

disease_df$timesupper[which(is.na(disease_df$timesupper))] <- "2020"
disease_df$timesupper
coltime <- strptime(disease_df$timesupper, format = "%H%M")
coldate <- "1940-04-18"
disease_df$timesupper <- paste(coldate, disease_df$timesupper)
head(disease_df$timesupper)
coltime <- strptime(disease_df$timesupper, format = "%Y-%m-%d %H%M")
disease_df$timesupper <- coltime
disease_df$timesupper

#Let's clean the data of column onsetdate
disease_df$onsetdate
which(disease_df$onsetdate == '18/4')
colonset <- as.character(disease_df$onsetdate)
colonset[16] <- "18-Apr"
disease_df$onsetdate <- colonset
disease_df$onsetdate

#Let's clean the data of column onsettime
disease_df$onsettime
disease_df$onsettime[which(disease_df$onsettime == "30")] <-"0030"
disease_df$onsettime[which(disease_df$onsettime == "100")] <-"0100"
disease_df$onsettime[which(disease_df$onsettime == "200")] <-"0200"
disease_df$onsettime[which(disease_df$onsettime == "230")] <-"0230"
disease_df$onsettime[which(disease_df$onsettime == "215")] <-"0215"
coltime <- strptime(disease_df$onsettime, format = "%H%M")
coltime <- format(coltime, "%H:%M")
coltime
colday <- paste(disease_df$onsetdate, coltime)
colday
disease_df$onsettime <- colday
disease_df$onsettime

#Let's find the most consumed food item among the patients
dis_food <- disease_df[,7:20]
dis_food
summary(dis_food)
#barplot(table(dis_food$baked_hamburgur))

ggplot(dis_food) + 
  geom_bar(aes(x = dis_food$baked_hamburgur)) + 
  coord_flip()

ggplot(dis_food) + 
  geom_bar(aes(x = dis_food$spinach)) + 
  coord_flip()

ggplot(dis_food) + 
  geom_bar(aes(x = dis_food$mashed_potato)) + 
  coord_flip()

ggplot(dis_food) + 
  geom_bar(aes(x = dis_food$cabbages)) + 
  coord_flip()

ggplot(dis_food) + 
  geom_bar(aes(x = dis_food$jello)) + 
  coord_flip()

ggplot(dis_food) + 
  geom_bar(aes(x = dis_food$rolls)) + 
  coord_flip()


ggplot(dis_food) + 
  geom_bar(aes(x = dis_food$brown)) + 
  coord_flip()

ggplot(dis_food) + 
  geom_bar(aes(x = dis_food$milk)) + 
  coord_flip()
#From the plots we know that baked hamburger is the most consumed food item

#Let's find the average age of people who are ill using boxplot
str(disease_df$Age)
disease_df$Age <- as.numeric(disease_df$Age)
boxplot(disease_df$Age)
summary(disease_df$Age)

#Let's visualize the gender ratio
plot(disease_df$sex)
