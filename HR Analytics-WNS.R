#Clear everything previously in environment
rm(list=is())
dev.off()
llibrary(readr)
require(dplyr)
#Load datasets
train <- read_csv('train.csv')
test <- read_csv('test.csv')
#Get a feel of the data at hand
head(train)
head(test)
#Bind dataset for cleaning into main file
main <- bind_rows(train,test)
View(main)
str(main)
####Data Cleaning#####
#Avoid case mismatch possibility
main <- mutate_if(main, is.character, tolower)

#Check missing values
colSums(is.na(main))
colSums(is.na(main)/nrow(main)*100)

#Blank in Education
summary(as.factor(main$education))

require(ggplot2)
ggplot(train,aes(x=education, fill=as.factor(is_promoted)))+ geom_bar(position='fill')
#Lets keep it simple for now
main$education[which(is.na(main$education))] <- 'Uneducated'
table(main$education)

#NA's in previous year rating
summary(main$previous_year_rating)
summary(as.factor(main$previous_year_rating))

#Check where previous rating is NA
df <- main[which(is.na(main$previous_year_rating)), ]
table(df$length_of_service)

#We see that where previous year rating is NA, length of service is '1'.
#So NA seems justified.

ggplot(train, aes(x=as.factor(previous_year_rating), fill=as.factor(is_promoted)))+geom_bar(position ='fill')
#And it is significant!
summary(main$previous_year_rating)
main$previous_year_rating[which(is.na(main$previous_year_rating))] <- 4
ggplot(train, aes(x=previous_year_rating, fill=as.factor(is_promoted)))+geom_bar(position = 'fill')

#Check Duplicates
sum(duplicated(main[-1]))
#147 duplicate rows
(147/nrow(main))*100
#Negligible duplicacy. Keeping it as it is.

#Check no of levels.
sapply(main, n_distinct)

#See structure now
str(main)

#Some categorical variables should not be numeric.
#####----EDA-----#####

#Univariate/Bivariate Analysis---
#1 Employer id
n_distinct(main$employee_id)==nrow(main)
main$employee_id <- as.character(main$employee_id)

#2 Department
dept_df <- group_by(main, department) %>%
  summarise(dept_influence = round(sum(is_promoted, na.rm = T)/n()*100, 2)) %>%
  arrange(dept_influence)
View(dept_df)
# There seems departmental bias but actually it's not.
# 3 region

ggplot(main, aes(x = region)) +
  geom_bar(fill = 'skyblue',color = 'black') + coord_flip()

# Way too many categories in region. Pattern detection not possible.

reg_df <- group_by(main, region) %>%
  summarise(region_precentage = round(n()/nrow(master)*100,2))

View(reg_df)
# It is way too many categories. Better to drop.
main$region <- NULL
# 4 education

edu_df <- group_by(main, education) %>%
  summarise(edu_influence = round(sum(is_promoted, na.rm = T)/n()*100,2)) %>%
  arrange(desc(edu_influence))

View(edu_df)
# Slightly higher fraction of promotions in Masters and Above category.
# Education matters !!!



# 5 gender
ggplot(main[!is.na(main$is_promoted),],
       aes(x = gender, fill =  as.factor(is_promoted))) +
  geom_bar()









require(caTools)
set.seed(999)
i<- sample.split(main$is_promoted, SplitRatio = 0.75)
trn <- train[i, ]
val <- train[!i, ]

# Building 1st default model with all predictors
log_1 <- glm(is_promoted ~ ., data = trn, family = 'binomial')
summary(log_1)


















