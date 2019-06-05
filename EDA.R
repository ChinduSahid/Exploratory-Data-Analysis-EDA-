### Exploratiory Data analysis

library(dplyr)
library(dlookr)

# Read data
data<-read.csv("ReadyforModelling.csv") 
# Checking structure of the dataset
str(data)
# Change the structure of the variables accordingly and remove extra variables not related to this analysis
data$Answered.by.specialist<- factor(data$Answered.by.specialist)
data$Booked.Status<- factor(data$Booked.Status)
data$EnquiryYear<-factor(data$EnquiryYear)
data$DepYear<-factor(data$DepYear)
data$X<-NULL

# check dimension of the dataset
dim(data)
# get better understanding of the numeric variables
diagnose_numeric(data)
# get better understanding of the categorical varaibles
View(diagnose_category(data))
# checking correlation between numerical variables
plot_correlate(data)

# Exploring relation between target varaible(BookedStaus) and a numeric variable
categ<-target_by(data,Booked.Status)
cat_num<-relate(categ,Duration)
cat_num
summary(cat_num)
plot(cat_num) # relationship between booked.status and duration is represented using a desity plot


# Exploring relation between target variable(BookedStatus) and a categorical variable
cat_cat<-relate(categ,Allocated.Time)
cat_cat
plot(cat_cat) #mosaics plot

# Checking for skewness in numeric variables (If skewness value lies above +1 or below -1, data is highly skewed. If it lies between +0.5 to -0.5, it is moderately skewed. If the value is 0, then the data is symmetric)

data %>%
  describe() %>%
  select(variable, skewness) %>% 
  filter(!is.na(skewness)) %>% 
  arrange(desc(abs(skewness)))

# the left-skewed distribution data, that is, the variables with large positive skewness
#should consider the log or sqrt transformations to follow the normal distribution

data$log_lead.time<-sqrt(data$Lead.Time)

#sometimes it is ideal to remove outliers/anomalies to achieve a normal distribution
#diagnose anomalies of all numeric variables of data
diagnose_outlier(data)

data %>%
  plot_outlier(diagnose_outlier(data) %>% 
                 filter(outliers_ratio >= 0.5) %>% 
                 select(variables) %>% 
                 unlist())
# The plot shows that the variable infants is highly skewed
table(data$Infants)
# This variable would not be of much use in this analysis considering the highly skewed distribution and hence should be elimited
data$Infants<-NULL

plot_normality(data)

### Answering questions based on datasets (Visualisation)

# how many unique destinations are there?
data %>%select(Destination)%>%distinct()%>%count()

# desination by popularity and what is the total enquiries for each destination?

pop_destination<- data %>% group_by(Destination) %>% count(Destination) %>%ungroup()

ggplot(data=pop_destination,aes(x=reorder(as.factor(Destination),n),y=n,fill=as.factor(Destination)))+geom_bar(stat="identity")+coord_flip()+
  labs(title= "most popular destination based on enquiries", x="Enquiries",y="Destinations")

# What are the day and month wise total enquiries?

day_month_sale<- data%>%group_by(EnquiryMonth,EnquiryDay)  %>% count(Destination)%>%arrange(EnquiryMonth,EnquiryDay) %>% ungroup()
ggplot(data=day_month_sale,aes(x=EnquiryDay,y=n,group=EnquiryMonth,color=as.factor(EnquiryMonth)))+geom_line()+geom_point()+scale_x_continuous(breaks=seq(min(0),max(31),by=1))+
  labs(title="Enquiries by month per day")

ggplot(data=day_month_sale, aes(x=EnquiryDay,y=n,fill=as.factor(EnquiryDay)))+geom_bar(stat="identity")+scale_x_continuous(breaks=seq(min(0),max(31),by=1))+facet_wrap(~EnquiryMonth,ncol=2)


# Total enquiries by year and month

year_month<- data%>%group_by(EnquiryYear,EnquiryMonth) %>% count(Destination)%>%arrange(EnquiryYear)%>%ungroup()
ggplot(data=year_month,aes(x=EnquiryYear,y=n,fill=as.factor(EnquiryMonth)))+ geom_bar(stat="identity")+
  labs(title="total enquiries per year-month",x="year",y="total enquiries",fill="EnquiryMonth")

# How many enquiries were booked based on Allocated.Time
data$Booked.Status<-as.integer(data$Booked.Status)
booked_Allocated<-data%>%group_by(Allocated.Time)%>% summarise(booked=sum(Booked.Status)) %>%arrange(Allocated.Time)%>%ungroup()
