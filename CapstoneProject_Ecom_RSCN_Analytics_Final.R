##############################################
#Data Loading
##############################################
library(readxl)
library(stringr)
library(lubridate)
library(dplyr)
library(tidyr)
library(sqldf)
library(ggplot2)
library(MASS)
library(carData)
library(car)
library(DataCombine)
library(zoo)
library(imputeTS)
library(DAAG)
library(data.table)
library( 'ggplot2' )
library( 'cowplot' )
library( 'gridExtra' )

# Load the main dataset for consumer electronics
ce_sale <- read.csv("ConsumerElectronics.csv", stringsAsFactors = FALSE)
#Load Monthly NPS score
monthly_nps <- as.data.frame(read_excel("Media data and other information.xlsx", sheet = "Monthly NPS Score", skip =1, col_names = TRUE))
monthly_nps <- monthly_nps[-1]
#Function to clean NPS score dataset and create a data frame
format_nps <- function(x) {
  
  tmp_df <- data.frame(matrix(ncol = 2 , nrow = length(monthly_nps)))
  y <- 1
  for(i in colnames(monthly_nps)){
    tmp_df[y,1] <- i
    tmp_df[y,2] <- monthly_nps[,y]
    y <- y + 1
  }
  colnames(tmp_df)[1] <- "Month_Year"
  colnames(tmp_df)[2] <- "NPS"
  tmp_df$Month_Year <- str_replace_all(tmp_df$Month_Year, "'", "/01/")
  tmp_df[1,1] <- "Jul/01/15"
  tmp_df[12,1] <- "Jun/01/16"
  tmp_df[3,1] <- "Sep/01/15"
  tmp_df$Month_Year <- as.character(as.Date(tmp_df$Month_Year,format="%b/%d/%y"))
  tmp_df <- separate(tmp_df,Month_Year,into=c("Year","Month"),sep="-")
  tmp_df
}
#call function to get clean NPS score data frame
monthly_nps <- format_nps(monthly_nps)

str(monthly_nps$Month)
#Changing Month format of NPS score data set to be aligned with main data set
monthly_nps$Month<-  ifelse(substr(monthly_nps$Month, 1, 1) == '0', sub("^.", "", monthly_nps$Month), monthly_nps$Month)

# Fetching data between July 2015 and June 2016

ce_sale_new <- sqldf("select * from ce_sale where Year == 2015 and Month >= 7 or Year == 2016 and Month <= 6;")


# Filtering data for product sub-categories  - camera accessory, home audio and gaming accessory 

ce_sales_final <- sqldf("select * from ce_sale_new 
                         where product_analytic_sub_category == 'CameraAccessory'
                         or product_analytic_sub_category == 'GamingAccessory' 
                         or product_analytic_sub_category == 'HomeAudio';") 

str(ce_sales_final)
summary(ce_sales_final)

##################################################

#Exploratory Data Analysis and Data Cleaning
#################################################

# Checking for NAs

check_nas <- sapply(ce_sales_final, function(x) sum(is.na(x)))
check_nas

#There are 1672 NAs in columns gmv , cust_id , pincode

# Removing columns which are not be required for Analysis/Modelling
ce_sales_final[,1] <- NULL
ce_sales_final$order_id <- NULL
ce_sales_final$order_item_id <- NULL
ce_sales_final$cust_id <- NULL
ce_sales_final$pincode <- NULL
ce_sales_final$product_analytic_super_category <- NULL
ce_sales_final$product_analytic_category <- NULL
#ce_sales_final$product_analytic_vertical <- NULL
ce_sales_final$deliverybdays <- NULL
ce_sales_final$deliverycdays <- NULL


# converting Order_date column as Date
ce_sales_final$order_date <- as.POSIXct(ce_sales_final$order_date,format="%Y-%m-%d %H:%M:%S")

#Removing Data points with MRP 0 or less
ce_sales_final <- subset(ce_sales_final,product_mrp>0)


# Removing Data points where GMV > product_mrp * units
ce_sales_final <- na.omit(ce_sales_final)
ce_sales_final <- subset(ce_sales_final, (product_mrp*units)>=gmv)

#Removing Negative value from product_procurement_sla and replacing it with 0

ce_sales_final$product_procurement_sla <- ifelse(ce_sales_final$product_procurement_sla < 0 , 0 ,ce_sales_final$product_procurement_sla)



# Converting payment type to numeric for linear model
ce_sales_final$is_COD <- ifelse(ce_sales_final$s1_fact.order_payment_type=='COD',1,0)
ce_sales_final$s1_fact.order_payment_type <- NULL




##################################################

#Deriving Data
#################################################
ce_sales_final$Week <- week(ce_sales_final$order_date)


#Continuing No. if weeks after Dec 2015 instead of resetting it to 1 from Jan 2016
ce_sales_final$Week <- ifelse(ce_sales_final$Year==2016,ce_sales_final$Week+53,ce_sales_final$Week)

# Keeping the Week# uniform from 1 to 54
#The 1st week of July 2015 is 1 and last week of june 2016 is 54.
ce_sales_final$Week <- ce_sales_final$Week-25

#Finding day of the week
ce_sales_final$weekday <- weekdays(ce_sales_final$order_date)

#List_price = gmv/units
ce_sales_final$list_price <- ce_sales_final$gmv/ce_sales_final$units

#Promotion offered
ce_sales_final$promotion_off <- (ce_sales_final$product_mrp - ce_sales_final$list_price)/ce_sales_final$product_mrp

#Adding holiday as a new variable special_sale

holiday_list <- c("2015-07-18" ,"2015-07-19" , "2015-08-15" , "2015-08-16","2015-08-17",
                  "2015-08-28" , "2015-08-29" , "2015-08-30" ,
                  "2015-10-15", "2015-10-16" , "2015-10-17",
                  "2015-11-07" , "2015-11-08" , "2015-11-09" , "2015-11-10",
                  "2015-11-11" , "2015-11-12" , "2015-11-13" , "2015-11-14",
                  "2015-12-25","2015-12-26","2015-12-27","2015-12-28","2015-12-29","2015-12-30",
                  "2015-12-31","2016-01-01","2016-01-02","2016-01-03",
                  "2016-01-20","201-01-21","2016-01-22",
                  "2016-02-01","201-02-02","2016-02-20",
                  "2016-02-21","2016-02-14","2016-02-15",
                  "2016-03-07","2016-03-08","2016-03-09",
                  "2016-05-25","2016-05-26","2016-05-27")
#Special sale is number of holidays in a week
ce_sales_final$special_sale <- ifelse(format(ce_sales_final$order_date , format="%Y-%m-%d") %in% holiday_list, 1,0)

# Tagging product based of mrp and units sold.

# Load product lists
product_list <- as.data.frame(read_excel("Media data and other information.xlsx", sheet = "Product List", skip =1, col_names = TRUE))
colnames(product_list)[1] <- "product_analytic_vertical"

product_vertical_avg_mrp <- summarise(group_by(ce_sales_final,product_analytic_vertical),avg_mrp = mean(product_mrp))

product_list <- merge(product_list,product_vertical_avg_mrp,by.x=c("product_analytic_vertical"),by.y=c("product_analytic_vertical"))
product_list$premium_product <- ifelse(product_list$avg_mrp > 5000 , 1,0)
product_list$mass_product <- ifelse(product_list$Frequency > 5000 & product_list$avg_mrp < 5000 , 1,0)
product_list$aspiring_product <- ifelse(product_list$Frequency < 5000 & product_list$avg_mrp < 5000 , 1,0)
product_list$product_under_5000 <- ifelse(product_list$avg_mrp < 5000 , 1,0)
product_list$product_btwn_5000_10000 <- ifelse(product_list$avg_mrp >= 5000 & product_list$avg_mrp < 10000 , 1,0)
product_list$product_btwn_10000_15000 <- ifelse(product_list$avg_mrp >= 10000 & product_list$avg_mrp < 15000 , 1,0)
product_list$product_btwn_15000_20000 <- ifelse(product_list$avg_mrp >= 15000 & product_list$avg_mrp < 20000 , 1,0)
product_list$product_above_20000 <- ifelse(product_list$avg_mrp > 20000 , 1,0)
ce_sales_final <- merge(ce_sales_final,product_list[,c(1,5,6,7,8,9,10,11,12)],by.x=c("product_analytic_vertical"),by.y=c("product_analytic_vertical"))
#####################################################################################################
#Initial EDA and Visualization
########################################################################################################

#GMV vs Year
ggplot(ce_sales_final,aes(as.factor(Year),gmv)) + geom_bar(stat="identity")
#Revenue in 2016 was little higher than that in 2015

ggplot(ce_sales_final,aes(as.factor(paste(ce_sales_final$Year,ce_sales_final$Month,sep="-")),gmv)) + geom_bar(stat="identity",aes(fill = product_analytic_sub_category)) +xlab("Year-Month")+ylab("GMV")+ggtitle("Year-Month Vs GMV")
#Maximum Revenue was in Month Sept 2015
#Camera Accessories generated maximum Revenue in Sept 2015
ggplot(ce_sales_final,aes(as.factor(paste(ce_sales_final$Year,ce_sales_final$Month,sep="-")),gmv)) + geom_bar(stat="identity",aes(fill = product_analytic_sub_category),position="dodge") +xlab("Year-Month")+ylab("GMV")+ggtitle("Year-Month Vs GMV")
# Camera Accessories generated maximum Revenue between July 2015-June 2016

gmv_week <- summarise(group_by(ce_sales_final,Week,product_analytic_sub_category),GMV = sum(gmv))
ggplot(gmv_week,aes(Week,GMV),) + geom_bar(stat="identity",aes(fill = product_analytic_sub_category),position="dodge") +xlab("Week")+ylab("GMV")+ggtitle("Week Vs GMV")
#Week 17 has highest revenue of 37084412 , out of that HomeAudio revenue was very high

units_week <- summarise(group_by(ce_sales_final,Week,product_analytic_sub_category),Units = sum(units))
ggplot(units_week,aes(Week,Units),) + geom_bar(stat="identity",aes(fill = product_analytic_sub_category),position="dodge")
#in week 17 maximum  number of units were sold , out of that Gaming Accessories were sold maximum

units_week_cod <- summarise(group_by(ce_sales_final,Week,is_COD),Units = sum(units))
ggplot(units_week_cod,aes(Week,Units),) + geom_bar(stat="identity",aes(fill = as.factor(is_COD)),position="dodge")
#in week 17 maximum  number of units were sold , out of that maximum was COD.

week_special_sale <- summarise(group_by(ce_sales_final,Week),no_special_sale = sum(special_sale))
ggplot(week_special_sale,aes(Week,no_special_sale)) + geom_bar(stat="identity")
# Week 17 had maximum special discounted sale , Hence Maxium revenue.

week_prom_off_avg <- summarise(group_by(ce_sales_final,Week),avg_promotion_off = mean(promotion_off))
gmv_week <- summarise(group_by(ce_sales_final,Week),GMV = sum(gmv))
merge_prom_off_gmv <- merge(week_prom_off_avg,gmv_week,by=c("Week"))
ggplot(merge_prom_off_gmv, aes(avg_promotion_off,GMV)) + geom_jitter()
# Maximum revenue is generated when promotion discount offered is between 40-55%

##################################################################################################

# After derivation of holiday calendar,Order date field is not required

ce_sales_final$order_date<-NULL

############################################################################################################

# Load Monthly Advertisement spends

monthly_investment <- as.data.frame(read_excel("Media data and other information.xlsx", sheet = "Media Investment", skip =1, col_names = FALSE))

# Clean and Prepare Advertisement spends Date

monthly_investment <- monthly_investment[-1,]
colnames(monthly_investment) <- monthly_investment[1,]
monthly_investment <- monthly_investment[-1,]
#removing total investment as the goal is to find optimal budget allocation for different marketing levers
monthly_investment <- monthly_investment[,-3]

# removing NA and replacing wih 0
monthly_investment$Radio <- ifelse(is.na(monthly_investment$Radio),0,monthly_investment$Radio)
monthly_investment$Other <- ifelse(is.na(monthly_investment$Other),0,monthly_investment$Other)

# Converting all to numeric,multiplying with 1crore and dividing by 4.3 (to join this data with weekly aggregated data)
# considering 4.3 weeks per month on an average.
# 10000000/4.3 = 2325581.4
add_conv <- 2325581.4
monthly_investment$TV<-as.numeric(monthly_investment$TV)*add_conv
monthly_investment$Digital<-as.numeric(monthly_investment$Digital)*add_conv
monthly_investment$Sponsorship<-as.numeric(monthly_investment$Sponsorship)*add_conv
monthly_investment$`Content Marketing`<-as.numeric(monthly_investment$`Content Marketing`)*add_conv
monthly_investment$`Online marketing`<-as.numeric(monthly_investment$`Online marketing`)*add_conv
monthly_investment$Affiliates<-as.numeric(monthly_investment$Affiliates)*add_conv
monthly_investment$SEM<-as.numeric(monthly_investment$SEM)*add_conv
monthly_investment$Radio<-as.numeric(monthly_investment$Radio)*add_conv
monthly_investment$Other<-as.numeric(monthly_investment$Other)*add_conv

#################################################################################################################
##### Creating dataset for  for 3 Product sub categories and joining with NPS and ad spends data
dataset_per_product_sub_category <- function(data,x) {
  data_new<- subset(data, data$product_analytic_sub_category == x)
  data_new$product_analytic_sub_category<-NULL
  #Merge NPS and Weekly Ad Spends
  data_new <- merge(data_new,monthly_nps,by.x = c("Year","Month"),by.y = c("Year","Month") )
  data_new <- merge(data_new,monthly_investment,by.x = c("Year","Month"),by.y = c("Year","Month") )
  data_new$product_analytic_vertical <- NULL
  #Removing few columns as it wont be needed
  data_new$weekday <- NULL
  data_new$Year <- NULL
  data_new$Month <- NULL
  
  # Aggregating w.r.t year,month,week
  data_for_sum <- data_new[,-which(colnames(data_new) %in% c("sla","product_mrp","product_procurement_sla","list_price","promotion_off","NPS","TV","Digital","Sponsorship","Content Marketing","Online marketing","Affiliates","SEM","Radio","Other"))]
  data_per_week_1 <- aggregate(. ~ Week,data_for_sum,sum)
  data_per_week_2 <- aggregate(cbind(data_new$sla,data_new$product_mrp,data_new$product_procurement_sla,data_new$list_price,data_new$promotion_off)~ Week,data_new,mean)
  colnames(data_per_week_2) <- c("Week","sla","product_mrp","product_procurement_sla","list_price","promotion_off")
  data_per_week <- merge(data_per_week_1,data_per_week_2,by=c("Week"))
  data_per_week_3 <- aggregate(cbind(data_new$NPS,data_new$TV,data_new$Digital,data_new$Sponsorship,data_new$`Content Marketing`,data_new$`Online marketing`,data_new$Affiliates,data_new$SEM,data_new$Radio,data_new$Other)~ Week,data_new,max)
  colnames(data_per_week_3) <- c("Week","NPS","TV","Digital","Sponsorship","Content Marketing","Online marketing","Affiliates","SEM","Radio","Other")
  data_per_week <- merge(data_per_week,data_per_week_3,by=c("Week"))
  data_per_week
  
}
 
# Get final data set for each product sub category after mergning with Add spends data , NPS data 
camera_accessory_final <- dataset_per_product_sub_category(ce_sales_final , 'CameraAccessory')
gameing_accessory_final <- dataset_per_product_sub_category(ce_sales_final , 'GamingAccessory')
home_audio_final <- dataset_per_product_sub_category(ce_sales_final , 'HomeAudio')

####################
#Copying Df's for Plots
####################

gameing_accessory_final_copy <- data.frame(gameing_accessory_final)
camera_accessory_final_copy <- data.frame(gameing_accessory_final)
home_audio_final_copy <- data.frame(gameing_accessory_final)

colnames(gameing_accessory_final_copy)[colnames(gameing_accessory_final_copy) == 'Content Marketing'] <- 'ContentMarketing'
colnames(camera_accessory_final_copy)[colnames(camera_accessory_final_copy) == 'Content Marketing'] <- 'ContentMarketing'
colnames(home_audio_final_copy)[colnames(home_audio_final_copy) == 'Content Marketing'] <- 'ContentMarketing'
colnames(gameing_accessory_final_copy)[colnames(gameing_accessory_final_copy) == 'Online marketing'] <- 'Onlinemarketing'
colnames(camera_accessory_final_copy)[colnames(camera_accessory_final_copy) == 'Online marketing'] <- 'Onlinemarketing'
colnames(home_audio_final_copy)[colnames(home_audio_final_copy) == 'Online marketing'] <- 'Onlinemarketing'



###############
#PLOTS
###############
Plots_EDA <- function(dataset,name){
  plot1 <- ggplot(dataset,aes(TV,gmv))+geom_point()+ geom_smooth(method="lm")+ggtitle(name) + labs(x = "TV Ad spends", y = "gmv")
  plot1

  plot2 <- ggplot(dataset,aes(Digital,gmv))+geom_point()+ geom_smooth(method="lm")+ggtitle(name) + labs(x = "Digital Ad Spend", y = "gmv")
  plot2
  
  plot3 <- ggplot(dataset,aes(Sponsorship,gmv))+geom_point()+ geom_smooth(method="lm")+ggtitle(name) + labs(x = "Sponsorship Ad Spend", y = "gmv")
  plot3
  
  plot4 <- ggplot(dataset,aes(Content.Marketing,gmv))+geom_point()+ geom_smooth(method="lm")+ggtitle(name) + labs(x = "Content.Marketing Ad Spend", y = "gmv")
  plot4
  
  plot5 <- ggplot(dataset,aes(Online.marketing,gmv))+geom_point()+ geom_smooth(method="lm")+ggtitle(name) + labs(x = "Online.marketing Ad Spend", y = "gmv")
  plot5
  
  plot6 <- ggplot(dataset,aes(Affiliates,gmv))+geom_point()+ geom_smooth(method="lm")+ggtitle(name) + labs(x = "Affiliates AdSpend", y = "gmv")
  plot6
  
  plot7 <- ggplot(dataset,aes(SEM,gmv))+geom_point()+ geom_smooth(method="lm")+ggtitle(name) + labs(x = "SEM Ad Spend", y = "gmv")
  plot7
  
  plot_grid(plot1,plot2,plot3,plot4,plot5,plot6,plot7)
  
}



Plots_EDA(gameing_accessory_final_copy,"Gaming Accessory")
Plots_EDA(camera_accessory_final_copy,"Camera Accessory")
Plots_EDA(home_audio_final_copy,"Home Audio")

################################################################################################################
# Data Modelling
###############################################################################################################

# Liner Modelling
###############################################################################################################

###############################################################################################################
# Building Linear Model for camera Accessory Dataset
###############################################################################################################


camera_accessory_final_LM <- camera_accessory_final
# Removing week column.This makes the dataset ready to build linear model.

camera_accessory_final_LM$Week <-NULL

#Removing list_price and units as it directly co-related to gmv
camera_accessory_final_LM$list_price <-NULL
camera_accessory_final_LM$units <-NULL
# scalling
camera_accessory_final_LM[,2:ncol(camera_accessory_final_LM)] <- scale(camera_accessory_final_LM[,2:ncol(camera_accessory_final_LM)])

# removing variables which cannot be scalled
camera_accessory_final_LM$product_btwn_10000_15000 <- NULL
camera_accessory_final_LM$product_btwn_15000_20000 <- NULL

# Divide into training and test data set
set.seed(100)

# randomly generate row indices for train dataset
trainindices_camera= sample(1:nrow(camera_accessory_final_LM), 0.8*nrow(camera_accessory_final_LM))
# generate the train data set
train_camera = camera_accessory_final_LM[trainindices_camera,]

#Similarly store the rest of the observations into an object "test".
test_camera = camera_accessory_final_LM[-trainindices_camera,]


Cam_Model1<-lm(gmv~.,data=train_camera)

summary(Cam_Model1)

alias(Cam_Model1)



# Perform stepAIC to remove insignificant variables


Cam_ModelStepAIC <- stepAIC(Cam_Model1 , direction = "both")
Cam_ModelStepAIC

#building model on variables after stepAIC


Cam_Model2<-lm(formula = gmv ~ is_COD + special_sale + premium_product + 
                 mass_product + aspiring_product + product_mrp + product_procurement_sla + 
                 promotion_off + NPS + TV + Digital + `Content Marketing` + 
                 Affiliates + SEM + Radio + Other, data = train_camera)
summary(Cam_Model2)
vif(Cam_Model2)


#removing  promotion_off as it has high pvalue


Cam_Model3<-lm(formula = gmv ~ is_COD + special_sale + premium_product + 
                 mass_product + aspiring_product + product_mrp + product_procurement_sla + 
                 NPS + TV + Digital + `Content Marketing` + 
                 Affiliates + SEM + Radio + Other, data = train_camera)
summary(Cam_Model3)
vif(Cam_Model3)

#removing product_procurement_sla as it has high pvalue


Cam_Model4<-lm(formula = gmv ~ is_COD + special_sale + premium_product + 
                 mass_product + aspiring_product + product_mrp + 
                 NPS + TV + Digital + `Content Marketing` + 
                 Affiliates + SEM + Radio + Other, data = train_camera)
summary(Cam_Model4)
vif(Cam_Model4)


#removing aspiring_product as it has high pvalue


Cam_Model5 <- lm(formula = gmv ~ is_COD + special_sale + premium_product + 
                   mass_product + product_mrp + 
                   NPS + TV + Digital + `Content Marketing` + 
                   Affiliates + SEM + Radio + Other, data = train_camera)

summary(Cam_Model5)
vif(Cam_Model5)



#removing Radio as it has high pvalue


Cam_Model6<-lm(formula = gmv ~ is_COD + special_sale + premium_product + 
                 mass_product + product_mrp + 
                 NPS + TV + Digital + `Content Marketing` + 
                 Affiliates + SEM + Other, data = train_camera)
summary(Cam_Model6)
vif(Cam_Model6)


#removing special_sale as it has high pvalue


Cam_Model7<-lm(formula = gmv ~ is_COD + premium_product + 
                 mass_product + product_mrp + 
                 NPS + TV + Digital + `Content Marketing` + 
                 Affiliates + SEM + Other, data = train_camera)
summary(Cam_Model7)
vif(Cam_Model7)

#removing Affiliates as it has high p value

Cam_Model8<-lm(formula = gmv ~ is_COD + premium_product + 
                 mass_product + product_mrp + 
                 NPS + TV + Digital + `Content Marketing` + 
                 SEM + Other, data = train_camera)
summary(Cam_Model8)
vif(Cam_Model8)

#removing NPS as it has high p value

Cam_Model9<-lm(formula = gmv ~ is_COD + premium_product + 
                 mass_product + product_mrp + 
                 TV + Digital + `Content Marketing` + 
                 SEM + Other, data = train_camera)
summary(Cam_Model9)
vif(Cam_Model9)

#removing is_COD as it has high pvalue
Cam_Model10<-lm(formula = gmv ~ premium_product + 
                  mass_product + product_mrp + 
                  TV + Digital + `Content Marketing` + 
                  SEM + Other, data = train_camera)
summary(Cam_Model10)
vif(Cam_Model10)

#removing Other as it has high p value
Cam_Model11<-lm(formula = gmv ~ premium_product + 
                  mass_product + product_mrp + 
                  TV + Digital + `Content Marketing` + 
                  SEM, data = train_camera)
summary(Cam_Model11)
vif(Cam_Model11)

#removing TV as it has high p value
Cam_Model12<-lm(formula = gmv ~ premium_product + 
                  mass_product + product_mrp + 
                  Digital + `Content Marketing` + 
                  SEM, data = train_camera)
summary(Cam_Model12)
vif(Cam_Model12)

#removing Digital as it has high p value
Cam_Model13<-lm(formula = gmv ~ premium_product + 
                  mass_product + product_mrp + 
                  `Content Marketing` + 
                  SEM, data = train_camera)
summary(Cam_Model13)
vif(Cam_Model13)

#removing SEM as it has high VIF and higher p value
Cam_Model14<-lm(formula = gmv ~ premium_product + 
                  mass_product + product_mrp + 
                  `Content Marketing`, data = train_camera)
summary(Cam_Model14)
vif(Cam_Model14)

#removing Content Marketing as it has high p value and high VIF
Cam_Model15<-lm(formula = gmv ~ premium_product + 
                  mass_product + product_mrp, data = train_camera)
summary(Cam_Model15)
#Multiple R-squared:  0.9726,	Adjusted R-squared:  0.9704
vif(Cam_Model15)

# predicting the results in test dataset
Predict_camgmv <- predict(Cam_Model15,test_camera[,-1])
test_camera$testgmv <- Predict_camgmv

# Checking the R Square of the prediction
rcam <- cor(test_camera$gmv,test_camera$testgmv)
rcam  # 0.9827204
rsquarecam <- rcam^2
rsquarecam  #0.9657394

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)      5351180      53933  99.219  < 2e-16 ***
#  premium_product   917950     152328   6.026 5.79e-07 ***
#  mass_product     1555081     123584  12.583 6.17e-15 ***
#  product_mrp       797981      68660  11.622 6.54e-14 ***

#######################################################################################################
# Building Linear Model for Gaming Accessory Dataset
#######################################################################################################


gameing_accessory_final_LM <- gameing_accessory_final

# Removing week column.This makes the dataset ready to build linear model.

gameing_accessory_final_LM$Week <-NULL

#Removing list_price and units as it directly co-related to gmv
gameing_accessory_final_LM$list_price <-NULL
gameing_accessory_final_LM$units <-NULL
# scalling

gameing_accessory_final_LM[,2:ncol(gameing_accessory_final_LM)] <- scale(gameing_accessory_final_LM[,2:ncol(gameing_accessory_final_LM)])

# Removing columns which cannot be scalled
gameing_accessory_final_LM$product_btwn_10000_15000 <- NULL
gameing_accessory_final_LM$product_btwn_15000_20000 <- NULL
gameing_accessory_final_LM$product_above_20000 <- NULL

# Divide into training and test data set
set.seed(100)

# randomly generate row indices for train dataset
trainindices_gaming= sample(1:nrow(gameing_accessory_final_LM), 0.8*nrow(gameing_accessory_final_LM))
# generate the train data set
train_gaming = gameing_accessory_final_LM[trainindices_gaming,]

#Similarly store the rest of the observations into an object "test".
test_gaming =gameing_accessory_final_LM[-trainindices_gaming,]




Game_Model1<-lm(gmv~.,data=train_gaming)

summary(Game_Model1)

# Perform stepAIC to remove insignificant variables

Game_ModelStepAIC<-stepAIC(Game_Model1,direction = "both")

Game_ModelStepAIC

Game_Model2<-lm(formula = gmv ~ is_COD + premium_product + mass_product + 
                  aspiring_product + product_mrp + product_procurement_sla + 
                  promotion_off + Digital + Sponsorship + `Content Marketing` + 
                  `Online marketing` + Other, data = train_gaming)
summary(Game_Model2)

vif(Game_Model2)

# removing product_mrp as it has high p value

Game_Model3<-lm(formula = gmv ~ is_COD + premium_product + mass_product + 
                  aspiring_product + product_procurement_sla + 
                  promotion_off + Digital + Sponsorship + `Content Marketing` + 
                  `Online marketing` + Other, data = train_gaming)

summary(Game_Model3)

vif(Game_Model3)


# removing Other as it has high p value

Game_Model4<-lm(formula = gmv ~ is_COD + premium_product + mass_product + 
                  aspiring_product + product_procurement_sla + 
                  promotion_off + Digital + Sponsorship + `Content Marketing` + 
                  `Online marketing`, data = train_gaming)
summary(Game_Model4)

vif(Game_Model4)

# removing product_procurement_sla as it has high p value
Game_Model5<-lm(formula = gmv ~ is_COD + premium_product + mass_product + 
                  aspiring_product + 
                  promotion_off + Digital + Sponsorship + `Content Marketing` + 
                  `Online marketing`, data = train_gaming)
summary(Game_Model5)

vif(Game_Model5)


# removing Content Marketing as it has high p value and higher vif
Game_Model6<-lm(formula = gmv ~ is_COD + premium_product + mass_product + 
                  aspiring_product + 
                  promotion_off + Digital + Sponsorship + 
                  `Online marketing`, data = train_gaming)
summary(Game_Model6)

vif(Game_Model6)


# removing Online marketing as it has high p value
Game_Model7<-lm(formula = gmv ~ is_COD + premium_product + mass_product + 
                  aspiring_product + 
                  promotion_off + Digital + Sponsorship, data = train_gaming)
summary(Game_Model7)

vif(Game_Model7)


# removing is_COD as it has high p value and higher vif
Game_Model8<-lm(formula = gmv ~ premium_product + mass_product + 
                  aspiring_product + 
                  promotion_off + Digital + Sponsorship, data = train_gaming)

summary(Game_Model8)

vif(Game_Model8)


# removing Sponsorship as it has high p value
Game_Model9<-lm(formula = gmv ~ premium_product + mass_product + 
                  aspiring_product + 
                  promotion_off + Digital, data = train_gaming)

summary(Game_Model9)

vif(Game_Model9)


# removing premium_product as it has high p value an high VIF
Game_Model10<-lm(formula = gmv ~ mass_product + 
                   aspiring_product + 
                   promotion_off + Digital, data = train_gaming)
summary(Game_Model10)
#Multiple R-squared:  0.9726,	Adjusted R-squared:  0.9697 
vif(Game_Model10)



#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)       3191720      36635  87.123  < 2e-16 ***
#  mass_product      1040861      66326  15.693  < 2e-16 ***
#  aspiring_product   437588      65095   6.722 6.66e-08 ***
#  promotion_off     -249251      44923  -5.548 2.57e-06 ***
#  Digital           -153057      35275  -4.339 0.000106 ***


# predicting the results in test dataset
Predict_gamegmv <- predict(Game_Model10,test_gaming[,-1])
test_gaming$testgmv <- Predict_gamegmv

# Checking the R Square of the prediction
rgame <- cor(test_gaming$gmv,test_gaming$testgmv)
rgame #0.9842979
rsquaregame <- rgame^2
rsquaregame #0.9688423

#############################################################################################################
# Building Linear Model for Home Audio Dataset
#############################################################################################################


home_audio_final_LM <- home_audio_final
# Removing week column.This makes the dataset ready to build linear model.

home_audio_final_LM$Week <-NULL

#Removing list_price and units as it directly co-related to gmv
home_audio_final_LM$list_price <-NULL
home_audio_final_LM$units <-NULL
# scalling
home_audio_final_LM[,2:ncol(home_audio_final_LM)] <- scale(home_audio_final_LM[,2:ncol(home_audio_final_LM)])

#Removing column which cannot be scalled
home_audio_final_LM$product_above_20000 <- NULL

# Divide into training and test data set
set.seed(100)

# randomly generate row indices for train dataset
trainindices_home= sample(1:nrow(home_audio_final_LM), 0.8*nrow(home_audio_final_LM))
# generate the train data set
train_home = home_audio_final_LM[trainindices_home,]

#Similarly store the rest of the observations into an object "test".
test_home =home_audio_final_LM[-trainindices_home,]


Home_Model1<-lm(gmv~.,data=train_home)

summary(Home_Model1)

# Perform stepAIC to remove insignificant variables

Home_ModelStepAIC<-stepAIC(Home_Model1,direction = "both")

Home_ModelStepAIC

Home_Model2<-lm(formula = gmv ~ premium_product + mass_product + product_mrp + 
                  product_procurement_sla + promotion_off + NPS + SEM + special_sale + 
                  product_btwn_5000_10000, data = train_home)
summary(Home_Model2)
vif(Home_Model2)


#removing product_btwn_5000_10000 as it has high p value


Home_Model3<-lm(formula = gmv ~ premium_product + mass_product + product_mrp + 
                  product_procurement_sla + promotion_off + 
                  NPS + SEM + special_sale, data = train_home)
summary(Home_Model3)
vif(Home_Model3)


#removing special_sale as it has high p value


Home_Model4<-lm(formula = gmv ~ premium_product + mass_product + product_mrp + 
                  product_procurement_sla + promotion_off + 
                  NPS + SEM, data = train_home)
summary(Home_Model4)
vif(Home_Model4)


#removing promotion_off as it has high vif


Home_Model5<-lm(formula = gmv ~ premium_product + mass_product + product_mrp + 
                  product_procurement_sla + 
                  NPS + SEM, data = train_home)
summary(Home_Model5)
vif(Home_Model5)

#removing NPS as it has high p value and high vif


Home_Model6<-lm(formula = gmv ~ premium_product + mass_product + product_mrp + 
                  product_procurement_sla +SEM, data = train_home)
summary(Home_Model6)
vif(Home_Model6)

#removing product_procurement_sla as it has high p value


Home_Model7<-lm(formula = gmv ~ premium_product + mass_product + product_mrp + 
                  SEM, data = train_home)
summary(Home_Model7)
vif(Home_Model7)

#removing SEM as it has high p value


Home_Model8<-lm(formula = gmv ~ premium_product + mass_product + 
                  product_mrp, data = train_home)
summary(Home_Model8)
#Multiple R-squared:  0.9899,	Adjusted R-squared:  0.9891
vif(Home_Model8)

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)      5043623      31686 159.174  < 2e-16 ***
#  premium_product   150576      47276   3.185  0.00299 ** 
#  mass_product     2337268      64394  36.296  < 2e-16 ***
#  product_mrp       223368      41284   5.411 4.27e-06 ***


# predicting the results in test dataset
Predict_homegmv <- predict(Home_Model8,test_home[,-1])
test_home$testgmv <- Predict_homegmv

# Checking the R Square of the prediction
rgame <- cor(test_home$gmv,test_home$testgmv)
rgame #0.9962693
rsquaregame <- rgame^2
rsquaregame #0.9925526

######################################################################################################################

# Build Multiplicative model

###########################################################################################

## Multiplicative model for Camera Accessories

camera_accessory_final_MM <- camera_accessory_final

# Removing week column.This makes the dataset ready to build linear model.

camera_accessory_final_MM$Week <-NULL

#Removing list_price and units as it directly co-related to gmv
camera_accessory_final_MM$list_price <-NULL
camera_accessory_final_MM$units <-NULL

## Replacing 0 with 0.00001 to get log of all KPIs
camera_accessory_final_MM[camera_accessory_final_MM == 0] <- 0.00001

# Log of all KPIs
camera_accessory_final_MM <- log(camera_accessory_final_MM)


# Divide into training and test data set
set.seed(100)

# randomly generate row indices for train dataset
trainindices_camera_mm= sample(1:nrow(camera_accessory_final_MM), 0.8*nrow(camera_accessory_final_MM))
# generate the train data set
train_camera_mm = camera_accessory_final_MM[trainindices_camera_mm,]

#Similarly store the rest of the observations into an object "test".
test_camera_mm = camera_accessory_final_MM[-trainindices_camera_mm,]


Cam_MM_Model1<-lm(gmv~.,data=train_camera_mm)

summary(Cam_MM_Model1)

# Perform stepAIC to remove insignificant variables


Cam_MM_ModelStepAIC <- stepAIC(Cam_MM_Model1 , direction = "both")
Cam_MM_ModelStepAIC


Cam_MM_Model2 <- lm(formula = gmv ~ premium_product + mass_product + aspiring_product + 
                      product_under_5000 + sla + product_mrp + promotion_off + 
                      Sponsorship + Other, data = train_camera_mm)
summary(Cam_MM_Model2)
vif(Cam_MM_Model2)


# removing Sponsorship due to high p value
Cam_MM_Model3 <- lm(formula = gmv ~ premium_product + mass_product + aspiring_product + 
                      product_under_5000 + sla + product_mrp + promotion_off 
                    + Other, data = train_camera_mm)
summary(Cam_MM_Model3)
vif(Cam_MM_Model3)

# removing sla due to high p value
Cam_MM_Model4 <- lm(formula = gmv ~ premium_product + mass_product + aspiring_product + 
                      product_under_5000 + product_mrp + promotion_off 
                    + Other, data = train_camera_mm)
summary(Cam_MM_Model4)
vif(Cam_MM_Model4)

#removing Other due to high p value
Cam_MM_Model5 <- lm(formula = gmv ~ premium_product + mass_product + aspiring_product + 
                      product_under_5000 + product_mrp +
                    promotion_off, data = train_camera_mm)
summary(Cam_MM_Model5)
vif(Cam_MM_Model5)

# removing aspiring_product due to high p value
Cam_MM_Model6 <- lm(formula = gmv ~ premium_product + mass_product + 
                      product_under_5000 + product_mrp +
                      promotion_off, data = train_camera_mm)
summary(Cam_MM_Model6)
vif(Cam_MM_Model6)

# removing mass_product due to high p value and high vif
Cam_MM_Model7 <- lm(formula = gmv ~ premium_product +
                      product_under_5000 + product_mrp +
                      promotion_off, data = train_camera_mm)
summary(Cam_MM_Model7)
vif(Cam_MM_Model7)

# removing premium_product due to high p vif
Cam_MM_Model8 <- lm(formula = gmv ~
                      product_under_5000 + product_mrp +
                      promotion_off, data = train_camera_mm)
summary(Cam_MM_Model8)
#Multiple R-squared:  0.998,	Adjusted R-squared:  0.9979 
vif(Cam_MM_Model8)

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)        -2.85043    0.73683  -3.868 0.000429 ***
#  product_under_5000  1.10074    0.01138  96.711  < 2e-16 ***
#  product_mrp         1.09945    0.09829  11.186 1.98e-13 ***
#  promotion_off      -1.14428    0.14494  -7.895 1.88e-09 ***

# predicting the results in test dataset
Predict_camgmv_mm <- predict(Cam_MM_Model8,test_camera_mm[,-1])
test_camera_mm$testgmv <- Predict_camgmv_mm

# Checking the R Square of the prediction
rcam_mm <- cor(test_camera_mm$gmv,test_camera_mm$testgmv)
rcam_mm  # 0.9996898
rsquarecam_mm <- rcam_mm^2
rsquarecam_mm  #0.9993798

#####################################################################################################################
## Multiplicative model for Gaming Accessories

######################################################################################################################


## Multiplicative model for Camera Accessories

gameing_accessory_final_MM <- gameing_accessory_final
# Removing week column.This makes the dataset ready to build linear model.

gameing_accessory_final_MM$Week <-NULL

#Removing list_price and units as it directly co-related to gmv
gameing_accessory_final_MM$list_price <-NULL
gameing_accessory_final_MM$units <-NULL

## Replacing 0 with 0.00001 to get log of all KPIs
gameing_accessory_final_MM[gameing_accessory_final_MM == 0] <- 0.00001

# Log of all KPIs
gameing_accessory_final_MM <- log(gameing_accessory_final_MM)


# Divide into training and test data set
set.seed(100)

# randomly generate row indices for train dataset
trainindices_gameing_mm= sample(1:nrow(gameing_accessory_final_MM), 0.8*nrow(gameing_accessory_final_MM))
# generate the train data set
train_gameing_mm = gameing_accessory_final_MM[trainindices_gameing_mm,]

#Similarly store the rest of the observations into an object "test".
test_gameing_mm = gameing_accessory_final_MM[-trainindices_gameing_mm,]


Game_MM_Model1<-lm(gmv~.,data=train_gameing_mm)

summary(Game_MM_Model1)

# Perform stepAIC to remove insignificant variables


Game_MM_ModelStepAIC <- stepAIC(Game_MM_Model1 , direction = "both")
Game_MM_ModelStepAIC

Game_MM_Model2 <- lm(formula = gmv ~ is_COD + mass_product + product_under_5000 + 
                       sla + product_mrp + promotion_off + Sponsorship + `Online marketing` + 
                       SEM + Radio, data = train_gameing_mm)
summary(Game_MM_Model2)
vif(Game_MM_Model2)

# removing SEM due to high p value
Game_MM_Model3 <- lm(formula = gmv ~ is_COD + mass_product + product_under_5000 + 
                       sla + product_mrp + promotion_off + Sponsorship + `Online marketing` + 
                       Radio, data = train_gameing_mm)
summary(Game_MM_Model3)
vif(Game_MM_Model3)

# removing Sponsorship due to hihg p value
Game_MM_Model4 <- lm(formula = gmv ~ is_COD + mass_product + product_under_5000 + 
                       sla + product_mrp + promotion_off +`Online marketing` + 
                       Radio, data = train_gameing_mm)
summary(Game_MM_Model4)
vif(Game_MM_Model4)

# removing mass_product due to high vif
Game_MM_Model5 <- lm(formula = gmv ~ is_COD + product_under_5000 + 
                       sla + product_mrp + promotion_off +`Online marketing` + 
                       Radio, data = train_gameing_mm)
summary(Game_MM_Model5)
vif(Game_MM_Model5)

# removing Online marketing due to high p value
Game_MM_Model6 <- lm(formula = gmv ~ is_COD + product_under_5000 + 
                       sla + product_mrp + promotion_off  + 
                       Radio, data = train_gameing_mm)
summary(Game_MM_Model6)
vif(Game_MM_Model6)

# removing Radio due to hihg p value
Game_MM_Model7 <- lm(formula = gmv ~ is_COD + product_under_5000 + 
                       sla + product_mrp 
                     + promotion_off, data = train_gameing_mm)
summary(Game_MM_Model7)
vif(Game_MM_Model7)

# removing product_mrp due to high p value and high VIF
Game_MM_Model8 <- lm(formula = gmv ~ is_COD + product_under_5000 + 
                       sla 
                     + promotion_off, data = train_gameing_mm)
summary(Game_MM_Model8)
vif(Game_MM_Model8)

# removing is_COD due to hihg p value
Game_MM_Model9 <- llm(formula = gmv ~ product_under_5000 + 
                        sla 
                      + promotion_off, data = train_gameing_mm)
summary(Game_MM_Model9)
#Multiple R-squared:  0.9946,	Adjusted R-squared:  0.9942 
vif(Game_MM_Model9)

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)          4.1551     0.3458  12.018 1.63e-14 ***
#  mass_product        -7.2693     1.0989  -6.615 8.20e-08 ***
#  product_under_5000   8.2430     1.0896   7.565 4.29e-09 ***
#  sla                  1.3723     0.2117   6.483 1.24e-07 ***

# predicting the results in test dataset
Predict_gamegmv_mm <- predict(Game_MM_Model9,test_gameing_mm[,-1])
test_gameing_mm$testgmv <- Predict_gamegmv_mm

# Checking the R Square of the prediction
rcam_game_mm <- cor(test_gameing_mm$gmv,test_gameing_mm$testgmv)
rcam_game_mm  #0.9978152
rsquarecam_game_mm <- rcam_game_mm^2
rsquarecam_game_mm  #0.9956353


##################################################################################################################

## Multiplicative model for Home Audio

home_audio_final_MM <- home_audio_final
# Removing week column.This makes the dataset ready to build linear model.

home_audio_final_MM$Week <-NULL

#Removing list_price and units as it directly co-related to gmv
home_audio_final_MM$list_price <-NULL
home_audio_final_MM$units <-NULL

## Replacing 0 with 0.00001 to get log of all KPIs
home_audio_final_MM[home_audio_final_MM == 0] <- 0.00001

# Log of all KPIs
home_audio_final_MM <- log(home_audio_final_MM)


# Divide into training and test data set
set.seed(100)

# randomly generate row indices for train dataset
trainindices_home_mm= sample(1:nrow(home_audio_final_MM), 0.8*nrow(home_audio_final_MM))
# generate the train data set
train_home_mm = home_audio_final_MM[trainindices_home_mm,]

#Similarly store the rest of the observations into an object "test".
test_home_mm = home_audio_final_MM[-trainindices_home_mm,]


Home_MM_Model1<-lm(gmv~.,data=train_home_mm)

summary(Home_MM_Model1)

# Perform stepAIC to remove insignificant variables


Home_MM_ModelStepAIC <- stepAIC(Home_MM_Model1 , direction = "both")
Home_MM_ModelStepAIC

Home_MM_Model2 <- lm(formula = gmv ~ is_COD + special_sale + premium_product + 
                       product_under_5000 + product_btwn_15000_20000 + sla + product_mrp + 
                       product_procurement_sla + promotion_off + NPS + TV + Digital + 
                       `Content Marketing` + Affiliates + SEM + Radio + Other + 
                       mass_product, data = train_home_mm)
summary(Home_MM_Model2)
vif(Home_MM_Model2)

#Removing mass_product due to high pvalue
Home_MM_Model3 <- lm(formula = gmv ~ is_COD + special_sale + premium_product + 
                       product_under_5000 + product_btwn_15000_20000 + sla + product_mrp + 
                       product_procurement_sla + promotion_off + NPS + TV + Digital + 
                       `Content Marketing` + Affiliates 
                     + SEM + Radio + Other,data = train_home_mm)
summary(Home_MM_Model3)
vif(Home_MM_Model3)

#Removing Digital due to high pvalue
Home_MM_Model4 <- lm(formula = gmv ~ is_COD + special_sale + premium_product + 
                       product_under_5000 + product_btwn_15000_20000 + sla + product_mrp + 
                       product_procurement_sla + promotion_off + NPS + TV  + 
                       `Content Marketing` + Affiliates 
                     + SEM + Radio + Other,data = train_home_mm)
summary(Home_MM_Model4)
vif(Home_MM_Model4)

#Removing Radio due to high pvalue and high vif
Home_MM_Model5 <- lm(formula = gmv ~ is_COD + special_sale + premium_product + 
                       product_under_5000 + product_btwn_15000_20000 + sla + product_mrp + 
                       product_procurement_sla + promotion_off + NPS + TV  + 
                       `Content Marketing` + Affiliates 
                     + SEM + Other,data = train_home_mm)
summary(Home_MM_Model5)
vif(Home_MM_Model5)

#Removing Other due to high pvalue
Home_MM_Model6 <- lm(formula = gmv ~ is_COD + special_sale + premium_product + 
                       product_under_5000 + product_btwn_15000_20000 + sla + product_mrp + 
                       product_procurement_sla + promotion_off + NPS + TV  + 
                       `Content Marketing` + Affiliates 
                     + SEM,data = train_home_mm)
summary(Home_MM_Model6)
vif(Home_MM_Model6)

#Removing TV due to high pvalue
Home_MM_Model7 <- lm(formula = gmv ~ is_COD + special_sale + premium_product + 
                       product_under_5000 + product_btwn_15000_20000 + sla + product_mrp + 
                       product_procurement_sla + promotion_off + NPS + 
                       `Content Marketing` + Affiliates 
                     + SEM,data = train_home_mm)
summary(Home_MM_Model7)
vif(Home_MM_Model7)

#Removing product_btwn_15000_20000 due to high pvalue
Home_MM_Model8 <- lm(formula = gmv ~ is_COD + special_sale + premium_product + 
                       product_under_5000 + sla + product_mrp + 
                       product_procurement_sla + promotion_off + NPS + 
                       `Content Marketing` + Affiliates 
                     + SEM,data = train_home_mm)
summary(Home_MM_Model8)
vif(Home_MM_Model8)

#Removing Affiliates due to high pvalue
Home_MM_Model9 <- lm(formula = gmv ~ is_COD + special_sale + premium_product + 
                       product_under_5000 + sla + product_mrp + 
                       product_procurement_sla + promotion_off + NPS + 
                       `Content Marketing`
                     + SEM,data = train_home_mm)
summary(Home_MM_Model9)
vif(Home_MM_Model9)

#Removing NPS due to high pvalue
Home_MM_Model10 <- lm(formula = gmv ~ is_COD + special_sale + premium_product + 
                        product_under_5000 + sla + product_mrp + 
                        product_procurement_sla + promotion_off +
                        `Content Marketing`
                      + SEM,data = train_home_mm)
summary(Home_MM_Model10)
vif(Home_MM_Model10)

#Removing is_COD due to high pvalue
Home_MM_Model11 <- lm(formula = gmv ~ special_sale + premium_product + 
                        product_under_5000 + sla + product_mrp + 
                        product_procurement_sla + promotion_off +
                        `Content Marketing`
                      + SEM,data = train_home_mm)
summary(Home_MM_Model11)
vif(Home_MM_Model11)

#Removing special_sale due to high pvalue
Home_MM_Model12 <- lm(formula = gmv ~ premium_product + 
                        product_under_5000 + sla + product_mrp + 
                        product_procurement_sla + promotion_off +
                        `Content Marketing`
                      + SEM,data = train_home_mm)
summary(Home_MM_Model12)
vif(Home_MM_Model12)

#Removing SEM due to high pvalue
Home_MM_Model13 <- lm(formula = gmv ~ premium_product + 
                        product_under_5000 + sla + product_mrp + 
                        product_procurement_sla + promotion_off +
                        `Content Marketing`,
                        data = train_home_mm)
summary(Home_MM_Model13)
vif(Home_MM_Model13)

#Removing Content Marketing due to high pvalue
Home_MM_Model14 <- lm(formula = gmv ~ premium_product + 
                        product_under_5000 + sla + product_mrp + 
                        product_procurement_sla + promotion_off,
                      data = train_home_mm)
summary(Home_MM_Model14)
vif(Home_MM_Model14)

#Removing sla due to high pvalue
Home_MM_Model15 <- lm(formula = gmv ~ premium_product + 
                        product_under_5000 + product_mrp + 
                        product_procurement_sla + promotion_off,
                      data = train_home_mm)
summary(Home_MM_Model15)
vif(Home_MM_Model15)

#Removing product_procurement_sla due to high pvalue
Home_MM_Model16 <- lm(formula = gmv ~ premium_product + 
                        product_under_5000 + product_mrp + 
                        promotion_off,
                      data = train_home_mm)
summary(Home_MM_Model16)
vif(Home_MM_Model16)

#Removing product_procurement_sla due to high pvalue
Home_MM_Model17 <- lm(formula = gmv ~ product_under_5000 + product_mrp + 
                        promotion_off,
                      data = train_home_mm)
summary(Home_MM_Model17)
#Multiple R-squared:  0.9937,	Adjusted R-squared:  0.9932 
vif(Home_MM_Model17)

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)         2.06471    0.94140   2.193 0.034833 *  
#  product_under_5000  0.97824    0.01436  68.143  < 2e-16 ***
#  product_mrp         0.65115    0.10044   6.483 1.58e-07 ***
#  promotion_off      -0.47997    0.11752  -4.084 0.000236 ***

# predicting the results in test dataset
Predict_homegmv_mm <- predict(Home_MM_Model17,test_home_mm[,-1])
test_home_mm$testgmv <- Predict_homegmv_mm

# Checking the R Square of the prediction
rcam_home_mm <- cor(test_home_mm$gmv,test_home_mm$testgmv)
rcam_home_mm  #0.999528
rsquarecam_home_mm <- rcam_home_mm^2
rsquarecam_home_mm  #0.9990563

######################################################################################################################

# Function to derive new KPIs for Kyock and Distribution Lag Model

lag_kpis <- function(df){
  lag_effect=0.50
  df$TV.adstock <- as.numeric(stats::filter(x=df$TV, filter=lag_effect, method="recursive"))
  df$Digital.adstock <- as.numeric(stats::filter(x=df$Digital, filter=lag_effect, method="recursive"))
  df$Sponsorship.adstock <- as.numeric(stats::filter(x=df$Sponsorship, filter=lag_effect, method="recursive"))
  df$ContentMarketing.adstock <- as.numeric(stats::filter(x=df$`Content Marketing`, filter=lag_effect, method="recursive"))
  df$OnlineMarketing.adstock <- as.numeric(stats::filter(x=df$`Online marketing`, filter=lag_effect, method="recursive"))
  df$Affiliates.adstock <- as.numeric(stats::filter(x=df$Affiliates, filter=lag_effect, method="recursive"))
  df$SEM.adstock <- as.numeric(stats::filter(x=df$SEM, filter=lag_effect, method="recursive"))
  df$Radio.adstock <- as.numeric(stats::filter(x=df$Radio, filter=lag_effect, method="recursive"))
  df$Other.adstock <- as.numeric(stats::filter(x=df$Other, filter=lag_effect, method="recursive"))

  df <- slide(df,Var="gmv",slideBy = -1) 
  df <- slide(df,Var="promotion_off",slideBy = -1)
  df <- slide(df,Var="list_price",slideBy = -1)
  df$`gmv-1` <- na.ma(df$`gmv-1`, k=1, weighting = "simple")
  df$`promotion_off-1` <- na.ma(df$`promotion_off-1`, k=1, weighting = "simple")
  df$`list_price-1` <- na.ma(df$`list_price-1`, k=1, weighting = "simple")
  df$list_price_ma2 <- rollmean(df$list_price, k = 2, fill = NA, align = "right")
  df$list_price_ma3 <- rollmean(df$list_price, k = 3, fill = NA, align = "right")
  df$list_price_ma4 <- rollmean(df$list_price, k = 4, fill = NA, align = "right")
  df$list_price_ma2 <- na.ma(df$list_price_ma2, k=2, weighting = "simple")
  df$list_price_ma3 <- na.ma(df$list_price_ma3, k=3, weighting = "simple")
  df$list_price_ma4 <- na.ma(df$list_price_ma4, k=4, weighting = "simple")
  df$list_price_wrt_lag1 <- df$list_price / df$`list_price-1`
  df$list_price_wrt_ma_2 <- df$list_price / df$list_price_ma2
  df$list_price_wrt_ma_3 <- df$list_price / df$list_price_ma3
  df$list_price_wrt_ma_4 <- df$list_price / df$list_price_ma4
  df$promotion_off_perc_change_prev_week <- (df$promotion_off - df$`promotion_off-1`)/df$`promotion_off-1`
  df[,c("list_price","list_price-1","list_price_ma2","list_price_ma3","list_price_ma4","Week","units")] <- NULL
  df
}

camera_accessory_final_new <- lag_kpis(camera_accessory_final)
gameing_accessory_final_new <- lag_kpis(gameing_accessory_final)
home_audio_final_new <- lag_kpis(home_audio_final)

#############################################################################################################
# Building Koyck Model for Camera Accessory Dataset
#############################################################################################################


camera_accessory_final_koyck <- camera_accessory_final_new[,-which(colnames(camera_accessory_final_new) %in% c("gmv-1"))]
# scalling
camera_accessory_final_koyck[,2:ncol(camera_accessory_final_koyck)] <- scale(camera_accessory_final_koyck[,2:ncol(camera_accessory_final_koyck)])

#Removing column which cannot be scalled
camera_accessory_final_koyck$product_btwn_10000_15000 <- NULL
camera_accessory_final_koyck$product_btwn_15000_20000 <- NULL
# Divide into training and test data set
set.seed(100)

# randomly generate row indices for train dataset
trainindices_camera_kyc= sample(1:nrow(camera_accessory_final_koyck), 0.8*nrow(camera_accessory_final_koyck))
# generate the train data set
train_camera_kyc = camera_accessory_final_koyck[trainindices_camera_kyc,]

#Similarly store the rest of the observations into an object "test".
test_camera_kyc =camera_accessory_final_koyck[-trainindices_camera_kyc,]


Camera_Kyc_Model1<-lm(gmv~.,data=train_camera_kyc)

summary(Camera_Kyc_Model1)

# Perform stepAIC to remove insignificant variables

Camera_kyc_ModelStepAIC<-stepAIC(Camera_Kyc_Model1,direction = "both")

Camera_kyc_ModelStepAIC

Camera_Kyc_Model2<-lm(formula = gmv ~ premium_product + mass_product + product_mrp + 
                        product_procurement_sla + promotion_off + NPS + TV + Digital + 
                        `Content Marketing` + `Online marketing` + Affiliates + SEM + 
                        Radio + Other + OnlineMarketing.adstock + Affiliates.adstock + 
                        SEM.adstock + Radio.adstock + Other.adstock + `promotion_off-1` + 
                        list_price_wrt_ma_2 + list_price_wrt_ma_3 + promotion_off_perc_change_prev_week, 
                      data = train_camera_kyc)

summary(Camera_Kyc_Model2)
vif(Camera_Kyc_Model2)

#Removing NPS due to high p value
Camera_Kyc_Model3<-lm(formula = gmv ~ premium_product + mass_product + product_mrp + 
                        product_procurement_sla + promotion_off + TV + Digital + 
                        `Content Marketing` + `Online marketing` + Affiliates + SEM + 
                        Radio + Other + OnlineMarketing.adstock + Affiliates.adstock + 
                        SEM.adstock + Radio.adstock + Other.adstock + `promotion_off-1` + 
                        list_price_wrt_ma_2 + list_price_wrt_ma_3 + promotion_off_perc_change_prev_week, 
                      data = train_camera_kyc)

summary(Camera_Kyc_Model3)
vif(Camera_Kyc_Model3)

#Removing Digital due to high p value
Camera_Kyc_Model4<-lm(formula = gmv ~ premium_product + mass_product + product_mrp + 
                        product_procurement_sla + promotion_off + TV + 
                        `Content Marketing` + `Online marketing` + Affiliates + SEM + 
                        Radio + Other + OnlineMarketing.adstock + Affiliates.adstock + 
                        SEM.adstock + Radio.adstock + Other.adstock + `promotion_off-1` + 
                        list_price_wrt_ma_2 + list_price_wrt_ma_3 + promotion_off_perc_change_prev_week, 
                      data = train_camera_kyc)

summary(Camera_Kyc_Model4)
vif(Camera_Kyc_Model4)

#Removing TV due to high p value
Camera_Kyc_Model5<-lm(formula = gmv ~ premium_product + mass_product + product_mrp + 
                        product_procurement_sla + promotion_off + 
                        `Content Marketing` + `Online marketing` + Affiliates + SEM + 
                        Radio + Other + OnlineMarketing.adstock + Affiliates.adstock + 
                        SEM.adstock + Radio.adstock + Other.adstock + `promotion_off-1` + 
                        list_price_wrt_ma_2 + list_price_wrt_ma_3 + promotion_off_perc_change_prev_week, 
                      data = train_camera_kyc)

summary(Camera_Kyc_Model5)
vif(Camera_Kyc_Model5)

#Removing OnlineMarketing.adstock due to high p value
Camera_Kyc_Model6<-lm(formula = gmv ~ premium_product + mass_product + product_mrp + 
                        product_procurement_sla + promotion_off + 
                        `Content Marketing` + `Online marketing` + Affiliates + SEM + 
                        Radio + Other + Affiliates.adstock + 
                        SEM.adstock + Radio.adstock + Other.adstock + `promotion_off-1` + 
                        list_price_wrt_ma_2 + list_price_wrt_ma_3 + promotion_off_perc_change_prev_week, 
                      data = train_camera_kyc)

summary(Camera_Kyc_Model6)
vif(Camera_Kyc_Model6)

#Removing Radio due to high p value
Camera_Kyc_Model7<-lm(formula = gmv ~ premium_product + mass_product + product_mrp + 
                        product_procurement_sla + promotion_off + 
                        `Content Marketing` + `Online marketing` + Affiliates + SEM + 
                        Other + Affiliates.adstock + 
                        SEM.adstock + Radio.adstock + Other.adstock + `promotion_off-1` + 
                        list_price_wrt_ma_2 + list_price_wrt_ma_3 + promotion_off_perc_change_prev_week, 
                      data = train_camera_kyc)

summary(Camera_Kyc_Model7)
vif(Camera_Kyc_Model7)

#Removing list_price_wrt_ma_3 due to high p value
Camera_Kyc_Model8<-lm(formula = gmv ~ premium_product + mass_product + product_mrp + 
                        product_procurement_sla + promotion_off + 
                        `Content Marketing` + `Online marketing` + Affiliates + SEM + 
                        Other + Affiliates.adstock + 
                        SEM.adstock + Radio.adstock + Other.adstock + `promotion_off-1` + 
                        list_price_wrt_ma_2 + promotion_off_perc_change_prev_week, 
                      data = train_camera_kyc)

summary(Camera_Kyc_Model8)
vif(Camera_Kyc_Model8)

#Removing Online marketing due to high p value
Camera_Kyc_Model9<-lm(formula = gmv ~ premium_product + mass_product + product_mrp + 
                        product_procurement_sla + promotion_off + 
                        `Content Marketing` + Affiliates + SEM + 
                        Other + Affiliates.adstock + 
                        SEM.adstock + Radio.adstock + Other.adstock + `promotion_off-1` + 
                        list_price_wrt_ma_2 + promotion_off_perc_change_prev_week, 
                      data = train_camera_kyc)

summary(Camera_Kyc_Model9)
vif(Camera_Kyc_Model9)

#Removing product_procurement_sla due to high p value
Camera_Kyc_Model10<-lm(formula = gmv ~ premium_product + mass_product + product_mrp + 
                         promotion_off + 
                         `Content Marketing` + Affiliates + SEM + 
                         Other + Affiliates.adstock + 
                         SEM.adstock + Radio.adstock + Other.adstock + `promotion_off-1` + 
                         list_price_wrt_ma_2 + promotion_off_perc_change_prev_week, 
                       data = train_camera_kyc)

summary(Camera_Kyc_Model10)
vif(Camera_Kyc_Model10)

#Removing list_price_wrt_ma_2 due to high p value

Camera_Kyc_Model11<-lm(formula = gmv ~ premium_product + mass_product + product_mrp + 
                         promotion_off + 
                         `Content Marketing` + Affiliates + SEM + 
                         Other + Affiliates.adstock + 
                         SEM.adstock + Radio.adstock + Other.adstock + `promotion_off-1` + 
                         promotion_off_perc_change_prev_week, 
                       data = train_camera_kyc)

summary(Camera_Kyc_Model11)
vif(Camera_Kyc_Model11)

#Removing Affiliates due to high p value 
Camera_Kyc_Model12<-lm(formula = gmv ~ premium_product + mass_product + product_mrp + 
                         promotion_off + 
                         `Content Marketing` + SEM + 
                         Other + Affiliates.adstock + 
                         SEM.adstock + Radio.adstock + Other.adstock + `promotion_off-1` + 
                         promotion_off_perc_change_prev_week, 
                       data = train_camera_kyc)

summary(Camera_Kyc_Model12)
vif(Camera_Kyc_Model12)

#removing promotion_off_perc_change_prev_week due to high p value and high vif
Camera_Kyc_Model13<-lm(formula = gmv ~ premium_product + mass_product + product_mrp + 
                         promotion_off + 
                         `Content Marketing` + SEM + 
                         Other + Affiliates.adstock + 
                         SEM.adstock + Radio.adstock 
                       + Other.adstock + `promotion_off-1`, 
                       data = train_camera_kyc)

summary(Camera_Kyc_Model13)
vif(Camera_Kyc_Model13)

#removing promotion_off due to high p value
Camera_Kyc_Model14<-lm(formula = gmv ~ premium_product + mass_product + product_mrp +
                         `Content Marketing` + SEM + 
                         Other + Affiliates.adstock + 
                         SEM.adstock + Radio.adstock 
                       + Other.adstock + `promotion_off-1`, 
                       data = train_camera_kyc)

summary(Camera_Kyc_Model14)
vif(Camera_Kyc_Model14)

#removing SEM.adstock due to high p value
Camera_Kyc_Model15<-lm(formula = gmv ~ premium_product + mass_product + product_mrp +
                         `Content Marketing` + SEM + 
                         Other + Affiliates.adstock + 
                         Radio.adstock 
                       + Other.adstock + `promotion_off-1`, 
                       data = train_camera_kyc)

summary(Camera_Kyc_Model15)
vif(Camera_Kyc_Model15)

#Removing Affiliates.adstock due to high p value
Camera_Kyc_Model16<-lm(formula = gmv ~ premium_product + mass_product + product_mrp +
                         `Content Marketing` + SEM + 
                         Other + 
                         Radio.adstock 
                       + Other.adstock + `promotion_off-1`, 
                       data = train_camera_kyc)

summary(Camera_Kyc_Model16)
vif(Camera_Kyc_Model16)

#Removing promotion_off-1 due to high p value
Camera_Kyc_Model17<-lm(formula = gmv ~ premium_product + mass_product + product_mrp +
                         `Content Marketing` + SEM + 
                         Other + 
                         Radio.adstock 
                       + Other.adstock, 
                       data = train_camera_kyc)

summary(Camera_Kyc_Model17)
vif(Camera_Kyc_Model17)

#Removing Other due to high p value
Camera_Kyc_Model18<-lm(formula = gmv ~ premium_product + mass_product + product_mrp +
                         `Content Marketing` + SEM +
                         Radio.adstock 
                       + Other.adstock, 
                       data = train_camera_kyc)

summary(Camera_Kyc_Model18)
#Multiple R-squared:  0.986,	Adjusted R-squared:  0.983
vif(Camera_Kyc_Model18)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)          5381116      41353 130.127  < 2e-16 ***
#  premium_product      1288033     144533   8.912 2.67e-10 ***
#  mass_product         1214966     116729  10.408 5.89e-12 ***
#  product_mrp           762860      59921  12.731 2.78e-14 ***
#  `Content Marketing`  1160655     275549   4.212 0.000184 ***
#  SEM                 -1175188     284763  -4.127 0.000234 ***
#  Radio.adstock        -676264     175089  -3.862 0.000496 ***
#  Other.adstock         571303     147455   3.874 0.000480 ***

# predicting the results in test dataset
Predict_camgmv_kyc <- predict(Camera_Kyc_Model18,test_camera_kyc[,-1])
test_camera_kyc$testgmv <- Predict_camgmv_kyc

# Checking the R Square of the prediction
rcam_kyc <- cor(test_camera_kyc$gmv,test_camera_kyc$testgmv)
rcam_kyc  #0.984483
rsquarecam_kyc <- rcam_kyc^2
rsquarecam_kyc  #0.9692067

### Cross-validation
cv.lm(data = train_camera_kyc, form.lm = Camera_Kyc_Model18, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)

# Calculate the Elasticity
cam_kyc_elasticity <- NULL
cam_kyc_elasticity <- data.frame()
c = 1
for (i in names(Camera_Kyc_Model18$coefficients)[-1]){

  i_new <- str_replace_all(i , "`" , "")
  print(i_new)
  elasticity <- as.numeric(Camera_Kyc_Model18$coefficients[-1][i]*mean(train_camera_kyc[,i_new])/mean(train_camera_kyc$gmv))
  print(elasticity)
  cam_kyc_elasticity[c,1] <- i_new
  cam_kyc_elasticity[c,2] <- elasticity
  c <- c + 1
}

colnames(cam_kyc_elasticity)[1] <- "variable"
colnames(cam_kyc_elasticity)[2] <- "elasticity"

###########################################################################################
# Building Koyck Model for Gaming Accessory Dataset
#############################################################################################################

gameing_accessory_final_koyck <- gameing_accessory_final_new[,-which(colnames(gameing_accessory_final_new) %in% c("gmv-1"))]
# scalling
gameing_accessory_final_koyck[,2:ncol(gameing_accessory_final_koyck)] <- scale(gameing_accessory_final_koyck[,2:ncol(gameing_accessory_final_koyck)])

#Removing column which cannot be scalled
gameing_accessory_final_koyck$product_btwn_10000_15000 <- NULL
gameing_accessory_final_koyck$product_btwn_15000_20000 <- NULL
gameing_accessory_final_koyck$product_above_20000 <- NULL
# Divide into training and test data set
set.seed(100)

# randomly generate row indices for train dataset
trainindices_gameing_kyc= sample(1:nrow(gameing_accessory_final_koyck), 0.8*nrow(gameing_accessory_final_koyck))
# generate the train data set
train_gameing_kyc = gameing_accessory_final_koyck[trainindices_gameing_kyc,]

#Similarly store the rest of the observations into an object "test".
test_gameing_kyc <- gameing_accessory_final_koyck[-trainindices_gameing_kyc,]

gameing_Kyc_Model1<-lm(gmv~.,data=train_gameing_kyc)

summary(gameing_Kyc_Model1)

# Perform stepAIC to remove insignificant variables

gameing_kyc_ModelStepAIC<-stepAIC(gameing_Kyc_Model1,direction = "both")

gameing_kyc_ModelStepAIC


gameing_Kyc_Model2<-lm(formula = gmv ~ is_COD + special_sale + mass_product + aspiring_product + 
                         sla + product_mrp + promotion_off + NPS + Digital + Sponsorship + 
                         `Content Marketing` + `Online marketing` + Affiliates + SEM + 
                         Radio + Other + TV.adstock + Digital.adstock + Sponsorship.adstock + 
                         ContentMarketing.adstock + OnlineMarketing.adstock + Affiliates.adstock + 
                         SEM.adstock + Radio.adstock + Other.adstock + `promotion_off-1` + 
                         list_price_wrt_lag1 + list_price_wrt_ma_2 + list_price_wrt_ma_4, 
                       data = train_gameing_kyc)

summary(gameing_Kyc_Model2)
vif(gameing_Kyc_Model2)

#Removing NPS due to high p value
gameing_Kyc_Model3<-lm(formula = gmv ~ is_COD + special_sale + mass_product + aspiring_product + 
                         sla + product_mrp + promotion_off + Digital + Sponsorship + 
                         `Content Marketing` + `Online marketing` + Affiliates + SEM + 
                         Radio + Other + TV.adstock + Digital.adstock + Sponsorship.adstock + 
                         ContentMarketing.adstock + OnlineMarketing.adstock + Affiliates.adstock + 
                         SEM.adstock + Radio.adstock + Other.adstock + `promotion_off-1` + 
                         list_price_wrt_lag1 + list_price_wrt_ma_2 + list_price_wrt_ma_4, 
                       data = train_gameing_kyc)

summary(gameing_Kyc_Model3)
vif(gameing_Kyc_Model3)

#Removing promotion_off due to high p value
gameing_Kyc_Model4 <-lm(formula = gmv ~ is_COD + special_sale + mass_product + aspiring_product + 
                         sla + product_mrp + Digital + Sponsorship + 
                         `Content Marketing` + `Online marketing` + Affiliates + SEM + 
                         Radio + Other + TV.adstock + Digital.adstock + Sponsorship.adstock + 
                         ContentMarketing.adstock + OnlineMarketing.adstock + Affiliates.adstock + 
                         SEM.adstock + Radio.adstock + Other.adstock + `promotion_off-1` + 
                         list_price_wrt_lag1 + list_price_wrt_ma_2 + list_price_wrt_ma_4, 
                       data = train_gameing_kyc)

summary(gameing_Kyc_Model4)
vif(gameing_Kyc_Model4)

#Removing TV.adstock due to high p value
gameing_Kyc_Model5 <-lm(formula = gmv ~ is_COD + special_sale + mass_product + aspiring_product + 
                          sla + product_mrp + Digital + Sponsorship + 
                          `Content Marketing` + `Online marketing` + Affiliates + SEM + 
                          Radio + Other + Digital.adstock + Sponsorship.adstock + 
                          ContentMarketing.adstock + OnlineMarketing.adstock + Affiliates.adstock + 
                          SEM.adstock + Radio.adstock + Other.adstock + `promotion_off-1` + 
                          list_price_wrt_lag1 + list_price_wrt_ma_2 + list_price_wrt_ma_4, 
                        data = train_gameing_kyc)

summary(gameing_Kyc_Model5)
vif(gameing_Kyc_Model5)

#Removing SEM.adstock due to high p value
gameing_Kyc_Model6 <-lm(formula = gmv ~ is_COD + special_sale + mass_product + aspiring_product + 
                          sla + product_mrp + Digital + Sponsorship + 
                          `Content Marketing` + `Online marketing` + Affiliates + SEM + 
                          Radio + Other + Digital.adstock + Sponsorship.adstock + 
                          ContentMarketing.adstock + OnlineMarketing.adstock + Affiliates.adstock + 
                          Radio.adstock + Other.adstock + `promotion_off-1` + 
                          list_price_wrt_lag1 + list_price_wrt_ma_2 + list_price_wrt_ma_4, 
                        data = train_gameing_kyc)

summary(gameing_Kyc_Model6)
vif(gameing_Kyc_Model6)

#Removing SEM due to high p value
gameing_Kyc_Model7 <-lm(formula = gmv ~ is_COD + special_sale + mass_product + aspiring_product + 
                          sla + product_mrp + Digital + Sponsorship + 
                          `Content Marketing` + `Online marketing` + Affiliates + 
                          Radio + Other + Digital.adstock + Sponsorship.adstock + 
                          ContentMarketing.adstock + OnlineMarketing.adstock + Affiliates.adstock + 
                          Radio.adstock + Other.adstock + `promotion_off-1` + 
                          list_price_wrt_lag1 + list_price_wrt_ma_2 + list_price_wrt_ma_4, 
                        data = train_gameing_kyc)

summary(gameing_Kyc_Model7)
vif(gameing_Kyc_Model7)

#Removing list_price_wrt_ma_4 due to high p value
gameing_Kyc_Model8 <-lm(formula = gmv ~ is_COD + special_sale + mass_product + aspiring_product + 
                          sla + product_mrp + Digital + Sponsorship + 
                          `Content Marketing` + `Online marketing` + Affiliates + 
                          Radio + Other + Digital.adstock + Sponsorship.adstock + 
                          ContentMarketing.adstock + OnlineMarketing.adstock + Affiliates.adstock + 
                          Radio.adstock + Other.adstock + `promotion_off-1` + 
                          list_price_wrt_lag1 + list_price_wrt_ma_2, 
                        data = train_gameing_kyc)

summary(gameing_Kyc_Model8)
vif(gameing_Kyc_Model8)

#Removing Radio.adstock due to high p value
gameing_Kyc_Model9 <-lm(formula = gmv ~ is_COD + special_sale + mass_product + aspiring_product + 
                          sla + product_mrp + Digital + Sponsorship + 
                          `Content Marketing` + `Online marketing` + Affiliates + 
                          Radio + Other + Digital.adstock + Sponsorship.adstock + 
                          ContentMarketing.adstock + OnlineMarketing.adstock + Affiliates.adstock + 
                          Other.adstock + `promotion_off-1` + 
                          list_price_wrt_lag1 + list_price_wrt_ma_2, 
                        data = train_gameing_kyc)

summary(gameing_Kyc_Model9)
vif(gameing_Kyc_Model9)

#Removing Radio due to high p value
gameing_Kyc_Model10 <-lm(formula = gmv ~ is_COD + special_sale + mass_product + aspiring_product + 
                          sla + product_mrp + Digital + Sponsorship + 
                          `Content Marketing` + `Online marketing` + Affiliates + 
                          Other + Digital.adstock + Sponsorship.adstock + 
                          ContentMarketing.adstock + OnlineMarketing.adstock + Affiliates.adstock + 
                          Other.adstock + `promotion_off-1` + 
                          list_price_wrt_lag1 + list_price_wrt_ma_2, 
                        data = train_gameing_kyc)

summary(gameing_Kyc_Model10)
vif(gameing_Kyc_Model10)

#Removing Other.adstock due to high p value
gameing_Kyc_Model11 <-lm(formula = gmv ~ is_COD + special_sale + mass_product + aspiring_product + 
                           sla + product_mrp + Digital + Sponsorship + 
                           `Content Marketing` + `Online marketing` + Affiliates + 
                           Other + Digital.adstock + Sponsorship.adstock + 
                           ContentMarketing.adstock + OnlineMarketing.adstock + Affiliates.adstock + 
                           `promotion_off-1` + 
                           list_price_wrt_lag1 + list_price_wrt_ma_2, 
                         data = train_gameing_kyc)

summary(gameing_Kyc_Model11)
vif(gameing_Kyc_Model11)

#Removing sla due to high p value
gameing_Kyc_Model12 <-lm(formula = gmv ~ is_COD + special_sale + mass_product + aspiring_product + 
                           product_mrp + Digital + Sponsorship + 
                           `Content Marketing` + `Online marketing` + Affiliates + 
                           Other + Digital.adstock + Sponsorship.adstock + 
                           ContentMarketing.adstock + OnlineMarketing.adstock + Affiliates.adstock + 
                           `promotion_off-1` + 
                           list_price_wrt_lag1 + list_price_wrt_ma_2, 
                         data = train_gameing_kyc)

summary(gameing_Kyc_Model12)
vif(gameing_Kyc_Model12)

#Removing Other due to high p value
gameing_Kyc_Model13 <-lm(formula = gmv ~ is_COD + special_sale + mass_product + aspiring_product + 
                           product_mrp + Digital + Sponsorship + 
                           `Content Marketing` + `Online marketing` + Affiliates + 
                           Digital.adstock + Sponsorship.adstock + 
                           ContentMarketing.adstock + OnlineMarketing.adstock + Affiliates.adstock + 
                           `promotion_off-1` + 
                           list_price_wrt_lag1 + list_price_wrt_ma_2, 
                         data = train_gameing_kyc)

summary(gameing_Kyc_Model13)
vif(gameing_Kyc_Model13)

#Removing special_sale due to high p value
gameing_Kyc_Model14 <-lm(formula = gmv ~ is_COD + mass_product + aspiring_product + 
                           product_mrp + Digital + Sponsorship + 
                           `Content Marketing` + `Online marketing` + Affiliates + 
                           Digital.adstock + Sponsorship.adstock + 
                           ContentMarketing.adstock + OnlineMarketing.adstock + Affiliates.adstock + 
                           `promotion_off-1` + 
                           list_price_wrt_lag1 + list_price_wrt_ma_2, 
                         data = train_gameing_kyc)

summary(gameing_Kyc_Model14)
vif(gameing_Kyc_Model14)

#Removing Online marketing due to high p value
gameing_Kyc_Model15 <-lm(formula = gmv ~ is_COD + mass_product + aspiring_product + 
                           product_mrp + Digital + Sponsorship + 
                           `Content Marketing` + Affiliates + 
                           Digital.adstock + Sponsorship.adstock + 
                           ContentMarketing.adstock + OnlineMarketing.adstock + Affiliates.adstock + 
                           `promotion_off-1` + 
                           list_price_wrt_lag1 + list_price_wrt_ma_2, 
                         data = train_gameing_kyc)

summary(gameing_Kyc_Model15)
vif(gameing_Kyc_Model15)

#Removing Affiliates due to high p value
gameing_Kyc_Model16 <-lm(formula = gmv ~ is_COD + mass_product + aspiring_product + 
                           product_mrp + Digital + Sponsorship + 
                           `Content Marketing` + 
                           Digital.adstock + Sponsorship.adstock + 
                           ContentMarketing.adstock + OnlineMarketing.adstock + Affiliates.adstock + 
                           `promotion_off-1` + 
                           list_price_wrt_lag1 + list_price_wrt_ma_2, 
                         data = train_gameing_kyc)

summary(gameing_Kyc_Model16)
vif(gameing_Kyc_Model16)

#Removing Content Marketing due to high p value
gameing_Kyc_Model17 <-lm(formula = gmv ~ is_COD + mass_product + aspiring_product + 
                           product_mrp + Digital + Sponsorship +
                           Digital.adstock + Sponsorship.adstock + 
                           ContentMarketing.adstock + OnlineMarketing.adstock + Affiliates.adstock + 
                           `promotion_off-1` + 
                           list_price_wrt_lag1 + list_price_wrt_ma_2, 
                         data = train_gameing_kyc)

summary(gameing_Kyc_Model17)
vif(gameing_Kyc_Model17)

#Removing Digital due to high p value
gameing_Kyc_Model18 <-lm(formula = gmv ~ is_COD + mass_product + aspiring_product + 
                           product_mrp + Sponsorship +
                           Digital.adstock + Sponsorship.adstock + 
                           ContentMarketing.adstock + OnlineMarketing.adstock + Affiliates.adstock + 
                           `promotion_off-1` + 
                           list_price_wrt_lag1 + list_price_wrt_ma_2, 
                         data = train_gameing_kyc)

summary(gameing_Kyc_Model18)
vif(gameing_Kyc_Model18)

#Removing Sponsorship due to high p value
gameing_Kyc_Model19 <-lm(formula = gmv ~ is_COD + mass_product + aspiring_product + 
                           product_mrp +
                           Digital.adstock + Sponsorship.adstock + 
                           ContentMarketing.adstock + OnlineMarketing.adstock + Affiliates.adstock + 
                           `promotion_off-1` + 
                           list_price_wrt_lag1 + list_price_wrt_ma_2, 
                         data = train_gameing_kyc)

summary(gameing_Kyc_Model19)
vif(gameing_Kyc_Model19)

#Removing is_COD due to high p value
gameing_Kyc_Model20 <-lm(formula = gmv ~ mass_product + aspiring_product + 
                           product_mrp +
                           Digital.adstock + Sponsorship.adstock + 
                           ContentMarketing.adstock + OnlineMarketing.adstock + Affiliates.adstock + 
                           `promotion_off-1` + 
                           list_price_wrt_lag1 + list_price_wrt_ma_2, 
                         data = train_gameing_kyc)

summary(gameing_Kyc_Model20)
vif(gameing_Kyc_Model20)

#Removing promotion_off-1 due to high p value
gameing_Kyc_Model21 <-lm(formula = gmv ~ mass_product + aspiring_product + 
                           product_mrp +
                           Digital.adstock + Sponsorship.adstock + 
                           ContentMarketing.adstock + OnlineMarketing.adstock + Affiliates.adstock + 
                           list_price_wrt_lag1 + list_price_wrt_ma_2, 
                         data = train_gameing_kyc)

summary(gameing_Kyc_Model21)
vif(gameing_Kyc_Model21)

#Removing list_price_wrt_ma_2 due to high p value
gameing_Kyc_Model22 <-lm(formula = gmv ~ mass_product + aspiring_product + 
                           product_mrp +
                           Digital.adstock + Sponsorship.adstock + 
                           ContentMarketing.adstock + OnlineMarketing.adstock + Affiliates.adstock + 
                           list_price_wrt_lag1, 
                         data = train_gameing_kyc)

summary(gameing_Kyc_Model22)
vif(gameing_Kyc_Model22)

#Removing product_mrp due to high p value
gameing_Kyc_Model23 <-lm(formula = gmv ~ mass_product + aspiring_product +
                           Digital.adstock + Sponsorship.adstock + 
                           ContentMarketing.adstock + OnlineMarketing.adstock + Affiliates.adstock + 
                           list_price_wrt_lag1, 
                         data = train_gameing_kyc)

summary(gameing_Kyc_Model23)
vif(gameing_Kyc_Model23)

#Removing ContentMarketing.adstock due to high p value
gameing_Kyc_Model24 <-lm(formula = gmv ~ mass_product + aspiring_product +
                           Digital.adstock + Sponsorship.adstock + 
                           OnlineMarketing.adstock + Affiliates.adstock + 
                           list_price_wrt_lag1, 
                         data = train_gameing_kyc)

summary(gameing_Kyc_Model24)
vif(gameing_Kyc_Model24)

#Removing Affiliates.adstock due to high p value
gameing_Kyc_Model25 <-lm(formula = gmv ~ mass_product + aspiring_product +
                           Digital.adstock + Sponsorship.adstock + 
                           OnlineMarketing.adstock+ 
                           list_price_wrt_lag1, 
                         data = train_gameing_kyc)

summary(gameing_Kyc_Model25)
vif(gameing_Kyc_Model25)

#Removing OnlineMarketing.adstock due to high p value
gameing_Kyc_Model26 <-lm(formula = gmv ~ mass_product + aspiring_product +
                           Digital.adstock + Sponsorship.adstock +
                           list_price_wrt_lag1, 
                         data = train_gameing_kyc)

summary(gameing_Kyc_Model26)
vif(gameing_Kyc_Model26)

#Removing Sponsorship.adstock due to high p value
gameing_Kyc_Model27 <-lm(formula = gmv ~ mass_product + aspiring_product +
                           Digital.adstock +
                           list_price_wrt_lag1, 
                         data = train_gameing_kyc)

summary(gameing_Kyc_Model27)
vif(gameing_Kyc_Model27)

#Removing Digital.adstock due to high p value
gameing_Kyc_Model28 <-lm(formula = gmv ~ mass_product + aspiring_product +
                           list_price_wrt_lag1, 
                         data = train_gameing_kyc)

summary(gameing_Kyc_Model28)
vif(gameing_Kyc_Model28)

#Removing list_price_wrt_lag1 due to high p value
gameing_Kyc_Model29 <-lm(formula = gmv ~ mass_product + aspiring_product,
                         data = train_gameing_kyc)

summary(gameing_Kyc_Model29)
#Multiple R-squared:  0.9428,	Adjusted R-squared:  0.9399 
vif(gameing_Kyc_Model29)

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)       3189240      51456   61.98  < 2e-16 ***
#  mass_product       828496      80501   10.29 1.13e-12 ***
#  aspiring_product   590704      85110    6.94 2.59e-08 ***

# predicting the results in test dataset
Predict_gamegmv_kyc <- predict(gameing_Kyc_Model29,test_gameing_kyc[,-1])
test_gameing_kyc$testgmv <- Predict_gamegmv_kyc

# Checking the R Square of the prediction
rgame_kyc <- cor(test_gameing_kyc$gmv,test_gameing_kyc$testgmv)
rgame_kyc  #0.9735158
rsquaregame_kyc <- rgame_kyc^2
rsquaregame_kyc  #0.9477331

### Cross-validation
cv.lm(data = train_gameing_kyc, form.lm = gameing_Kyc_Model29, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)

# Calculate the Elasticity
game_kyc_elasticity <- NULL
game_kyc_elasticity <- data.frame()
c = 1
for (i in names(gameing_Kyc_Model29$coefficients)[-1]){
  
  i_new <- str_replace_all(i , "`" , "")
  print(i_new)
  elasticity <- as.numeric(gameing_Kyc_Model29$coefficients[-1][i]*mean(train_gameing_kyc[,i_new])/mean(train_gameing_kyc$gmv))
  print(elasticity)
  game_kyc_elasticity[c,1] <- i_new
  game_kyc_elasticity[c,2] <- elasticity
  c <- c + 1
}

colnames(game_kyc_elasticity)[1] <- "variable"
colnames(game_kyc_elasticity)[2] <- "elasticity"

###########################################################################################

#############################################################################################################
# Building Koyck Model for Home Audio Dataset
#############################################################################################################


home_audio_final_koyck <- home_audio_final_new[,-which(colnames(home_audio_final_new) %in% c("gmv-1"))]
# scalling
home_audio_final_koyck[,2:ncol(home_audio_final_koyck)] <- scale(home_audio_final_koyck[,2:ncol(home_audio_final_koyck)])

#Removing column which cannot be scalled
home_audio_final_koyck$product_above_20000 <- NULL
# Divide into training and test data set
set.seed(100)

# randomly generate row indices for train dataset
trainindices_home_kyc= sample(1:nrow(home_audio_final_koyck), 0.8*nrow(home_audio_final_koyck))
# generate the train data set
train_home_kyc = home_audio_final_koyck[trainindices_home_kyc,]

#Similarly store the rest of the observations into an object "test".
test_home_kyc =home_audio_final_koyck[-trainindices_home_kyc,]


Home_Kyc_Model1<-lm(gmv~.,data=train_home_kyc)

summary(Home_Kyc_Model1)

# Perform stepAIC to remove insignificant variables

Home_kyc_ModelStepAIC<-stepAIC(Home_Kyc_Model1,direction = "both")

Home_kyc_ModelStepAIC

Home_Kyc_Model2<-lm(formula = gmv ~ is_COD + special_sale + premium_product + 
                      mass_product + aspiring_product + product_btwn_5000_10000 + 
                      product_btwn_10000_15000 + sla + product_mrp + product_procurement_sla + 
                      promotion_off + NPS + TV + Digital + `Content Marketing` + 
                      `Online marketing` + Affiliates + SEM + Radio + Other + TV.adstock + 
                      Sponsorship.adstock + ContentMarketing.adstock + OnlineMarketing.adstock + 
                      Affiliates.adstock + SEM.adstock + Radio.adstock + Other.adstock + 
                      list_price_wrt_ma_2 + list_price_wrt_ma_3, data = train_home_kyc)

summary(Home_Kyc_Model2)
vif(Home_Kyc_Model2)

#Removing Other due to high p value
Home_Kyc_Model3<-lm(formula = gmv ~ is_COD + special_sale + premium_product + 
                      mass_product + aspiring_product + product_btwn_5000_10000 + 
                      product_btwn_10000_15000 + sla + product_mrp + product_procurement_sla + 
                      promotion_off + NPS + TV + Digital + `Content Marketing` + 
                      `Online marketing` + Affiliates + SEM + Radio +TV.adstock + 
                      Sponsorship.adstock + ContentMarketing.adstock + OnlineMarketing.adstock + 
                      Affiliates.adstock + SEM.adstock + Radio.adstock + Other.adstock + 
                      list_price_wrt_ma_2 + list_price_wrt_ma_3, data = train_home_kyc)

summary(Home_Kyc_Model3)
vif(Home_Kyc_Model3)

#Removing TV.adstock due to high p value
Home_Kyc_Model4<-lm(formula = gmv ~ is_COD + special_sale + premium_product + 
                      mass_product + aspiring_product + product_btwn_5000_10000 + 
                      product_btwn_10000_15000 + sla + product_mrp + product_procurement_sla + 
                      promotion_off + NPS + TV + Digital + `Content Marketing` + 
                      `Online marketing` + Affiliates + SEM + Radio + 
                      Sponsorship.adstock + ContentMarketing.adstock + OnlineMarketing.adstock + 
                      Affiliates.adstock + SEM.adstock + Radio.adstock + Other.adstock + 
                      list_price_wrt_ma_2 + list_price_wrt_ma_3, data = train_home_kyc)

summary(Home_Kyc_Model4)
vif(Home_Kyc_Model4)


#Removing Other.adstock due to high p value
Home_Kyc_Model5<-lm(formula = gmv ~ is_COD + special_sale + premium_product + 
                      mass_product + aspiring_product + product_btwn_5000_10000 + 
                      product_btwn_10000_15000 + sla + product_mrp + product_procurement_sla + 
                      promotion_off + NPS + TV + Digital + `Content Marketing` + 
                      `Online marketing` + Affiliates + SEM + Radio + 
                      Sponsorship.adstock + ContentMarketing.adstock + OnlineMarketing.adstock + 
                      Affiliates.adstock + SEM.adstock + Radio.adstock + 
                      list_price_wrt_ma_2 + list_price_wrt_ma_3, data = train_home_kyc)

summary(Home_Kyc_Model5)
vif(Home_Kyc_Model5)

#Removing Online marketing due to high p value
Home_Kyc_Model6<-lm(formula = gmv ~ is_COD + special_sale + premium_product + 
                      mass_product + aspiring_product + product_btwn_5000_10000 + 
                      product_btwn_10000_15000 + sla + product_mrp + product_procurement_sla + 
                      promotion_off + NPS + TV + Digital + `Content Marketing` + Affiliates + SEM + Radio + 
                      Sponsorship.adstock + ContentMarketing.adstock + OnlineMarketing.adstock + 
                      Affiliates.adstock + SEM.adstock + Radio.adstock + 
                      list_price_wrt_ma_2 + list_price_wrt_ma_3, data = train_home_kyc)

summary(Home_Kyc_Model6)
vif(Home_Kyc_Model6)


#Removing Affiliates due to high p value
Home_Kyc_Model7<-lm(formula = gmv ~ is_COD + special_sale + premium_product + 
                      mass_product + aspiring_product + product_btwn_5000_10000 + 
                      product_btwn_10000_15000 + sla + product_mrp + product_procurement_sla + 
                      promotion_off + NPS + TV + Digital 
                      + `Content Marketing` +SEM + Radio + 
                      Sponsorship.adstock + ContentMarketing.adstock + OnlineMarketing.adstock + 
                      Affiliates.adstock + SEM.adstock + Radio.adstock + 
                      list_price_wrt_ma_2 + list_price_wrt_ma_3, data = train_home_kyc)

summary(Home_Kyc_Model7)
vif(Home_Kyc_Model7)

#Removing NPS due to high p value
Home_Kyc_Model8<-lm(formula = gmv ~ is_COD + special_sale + premium_product + 
                      mass_product + aspiring_product + product_btwn_5000_10000 + 
                      product_btwn_10000_15000 + sla + product_mrp + product_procurement_sla + 
                      promotion_off + TV + Digital 
                    + `Content Marketing` +SEM + Radio + 
                      Sponsorship.adstock + ContentMarketing.adstock + OnlineMarketing.adstock + 
                      Affiliates.adstock + SEM.adstock + Radio.adstock + 
                      list_price_wrt_ma_2 + list_price_wrt_ma_3, data = train_home_kyc)

summary(Home_Kyc_Model8)
vif(Home_Kyc_Model8)

#Removing Digital due to high p value
Home_Kyc_Model9<-lm(formula = gmv ~ is_COD + special_sale + premium_product + 
                      mass_product + aspiring_product + product_btwn_5000_10000 + 
                      product_btwn_10000_15000 + sla + product_mrp + product_procurement_sla + 
                      promotion_off + TV 
                      + `Content Marketing` +SEM + Radio + 
                      Sponsorship.adstock + ContentMarketing.adstock + OnlineMarketing.adstock + 
                      Affiliates.adstock + SEM.adstock + Radio.adstock + 
                      list_price_wrt_ma_2 + list_price_wrt_ma_3, data = train_home_kyc)

summary(Home_Kyc_Model9)
vif(Home_Kyc_Model9)

#Removing list_price_wrt_ma_2 due to high p value
Home_Kyc_Model10<-lm(formula = gmv ~ is_COD + special_sale + premium_product + 
                      mass_product + aspiring_product + product_btwn_5000_10000 + 
                      product_btwn_10000_15000 + sla + product_mrp + product_procurement_sla + 
                      promotion_off + TV 
                    + `Content Marketing` +SEM + Radio + 
                      Sponsorship.adstock + ContentMarketing.adstock + OnlineMarketing.adstock + 
                      Affiliates.adstock + SEM.adstock + Radio.adstock + 
                      list_price_wrt_ma_3, data = train_home_kyc)

summary(Home_Kyc_Model10)
vif(Home_Kyc_Model10)

#Removing aspiring_product due to high p value
Home_Kyc_Model11 <-lm(formula = gmv ~ is_COD + special_sale + premium_product + 
                       mass_product + product_btwn_5000_10000 + 
                       product_btwn_10000_15000 + sla + product_mrp + product_procurement_sla + 
                       promotion_off + TV 
                     + `Content Marketing` +SEM + Radio + 
                       Sponsorship.adstock + ContentMarketing.adstock + OnlineMarketing.adstock + 
                       Affiliates.adstock + SEM.adstock + Radio.adstock + 
                       list_price_wrt_ma_3, data = train_home_kyc)

summary(Home_Kyc_Model11)
vif(Home_Kyc_Model11)

#Removing product_btwn_10000_15000 due to high p value
Home_Kyc_Model12 <-lm(formula = gmv ~ is_COD + special_sale + premium_product + 
                        mass_product + product_btwn_5000_10000 + 
                        sla + product_mrp + product_procurement_sla + 
                        promotion_off + TV 
                        + `Content Marketing` +SEM + Radio + 
                        Sponsorship.adstock + ContentMarketing.adstock + OnlineMarketing.adstock + 
                        Affiliates.adstock + SEM.adstock + Radio.adstock + 
                        list_price_wrt_ma_3, data = train_home_kyc)

summary(Home_Kyc_Model12)
vif(Home_Kyc_Model12)

#Removing list_price_wrt_ma_3 due to high p value
Home_Kyc_Model13 <-lm(formula = gmv ~ is_COD + special_sale + premium_product + 
                        mass_product + product_btwn_5000_10000 + 
                        sla + product_mrp + product_procurement_sla + 
                        promotion_off + TV 
                      + `Content Marketing` +SEM + Radio + 
                        Sponsorship.adstock + ContentMarketing.adstock + OnlineMarketing.adstock + 
                        Affiliates.adstock + SEM.adstock + Radio.adstock,
                       data = train_home_kyc)

summary(Home_Kyc_Model13)
vif(Home_Kyc_Model13)


#Removing sla due to high p value
Home_Kyc_Model14 <-lm(formula = gmv ~ is_COD + special_sale + premium_product + 
                        mass_product + product_btwn_5000_10000 + 
                        product_mrp + product_procurement_sla + 
                        promotion_off + TV 
                      + `Content Marketing` +SEM + Radio + 
                        Sponsorship.adstock + ContentMarketing.adstock + OnlineMarketing.adstock + 
                        Affiliates.adstock + SEM.adstock + Radio.adstock,
                      data = train_home_kyc)

summary(Home_Kyc_Model14)
vif(Home_Kyc_Model14)

#Removing is_COD due to high p value
Home_Kyc_Model15 <-lm(formula = gmv ~ special_sale + premium_product + 
                        mass_product + product_btwn_5000_10000 + 
                        product_mrp + product_procurement_sla + 
                        promotion_off + TV 
                      + `Content Marketing` +SEM + Radio + 
                        Sponsorship.adstock + ContentMarketing.adstock + OnlineMarketing.adstock + 
                        Affiliates.adstock + SEM.adstock + Radio.adstock,
                      data = train_home_kyc)

summary(Home_Kyc_Model15)
vif(Home_Kyc_Model15)


#Removing OnlineMarketing.adstock due to high p value
Home_Kyc_Model16 <-lm(formula = gmv ~ special_sale + premium_product + 
                        mass_product + product_btwn_5000_10000 + 
                        product_mrp + product_procurement_sla + 
                        promotion_off + TV 
                      + `Content Marketing` +SEM + Radio + 
                        Sponsorship.adstock + ContentMarketing.adstock +
                        Affiliates.adstock + SEM.adstock + Radio.adstock,
                      data = train_home_kyc)

summary(Home_Kyc_Model16)
vif(Home_Kyc_Model16)

#Removing TV due to high p value
Home_Kyc_Model17 <-lm(formula = gmv ~ special_sale + premium_product + 
                        mass_product + product_btwn_5000_10000 + 
                        product_mrp + product_procurement_sla + 
                        promotion_off
                      + `Content Marketing` +SEM + Radio + 
                        Sponsorship.adstock + ContentMarketing.adstock +
                        Affiliates.adstock + SEM.adstock + Radio.adstock,
                      data = train_home_kyc)

summary(Home_Kyc_Model17)
vif(Home_Kyc_Model17)

#Removing product_btwn_5000_10000 due to high p value
Home_Kyc_Model18 <-lm(formula = gmv ~ special_sale + premium_product + 
                        mass_product +
                        product_mrp + product_procurement_sla + 
                        promotion_off
                      + `Content Marketing` +SEM + Radio + 
                        Sponsorship.adstock + ContentMarketing.adstock +
                        Affiliates.adstock + SEM.adstock + Radio.adstock,
                      data = train_home_kyc)

summary(Home_Kyc_Model18)
vif(Home_Kyc_Model18)

#Removing Sponsorship.adstock due to high p value
Home_Kyc_Model19 <-lm(formula = gmv ~ special_sale + premium_product + 
                        mass_product +
                        product_mrp + product_procurement_sla + 
                        promotion_off
                      + `Content Marketing` +SEM + Radio + 
                        ContentMarketing.adstock +
                        Affiliates.adstock + SEM.adstock + Radio.adstock,
                      data = train_home_kyc)

summary(Home_Kyc_Model19)
vif(Home_Kyc_Model19)

#Removing Affiliates.adstock due to high p value
Home_Kyc_Model20 <-lm(formula = gmv ~ special_sale + premium_product + 
                        mass_product +
                        product_mrp + product_procurement_sla + 
                        promotion_off
                      + `Content Marketing` +SEM + Radio + 
                        ContentMarketing.adstock +
                        SEM.adstock + Radio.adstock,
                      data = train_home_kyc)

summary(Home_Kyc_Model20)
vif(Home_Kyc_Model20)

#Removing special_sale due to high p value
Home_Kyc_Model21 <-lm(formula = gmv ~ premium_product + 
                        mass_product +
                        product_mrp + product_procurement_sla + 
                        promotion_off
                      + `Content Marketing` +SEM + Radio + 
                        ContentMarketing.adstock +
                        SEM.adstock + Radio.adstock,
                      data = train_home_kyc)

summary(Home_Kyc_Model21)
vif(Home_Kyc_Model21)

#Removing SEM due to high p value
Home_Kyc_Model22 <-lm(formula = gmv ~ premium_product + 
                        mass_product +
                        product_mrp + product_procurement_sla + 
                        promotion_off
                      + `Content Marketing` +Radio + 
                        ContentMarketing.adstock +
                        SEM.adstock + Radio.adstock,
                      data = train_home_kyc)

summary(Home_Kyc_Model22)
vif(Home_Kyc_Model22)


#Removing Content Marketing due to high p value
Home_Kyc_Model23 <-lm(formula = gmv ~ premium_product + 
                        mass_product +
                        product_mrp + product_procurement_sla + 
                        promotion_off
                        + Radio + 
                        ContentMarketing.adstock +
                        SEM.adstock + Radio.adstock,
                      data = train_home_kyc)

summary(Home_Kyc_Model23)
vif(Home_Kyc_Model23)


#Removing Radio due to high p value
Home_Kyc_Model24 <-lm(formula = gmv ~ premium_product + 
                        mass_product +
                        product_mrp + product_procurement_sla + 
                        promotion_off +
                        ContentMarketing.adstock +
                        SEM.adstock + Radio.adstock,
                      data = train_home_kyc)

summary(Home_Kyc_Model24)
vif(Home_Kyc_Model24)

#Removing Radio.adstock due to high p value
Home_Kyc_Model25 <-lm(formula = gmv ~ premium_product + 
                        mass_product +
                        product_mrp + product_procurement_sla + 
                        promotion_off +
                        ContentMarketing.adstock +
                        SEM.adstock,
                      data = train_home_kyc)

summary(Home_Kyc_Model25)
vif(Home_Kyc_Model25)

#Removing ContentMarketing.adstock due to high p value
Home_Kyc_Model26 <-lm(formula = gmv ~ premium_product + 
                        mass_product +
                        product_mrp + product_procurement_sla + 
                        promotion_off +
                        SEM.adstock,
                      data = train_home_kyc)

summary(Home_Kyc_Model26)
vif(Home_Kyc_Model26)

#Removing SEM.adstock due to high p value
Home_Kyc_Model27 <-lm(formula = gmv ~ premium_product + 
                        mass_product +
                        product_mrp + product_procurement_sla + 
                        promotion_off,
                      data = train_home_kyc)

summary(Home_Kyc_Model27)
#Multiple R-squared:  0.997,	Adjusted R-squared:  0.996 
vif(Home_Kyc_Model27)

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)              5064646      19159  264.35  < 2e-16 ***
#  premium_product           260297      30385    8.57  5.2e-10 ***
#  mass_product             2307106      38359   60.15  < 2e-16 ***
#  product_mrp               501344      39944   12.55  2.6e-14 ***
#  product_procurement_sla  -160282      36945   -4.34  0.00012 ***
#  promotion_off            -391576      47175   -8.30  1.1e-09 ***

# predicting the results in test dataset
Predict_homegmv_kyc <- predict(Home_Kyc_Model27,test_home_kyc[,-1])
test_home_kyc$testgmv <- Predict_homegmv_kyc

# Checking the R Square of the prediction
rhome_kyc <- cor(test_home_kyc$gmv,test_home_kyc$testgmv)
rhome_kyc  #0.998
rsquarerhome_kyc <- rhome_kyc^2
rsquarerhome_kyc  #0.997

### Cross-validation
cv.lm(data = train_home_kyc, form.lm = Home_Kyc_Model27, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)

# Calculate the Elasticity
home_kyc_elasticity <- NULL
home_kyc_elasticity <- data.frame()
c = 1
for (i in names(Home_Kyc_Model27$coefficients)[-1]){
  
  i_new <- str_replace_all(i , "`" , "")
  print(i_new)
  elasticity <- as.numeric(Home_Kyc_Model27$coefficients[-1][i]*mean(train_home_kyc[,i_new])/mean(train_home_kyc$gmv))
  print(elasticity)
  home_kyc_elasticity[c,1] <- i_new
  home_kyc_elasticity[c,2] <- elasticity
  c <- c + 1
}

colnames(home_kyc_elasticity)[1] <- "variable"
colnames(home_kyc_elasticity)[2] <- "elasticity"

###########################################################################################
# Plotting the Kyocks model elasticity
###########################################################################################
#Gaming Accessory
game_kyc_elasticity$polarity <- ifelse(game_kyc_elasticity$elasticity > 0, "Positive", "Negative")
ggplot(game_kyc_elasticity, aes(x=reorder(variable,elasticity),y=elasticity, fill = polarity)) +
  geom_bar(position="dodge",stat="identity", width = 0.9)  + coord_flip() +ggtitle("Kyocks Model Elasticity for Gaming Accessory") +xlab("Variables")+theme(axis.text=element_text(size=8),axis.title=element_text(size=10,face="bold"))

#Camera Accessory
cam_kyc_elasticity$polarity <- ifelse(cam_kyc_elasticity$elasticity > 0, "Positive", "Negative")
ggplot(cam_kyc_elasticity, aes(x=reorder(variable,elasticity),y=elasticity, fill = polarity)) +
  geom_bar(position="dodge",stat="identity", width = 0.9)  + coord_flip() +ggtitle("Kyocks Model Elasticity for Camera Accessory") +xlab("Variables")+theme(axis.text=element_text(size=8),axis.title=element_text(size=10,face="bold"))

#Home Audio
home_kyc_elasticity$polarity <- ifelse(home_kyc_elasticity$elasticity > 0, "Positive", "Negative")
ggplot(home_kyc_elasticity, aes(x=reorder(variable,elasticity),y=elasticity, fill = polarity)) +
  geom_bar(position="dodge",stat="identity", width = 0.9)  + coord_flip() +ggtitle("Kyocks Model Elasticity for Home Audio") +xlab("Variables")+theme(axis.text=element_text(size=8),axis.title=element_text(size=10,face="bold"))

################################################################################################################

#Building Distribution Lag Model for Camera Accessories
#######################################################################################

camera_accessory_final_dist <- camera_accessory_final_new

# scalling
camera_accessory_final_dist[,2:ncol(camera_accessory_final_dist)] <- scale(camera_accessory_final_dist[,2:ncol(camera_accessory_final_dist)])

#Removing column which cannot be scalled
camera_accessory_final_dist$product_btwn_10000_15000 <- NULL
camera_accessory_final_dist$product_btwn_15000_20000 <- NULL
# Divide into training and test data set
set.seed(100)

# randomly generate row indices for train dataset
trainindices_camera_dist= sample(1:nrow(camera_accessory_final_dist), 0.8*nrow(camera_accessory_final_dist))
# generate the train data set
train_camera_dist = camera_accessory_final_dist[trainindices_camera_dist,]

#Similarly store the rest of the observations into an object "test".
test_camera_dist =camera_accessory_final_dist[-trainindices_camera_dist,]


Camera_dist_Model1<-lm(gmv~.,data=train_camera_dist)

summary(Camera_dist_Model1)

# Perform stepAIC to remove insignificant variables

Camera_dist_ModelStepAIC<-stepAIC(Camera_dist_Model1,direction = "both")

Camera_dist_ModelStepAIC

Camera_dist_Model2<-lm(formula = gmv ~ premium_product + mass_product + product_mrp + 
                         product_procurement_sla + NPS + TV + Sponsorship + `Content Marketing` + 
                         `Online marketing` + Affiliates + SEM + Radio + Other + TV.adstock + 
                         Digital.adstock + Sponsorship.adstock + ContentMarketing.adstock + 
                         OnlineMarketing.adstock + Affiliates.adstock + SEM.adstock + 
                         `gmv-1` + `promotion_off-1` + list_price_wrt_lag1 + list_price_wrt_ma_3 + 
                         list_price_wrt_ma_4 + promotion_off_perc_change_prev_week, 
                       data = train_camera_dist)

summary(Camera_dist_Model2)
vif(Camera_dist_Model2)

#Removing ContentMarketing.adstock due to high p value
Camera_dist_Model3<-lm(formula = gmv ~ premium_product + mass_product + product_mrp + 
                         product_procurement_sla + NPS + TV + Sponsorship + `Content Marketing` + 
                         `Online marketing` + Affiliates + SEM + Radio + Other + TV.adstock + 
                         Digital.adstock + Sponsorship.adstock + 
                         OnlineMarketing.adstock + Affiliates.adstock + SEM.adstock + 
                         `gmv-1` + `promotion_off-1` + list_price_wrt_lag1 + list_price_wrt_ma_3 + 
                         list_price_wrt_ma_4 + promotion_off_perc_change_prev_week, 
                       data = train_camera_dist)

summary(Camera_dist_Model3)
vif(Camera_dist_Model3)

#Removing list_price_wrt_ma_4 due tom high p value
Camera_dist_Model4<-lm(formula = gmv ~ premium_product + mass_product + product_mrp + 
                         product_procurement_sla + NPS + TV + Sponsorship + `Content Marketing` + 
                         `Online marketing` + Affiliates + SEM + Radio + Other + TV.adstock + 
                         Digital.adstock + Sponsorship.adstock + 
                         OnlineMarketing.adstock + Affiliates.adstock + SEM.adstock + 
                         `gmv-1` + `promotion_off-1` + list_price_wrt_lag1 + list_price_wrt_ma_3 + 
                         promotion_off_perc_change_prev_week, 
                       data = train_camera_dist)

summary(Camera_dist_Model4)
vif(Camera_dist_Model4)

#Removing NPS due to high p value
Camera_dist_Model5<-lm(formula = gmv ~ premium_product + mass_product + product_mrp + 
                         product_procurement_sla + TV + Sponsorship + `Content Marketing` + 
                         `Online marketing` + Affiliates + SEM + Radio + Other + TV.adstock + 
                         Digital.adstock + Sponsorship.adstock + 
                         OnlineMarketing.adstock + Affiliates.adstock + SEM.adstock + 
                         `gmv-1` + `promotion_off-1` + list_price_wrt_lag1 + list_price_wrt_ma_3 + 
                         promotion_off_perc_change_prev_week, 
                       data = train_camera_dist)

summary(Camera_dist_Model5)
vif(Camera_dist_Model5)

#Removing TV due to high p value
Camera_dist_Model6<-lm(formula = gmv ~ premium_product + mass_product + product_mrp + 
                         product_procurement_sla + Sponsorship + `Content Marketing` + 
                         `Online marketing` + Affiliates + SEM + Radio + Other + TV.adstock + 
                         Digital.adstock + Sponsorship.adstock + 
                         OnlineMarketing.adstock + Affiliates.adstock + SEM.adstock + 
                         `gmv-1` + `promotion_off-1` + list_price_wrt_lag1 + list_price_wrt_ma_3 + 
                         promotion_off_perc_change_prev_week, 
                       data = train_camera_dist)

summary(Camera_dist_Model6)
vif(Camera_dist_Model6)

#Removing gmv-1 due to high p value
Camera_dist_Model7<-lm(formula = gmv ~ premium_product + mass_product + product_mrp + 
                         product_procurement_sla + Sponsorship + `Content Marketing` + 
                         `Online marketing` + Affiliates + SEM + Radio + Other + TV.adstock + 
                         Digital.adstock + Sponsorship.adstock + 
                         OnlineMarketing.adstock + Affiliates.adstock + SEM.adstock + 
                         `promotion_off-1` + list_price_wrt_lag1 + list_price_wrt_ma_3 + 
                         promotion_off_perc_change_prev_week, 
                       data = train_camera_dist)

summary(Camera_dist_Model7)
vif(Camera_dist_Model7)

#Removing promotion_off-1 due to high p value
Camera_dist_Model8 <-lm(formula = gmv ~ premium_product + mass_product + product_mrp + 
                         product_procurement_sla + Sponsorship + `Content Marketing` + 
                         `Online marketing` + Affiliates + SEM + Radio + Other + TV.adstock + 
                         Digital.adstock + Sponsorship.adstock + 
                         OnlineMarketing.adstock + Affiliates.adstock + SEM.adstock + 
                         list_price_wrt_lag1 + list_price_wrt_ma_3 + 
                         promotion_off_perc_change_prev_week, 
                       data = train_camera_dist)

summary(Camera_dist_Model8)
vif(Camera_dist_Model8)

#Removing Sponsorship.adstock due to high p value
Camera_dist_Model9 <-lm(formula = gmv ~ premium_product + mass_product + product_mrp + 
                          product_procurement_sla + Sponsorship + `Content Marketing` + 
                          `Online marketing` + Affiliates + SEM + Radio + Other + TV.adstock + 
                          Digital.adstock + 
                          OnlineMarketing.adstock + Affiliates.adstock + SEM.adstock + 
                          list_price_wrt_lag1 + list_price_wrt_ma_3 + 
                          promotion_off_perc_change_prev_week, 
                        data = train_camera_dist)

summary(Camera_dist_Model9)
vif(Camera_dist_Model9)

#Removing Sponsorship due to high p value
Camera_dist_Model10 <-lm(formula = gmv ~ premium_product + mass_product + product_mrp + 
                          product_procurement_sla + `Content Marketing` + 
                          `Online marketing` + Affiliates + SEM + Radio + Other + TV.adstock + 
                          Digital.adstock + 
                          OnlineMarketing.adstock + Affiliates.adstock + SEM.adstock + 
                          list_price_wrt_lag1 + list_price_wrt_ma_3 + 
                          promotion_off_perc_change_prev_week, 
                        data = train_camera_dist)

summary(Camera_dist_Model10)
vif(Camera_dist_Model10)

#Removing list_price_wrt_ma_3 due to high p value
Camera_dist_Model11 <-lm(formula = gmv ~ premium_product + mass_product + product_mrp + 
                           product_procurement_sla + `Content Marketing` + 
                           `Online marketing` + Affiliates + SEM + Radio + Other + TV.adstock + 
                           Digital.adstock + 
                           OnlineMarketing.adstock + Affiliates.adstock + SEM.adstock + 
                           list_price_wrt_lag1 + 
                           promotion_off_perc_change_prev_week, 
                         data = train_camera_dist)

summary(Camera_dist_Model11)
vif(Camera_dist_Model11)

#Removing Radio due to high p value
Camera_dist_Model12 <-lm(formula = gmv ~ premium_product + mass_product + product_mrp + 
                           product_procurement_sla + `Content Marketing` + 
                           `Online marketing` + Affiliates + SEM + Other + TV.adstock + 
                           Digital.adstock + 
                           OnlineMarketing.adstock + Affiliates.adstock + SEM.adstock + 
                           list_price_wrt_lag1 + 
                           promotion_off_perc_change_prev_week, 
                         data = train_camera_dist)

summary(Camera_dist_Model12)
vif(Camera_dist_Model12)

#Removing SEM.adstock due to high p value
Camera_dist_Model13 <-lm(formula = gmv ~ premium_product + mass_product + product_mrp + 
                           product_procurement_sla + `Content Marketing` + 
                           `Online marketing` + Affiliates + SEM + Other + TV.adstock + 
                           Digital.adstock + 
                           OnlineMarketing.adstock + Affiliates.adstock + 
                           list_price_wrt_lag1 + 
                           promotion_off_perc_change_prev_week, 
                         data = train_camera_dist)

summary(Camera_dist_Model13)
vif(Camera_dist_Model13)

#Removing OnlineMarketing.adstock due to high p value
Camera_dist_Model14 <-lm(formula = gmv ~ premium_product + mass_product + product_mrp + 
                           product_procurement_sla + `Content Marketing` + 
                           `Online marketing` + Affiliates + SEM + Other + TV.adstock + 
                           Digital.adstock + 
                           Affiliates.adstock + 
                           list_price_wrt_lag1 + 
                           promotion_off_perc_change_prev_week, 
                         data = train_camera_dist)

summary(Camera_dist_Model14)
vif(Camera_dist_Model14)

#Removing TV.adstock due to high p value
Camera_dist_Model15 <-lm(formula = gmv ~ premium_product + mass_product + product_mrp + 
                           product_procurement_sla + `Content Marketing` + 
                           `Online marketing` + Affiliates + SEM + Other + 
                           Digital.adstock + 
                           Affiliates.adstock + 
                           list_price_wrt_lag1 + 
                           promotion_off_perc_change_prev_week, 
                         data = train_camera_dist)

summary(Camera_dist_Model15)
vif(Camera_dist_Model15)

#Removing Other due to high  p value
Camera_dist_Model16 <-lm(formula = gmv ~ premium_product + mass_product + product_mrp + 
                           product_procurement_sla + `Content Marketing` + 
                           `Online marketing` + Affiliates + SEM + 
                           Digital.adstock + 
                           Affiliates.adstock + 
                           list_price_wrt_lag1 + 
                           promotion_off_perc_change_prev_week, 
                         data = train_camera_dist)

summary(Camera_dist_Model16)
vif(Camera_dist_Model16)

#Removing Affiliates.adstock due to high p value
Camera_dist_Model17 <-lm(formula = gmv ~ premium_product + mass_product + product_mrp + 
                           product_procurement_sla + `Content Marketing` + 
                           `Online marketing` + Affiliates + SEM + 
                           Digital.adstock + 
                           list_price_wrt_lag1 + 
                           promotion_off_perc_change_prev_week, 
                         data = train_camera_dist)

summary(Camera_dist_Model17)
vif(Camera_dist_Model17)

#Removing Digital.adstock due to high p value and high vif
Camera_dist_Model18 <-lm(formula = gmv ~ premium_product + mass_product + product_mrp + 
                           product_procurement_sla + `Content Marketing` + 
                           `Online marketing` + Affiliates + SEM +
                           list_price_wrt_lag1 + 
                           promotion_off_perc_change_prev_week, 
                         data = train_camera_dist)

summary(Camera_dist_Model18)
vif(Camera_dist_Model18)

#Removing list_price_wrt_lag1 due to high p value
Camera_dist_Model19 <-lm(formula = gmv ~ premium_product + mass_product + product_mrp + 
                           product_procurement_sla + `Content Marketing` + 
                           `Online marketing` + Affiliates + SEM +
                           promotion_off_perc_change_prev_week, 
                         data = train_camera_dist)

summary(Camera_dist_Model19)
vif(Camera_dist_Model19)

#Removing promotion_off_perc_change_prev_week due to high p value
Camera_dist_Model20 <-lm(formula = gmv ~ premium_product + mass_product + product_mrp + 
                           product_procurement_sla + `Content Marketing` + 
                           `Online marketing` + Affiliates + SEM,
                         data = train_camera_dist)

summary(Camera_dist_Model20)
vif(Camera_dist_Model20)

#Removing product_procurement_sla due to high p value
Camera_dist_Model21 <-lm(formula = gmv ~ premium_product + mass_product + product_mrp + 
                           `Content Marketing` + 
                           `Online marketing` + Affiliates + SEM,
                         data = train_camera_dist)

summary(Camera_dist_Model21)
vif(Camera_dist_Model21)

#Removing Affiliates due to high p value
Camera_dist_Model22 <-lm(formula = gmv ~ premium_product + mass_product + product_mrp + 
                           `Content Marketing` + 
                           `Online marketing` + SEM,
                         data = train_camera_dist)

summary(Camera_dist_Model22)
vif(Camera_dist_Model22)

#Removing Online marketing due to high p value
Camera_dist_Model23 <-lm(formula = gmv ~ premium_product + mass_product + product_mrp + 
                           `Content Marketing` + SEM,
                         data = train_camera_dist)

summary(Camera_dist_Model23)
vif(Camera_dist_Model23)

#Removing SEM due to high p value and vif
Camera_dist_Model24 <-lm(formula = gmv ~ premium_product + mass_product + product_mrp + 
                           `Content Marketing`,
                         data = train_camera_dist)

summary(Camera_dist_Model24)
vif(Camera_dist_Model24)

#Removing Content Marketing due to high  vif
Camera_dist_Model25 <-lm(formula = gmv ~ premium_product + mass_product + product_mrp,
                         data = train_camera_dist)

summary(Camera_dist_Model25)
#Multiple R-squared:  0.973,	Adjusted R-squared:  0.97
vif(Camera_dist_Model25)

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)      5351180      53933   99.22  < 2e-16 ***
#  premium_product   917950     152328    6.03  5.8e-07 ***
#  mass_product     1555081     123584   12.58  6.2e-15 ***
#  product_mrp       797981      68660   11.62  6.5e-14 ***

# predicting the results in test dataset
Predict_camgmv_dist <- predict(Camera_dist_Model25,test_camera_dist[,-1])
test_camera_dist$testgmv <- Predict_camgmv_dist

# Checking the R Square of the prediction
rcam_dist <- cor(test_camera_dist$gmv,test_camera_dist$testgmv)
rcam_dist  #0.983
rsquarecam_dist <- rcam_dist^2
rsquarecam_dist  #0.966

### Cross-validation
cv.lm(data = train_camera_dist, form.lm = Camera_dist_Model25, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)

# Calculate the Elasticity
cam_dist_elasticity <- NULL
cam_dist_elasticity <- data.frame()
c = 1
for (i in names(Camera_dist_Model25$coefficients)[-1]){
  
  i_new <- str_replace_all(i , "`" , "")
  print(i_new)
  elasticity <- as.numeric(Camera_dist_Model25$coefficients[-1][i]*mean(train_camera_dist[,i_new])/mean(train_camera_dist$gmv))
  print(elasticity)
  cam_dist_elasticity[c,1] <- i_new
  cam_dist_elasticity[c,2] <- elasticity
  c <- c + 1
}

colnames(cam_dist_elasticity)[1] <- "variable"
colnames(cam_dist_elasticity)[2] <- "elasticity"

#########################################################################################

#Building Distribution Lag Model for Gaming Accessories
#######################################################################################

gameing_accessory_final_dist <- gameing_accessory_final_new

# scalling
gameing_accessory_final_dist[,2:ncol(gameing_accessory_final_dist)] <- scale(gameing_accessory_final_dist[,2:ncol(gameing_accessory_final_dist)])

#Removing column which cannot be scalled
gameing_accessory_final_dist$product_btwn_10000_15000 <- NULL
gameing_accessory_final_dist$product_btwn_15000_20000 <- NULL
gameing_accessory_final_dist$product_above_20000 <- NULL
# Divide into training and test data set
set.seed(100)

# randomly generate row indices for train dataset
trainindices_gameing_dist= sample(1:nrow(gameing_accessory_final_dist), 0.8*nrow(gameing_accessory_final_dist))
# generate the train data set
train_gameing_dist = gameing_accessory_final_dist[trainindices_gameing_dist,]

#Similarly store the rest of the observations into an object "test".
test_gameing_dist =gameing_accessory_final_dist[-trainindices_gameing_dist,]


Game_dist_Model1<-lm(gmv~.,data=train_gameing_dist)

summary(Game_dist_Model1)

# Perform stepAIC to remove insignificant variables

Game_dist_ModelStepAIC<-stepAIC(Game_dist_Model1,direction = "both")

Game_dist_ModelStepAIC

Game_dist_Model2<-lm(formula = gmv ~ is_COD + special_sale + mass_product + aspiring_product + 
                        sla + product_mrp + product_procurement_sla + promotion_off + 
                        NPS + Digital + Sponsorship + `Content Marketing` + `Online marketing` + 
                        Affiliates + SEM + Radio + Other + TV.adstock + Digital.adstock + 
                        Sponsorship.adstock + ContentMarketing.adstock + OnlineMarketing.adstock + 
                        Affiliates.adstock + SEM.adstock + Radio.adstock + Other.adstock + 
                        `gmv-1` + `promotion_off-1` + list_price_wrt_lag1 + list_price_wrt_ma_2 + 
                        list_price_wrt_ma_4 + promotion_off_perc_change_prev_week, 
                      data = train_gameing_dist)

summary(Game_dist_Model2)
vif(Game_dist_Model2)
#Removing promotion_off_perc_change_prev_week due to high p value
Game_dist_Model3<-lm(formula = gmv ~ is_COD + special_sale + mass_product + aspiring_product + 
                       sla + product_mrp + product_procurement_sla + promotion_off + 
                       NPS + Digital + Sponsorship + `Content Marketing` + `Online marketing` + 
                       Affiliates + SEM + Radio + Other + TV.adstock + Digital.adstock + 
                       Sponsorship.adstock + ContentMarketing.adstock + OnlineMarketing.adstock + 
                       Affiliates.adstock + SEM.adstock + Radio.adstock + Other.adstock + 
                       `gmv-1` + `promotion_off-1` + list_price_wrt_lag1 + list_price_wrt_ma_2 + 
                       list_price_wrt_ma_4, 
                     data = train_gameing_dist)

summary(Game_dist_Model3)
vif(Game_dist_Model3)

#Removing gmv-1 due to high p value
Game_dist_Model4<-lm(formula = gmv ~ is_COD + special_sale + mass_product + aspiring_product + 
                       sla + product_mrp + product_procurement_sla + promotion_off + 
                       NPS + Digital + Sponsorship + `Content Marketing` + `Online marketing` + 
                       Affiliates + SEM + Radio + Other + TV.adstock + Digital.adstock + 
                       Sponsorship.adstock + ContentMarketing.adstock + OnlineMarketing.adstock + 
                       Affiliates.adstock + SEM.adstock + Radio.adstock + Other.adstock + 
                       `promotion_off-1` + list_price_wrt_lag1 + list_price_wrt_ma_2 + 
                       list_price_wrt_ma_4, 
                     data = train_gameing_dist)

summary(Game_dist_Model4)
vif(Game_dist_Model4)

#Removing product_procurement_sla due to high p value
Game_dist_Model5<-lm(formula = gmv ~ is_COD + special_sale + mass_product + aspiring_product + 
                       sla + product_mrp  + promotion_off + 
                       NPS + Digital + Sponsorship + `Content Marketing` + `Online marketing` + 
                       Affiliates + SEM + Radio + Other + TV.adstock + Digital.adstock + 
                       Sponsorship.adstock + ContentMarketing.adstock + OnlineMarketing.adstock + 
                       Affiliates.adstock + SEM.adstock + Radio.adstock + Other.adstock + 
                       `promotion_off-1` + list_price_wrt_lag1 + list_price_wrt_ma_2 + 
                       list_price_wrt_ma_4, 
                     data = train_gameing_dist)

summary(Game_dist_Model5)
vif(Game_dist_Model5)

#Removing NPS due to high p value
Game_dist_Model6<-lm(formula = gmv ~ is_COD + special_sale + mass_product + aspiring_product + 
                       sla + product_mrp  + promotion_off + Digital + Sponsorship + `Content Marketing` + `Online marketing` + 
                       Affiliates + SEM + Radio + Other + TV.adstock + Digital.adstock + 
                       Sponsorship.adstock + ContentMarketing.adstock + OnlineMarketing.adstock + 
                       Affiliates.adstock + SEM.adstock + Radio.adstock + Other.adstock + 
                       `promotion_off-1` + list_price_wrt_lag1 + list_price_wrt_ma_2 + 
                       list_price_wrt_ma_4, 
                     data = train_gameing_dist)

summary(Game_dist_Model6)
vif(Game_dist_Model6)

#Removing promotion_off due to high p value
Game_dist_Model7 <-lm(formula = gmv ~ is_COD + special_sale + mass_product + aspiring_product + 
                       sla + product_mrp   + Digital + Sponsorship + `Content Marketing` + `Online marketing` + 
                       Affiliates + SEM + Radio + Other + TV.adstock + Digital.adstock + 
                       Sponsorship.adstock + ContentMarketing.adstock + OnlineMarketing.adstock + 
                       Affiliates.adstock + SEM.adstock + Radio.adstock + Other.adstock + 
                       `promotion_off-1` + list_price_wrt_lag1 + list_price_wrt_ma_2 + 
                       list_price_wrt_ma_4, 
                     data = train_gameing_dist)

summary(Game_dist_Model7)
vif(Game_dist_Model7)

#Removing TV.adstock due to high p value
Game_dist_Model8 <-lm(formula = gmv ~ is_COD + special_sale + mass_product + aspiring_product + 
                        sla + product_mrp   + Digital + Sponsorship + `Content Marketing` + `Online marketing` + 
                        Affiliates + SEM + Radio + Other  + Digital.adstock + 
                        Sponsorship.adstock + ContentMarketing.adstock + OnlineMarketing.adstock + 
                        Affiliates.adstock + SEM.adstock + Radio.adstock + Other.adstock + 
                        `promotion_off-1` + list_price_wrt_lag1 + list_price_wrt_ma_2 + 
                        list_price_wrt_ma_4, 
                      data = train_gameing_dist)

summary(Game_dist_Model8)
vif(Game_dist_Model8)

#Removing SEM.adstock due to high pvalue
Game_dist_Model9 <-lm(formula = gmv ~ is_COD + special_sale + mass_product + aspiring_product + 
                        sla + product_mrp   + Digital + Sponsorship + `Content Marketing` + `Online marketing` + 
                        Affiliates + SEM + Radio + Other  + Digital.adstock + 
                        Sponsorship.adstock + ContentMarketing.adstock + OnlineMarketing.adstock + 
                        Affiliates.adstock + Radio.adstock + Other.adstock + 
                        `promotion_off-1` + list_price_wrt_lag1 + list_price_wrt_ma_2 + 
                        list_price_wrt_ma_4, 
                      data = train_gameing_dist)

summary(Game_dist_Model9)
vif(Game_dist_Model9)

#Removing SEM due to high p value
Game_dist_Model10 <-lm(formula = gmv ~ is_COD + special_sale + mass_product + aspiring_product + 
                        sla + product_mrp   + Digital + Sponsorship + `Content Marketing` + `Online marketing` + 
                        Affiliates + Radio + Other  + Digital.adstock + 
                        Sponsorship.adstock + ContentMarketing.adstock + OnlineMarketing.adstock + 
                        Affiliates.adstock + Radio.adstock + Other.adstock + 
                        `promotion_off-1` + list_price_wrt_lag1 + list_price_wrt_ma_2 + 
                        list_price_wrt_ma_4, 
                      data = train_gameing_dist)

summary(Game_dist_Model10)
vif(Game_dist_Model10)

#Removing list_price_wrt_ma_4 due to high p value
Game_dist_Model11 <-lm(formula = gmv ~ is_COD + special_sale + mass_product + aspiring_product + 
                         sla + product_mrp   + Digital + Sponsorship + `Content Marketing` + `Online marketing` + 
                         Affiliates + Radio + Other  + Digital.adstock + 
                         Sponsorship.adstock + ContentMarketing.adstock + OnlineMarketing.adstock + 
                         Affiliates.adstock + Radio.adstock + Other.adstock + 
                         `promotion_off-1` + list_price_wrt_lag1 + list_price_wrt_ma_2,
                       data = train_gameing_dist)

summary(Game_dist_Model11)
vif(Game_dist_Model11)

#Removing Radio.adstock due to high pvalue
Game_dist_Model12 <-lm(formula = gmv ~ is_COD + special_sale + mass_product + aspiring_product + 
                         sla + product_mrp   + Digital + Sponsorship + `Content Marketing` + `Online marketing` + 
                         Affiliates + Radio + Other  + Digital.adstock + 
                         Sponsorship.adstock + ContentMarketing.adstock + OnlineMarketing.adstock + 
                         Affiliates.adstock + Other.adstock + 
                         `promotion_off-1` + list_price_wrt_lag1 + list_price_wrt_ma_2,
                       data = train_gameing_dist)

summary(Game_dist_Model12)
vif(Game_dist_Model12)

#Removing Radio due to high pvalue
Game_dist_Model13 <-lm(formula = gmv ~ is_COD + special_sale + mass_product + aspiring_product + 
                         sla + product_mrp   + Digital + Sponsorship + `Content Marketing` + `Online marketing` + 
                         Affiliates +  Other  + Digital.adstock + 
                         Sponsorship.adstock + ContentMarketing.adstock + OnlineMarketing.adstock + 
                         Affiliates.adstock + Other.adstock + 
                         `promotion_off-1` + list_price_wrt_lag1 + list_price_wrt_ma_2,
                       data = train_gameing_dist)

summary(Game_dist_Model13)
vif(Game_dist_Model13)

#Removing Other.adstock due to high pvalue
Game_dist_Model14 <-lm(formula = gmv ~ is_COD + special_sale + mass_product + aspiring_product + 
                         sla + product_mrp   + Digital + Sponsorship + `Content Marketing` + `Online marketing` + 
                         Affiliates +  Other  + Digital.adstock + 
                         Sponsorship.adstock + ContentMarketing.adstock + OnlineMarketing.adstock + 
                         Affiliates.adstock +
                         `promotion_off-1` + list_price_wrt_lag1 + list_price_wrt_ma_2,
                       data = train_gameing_dist)

summary(Game_dist_Model14)
vif(Game_dist_Model14)

#Removing sla due to high pvalue
Game_dist_Model15 <-lm(formula = gmv ~ is_COD + special_sale + mass_product + aspiring_product + 
                         product_mrp   + Digital + Sponsorship + `Content Marketing` + `Online marketing` + 
                         Affiliates +  Other  + Digital.adstock + 
                         Sponsorship.adstock + ContentMarketing.adstock + OnlineMarketing.adstock + 
                         Affiliates.adstock +
                         `promotion_off-1` + list_price_wrt_lag1 + list_price_wrt_ma_2,
                       data = train_gameing_dist)

summary(Game_dist_Model15)
vif(Game_dist_Model15)

#Removing Other due to high pvalue
Game_dist_Model16 <-lm(formula = gmv ~ is_COD + special_sale + mass_product + aspiring_product + 
                         product_mrp   + Digital + Sponsorship + `Content Marketing` + `Online marketing` + 
                         Affiliates +  Digital.adstock + 
                         Sponsorship.adstock + ContentMarketing.adstock + OnlineMarketing.adstock + 
                         Affiliates.adstock +
                         `promotion_off-1` + list_price_wrt_lag1 + list_price_wrt_ma_2,
                       data = train_gameing_dist)

summary(Game_dist_Model16)
vif(Game_dist_Model16)

#Removing special_sale due to high pvalue
Game_dist_Model17 <-lm(formula = gmv ~ is_COD + mass_product + aspiring_product + 
                         product_mrp   + Digital + Sponsorship + `Content Marketing` + `Online marketing` + 
                         Affiliates +  Digital.adstock + 
                         Sponsorship.adstock + ContentMarketing.adstock + OnlineMarketing.adstock + 
                         Affiliates.adstock +
                         `promotion_off-1` + list_price_wrt_lag1 + list_price_wrt_ma_2,
                       data = train_gameing_dist)

summary(Game_dist_Model17)
vif(Game_dist_Model17)

#Removing Online marketing due to high pvalue
Game_dist_Model18 <-lm(formula = gmv ~ is_COD + mass_product + aspiring_product + 
                         product_mrp   + Digital + Sponsorship + `Content Marketing`+ 
                         Affiliates +  Digital.adstock + 
                         Sponsorship.adstock + ContentMarketing.adstock + OnlineMarketing.adstock + 
                         Affiliates.adstock +
                         `promotion_off-1` + list_price_wrt_lag1 + list_price_wrt_ma_2,
                       data = train_gameing_dist)

summary(Game_dist_Model18)
vif(Game_dist_Model18)

#Removing Affiliates due to high pvalue
Game_dist_Model19 <-lm(formula = gmv ~ is_COD + mass_product + aspiring_product + 
                         product_mrp   + Digital + Sponsorship + `Content Marketing`+ 
                         Digital.adstock + 
                         Sponsorship.adstock + ContentMarketing.adstock + OnlineMarketing.adstock + 
                         Affiliates.adstock +
                         `promotion_off-1` + list_price_wrt_lag1 + list_price_wrt_ma_2,
                       data = train_gameing_dist)

summary(Game_dist_Model19)
vif(Game_dist_Model19)

#Removing Content Marketing due to high pvalue
Game_dist_Model20 <-lm(formula = gmv ~ is_COD + mass_product + aspiring_product + 
                         product_mrp   + Digital + Sponsorship +  
                         Digital.adstock + 
                         Sponsorship.adstock + ContentMarketing.adstock + OnlineMarketing.adstock + 
                         Affiliates.adstock +
                         `promotion_off-1` + list_price_wrt_lag1 + list_price_wrt_ma_2,
                       data = train_gameing_dist)

summary(Game_dist_Model20)
vif(Game_dist_Model20)

#Removing DIgital due to high pvalue
Game_dist_Model21 <-lm(formula = gmv ~ is_COD + mass_product + aspiring_product + 
                         product_mrp   + Sponsorship +  
                         Digital.adstock + 
                         Sponsorship.adstock + ContentMarketing.adstock + OnlineMarketing.adstock + 
                         Affiliates.adstock +
                         `promotion_off-1` + list_price_wrt_lag1 + list_price_wrt_ma_2,
                       data = train_gameing_dist)

summary(Game_dist_Model21)
vif(Game_dist_Model21)

#Removing Sponsorship due to high pvalue
Game_dist_Model22 <-lm(formula = gmv ~ is_COD + mass_product + aspiring_product + 
                         product_mrp   +  
                         Digital.adstock + 
                         Sponsorship.adstock + ContentMarketing.adstock + OnlineMarketing.adstock + 
                         Affiliates.adstock +
                         `promotion_off-1` + list_price_wrt_lag1 + list_price_wrt_ma_2,
                       data = train_gameing_dist)

summary(Game_dist_Model22)
vif(Game_dist_Model22)

#Removing is_COD due to high pvalue
Game_dist_Model23 <-lm(formula = gmv ~ mass_product + aspiring_product + 
                         product_mrp   +  
                         Digital.adstock + 
                         Sponsorship.adstock + ContentMarketing.adstock + OnlineMarketing.adstock + 
                         Affiliates.adstock +
                         `promotion_off-1` + list_price_wrt_lag1 + list_price_wrt_ma_2,
                       data = train_gameing_dist)

summary(Game_dist_Model23)
vif(Game_dist_Model23)

#Removing promotion_off-1 due to high pvalue
Game_dist_Model24 <-lm(formula = gmv ~ mass_product + aspiring_product + 
                         product_mrp   +  
                         Digital.adstock + 
                         Sponsorship.adstock + ContentMarketing.adstock + OnlineMarketing.adstock + 
                         Affiliates.adstock +
                         list_price_wrt_lag1 + list_price_wrt_ma_2,
                       data = train_gameing_dist)

summary(Game_dist_Model24)
vif(Game_dist_Model24)

#Removing list_price_wrt_ma_2 due to high pvalue
Game_dist_Model25 <-lm(formula = gmv ~ mass_product + aspiring_product + 
                         product_mrp   +  
                         Digital.adstock + 
                         Sponsorship.adstock + ContentMarketing.adstock + OnlineMarketing.adstock + 
                         Affiliates.adstock +
                         list_price_wrt_lag1 ,
                       data = train_gameing_dist)

summary(Game_dist_Model25)
vif(Game_dist_Model25)

#Removing product_mrp due to high pvalue
Game_dist_Model26 <-lm(formula = gmv ~ mass_product + aspiring_product +
                         Digital.adstock + 
                         Sponsorship.adstock + ContentMarketing.adstock + OnlineMarketing.adstock + 
                         Affiliates.adstock +
                         list_price_wrt_lag1 ,
                       data = train_gameing_dist)

summary(Game_dist_Model26)
vif(Game_dist_Model26)

#Removing ContentMarketing.adstock due to high pvalue
Game_dist_Model27 <-lm(formula = gmv ~ mass_product + aspiring_product +
                         Digital.adstock + 
                         Sponsorship.adstock + OnlineMarketing.adstock + 
                         Affiliates.adstock +
                         list_price_wrt_lag1 ,
                       data = train_gameing_dist)

summary(Game_dist_Model27)
vif(Game_dist_Model27)

#Removing Affiliates.adstock due to high pvalue
Game_dist_Model28 <-lm(formula = gmv ~ mass_product + aspiring_product +
                         Digital.adstock + 
                         Sponsorship.adstock + OnlineMarketing.adstock +
                         list_price_wrt_lag1 ,
                       data = train_gameing_dist)

summary(Game_dist_Model28)
vif(Game_dist_Model28)

#Removing OnlineMarketing.adstock due to high pvalue
Game_dist_Model29 <-lm(formula = gmv ~ mass_product + aspiring_product +
                         Digital.adstock + 
                         Sponsorship.adstock +
                         list_price_wrt_lag1 ,
                       data = train_gameing_dist)

summary(Game_dist_Model29)
vif(Game_dist_Model29)

#Removing Sponsorship.adstock due to high pvalue
Game_dist_Model30 <-lm(formula = gmv ~ mass_product + aspiring_product +
                         Digital.adstock +
                         list_price_wrt_lag1 ,
                       data = train_gameing_dist)

summary(Game_dist_Model30)
#Multiple R-squared:  0.954,	Adjusted R-squared:  0.951
vif(Game_dist_Model30)


#Removing Digital.adstock due to high pvalue
Game_dist_Model31 <-lm(formula = gmv ~ mass_product + aspiring_product +
                         list_price_wrt_lag1 ,
                       data = train_gameing_dist)

summary(Game_dist_Model31)
#Multiple R-squared:  0.954,	Adjusted R-squared:  0.951
vif(Game_dist_Model31)

#Removing list_price_wrt_lag1 due to high pvalue
Game_dist_Model32 <-lm(formula = gmv ~ mass_product + aspiring_product,
                       data = train_gameing_dist)

summary(Game_dist_Model32)
#Multiple R-squared:  0.954,	Adjusted R-squared:  0.951
vif(Game_dist_Model32)

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)       3189240      51456   61.98  < 2e-16 ***
#  mass_product       828496      80501   10.29  1.1e-12 ***
#  aspiring_product   590704      85110    6.94  2.6e-08 ***

# predicting the results in test dataset
Predict_gamegmv_dist <- predict(Game_dist_Model32,test_gameing_dist[,-1])
test_gameing_dist$testgmv <- Predict_gamegmv_dist

# Checking the R Square of the prediction
rgame_dist <- cor(test_gameing_dist$gmv,test_gameing_dist$testgmv)
rgame_dist  #0.974
rsquaregame_dist <- rgame_dist^2
rsquaregame_dist  #0.948

### Cross-validation
cv.lm(data = train_gameing_dist, form.lm = Game_dist_Model32, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)

# Calculate the Elasticity
game_dist_elasticity <- NULL
game_dist_elasticity <- data.frame()
c = 1
for (i in names(Game_dist_Model32$coefficients)[-1]){
  
  i_new <- str_replace_all(i , "`" , "")
  print(i_new)
  elasticity <- as.numeric(Game_dist_Model32$coefficients[-1][i]*mean(train_gameing_dist[,i_new])/mean(train_gameing_dist$gmv))
  print(elasticity)
  game_dist_elasticity[c,1] <- i_new
  game_dist_elasticity[c,2] <- elasticity
  c <- c + 1
}

colnames(game_dist_elasticity)[1] <- "variable"
colnames(game_dist_elasticity)[2] <- "elasticity"

#########################################################################################
##########################################################################################

#Building Distribution Lag Model for Home Audio
#######################################################################################

home_audio_final_dist <- home_audio_final_new

# scalling
home_audio_final_dist[,2:ncol(home_audio_final_dist)] <- scale(home_audio_final_dist[,2:ncol(home_audio_final_dist)])

#Removing column which cannot be scalled
home_audio_final_dist$product_above_20000 <- NULL
# Divide into training and test data set
set.seed(100)

# randomly generate row indices for train dataset
trainindices_home_dist= sample(1:nrow(home_audio_final_dist), 0.8*nrow(home_audio_final_dist))
# generate the train data set
train_home_dist = home_audio_final_dist[trainindices_home_dist,]

#Similarly store the rest of the observations into an object "test".
test_home_dist =home_audio_final_dist[-trainindices_home_dist,]


Home_dist_Model1<-lm(gmv~.,data=train_home_dist)

summary(Home_dist_Model1)

# Perform stepAIC to remove insignificant variables

Home_dist_ModelStepAIC<-stepAIC(Home_dist_Model1,direction = "both")

Home_dist_ModelStepAIC

Home_dist_Model2<-lm(formula = gmv ~ special_sale + premium_product + mass_product + 
                       aspiring_product + product_btwn_5000_10000 + product_btwn_10000_15000 + 
                       sla + product_mrp + product_procurement_sla + promotion_off + 
                       NPS + TV + Digital + Sponsorship + `Content Marketing` + 
                       Affiliates + SEM + Radio + Other + TV.adstock + Digital.adstock + 
                       Sponsorship.adstock + ContentMarketing.adstock + OnlineMarketing.adstock + 
                       Affiliates.adstock + SEM.adstock + Radio.adstock + Other.adstock + 
                       `gmv-1` + list_price_wrt_lag1 + list_price_wrt_ma_2 + list_price_wrt_ma_3 + 
                       list_price_wrt_ma_4 + promotion_off_perc_change_prev_week, 
                     data = train_home_dist)

summary(Home_dist_Model2)
vif(Home_dist_Model2)

#Removing OnlineMarketing.adstock due to high pvalue
Home_dist_Model3<-lm(formula = gmv ~ special_sale + premium_product + mass_product + 
                       aspiring_product + product_btwn_5000_10000 + product_btwn_10000_15000 + 
                       sla + product_mrp + product_procurement_sla + promotion_off + 
                       NPS + TV + Digital + Sponsorship + `Content Marketing` + 
                       Affiliates + SEM + Radio + Other + TV.adstock + Digital.adstock + 
                       Sponsorship.adstock + ContentMarketing.adstock + 
                       Affiliates.adstock + SEM.adstock + Radio.adstock + Other.adstock + 
                       `gmv-1` + list_price_wrt_lag1 + list_price_wrt_ma_2 + list_price_wrt_ma_3 + 
                       list_price_wrt_ma_4 + promotion_off_perc_change_prev_week, 
                     data = train_home_dist)

summary(Home_dist_Model3)
vif(Home_dist_Model3)

#Removing list_price_wrt_ma_2 due to high pvalue
Home_dist_Model4<-lm(formula = gmv ~ special_sale + premium_product + mass_product + 
                       aspiring_product + product_btwn_5000_10000 + product_btwn_10000_15000 + 
                       sla + product_mrp + product_procurement_sla + promotion_off + 
                       NPS + TV + Digital + Sponsorship + `Content Marketing` + 
                       Affiliates + SEM + Radio + Other + TV.adstock + Digital.adstock + 
                       Sponsorship.adstock + ContentMarketing.adstock + 
                       Affiliates.adstock + SEM.adstock + Radio.adstock + Other.adstock + 
                       `gmv-1` + list_price_wrt_lag1 + list_price_wrt_ma_3 + 
                       list_price_wrt_ma_4 + promotion_off_perc_change_prev_week, 
                     data = train_home_dist)

summary(Home_dist_Model4)
vif(Home_dist_Model4)

#Removing sla due to high pvalue
Home_dist_Model5<-lm(formula = gmv ~ special_sale + premium_product + mass_product + 
                       aspiring_product + product_btwn_5000_10000 + product_btwn_10000_15000 + 
                       product_mrp + product_procurement_sla + promotion_off + 
                       NPS + TV + Digital + Sponsorship + `Content Marketing` + 
                       Affiliates + SEM + Radio + Other + TV.adstock + Digital.adstock + 
                       Sponsorship.adstock + ContentMarketing.adstock + 
                       Affiliates.adstock + SEM.adstock + Radio.adstock + Other.adstock + 
                       `gmv-1` + list_price_wrt_lag1 + list_price_wrt_ma_3 + 
                       list_price_wrt_ma_4 + promotion_off_perc_change_prev_week, 
                     data = train_home_dist)

summary(Home_dist_Model5)
vif(Home_dist_Model5)

#Removing list_price_wrt_ma_4 due to high pvalue
Home_dist_Model6<-lm(formula = gmv ~ special_sale + premium_product + mass_product + 
                       aspiring_product + product_btwn_5000_10000 + product_btwn_10000_15000 + 
                       product_mrp + product_procurement_sla + promotion_off + 
                       NPS + TV + Digital + Sponsorship + `Content Marketing` + 
                       Affiliates + SEM + Radio + Other + TV.adstock + Digital.adstock + 
                       Sponsorship.adstock + ContentMarketing.adstock + 
                       Affiliates.adstock + SEM.adstock + Radio.adstock + Other.adstock + 
                       `gmv-1` + list_price_wrt_lag1 + list_price_wrt_ma_3 + promotion_off_perc_change_prev_week, 
                     data = train_home_dist)

summary(Home_dist_Model6)
vif(Home_dist_Model6)

#Removing aspiring_product due to high pvalue
Home_dist_Model7<-lm(formula = gmv ~ special_sale + premium_product + mass_product + 
                       product_btwn_5000_10000 + product_btwn_10000_15000 + 
                       product_mrp + product_procurement_sla + promotion_off + 
                       NPS + TV + Digital + Sponsorship + `Content Marketing` + 
                       Affiliates + SEM + Radio + Other + TV.adstock + Digital.adstock + 
                       Sponsorship.adstock + ContentMarketing.adstock + 
                       Affiliates.adstock + SEM.adstock + Radio.adstock + Other.adstock + 
                       `gmv-1` + list_price_wrt_lag1 + list_price_wrt_ma_3 + promotion_off_perc_change_prev_week, 
                     data = train_home_dist)

summary(Home_dist_Model7)
vif(Home_dist_Model7)

#Removing Affiliates due to high pvalue
Home_dist_Model8<-lm(formula = gmv ~ special_sale + premium_product + mass_product + 
                       product_btwn_5000_10000 + product_btwn_10000_15000 + 
                       product_mrp + product_procurement_sla + promotion_off + 
                       NPS + TV + Digital + Sponsorship + `Content Marketing` + 
                       SEM + Radio + Other + TV.adstock + Digital.adstock + 
                       Sponsorship.adstock + ContentMarketing.adstock + 
                       Affiliates.adstock + SEM.adstock + Radio.adstock + Other.adstock + 
                       `gmv-1` + list_price_wrt_lag1 + list_price_wrt_ma_3 + promotion_off_perc_change_prev_week, 
                     data = train_home_dist)

summary(Home_dist_Model8)
vif(Home_dist_Model8)

#Removing Affiliates.adstock due to high pvalue
Home_dist_Model9<-lm(formula = gmv ~ special_sale + premium_product + mass_product + 
                       product_btwn_5000_10000 + product_btwn_10000_15000 + 
                       product_mrp + product_procurement_sla + promotion_off + 
                       NPS + TV + Digital + Sponsorship + `Content Marketing` + 
                       SEM + Radio + Other + TV.adstock + Digital.adstock + 
                       Sponsorship.adstock + ContentMarketing.adstock + 
                       SEM.adstock + Radio.adstock + Other.adstock + 
                       `gmv-1` + list_price_wrt_lag1 + list_price_wrt_ma_3 + promotion_off_perc_change_prev_week, 
                     data = train_home_dist)

summary(Home_dist_Model9)
vif(Home_dist_Model9)

#Removing list_price_wrt_lag1 due to high pvalue
Home_dist_Model10 <-lm(formula = gmv ~ special_sale + premium_product + mass_product + 
                       product_btwn_5000_10000 + product_btwn_10000_15000 + 
                       product_mrp + product_procurement_sla + promotion_off + 
                       NPS + TV + Digital + Sponsorship + `Content Marketing` + 
                       SEM + Radio + Other + TV.adstock + Digital.adstock + 
                       Sponsorship.adstock + ContentMarketing.adstock + 
                       SEM.adstock + Radio.adstock + Other.adstock + 
                       `gmv-1` + list_price_wrt_ma_3 + promotion_off_perc_change_prev_week, 
                     data = train_home_dist)

summary(Home_dist_Model10)
vif(Home_dist_Model10)

#Removing gmv-1 due to high pvalue
Home_dist_Model11 <-lm(formula = gmv ~ special_sale + premium_product + mass_product + 
                         product_btwn_5000_10000 + product_btwn_10000_15000 + 
                         product_mrp + product_procurement_sla + promotion_off + 
                         NPS + TV + Digital + Sponsorship + `Content Marketing` + 
                         SEM + Radio + Other + TV.adstock + Digital.adstock + 
                         Sponsorship.adstock + ContentMarketing.adstock + 
                         SEM.adstock + Radio.adstock + Other.adstock + 
                         list_price_wrt_ma_3 + promotion_off_perc_change_prev_week, 
                       data = train_home_dist)

summary(Home_dist_Model11)
vif(Home_dist_Model11)

#Removing promotion_off_perc_change_prev_week due to high pvalue
Home_dist_Model12 <-lm(formula = gmv ~ special_sale + premium_product + mass_product + 
                         product_btwn_5000_10000 + product_btwn_10000_15000 + 
                         product_mrp + product_procurement_sla + promotion_off + 
                         NPS + TV + Digital + Sponsorship + `Content Marketing` + 
                         SEM + Radio + Other + TV.adstock + Digital.adstock + 
                         Sponsorship.adstock + ContentMarketing.adstock + 
                         SEM.adstock + Radio.adstock + Other.adstock + 
                         list_price_wrt_ma_3, 
                       data = train_home_dist)

summary(Home_dist_Model12)
vif(Home_dist_Model12)

#Removing product_btwn_10000_15000 due to high pvalue
Home_dist_Model13 <-lm(formula = gmv ~ special_sale + premium_product + mass_product + 
                         product_btwn_5000_10000 + 
                         product_mrp + product_procurement_sla + promotion_off + 
                         NPS + TV + Digital + Sponsorship + `Content Marketing` + 
                         SEM + Radio + Other + TV.adstock + Digital.adstock + 
                         Sponsorship.adstock + ContentMarketing.adstock + 
                         SEM.adstock + Radio.adstock + Other.adstock + 
                         list_price_wrt_ma_3, 
                       data = train_home_dist)

summary(Home_dist_Model13)
vif(Home_dist_Model13)

#Removing TV due to high pvalue
Home_dist_Model14 <-lm(formula = gmv ~ special_sale + premium_product + mass_product + 
                         product_btwn_5000_10000 + 
                         product_mrp + product_procurement_sla + promotion_off + 
                         NPS + Digital + Sponsorship + `Content Marketing` + 
                         SEM + Radio + Other + TV.adstock + Digital.adstock + 
                         Sponsorship.adstock + ContentMarketing.adstock + 
                         SEM.adstock + Radio.adstock + Other.adstock + 
                         list_price_wrt_ma_3, 
                       data = train_home_dist)

summary(Home_dist_Model14)
vif(Home_dist_Model14)

#Removing SEM.adstock due to high pvalue
Home_dist_Model15 <-lm(formula = gmv ~ special_sale + premium_product + mass_product + 
                         product_btwn_5000_10000 + 
                         product_mrp + product_procurement_sla + promotion_off + 
                         NPS + Digital + Sponsorship + `Content Marketing` + 
                         SEM + Radio + Other + TV.adstock + Digital.adstock + 
                         Sponsorship.adstock + ContentMarketing.adstock + 
                         Radio.adstock + Other.adstock + 
                         list_price_wrt_ma_3, 
                       data = train_home_dist)

summary(Home_dist_Model15)
vif(Home_dist_Model15)

#Removing SEM due to high pvalue
Home_dist_Model16 <-lm(formula = gmv ~ special_sale + premium_product + mass_product + 
                         product_btwn_5000_10000 + 
                         product_mrp + product_procurement_sla + promotion_off + 
                         NPS + Digital + Sponsorship + `Content Marketing` + 
                         Radio + Other + TV.adstock + Digital.adstock + 
                         Sponsorship.adstock + ContentMarketing.adstock + 
                         Radio.adstock + Other.adstock + 
                         list_price_wrt_ma_3, 
                       data = train_home_dist)

summary(Home_dist_Model16)
vif(Home_dist_Model16)

#Removing product_btwn_5000_10000 due to high pvalue
Home_dist_Model17 <-lm(formula = gmv ~ special_sale + premium_product + mass_product +
                         product_mrp + product_procurement_sla + promotion_off + 
                         NPS + Digital + Sponsorship + `Content Marketing` + 
                         Radio + Other + TV.adstock + Digital.adstock + 
                         Sponsorship.adstock + ContentMarketing.adstock + 
                         Radio.adstock + Other.adstock + 
                         list_price_wrt_ma_3, 
                       data = train_home_dist)

summary(Home_dist_Model17)
vif(Home_dist_Model17)

#Removing NPS due to high pvalue
Home_dist_Model18 <-lm(formula = gmv ~ special_sale + premium_product + mass_product +
                         product_mrp + product_procurement_sla + promotion_off + 
                         Digital + Sponsorship + `Content Marketing` + 
                         Radio + Other + TV.adstock + Digital.adstock + 
                         Sponsorship.adstock + ContentMarketing.adstock + 
                         Radio.adstock + Other.adstock + 
                         list_price_wrt_ma_3, 
                       data = train_home_dist)

summary(Home_dist_Model18)
vif(Home_dist_Model18)

#Removing TV.adstock due to high pvalue
Home_dist_Model19 <-lm(formula = gmv ~ special_sale + premium_product + mass_product +
                         product_mrp + product_procurement_sla + promotion_off + 
                         Digital + Sponsorship + `Content Marketing` + 
                         Radio + Other + Digital.adstock + 
                         Sponsorship.adstock + ContentMarketing.adstock + 
                         Radio.adstock + Other.adstock + 
                         list_price_wrt_ma_3, 
                       data = train_home_dist)

summary(Home_dist_Model19)
vif(Home_dist_Model19)

#Removing list_price_wrt_ma_3 due to high pvalue
Home_dist_Model20 <-lm(formula = gmv ~ special_sale + premium_product + mass_product +
                         product_mrp + product_procurement_sla + promotion_off + 
                         Digital + Sponsorship + `Content Marketing` + 
                         Radio + Other + Digital.adstock + 
                         Sponsorship.adstock + ContentMarketing.adstock + 
                         Radio.adstock + Other.adstock, 
                       data = train_home_dist)

summary(Home_dist_Model20)
vif(Home_dist_Model20)

#Removing Sponsorship due to high pvalue
Home_dist_Model21 <-lm(formula = gmv ~ special_sale + premium_product + mass_product +
                         product_mrp + product_procurement_sla + promotion_off + 
                         Digital +  `Content Marketing` + 
                         Radio + Other + Digital.adstock + 
                         Sponsorship.adstock + ContentMarketing.adstock + 
                         Radio.adstock + Other.adstock, 
                       data = train_home_dist)

summary(Home_dist_Model21)
vif(Home_dist_Model21)

#Removing Sponsorship.adstock due to high pvalue
Home_dist_Model22 <-lm(formula = gmv ~ special_sale + premium_product + mass_product +
                         product_mrp + product_procurement_sla + promotion_off + 
                         Digital +  `Content Marketing` + 
                         Radio + Other + Digital.adstock + 
                         ContentMarketing.adstock + 
                         Radio.adstock + Other.adstock, 
                       data = train_home_dist)

summary(Home_dist_Model22)
vif(Home_dist_Model22)

#Removing Other due to high pvalue
Home_dist_Model23 <-lm(formula = gmv ~ special_sale + premium_product + mass_product +
                         product_mrp + product_procurement_sla + promotion_off + 
                         Digital +  `Content Marketing` + 
                         Radio + Digital.adstock + 
                         ContentMarketing.adstock + 
                         Radio.adstock + Other.adstock, 
                       data = train_home_dist)

summary(Home_dist_Model23)
vif(Home_dist_Model23)

#Removing special_sale due to high pvalue
Home_dist_Model24 <-lm(formula = gmv ~  premium_product + mass_product +
                         product_mrp + product_procurement_sla + promotion_off + 
                         Digital +  `Content Marketing` + 
                         Radio + Digital.adstock + 
                         ContentMarketing.adstock + 
                         Radio.adstock + Other.adstock, 
                       data = train_home_dist)

summary(Home_dist_Model24)
vif(Home_dist_Model24)

#Removing Digital due to high pvalue
Home_dist_Model25 <-lm(formula = gmv ~  premium_product + mass_product +
                         product_mrp + product_procurement_sla + promotion_off + 
                         `Content Marketing` + 
                         Radio + Digital.adstock + 
                         ContentMarketing.adstock + 
                         Radio.adstock + Other.adstock, 
                       data = train_home_dist)

summary(Home_dist_Model25)
vif(Home_dist_Model25)

#Removing Radio due to high pvalue
Home_dist_Model26 <-lm(formula = gmv ~  premium_product + mass_product +
                         product_mrp + product_procurement_sla + promotion_off + 
                         `Content Marketing` + 
                         Digital.adstock + 
                         ContentMarketing.adstock + 
                         Radio.adstock + Other.adstock, 
                       data = train_home_dist)

summary(Home_dist_Model26)
vif(Home_dist_Model26)

#Removing Content Marketing due to high pvalue
Home_dist_Model27 <-lm(formula = gmv ~  premium_product + mass_product +
                         product_mrp + product_procurement_sla + promotion_off +
                         Digital.adstock + 
                         ContentMarketing.adstock + 
                         Radio.adstock + Other.adstock, 
                       data = train_home_dist)

summary(Home_dist_Model27)
vif(Home_dist_Model27)

#Removing other.adstock due to high vif
Home_dist_Model28 <-lm(formula = gmv ~  premium_product + mass_product +
                         product_mrp + product_procurement_sla + promotion_off +
                         Digital.adstock + 
                         ContentMarketing.adstock + 
                         Radio.adstock, 
                       data = train_home_dist)

summary(Home_dist_Model28)
vif(Home_dist_Model28)

#Removing Radio.adstock due to high vif
Home_dist_Model29 <-lm(formula = gmv ~  premium_product + mass_product +
                         product_mrp + product_procurement_sla + promotion_off +
                         Digital.adstock + 
                         ContentMarketing.adstock, 
                       data = train_home_dist)

summary(Home_dist_Model29)
vif(Home_dist_Model29)

#Removing ContentMarketing.adstock due to high pvalue
Home_dist_Model30 <-lm(formula = gmv ~  premium_product + mass_product +
                         product_mrp + product_procurement_sla + promotion_off +
                         Digital.adstock, 
                       data = train_home_dist)

summary(Home_dist_Model30)
vif(Home_dist_Model30)

#Removing product_procurement_sla due to high vif
Home_dist_Model31 <-lm(formula = gmv ~  premium_product + mass_product +
                         product_mrp +  promotion_off +
                         Digital.adstock, 
                       data = train_home_dist)

summary(Home_dist_Model31)
vif(Home_dist_Model31)

#Removing Digital.adstock due to high vif
Home_dist_Model32 <-lm(formula = gmv ~  premium_product + mass_product +
                         product_mrp +  promotion_off,
                       data = train_home_dist)

summary(Home_dist_Model32)
#Multiple R-squared:  0.995,	Adjusted R-squared:  0.995 
vif(Home_dist_Model32)

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)      5081507      23047  220.48  < 2e-16 ***
#  premium_product   210164      34522    6.09  5.9e-07 ***
#  mass_product     2353789      45231   52.04  < 2e-16 ***
#  product_mrp       453671      47177    9.62  2.3e-11 ***
#  promotion_off    -351277      56820   -6.18  4.4e-07 ***

# predicting the results in test dataset
Predict_homegmv_dist <- predict(Home_dist_Model32,test_home_dist[,-1])
test_home_dist$testgmv <- Predict_homegmv_dist

# Checking the R Square of the prediction
rhome_dist <- cor(test_home_dist$gmv,test_home_dist$testgmv)
rhome_dist  #0.997
rsquarehome_dist <- rhome_dist^2
rsquarehome_dist  #0.994

### Cross-validation
cv.lm(data = train_home_dist, form.lm = Home_dist_Model32, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)

# Calculate the Elasticity
home_dist_elasticity <- NULL
home_dist_elasticity <- data.frame()
c = 1
for (i in names(Home_dist_Model32$coefficients)[-1]){
  
  i_new <- str_replace_all(i , "`" , "")
  print(i_new)
  elasticity <- as.numeric(Home_dist_Model32$coefficients[-1][i]*mean(train_home_dist[,i_new])/mean(train_home_dist$gmv))
  print(elasticity)
  home_dist_elasticity[c,1] <- i_new
  home_dist_elasticity[c,2] <- elasticity
  c <- c + 1
}

colnames(home_dist_elasticity)[1] <- "variable"
colnames(home_dist_elasticity)[2] <- "elasticity"

#########################################################################################

###########################################################################################
# Plotting the Distribution Lag model elasticity
###########################################################################################
#Gaming Accessory
game_dist_elasticity$polarity <- ifelse(game_dist_elasticity$elasticity > 0, "Positive", "Negative")
ggplot(game_dist_elasticity, aes(x=reorder(variable,elasticity),y=elasticity, fill = polarity)) +
  geom_bar(position="dodge",stat="identity", width = 0.9)  + coord_flip() +ggtitle("Distribution Lag Model Elasticity for Gaming Accessory") +xlab("Variables")+theme(axis.text=element_text(size=8),axis.title=element_text(size=10,face="bold"))

#Camera Accessory
cam_dist_elasticity$polarity <- ifelse(cam_dist_elasticity$elasticity > 0, "Positive", "Negative")
ggplot(cam_dist_elasticity, aes(x=reorder(variable,elasticity),y=elasticity, fill = polarity)) +
  geom_bar(position="dodge",stat="identity", width = 0.9)  + coord_flip() +ggtitle("Distribution Lag Model Elasticity for Camera Accessory") +xlab("Variables")+theme(axis.text=element_text(size=8),axis.title=element_text(size=10,face="bold"))

#Home Audio
home_dist_elasticity$polarity <- ifelse(home_kyc_elasticity$elasticity > 0, "Positive", "Negative")
ggplot(home_dist_elasticity, aes(x=reorder(variable,elasticity),y=elasticity, fill = polarity)) +
  geom_bar(position="dodge",stat="identity", width = 0.9)  + coord_flip() +ggtitle("Distribution Lag Model Elasticity for Home Audio") +xlab("Variables")+theme(axis.text=element_text(size=8),axis.title=element_text(size=10,face="bold"))

###########################################################################################################33