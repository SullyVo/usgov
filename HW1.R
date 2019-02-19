#1.1
setwd("/Users/sullivo/Documents/STA141C")
zip_file_path = "/Users/sullivo/Documents/STA141C/awards.zip"
files = unzip(zip_file_path,list = TRUE)
my_files = list.files("/Users/sullivo/Documents/STA141C/", pattern = '*.csv')
library(data.table)
readdata <- function(fn){
  dt_temp <- fread(fn, sep=",",fill = FALSE , 
                   select = c("total_obligation", "period_of_performance_start_date",'funding_agency_id'))
  dt_temp$date = as.Date(dt_temp$period_of_performance_start_date, format = "%Y-%m-%d")
  dt_temp$year = format(dt_temp$date, "%Y")
  dt_temp <- dt_temp[,c("total_obligation", "year",'funding_agency_id' )]
  dt_temp$sum_agency = tapply(dt_temp$total_obligation, dt_temp$year,sum)
  dt_temp$median_agency = median(dt_temp$sum_agency)
  dt_temp <- dt_temp[,c( "funding_agency_id",'median_agency' )][1]
  return(dt_temp)
}
mylist <- lapply(my_files, readdata)#lapply the function into the file
mydata <- rbindlist( mylist )#combine list together
mydata = mydata[-1]#remove the 0.csv
max(na.omit(mydata$median_agency))#find the highest median annual spending
mydata$funding_agency_id[which.max(mydata$median_agency)]#find the ID that has highest median of annual speanding
#1.2: plot the histogram
library(ggplot2)
ggplot(data = mydata, mapping = aes(x = mydata$median_agency,na.rm = TRUE)) +
  geom_histogram(color = "white", bins = 20)+
  ggtitle("Plot of Distribution of median annual spending") +
  xlab("Median based on agency") 
#1.3plot the histogram of log med 
ggplot(data = mydata, mapping = aes(x = log(mydata$median_agency),na.rm = TRUE)) +
  geom_histogram(color = "white", bins = 20)+
  ggtitle("Distribution of logarithm median annual spending") +
  xlab("Logarithm of median based on agency")

#1.4plot the scatter plot 
a1<-ggplot(data = mydata, mapping = aes(x= mydata$funding_agency_id,y = mydata$median_agency,
                                    na.rm = TRUE)) +
  geom_point() +
  ggtitle("Scattered plot of Distribution of median annual spending ") +
  xlab("Agency ID")+ylab('Median based on agency')

a2<-ggplot(data = mydata, mapping = aes(x= mydata$funding_agency_id,y = log(mydata$median_agency),
                                    na.rm = TRUE)) +
  geom_point() +
  ggtitle("Scattered plot Distribution of logarithm median annual spending") +
  xlab("Agency ID")+ylab('Log of median based on agency')
install.packages("gridExtra")
library(gridExtra)
grid.arrange(a1,a2)
#2.1 plot the dist of file size
mydata$size= file.size(my_files[-1])
ggplot(data = mydata, mapping = aes(x = mydata$size,na.rm = TRUE)) +
  geom_histogram(color = "white", bins = 30)+
  ggtitle("Plot of Distribution of Distribution of file size") +
  xlab("Files size based on each agency") +xlim(c(0, 500000))
head(file.info(dir()[-1], extra_cols = FALSE))
summary(mydata)
#2.2
mydata$size[1:5]#get size of 1st 5 csv files
file_r = my_files[2:6]#name of the first 5 files
unlist(lapply(file_r, function(m) df <- nrow(data.table::fread(m))))#num of rows of 1st 5 files
#2.3
system.time(source('HW1.R'))#to run the time elapse of the file

