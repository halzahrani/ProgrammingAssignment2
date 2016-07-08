# Plesae start from step 5
#---------------utils::View()-----------
# Step 0: Load data 
#--------------------------
setwd("/Users/hassan/Hassan_Data/Universities/Towson/Datamining/Baltmore Police 911 Calls")
data = read.csv("calls_service.csv", header=T,  fill=F, stringsAsFactors=F)
saveRDS(data, file = "calls_service.rds")
rm(data)

## We don't need any more to run the previous lines
##-------------------------------------------------------
setwd("/Users/hassan/Hassan Data/Universities/Towson/Datamining/Baltmore Police 911 Calls")
data0 = readRDS("calls_service.rds")
View(data0)
# Step 3 : data manipulation
#----------------------------------

# step 3-1
#----------
names(data0) = tolower(names(data0))
data0 = plyr::rename(data0, c("calldatetime"="datetime", "incidentlocation"="address"))
data0$datetime = strptime(data0$datetime, format="%m/%d/%Y %I:%M:%S %p")

# step 3-2
#----------
data0$priority = gsub("[[:space:]]", "", toupper(data0$priority))
data0$district = gsub("[[:space:]]", "", toupper(data0$district))
data0$description = toupper(data0$description)
data0$address = toupper(data0$address)
data0$callnumber = as.numeric(paste(data0$callnumber))

# step 3-3
#----------
location = strsplit(gsub("\\(|\\)", "", data0$location), ",")
location = t(as.data.frame(location))
rownames(location) = c(1:nrow(data0))
data0$longitude = as.numeric(paste(location[,1]))
data0$latitude = as.numeric(paste0(location[,2]))

# Step 4 
#----------
data = subset(data0, select=-c(callnumber, location))
saveRDS(data, "calls_service_clean.rds")
write.csv(data0, "/Users/hassan/Hassan Data/Universities/Towson/Datamining/Baltmore Police 911 Calls/cleandatav0.csv")

# Step 5 (start from here)
#-----------------------------
setwd("/Users/hassan/Hassan Data/Universities/Towson/Datamining/Baltmore Police 911 Calls")
library(plyr)
library(dplyr)
library(zoo)
library(xts)
library(lubridate)
library(ggplot2)
library(easyGgplot2)
data = readRDS("calls_service_clean.rds")
data = data[order(data$datetime), ]
data$hour = hour(data$datetime)
data$day = weekdays(data$datetime, abbreviate=T)
data$month = months(data$datetime, abbreviate=T)
data$day = factor(data$day, 
                  levels=c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"), 
                  labels=c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"))
data$month = factor(data$month, 
             levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
             labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
write.csv(data0, "/Users/hassan/Hassan Data/Universities/Towson/Datamining/Baltmore Police 911 Calls/cleandatav1.csv")

# Step 6
#----------
tempo1 = subset(data, select = -datetime)
tempo2 = subset(tempo1, !is.na(priority))
tempo2 = subset(tempo2, 
         (priority %in% c("EMERGENCY","HIGH","LOW","MEDIUM","NON-EMERGENCY","OUTOFSERVICE")))

priority0 = ddply(tempo2, c("priority","hour"), summarise, Freq=length(hour))
priority1 = ddply(tempo2, c("priority", "day"), summarise, Freq=length(day))
priority2 = ddply(tempo2, c("priority","month"), summarise, Freq=length(month))

tempo1 = subset(tempo1, !is.na(district))
district0 = ddply(tempo1, c("district", "hour"), summarise, Freq=length(hour))
district1 = ddply(tempo1, c("district", "day"), summarise, Freq=length(day))
district2 = ddply(tempo1, c("district","month"), summarise, Freq=length(month))

tempo3 = subset(data, select = -datetime)
hour  = ddply(tempo3, c("hour"), summarise, Freq=length(hour))
day   = ddply(tempo3, c("day"), summarise, Freq=length(day))
month = ddply(tempo3, c("month"), summarise, Freq=length(month))
write.csv(data0, "/Users/hassan/Hassan Data/Universities/Towson/Datamining/Baltmore Police 911 Calls/cleandatav2.csv")
# Step 7
#----------

p1 = ggplot2.lineplot(hour, xName="hour", yName="Freq", addPoint=T, pointShape=21, 
                      pointFill="white", pointSize=2, colour="dodgerblue4") + 
  xlab("Hours (0 is midnight)") + ylab("Nb of calls") +
  scale_colour_hue(name="Priority", l=40) + theme_light() +
  theme(axis.text.x=element_text(size=7), text=element_text(size=9))

p2 = ggplot2.lineplot(day, xName="day", yName="Freq", addPoint=T, pointShape=21,
                      pointFill="white", pointSize=2, colour="dodgerblue4") +
  xlab("Days") + ylab("Nb of calls") + scale_colour_hue(name="Priority", l=40) + theme_light() +
  theme(axis.text.x=element_text(size=7), text=element_text(size=9))

ggplot2.multiplot(p1, p2, cols=2)

p1=ggplot(priority0, aes(x=hour, y=Freq, colour=priority, group=priority)) + 
  geom_line(position="identity", size=1) +  
  geom_point(position="identity", size=2, shape=21, fill="white") +
  xlab("Hours (0 is midnight)") + ylab("Nb of calls") + ggtitle("Priority by hour") +
  scale_colour_hue(name="Priority", l=40) + theme_light() + 
  theme(axis.text.x=element_text(size=7), text=element_text(size=9), legend.position="none")

p2=ggplot(priority1, aes(x=day, y=Freq, colour=priority, group=priority)) + 
  geom_line(position="identity", size=1) +  
  geom_point(position="identity", size=2, shape=21, fill="white") +
  xlab("Days") + ylab("Nb of calls") + ggtitle("Priority by day") +
  scale_colour_hue(name="Priority", l=40) + theme_light() +          
  theme(axis.text.x=element_text(size=9), text=element_text(size=10))       

ggplot2.multiplot(p1, p2, cols=2)

p1=ggplot(district0, aes(x=hour, y=Freq, colour=district, group=district)) + 
  geom_line(position="identity", size=1) +  
  geom_point(position="identity", size=2, shape=21, fill="white") +
  xlab("Hours (0 is midnight)") + ylab("Nb of calls") + ggtitle("District by hour") +
  scale_colour_hue(name="District", l=40) + theme_light() + 
  theme(axis.text.x=element_text(size=7), text=element_text(size=9), legend.position="none")

p2=ggplot(district1, aes(x=day, y=Freq, colour=district, group=district)) + 
  geom_line(position="dodge", size=1) + 
  geom_point(position="dodge", size=2, shape=21, fill="white") +
  xlab("Days") + ylab("Nb of calls") + ggtitle("District by day") +
  scale_colour_hue(name="District", l=40) + theme_light() +            
  theme(axis.text.x=element_text(size=8), text=element_text(size=9))  

ggplot2.multiplot(p1, p2, cols=2)


# Step 8: aggregate data by priority
#-------------------------------------
aggreg = function(data, col, type, date) {
  dt = subset(data, (data[,col] %in% type))
  dt = aggregate(dt[ ,"datetime"], 
                 list(datetime.h=cut(as.POSIXct(dt[,"datetime"])-1, date)), length)
  dt = plyr::rename(dt, c("x"="Nb.calls"))
  if(date=="week") dt$datetime.h = as_date(ymd_hms(dt$datetime.h))
  else 
    dt$datetime.h = ymd(dt$datetime.h)
  return(dt)
}
date = "month" # or "week" or "day"
col = "priority"
emergency = aggreg(data=data, col=col, type="EMERGENCY", date=date)
high = aggreg(data=data, col=col, type="HIGH", date=date)
medium = aggreg(data=data, col=col, type="MEDIUM", date=date)
low = aggreg(data=data, col=col, type="LOW", date=date)
no.emergency = aggreg(data=data, col=col, type="NON-EMERGENCY", date=date)
out.service = aggreg(data=data, col=col, type="OUTOFSERVICE", date=date)

prio = join(x=emergency, y=high, by="datetime.h", type="right", match="first")
names(prio)[2:3] = c("emergency", "high")
prio = join(x=prio, y=medium, by="datetime.h", type="left", match="first")
names(prio)[4] = "medium"
prio = join(x=prio, y=low, by="datetime.h", type="left", match="first")
names(prio)[5] = "low"
prio = join(x=prio, y=no.emergency, by="datetime.h", type="left", match="first")
names(prio)[6] = "no.emergency"
prio = join(x=prio, y=out.service, by="datetime.h", type="left", match="first")
names(prio)[7] = "out.service"
prio$total = rowSums(prio[,-1])
prio = na.omit(prio)
prio = xts(prio, order.by=prio$datetime.h)[,-1]


plot.zoo(prio[,-7], screens=1, col=1:6, ylim=c(0,60000), xaxt="n", 
         lwd=2, pch=20, type="b", ylab="Nb calls", xlab="Months period 2015-2016")
t = time(prio)   
k = seq(1, length(t), 1) 
#k = seq(1, length(t), 4) 
labs = format(t, "%b")
#labs = format(t, "%V")
axis(1, side=1, at=t[k], labels=labs[k], cex=0.8)
legend("topright", toupper(colnames(prio[,-7])), pch=20, col=1:6, 
       lwd=3, lty=1, cex=0.75, ncol = 3, bty="n") 


plot.zoo(prio[,7], screens=1, ylim=c(30000,110000), xaxt="n", lwd=1, pch=21, 
         type="b", bg=9, ylab="Nb calls", xlab="Months period 2015-2016")
axis(1, side=1, at=t[k], labels=labs[k], cex=0.8)

#-------------------------------------------------------------------------------------------

# Step 9:
#-----------
library(ReporteRs)
tab1 = as.data.frame(table(tempo2$priority))
colnames(tab1) = c("Priority","Frequency")
tab1$Percent = round(100*(tab1$Frequency/sum(tab1$Frequency)),2)
tab1 = tab1[rev(order(tab1[,2])), ]

tab2 = as.data.frame(table(tempo2$district))
colnames(tab2) = c("District","Frequency")
tab2$Percent = round(100*(tab2$Frequency/sum(tab2$Frequency)),2)
tab2 = tab2[rev(order(tab2[,2])), ]

tables = docx( )
tables = addFlexTable(tables, FlexTable(tab1))
tables = addSection(tables)
tables = addFlexTable(tables, FlexTable(tab2))
writeDoc(tables, file = "tables.docx")



#-------------------

















