library(data.table)
#Read in Final Attributes:
dat <- read.csv("attributes_edited.final.csv", header= TRUE)
dat$Start.Up.Date <- as.Date(dat$Start.Up.Date, "%d/%m/%Y")
dat$Start_Date <- dat$Start.Up.Date
dat$Start.Up.Date = NULL

#str(dat)

##CHANGING DATES TO READ from 1900's rather 2000s before 1970, as currently as.Date assumes 1955 is actually 2055. 
#library(dplyr) 
#dat$Start_Date<-(dat$Start.Up.Date %>% as.Date(format="%Y-%m-%d") %>% 
                #   format("%y%m%d") %>%
                 #  (function(d){
                 #    paste0(ifelse(d>171231,"19","20"),d)
               #    }) %>% 
               # #   as.Date("%Y%m%d"))
#str(dat)

#Removing original date column from dataset
#dat$Start.Up.Date = NULL
#all static attributes to attach to pump stations in work order. 
dat_attributes <- data.frame(dat)

str(dat_attributes)

#JOINING WORK ORDER 
#Work Order Dateset:
#Compiling Logistic regression Dataset
work_order <- read.csv("final_POISSION_workorders.csv", header=TRUE)
str(work_order)
work_order$Month <- as.Date(work_order$Month, format="%d/%m/%Y")
str(work_order)


##JOINING WORK ORDERS TO STATIC ATTRIBUTES:
test1 <- left_join(work_order, dat_attributes, by = c('FL'))



dat <- as.data.frame(test1)
str(dat)
saveRDS(dat, file = "poisson_fault_dataset.rds")
write.csv(dat, file= "poisson_fault_dataset.csv")


#######creating grid of dates to join work orders to
#DATAFRAME OF ALL MONTHS between 2006-2017
#1 is used as placeholder for day as it date functioned needed a value for day
tmp2 <- expand.grid(day = "1", month=c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"), year=c("2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017"))
tmp2 <- paste(tmp2[,1], tmp2[,2],tmp2[,3], sep="/")
tmp2 <- as.Date(tmp2,format="%d/%m/%Y")

str(dat)

res <- NULL
for(name in unique(dat$FL)){
  tmp <- subset(dat, subset= FL==name)

  tmp4 <- data.frame(Month = tmp2[ tmp2 >= tmp$Start_Date[1]])
  
  tmp3 <- merge(tmp, tmp4, all=TRUE, sort=FALSE)
  tmp3 <- within(tmp3, {
    FL <- unique(FL[!is.na(FL)])
    Name <- unique(Name[!is.na(Name)])
    Superior.FL <- unique(Superior.FL[!is.na(Superior.FL)])
    Pump.station.Group <- unique(Pump.station.Group[!is.na(Pump.station.Group)])
    Suburb.Name <- unique(Suburb.Name[!is.na(Suburb.Name)])
    Installation.type.design <- unique(Installation.type.design[!is.na(Installation.type.design)])
    Pressure.Main.Diameter..mm. <- unique(Pressure.Main.Diameter..mm.[!is.na(Pressure.Main.Diameter..mm.)])
    Pressure.Main.Material <- unique(Pressure.Main.Material[!is.na(Pressure.Main.Material)])
    Pump.Brand <- unique(Pump.Brand[!is.na(Pump.Brand)])
    Pump_Type<- unique(Pump_Type[!is.na(Pump_Type)])
    Motor_Make.y<- unique(Motor_Make.y[!is.na(Motor_Make.y)])
    Motor_Rating_kw<- unique(Motor_Rating_kw[!is.na(Motor_Rating_kw)])
    No..of.Motor.Poles<- unique(No..of.Motor.Poles[!is.na(No..of.Motor.Poles)])
    Pump.Impeller.TYPE<- unique(Pump.Impeller.TYPE[!is.na(Pump.Impeller.TYPE)])
    Pump_Impeller.Model<- unique(Pump_Impeller.Model[!is.na(Pump_Impeller.Model)])
    Impeller_Throughlet<- unique(Impeller_Throughlet[!is.na(Impeller_Throughlet)])
    Impeller_Diameter<- unique(Impeller_Diameter[!is.na(Impeller_Diameter)])
    Design.Duty.FLOWRATE<- unique(Design.Duty.FLOWRATE[!is.na(Design.Duty.FLOWRATE)])
    Design_Duty_MHW_HEAD<- unique(Design_Duty_MHW_HEAD[!is.na(Design_Duty_MHW_HEAD)])
    Design.Duty.RPM.pumpspeed<- unique(Design.Duty.RPM.pumpspeed[!is.na(Design.Duty.RPM.pumpspeed)])
    Start_Date<- unique(Start_Date[!is.na(Start_Date)])
    start.up.YEAR<- unique(start.up.YEAR[!is.na(start.up.YEAR)])
    Motor_Rating_kw_LEVELS<- unique(Motor_Rating_kw_LEVELS[!is.na(Motor_Rating_kw_LEVELS)])
    start_up_decade<- unique(start_up_decade[!is.na(start_up_decade)])
    Failure_total_cohort<- unique(Failure_total_cohort[!is.na(Failure_total_cohort)])
    
  })
  
  res <- rbind(res, tmp3)
}


###  #Replace zeros with NA's
res[c("Count.of.Obstruction.Event.Occurred")][is.na(res[c("Count.of.Obstruction.Event.Occurred")])] <- 0
###  tmp3[c("Cost..")][is.na(tmp3[c("Cost..")])] <- 0

work_order_expanded <- data.frame(res)
str(work_order_expanded)

work_order_expanded$X12_year_total = NULL

saveRDS(work_order_expanded, file = "complete_poisson_dataset.rds")
write.csv(work_order_expanded, file= "complete_poisson_dataset.csv")

dat <-read.csv("complete_poisson_dataset.csv")
str(dat)

ggplot(dat, aes(as.numeric(dat$Count.of.Obstruction.Event.Occurred))) + geom_histogram(bins=50) +theme_minimal()+xlab("Count of total Monthly Faults")+scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24))


