#This is the original R-File
library("tidyverse")
library("broom")
library("ggplot2")
library("readxl")
library("dplyr")
library("rstatix")
#Step 1: Open excel file----
df = read_xlsx("ap_original.xlsx")
View(df)

#Step 2: Data Table

#Step 3: Plot
summary(df)
arm_id<-unique(df$arm)

df2<- df
df2$arm<-factor(df2$arm)
df2['arm']<-as.factor(df2['arm'])
options(digits=2)
  #summarise_each(funs(t.test(.[arm == 1], .[arm == 2])$vac_no))
#Comparing Vaccine
vaccine_res<-df2 %>% group_by(arm) %>%
  summarise("Mean vaccincation"= mean(vac_no,trim=.2, na.rm = TRUE),"SD." = format(sd(vac_no,na.rm = T),nsmall=2), "Min" = min(vac_no),"Max" = max(vac_no))
vaccine_res
t.test(df2[df2$arm == 1,]$vac_no , df2[df2$arm == 2,]$vac_no,paired= F)
dd<-df2 %>% select(ID,arm,ddpcr_day0,ddpcr_day5,ddpcr_day10,ddpcr_day0_log,ddpcr_day5_log,ddpcr_day10_log)

#dd<-df2 %>% select(ID,arm,ddpcr_day0,ddpcr_day5,ddpcr_day10)
dd_sub <- dd %>% filter(ddpcr_day0!=999999999 & ddpcr_day5!=999999999 & ddpcr_day10!=999999999)
dt<-dd_sub
#dt<-dd_sub[1:50,]
dt$arm <-factor(dt$arm)
dl <- gather(dt, nth_date, viral_load, ddpcr_day0:ddpcr_day10)

dl2<-dl %>% mutate("samp_date"=if_else(nth_date=='ddpcr_day0',as.Date('2023-01-01'),if_else(nth_date=='ddpcr_day5',as.Date('2023-01-05'),as.Date('2023-01-10'))))
dl2 %>% ggplot(data=.,aes(x=samp_date)) + 
  geom_line(aes(y=viral_load,group=ID,col=arm)) + theme(legend.position = "none") + scale_y_continuous(trans='log10')
dl2 %>% ggplot(data=.,aes(x=samp_date)) + 
  geom_line(aes(y=viral_load,group=ID,col=arm)) + scale_y_continuous(trans='log10')



dd<-df2 %>% select(ID,arm,ddpcr_day0,ddpcr_day5,ddpcr_day10)
dd_sub <- dd %>% filter(ddpcr_day0!=999999999 & ddpcr_day5!=999999999 & ddpcr_day10!=999999999)
dt<-dd_sub
#dt<-dd_sub[1:50,]
dt$arm <-factor(dt$arm)
dl <- gather(dt, nth_date, viral_load, ddpcr_day0:ddpcr_day10)
#Mean only
res<-dt %>% group_by(arm) %>% summarise("Mean viral load Day 0"=format(mean(log10(ddpcr_day0)),big.mark=",",nsmall=2),"Mean viral load Day 5"=format(mean(log10(ddpcr_day5)),big.mark=",",nsmall=2),"Mean viral load day 10"=format(mean(log10(ddpcr_day10)),big.mark=",",nsmall=2))
res
#Difference between D0 to D5
dt_gap = dt %>% mutate("gap_d0_d5"=(ddpcr_day0-ddpcr_day5),"gap_d0_d10"=(ddpcr_day0-ddpcr_day10))
dt_gap[dt_gap$gap_d0_d5<=0,]$gap_d0_d5=1
dt_gap[dt_gap$gap_d0_d10<=0,]$gap_d0_d10=1

res_gap<-dt_gap %>% group_by(arm) %>% summarise("Mean difference viral load Day 0-5"=format(mean(log10(gap_d0_d5)),big.mark=",",nsmall=2),"Mean difference viral load Day 0-10"=format(mean(log10(gap_d0_d10)),big.mark=",",nsmall=2))
res_gap

#Differ ratio
Threshold_level=0.35
dt_ratio =  dt %>% mutate("ratio_d0_d5"=(ddpcr_day5/ddpcr_day0),"ratio_d0_d10"=(ddpcr_day10/ddpcr_day0)) %>% 
            mutate("day5_reduction_success"=if_else(ratio_d0_d5<=Threshold_level,1,0),"day10_reduction_success"=if_else(ratio_d0_d10<=Threshold_level,1,0)) %>%
            mutate("day5_eradicate_success"=if_else(ddpcr_day5<100,1,0),"day10_eradicate_success"=if_else(ddpcr_day10<100,1,0))
#Descriptive vial replication ratio
res_ratio<-dt_ratio %>% group_by(arm) %>% summarise("Mean ratio viral load Day 0-5"=format(mean(ratio_d0_d5),big.mark=",",nsmall=2),"Mean ratio viral load Day 0-10"=format(mean(ratio_d0_d10),big.mark=",",nsmall=2))#,"Mean ratio viral load Day 0-10"=format(mean(ratio_d0_d10),big.mark=",",nsmall=2))
res_ratio
#Percentage of vial replication equal or less than 35%
res_threshold<-dt_ratio %>% group_by(arm) %>% summarise("Percent 35% viral load reduction in Day 5"=format(mean(day5_reduction_success),big.mark=",",nsmall=2),"Percent 35% viral load reduction in Day 10"=format(mean(day10_reduction_success),big.mark=",",nsmall=2))
res_threshold
#Eradicate VL<100
res_eradicate<-dt_ratio %>% group_by(arm) %>% summarise("Percent eradicate in Day 5"=format(mean(day5_eradicate_success),big.mark=",",nsmall=2),"Percent eradicate in Day 10"=format(mean(day10_eradicate_success),big.mark=",",nsmall=2))
res_eradicate
#T.Test
t.test(dt_ratio[dt_ratio$arm == 1,]$day5_reduction_success , dt_ratio[dt_ratio$arm == 2,]$day5_reduction_success,paired= F)
t.test(dt_ratio[dt_ratio$arm == 1,]$day10_reduction_success , dt_ratio[dt_ratio$arm == 2,]$day10_reduction_success,paired= F)

t.test(dt_ratio[dt_ratio$arm == 1,]$day5_eradicate_success , dt_ratio[dt_ratio$arm == 2,]$day5_eradicate_success,paired= F)
t.test(dt_ratio[dt_ratio$arm == 1,]$day10_eradicate_success , dt_ratio[dt_ratio$arm == 2,]$day10_eradicate_success,paired= F)

#Difference between D0 to D10


#T.test in starting point
t.test(log10(dt[dt$arm == 1,]$ddpcr_day0), log10(dt[dt$arm == 2,]$ddpcr_day0),paired= F)
#Median only
dt %>% group_by(arm) %>% summarise("Median viral load Day 0"=format(median(ddpcr_day0),big.mark=",",nsmall=2),"Median viral load Day 5"=format(median(ddpcr_day5),big.mark=",",nsmall=2),"Median viral load day 10"=format(median(ddpcr_day10),big.mark=",",nsmall=2))
#Mean and SD only
dt %>% group_by(arm) %>% summarise("Mean viral load Day 0"=format(mean(ddpcr_day0),big.mark=",",nsmall=2),"SD. Day 0"=format(sd(ddpcr_day0),big.mark=",",nsmall=2),"Mean viral load Day 5"=format(mean(ddpcr_day5),big.mark=",",nsmall=2),"SD. Day 5"=format(sd(ddpcr_day5),big.mark=",",nsmall=2),"Mean viral load day 10"=format(mean(ddpcr_day10),big.mark=",",nsmall=2),"SD. Day 10"=format(sd(ddpcr_day10),big.mark=",",nsmall=2))

hist(dt$ddpcr_day10)
dt %>% ggplot(data=., aes(ddpcr_day0)) +               # Histogram without logarithmic axis
  geom_histogram(bins =10)+scale_x_log10()


#Split Histogram 
dt %>% ggplot(data=., aes(ddpcr_day0)) +               # Histogram without logarithmic axis
  geom_histogram(fill = "white", colour = "black",bins = 10) +
  facet_grid(arm ~ .)+scale_x_log10()

plt_ddpcr_log_0<-dt %>% ggplot(data=., aes(ddpcr_day0,fill=arm)) +               # Histogram with logarithmic axis
  geom_histogram(position = "identity", alpha = 0.4,bins =10)+scale_x_log10()

plt_ddpcr_log_5<-dt %>% ggplot(data=., aes(ddpcr_day5,fill=arm)) +               # Histogram with logarithmic axis
  geom_histogram(bins =10)+scale_x_log10()

plt_ddpcr_log_10<-dt %>% ggplot(data=., aes(ddpcr_day10,fill=arm)) +               # Histogram with logarithmic axis
  geom_histogram(bins =10)+scale_x_log10()
#Kolmogorov-Smirnov Test and Shapiro-Wilk Test on Normal Data 
log10_of_arm1_day0<-log10(dt[dt$arm==1,]$ddpcr_day0)
log10_of_arm2_day0<-log10(dt[dt$arm==2,]$ddpcr_day0)
log10_of_arm12_day0<-log10(dt$ddpcr_day0)
shapiro.test(log10_of_arm1_day0)
shapiro.test(log10_of_arm2_day0)
shapiro.test(log10_of_arm12_day0)
#This is normal distribution data

#Graph viral load and day after admission
dl2<-dl %>% mutate("Dates_after_admission"=if_else(nth_date=='ddpcr_day0',0,if_else(nth_date=='ddpcr_day5',5,10)))
dl2 %>% ggplot(data=.,aes(x=Dates_after_admission)) + 
  geom_line(aes(y=viral_load,group=ID,col=arm)) + theme(legend.position = "none") + scale_y_continuous(trans='log10')
mean <- dl2 %>% group_by(arm,Dates_after_admission) %>% summarise(mean_val=(10^mean(log10(viral_load))))

#tgc <- summarySE(dl2, measurevar="viral_load", groupvars=c("arm","nth_date"))
dl2 %>% ggplot(data=.,aes(x=Dates_after_admission)) + 
  geom_line(aes(y=viral_load,group=ID,col=arm),linetype = "twodash") + 
  geom_line(data=mean,aes(y=mean_val,col=arm),size=2)+
  scale_y_continuous(trans='log10')

#+ geom_errorbar(aes(ymin=viral_load-se, ymax=viral_load+se), width=.1) 




dl$nth_date[dl$nth_date=='ddpcr_day0']<-as.Date('2023-01-01')
dl$nth_date[dl$nth_date=='ddpcr_day5']<-as.Date('2023-01-05')
dl$nth_date[dl$nth_date=='ddpcr_day10']<-as.Date('2023-01-10')
#dl$nth_date<-as.numeric(dl$nth_date)
dl$nth_date<-as.Date(dl$nth_date)
dl
data_long
 dd %>%
  ggplot(.,aes(y=ddpcr_day0))+geom_point(size=2, shape=23)

df2 %>% group_by(arm) %>% summarize("t"=t.test(vac_no , arm))
df2 %>% select(vac_no,arm) %>% group_by(arm) %>% summarize_all(funs(mean, sd))
df2 %>% select(vac_no,arm) %>% group_by(arm) %>% summarize_all(list(Mean= mean(., trim = .2)))

#tmp<-df2 %>% select("vac_no","arm") %>% group_by("arm") %>% tally()
tmp
df2 %>% select("vac_no","arm")  %>% head()
tmp<-df2 %>% group_by("vac_no","arm") %>% tally() %>% spread("arm",n)
tmp
# Basic piechart
ggplot(data, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)