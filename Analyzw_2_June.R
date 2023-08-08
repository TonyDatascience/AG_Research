#Prepare Data
#This is the original R-File
library("tidyverse")
library("broom")
library("ggplot2")
library("readxl")
library("dplyr")
library("rstatix")
library("knitr")


#Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#Step 0: Reading file----

df0 = read_xlsx("ap_original_25_May_2023_Run.xlsx")
df<-df0
df2<- df
#df2$arm<-factor(df2$arm)
#df2['arm']<-as.factor(df2['arm'])
options(digits=2)
#Step 1: Table descriptive----
#..1.1 Age----
AgeIQR<-df %>% group_by(arm) %>% get_summary_stats(age, type = "median_iqr")
w<-wilcox.test(df[df['arm']==1,]$age,df[df['arm']==2,]$age,paired=FALSE)
AgeIQR$pValue= c(w$p.value,w$method)
View(AgeIQR)
#..-1.2 Sex----
GenderTable<-data.frame(Arm1=c(sum(df[df['arm']==1,]$gender==1),sum(df[df['arm']==1,]$gender==2)),Arm2=c(sum(df[df['arm']==2,]$gender==1),sum(df[df['arm']==2,]$gender==2)))
ChiTest<-chisq.test(GenderTable)
GenderTable$pValue=c(ChiTest$p.value,ChiTest$method)
row.names(GenderTable)<-c("Male","Female")
View(GenderTable)
#..1.3 BMI----
BMI<-df %>% group_by(arm) %>% get_summary_stats(bmi, type = "mean_sd")
tBMI<-t.test(df[df['arm']==1,]$bmi,df[df['arm']==2,]$bmi,paired=FALSE)
BMI$pValue<-c(tBMI$p.value,tBMI$method)
View(BMI)
#..1.4 Underlying disease----
rxallergyTable<-data.frame(Arm1=c(sum(df[df['arm']==1,]$underly==1),sum(df[df['arm']==1,]$underly==2)),Arm2=c(sum(df[df['arm']==2,]$underly==1),sum(df[df['arm']==2,]$underly==2)))
Chirxallergy<-chisq.test(rxallergyTable)
rxallergyTable$pValue=c(Chirxallergy$p.value,Chirxallergy$method)
row.names(rxallergyTable)<-c("No underlying disease","Underlying disease presented")
View(rxallergyTable)
#..1.5 Smoke----
#....1.5.1 Recode to smoke or not----
df_recode<-df %>% mutate(smoke_bit = if_else(((smoke==2) |(smoke==3)),2,1 ))
df_recode$smoke_bit
#Comment: 2 = Smoke, 1 = not smoke or stopped
smoke_bitTable<-data.frame(Arm1=c(sum(df_recode[df_recode['arm']==1,]$smoke_bit==1),sum(df_recode[df_recode['arm']==1,]$smoke_bit==2)),Arm2=c(sum(df_recode[df_recode['arm']==2,]$smoke_bit==1),sum(df_recode[df_recode['arm']==2,]$smoke_bit==2)))
Chismoke_bit<-chisq.test(smoke_bitTable)
smoke_bitTable$pValue=c(Chismoke_bit$p.value,Chismoke_bit$method)
row.names(smoke_bitTable)<-c("No smoke","Smoke")
View(smoke_bitTable)
#..1.6 Alcohol----
#....1.6.1 Drink or not----
df_recode<-df_recode %>% mutate(alc_bit = if_else(((alc==2) |(alc==3)),2,1 ))
alc_bitTable<-data.frame(Arm1=c(sum(df_recode[df_recode['arm']==1,]$alc_bit==1),sum(df_recode[df_recode['arm']==1,]$alc_bit==2)),Arm2=c(sum(df_recode[df_recode['arm']==2,]$alc_bit==1),sum(df_recode[df_recode['arm']==2,]$alc_bit==2)))
Chialc_bit<-chisq.test(alc_bitTable)
alc_bitTable$pValue=c(Chialc_bit$p.value,Chialc_bit$method)
row.names(alc_bitTable)<-c("No alcohol drinking","Alcohol drinking")
View(alc_bitTable)
#..1.7 Drug allergy----
rxallergyTable<-data.frame(Arm1=c(sum(df[df['arm']==1,]$rxallergy==1),sum(df[df['arm']==1,]$rxallergy==2)),Arm2=c(sum(df[df['arm']==2,]$rxallergy==1),sum(df[df['arm']==2,]$rxallergy==2)))
Chirxallergy<-chisq.test(rxallergyTable)
rxallergyTable$pValue=c(Chirxallergy$p.value,Chirxallergy$method)
row.names(rxallergyTable)<-c("No history of drug allergy","History of drug allergy")
View(rxallergyTable)
#..1.8 Low dose AG----
apbeforetrTable<-data.frame(Arm1=c(sum(df[df['arm']==1,]$apbeforetr==1),sum(df[df['arm']==1,]$apbeforetr==2)),Arm2=c(sum(df[df['arm']==2,]$apbeforetr==1),sum(df[df['arm']==2,]$apbeforetr==2)))
Chiapbeforetr<-chisq.test(apbeforetrTable)
apbeforetrTable$pValue=c(Chiapbeforetr$p.value,Chiapbeforetr$method)
row.names(apbeforetrTable)<-c("No andrographeride usage before","Low dose andrographeride usage before")
View(apbeforetrTable)
#..1.9 Herb----
herbalTable<-data.frame(Arm1=c(sum(df[df['arm']==1,]$herbal==1),sum(df[df['arm']==1,]$herbal==2)),Arm2=c(sum(df[df['arm']==2,]$herbal==1),sum(df[df['arm']==2,]$herbal==2)))
Chiherbal<-chisq.test(herbalTable)
herbalTable$pValue=c(Chiherbal$p.value,Chiherbal$method)
row.names(herbalTable)<-c("No herbal usage before","Other herbal usage presented")
View(herbalTable)
#..1.10 Vaccine Table----
#....Vaccine as continuing data----
vaccine_stat<-df %>% group_by(arm) %>% get_summary_stats(vac_no, type = "mean_sd")
tVaccine<-t.test(df[df['arm']==1,]$vac_no,df[df['arm']==2,]$vac_no,paired=FALSE)
vaccine_stat$pValue<-c(tVaccine$p.value,tVaccine$method)
View(vaccine_stat)
#vac_no_freq_table<-df %>% group_by(arm) %>% count(vac_no)
#View(vac_no_freq_table)
#vaccine_res<-df %>% group_by(arm) %>%
#summarise("Mean vaccincation"= mean(vac_no,trim=.2, na.rm = TRUE),"SD." = format(sd(vac_no,na.rm = T),nsmall=2), "Min" = min(vac_no),"Max" = max(vac_no))
#View(vaccine_res)
#....Vaccine as rank data----
df %>% group_by(arm) %>% summarise("Mean vaccincation"= mean(vac_no,trim=.2, na.rm = TRUE),"SD." = format(sd(vac_no,na.rm = T),nsmall=2), "Min" = min(vac_no),"Max" = max(vac_no))
#
VaccineIQR<-df %>% group_by(arm) %>% get_summary_stats(vac_no, type = "median_iqr")
vac_no_w<-wilcox.test(df[df['arm']==1,]$vac_no,df[df['arm']==2,]$vac_no,paired=FALSE)
VaccineIQR$pValue= c(vac_no_w$p.value,vac_no_w$method)
View(VaccineIQR)

#2 Illustrate Graph of gen descriptive----
#Use Excel 
#Step 3: Viral Load--
#..3.1 Mean Viral load comparison----
dd<-df2 %>% select(ID,arm,ddpcr_day0,ddpcr_day5,ddpcr_day10)
dd_sub <- dd %>% filter(ddpcr_day0!=999999999 & ddpcr_day5!=999999999 & ddpcr_day10!=999999999)
dt<-dd_sub
dt$arm <-factor(dt$arm)
dl <- gather(dt, nth_date, viral_load, ddpcr_day0:ddpcr_day10)
#Mean only
res<-dt %>% group_by(arm) %>% 
  summarise("Mean viral load Day 0"=format(mean(log10(ddpcr_day0)),big.mark=",",nsmall=2),"S.D. D0"=format(sd(log10(ddpcr_day0)),big.mark=",",nsmall=2),"Mean viral load Day 5"=format(mean(log10(ddpcr_day5)),big.mark=",",nsmall=2),"S.D. D5"=format(sd(log10(ddpcr_day5)),big.mark=",",nsmall=2),"Mean viral load day 10"=format(mean(log10(ddpcr_day10)),big.mark=",",nsmall=2),"S.D. D10"=format(sd(log10(ddpcr_day10)),big.mark=",",nsmall=2))
append(res,c(1,20,3,30,4,40,5))
res[2,]
res[3,]=as_tibble(c(1,20,3,30,4,40,5))
View(res)
#..3,2 PLot mean viral load per day----
dd<-df2 %>% select(ID,arm,ddpcr_day0,ddpcr_day5,ddpcr_day10,ddpcr_day0_log,ddpcr_day5_log,ddpcr_day10_log)
#exclude empty row
dd_sub <- dd %>% filter(ddpcr_day0!=999999999 & ddpcr_day5!=999999999 & ddpcr_day10!=999999999)
dt<-dd_sub
dt$arm <-factor(dt$arm)
Daylabs <- c("Day0", "Day 5", "Day 10")
dl <- gather(dt, nth_date, viral_load, ddpcr_day0:ddpcr_day10)
dl2<-dl %>% mutate("samp_date"=if_else(nth_date=='ddpcr_day0',as.Date('2023-01-01'),if_else(nth_date=='ddpcr_day5',as.Date('2023-01-05'),as.Date('2023-01-10'))))
dl2 %>% ggplot(data=.,aes(x=samp_date)) + 
  geom_line(aes(y=viral_load,group=ID,col=arm))+
  ylab("Viral load(in log. scale)") + xlab("Days after diagnosis.") +
  #scale_x_continuous(name="Speed of cars", limits=c(0, 12)) +
  #scale_x_continuous(labels=Daylabs)+
  #scale_x_continuous(breaks = c(1:5), labels = c("A", "B", "C","D","E"))+
  theme(axis.text.x=element_blank())+
  scale_y_continuous(trans='log10')




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
t.test(log10(dt[dt$arm == 1,]$ddpcr_day5), log10(dt[dt$arm == 2,]$ddpcr_day5),paired= F)
t.test(log10(dt[dt$arm == 1,]$ddpcr_day10), log10(dt[dt$arm == 2,]$ddpcr_day10),paired= F)

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
  ylab("Viral load(in logarithmic scale)") + xlab("Days after diagnosis.") +
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

#2 Analytic of efficacy


