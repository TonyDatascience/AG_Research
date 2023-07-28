#This is the original R-File
library("tidyverse")
library("broom")
library("ggplot2")
library("readxl")
library("dplyr")
library("rstatix")
library(gmodels)
library(epiR)

#Step 1: Open excel file----
df = read_xlsx("ap_new.xlsx")
View(df)
summary(df)
arm_id<-unique(df$arm)
df2<- df
df2$arm<-factor(df2$arm)
options(digits=2)

#ALT section
df3<-df2 %>% select('arm','alt_3Xday10','alt_3Xday5','alt_3Xday0')
df_alt_day0<- df3 %>% drop_na(alt_3Xday0)
df_alt_day5<- df3 %>% drop_na(alt_3Xday5)
df_alt_day10<- df3 %>% drop_na(alt_3Xday10)
df_alt_all <- df3 %>% drop_na()

#ALT3x
Tab0 = table(df_alt_day0$arm,df_alt_day0$alt_3Xday0)
fish0<-fisher.test(df_alt_day0$alt_3Xday0,df_alt_day0$arm,alternative = "greater")
fish0
Tab5 = table(df_alt_day5$arm,df_alt_day5$alt_3Xday5)
Tab5
fish5<-fisher.test(df_alt_day5$alt_3Xday5,df_alt_day5$arm,alternative = "greater")
fish5
Tab10 = table(df_alt_day10$arm,df_alt_day10$alt_3Xday10)
Tab10
fish10<-fisher.test(df_alt_day10$alt_3Xday10,df_alt_day10$arm,alternative = "greater")
fish10
#ALT Level----
#ALT
df3<-df2 %>% select('arm','alt_day10','alt_day5','alt_day0')
df3$alt_day0<-as.integer(df3$alt_day0)
df3$alt_day5<-as.integer(df3$alt_day5)
df3$alt_day10<-as.integer(df3$alt_day10)

df3$alt_day0<-na_if(df3$alt_day0,999999999)
df3$alt_day5<-na_if(df3$alt_day5,999999999)
df3$alt_day10<-na_if(df3$alt_day10,999999999)

df_alt_day0<- df3 %>% drop_na(alt_day0)
df_alt_day5<- df3 %>% drop_na(alt_day5)
df_alt_day10<- df3 %>% drop_na(alt_day10)


df_alt_all <- df3 %>% drop_na()

shapiro.test(df_alt_day0$alt_day0)
w_alt_day0<-wilcox.test(alt_day0 ~ arm, data=df_alt_day0, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(w_alt_day0)

shapiro.test(df_alt_day5$alt_day5)
w_alt_day5<-wilcox.test(alt_day5 ~ arm, data=df_alt_day5, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(w_alt_day5)

shapiro.test(df_alt_day10$alt_day10)
w_alt_day10<-wilcox.test(alt_day10 ~ arm, data=df_alt_day10, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(w_alt_day10)
df_alt_day0 %>% group_by(arm) %>% summarise(Median = median(alt_day0, na.rm=T),n = n())

df_alt_day5 %>% group_by(arm) %>% summarise(Median = median(alt_day5, na.rm=T),n = n())

df_alt_day10 %>% group_by(arm) %>% summarise(Median = median(alt_day10, na.rm=T),n = n())

#AST section----
df3<-df2 %>% select('arm','ast_3Xday10','ast_3Xday5','ast_3Xday0')
df_ast_day0<- df3 %>% drop_na(ast_3Xday0)
df_ast_day5<- df3 %>% drop_na(ast_3Xday5)
df_ast_day10<- df3 %>% drop_na(ast_3Xday10)
df_ast_all <- df3 %>% drop_na()

#....AST3x----

Tab0 = table(df_ast_day0$arm,df_ast_day0$ast_3Xday0)
Tab0
fish0<-fisher.test(df_ast_day0$ast_3Xday0,df_ast_day0$arm,alternative = "greater")
fish0
Tab5 = table(df_ast_day5$arm,df_ast_day5$ast_3Xday5)
Tab5
fish5<-fisher.test(df_ast_day5$ast_3Xday5,df_ast_day5$arm,alternative = "greater")
fish5
Tab10 = table(df_ast_day10$arm,df_ast_day10$ast_3Xday10)
Tab10
fish10<-fisher.test(df_ast_day10$ast_3Xday10,df_ast_day10$arm,alternative = "greater")
fish10




#....AST level----
df3<-df2 %>% select('arm','ast_day10','ast_day5','ast_day0')
df3$ast_day0<-as.integer(df3$ast_day0)
df3$ast_day5<-as.integer(df3$ast_day5)
df3$ast_day10<-as.integer(df3$ast_day10)

df3$ast_day0<-na_if(df3$ast_day0,999999999)
df3$ast_day5<-na_if(df3$ast_day5,999999999)
df3$ast_day10<-na_if(df3$ast_day10,999999999)

df_ast_day0<- df3 %>% drop_na(ast_day0)
df_ast_day5<- df3 %>% drop_na(ast_day5)
df_ast_day10<- df3 %>% drop_na(ast_day10)


df_ast_all <- df3 %>% drop_na()

shapiro.test(df_ast_day0$ast_day0)
w_ast_day0<-wilcox.test(ast_day0 ~ arm, data=df_ast_day0, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(w_ast_day0)

shapiro.test(df_ast_day5$ast_day5)
w_ast_day5<-wilcox.test(ast_day5 ~ arm, data=df_ast_day5, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(w_ast_day5)

shapiro.test(df_ast_day10$ast_day10)
w_ast_day10<-wilcox.test(ast_day10 ~ arm, data=df_ast_day10, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(w_ast_day10)
df_ast_day0 %>% group_by(arm) %>% summarise(Median = median(ast_day0, na.rm=T),n = n())

df_ast_day5 %>% group_by(arm) %>% summarise(Median = median(ast_day5, na.rm=T),n = n())

df_ast_day10 %>% group_by(arm) %>% summarise(Median = median(ast_day10, na.rm=T),n = n())


#Ativan
#AST level----
df_Ativan<-df2 %>% select('arm','Ativan','alt_day10','alt_day5','alt_day0','alt_3Xday10','alt_3Xday5','alt_3Xday0')
df_Ativan$Ativan<-as.integer(df_Ativan$Ativan)
df_Ativan$alt_day0<-as.integer(df_Ativan$alt_day0)
df_Ativan$alt_day5<-as.integer(df_Ativan$alt_day5)
df_Ativan$alt_day10<-as.integer(df_Ativan$alt_day10)

df_Ativan$alt_day0<-na_if(df_Ativan$alt_day0,999999999)
df_Ativan$alt_day5<-na_if(df_Ativan$alt_day5,999999999)
df_Ativan$alt_day10<-na_if(df_Ativan$alt_day10,999999999)
df_Ativan$Ativan<-na_if(df_Ativan$Ativan,999999999)

Tab0 = table(df_Ativan$arm,df_Ativan$Ativan)
Tab0
fish0<-fisher.test(df_Ativan$Ativan,df_Ativan$arm,alternative = "greater")
fish0

df_Ativan %>% group_by(Ativan) %>% summarise(Median = median(alt_day0, na.rm=T),n = n())

df_Ativan %>% group_by(Ativan) %>% summarise(Median = median(alt_day5, na.rm=T),n = n())

df_Ativan %>% group_by(Ativan) %>% summarise(Median = median(alt_day10, na.rm=T),n = n())

Tab0 = table(df_Ativan$Ativan,df_Ativan$alt_3Xday10)
Tab0
fish0<-fisher.test(df_Ativan$alt_3Xday10,df_Ativan$Ativan,alternative = "greater")
fish0