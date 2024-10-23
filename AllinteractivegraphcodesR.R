#### Final interactive graphs ####
#packages needed
library(car)
library(dplyr)
library(tibble)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(ggiraph)
library(plotly)

#data
Datatable <- read.csv(file = "ITOLLSR.csv", header = T, stringsAsFactors = T)

weekends <- Datatable %>% filter(TX_DATE %in% c("5/4/2024", "5/5/2024", "5/11/2024", "5/12/2024", "5/18/2024",
                                                "5/19/2024", "5/25/2024", "5/26/2024", "6/1/2024", "6/2/2024",
                                                "6/8/2024","6/9/2024","6/15/2024","6/16/2024"))

## weekend frequencies by class
wkclass<- weekends %>% count(ACTUAL_CLASS, sort = F, name = "trxbyclass")






perclabelsweekend <- c("95.41 %", "1.90 %", ".21 %", ".24 %", "1.26 %", ".21 %", ".86 %")

percenta<- c("26.96 %", ".54 %", ".06 %", ".07 %", ".36 %", ".06%", ".24 %")
wkclass$Percentages <- percenta
as.factor(wkclass$ACTUAL_CLASS)
as.factor(wkclass$Percentages)

########################################################
########### Weekend interactive Plot ###################

v <- ggplot(wkclass, aes(x = as.factor(ACTUAL_CLASS), y = trxbyclass, fill = ACTUAL_CLASS,
                         data_id = ACTUAL_CLASS)) +
  ggtitle("Weekend Transactions (By Class)")+
  labs(y = "Transactions", x = "Vehicle Class")+
  geom_bar_interactive(stat = "identity",
                       data_id = c("1", "2", "3", "4", "5", "6", "8"),
                       tooltip = c("26.96 %" = "116,663 Transactions <br> 95.41 % of Wknd <br> 26.96 % of Total", 
                                   ".54 %" = "2,326 Transactions <br> 1.90 % of Wknd <br> .54 % of Total",
                                   ".06 %" = "262 Transactions <br> .21 % of Wknd <br> .06 % of Total",
                                   ".07 %" = "296 Transactions <br> .24 % of Wknd <br> .07 % of Total",
                                   ".36 %" = "1,543 Transactions <br> 1.26 % of Wknd <br> .36 % of Total",
                                   ".06 %" = "132 Transactions <br> .21 % of Wknd <br> .06 % of Total",
                                   ".24 %" = "1,051 Transactions <br> .86 % of Wknd <br> .24 % of Total")) +
  scale_fill_manual_interactive(
    values = c("1" = "slateblue2", "2" = "blue2", "3" = "seagreen2",
               "4" = "palevioletred2", "5" = "cyan2", "6" = "indianred2", "8" = "darkorchid3"),
    data_id = c("1", "2","3","4","5","6","8"),
    tooltip = c("1" = "116,663 Transactions <br> 95.41 % of Wknd <br> 26.96 % of Total", 
                "2" = "2,326 Transactions <br> 1.90 % of Wknd <br> .54 % of Total",
                "3" = "262 Transactions <br> .21 % of Wknd <br> .06 % of Total",
                "4" = "296 Transactions <br> .24 % of Wknd <br> .07 % of Total",
                "5" = "1,543 Transactions <br> 1.26 % of Wknd <br> .36 % of Total",
                "6" = "132 Transactions <br> .21 % of Wknd <br> .06 % of Total",
                "8" = "1,051 Transactions <br> .86 % of Wknd <br> .24 % of Total"),
    labels = c("Class 1", "Class 2", "Class 3", "Class 4", "Class 5", "Class 6", "Class 8"))+
  geom_text_interactive(aes(label = c("95.41% Wk\n26.29% All", "1.90% Wk\n.54% All", ".21% Wk\n.06% All",
                                      ".24% wk\n.07% All", "1.26% Wk\n.36% All",".21% Wk\n.06% All", ".86% Wk\n.24% All"), 
                            y = c(120700,7000,5000,5000,6000,5000,5500)), size = 2.6, check_overlap = T,
                        tooltip = c("1" = "116,663 Transactions <br> 95.41 % of Wknd <br> 26.96 % of Total", 
                                    "2" = "2,326 Transactions <br> 1.90 % of Wknd <br> .54 % of Total",
                                    "3" = "262 Transactions <br> .21 % of Wknd <br> .06 % of Total",
                                    "4" = "296 Transactions <br> .24 % of Wknd <br> .07 % of Total",
                                    "5" = "1,543 Transactions <br> 1.26 % of Wknd <br> .36 % of Total",
                                    "6" = "132 Transactions <br> .21 % of Wknd <br> .06 % of Total",
                                    "8" = "1,051 Transactions <br> .86 % of Wknd <br> .24 % of Total"))


x <- girafe(ggobj = v)
x <- girafe_options(x,
                    opts_hover(css = "fill:violet"),
                    opts_hover_key(girafe_css("fill:violet;stroke:white;")))
x


#############################################################################################
######################## Weekday interactive plot ###########################################
#data
weekday<- Datatable %>%
  filter(!TX_DATE %in% c("5/4/2024", "5/5/2024", "5/11/2024", "5/12/2024", "5/18/2024",
                         "5/19/2024", "5/25/2024", "5/26/2024", "6/1/2024", "6/2/2024",
                         "6/8/2024","6/9/2024","6/15/2024","6/16/2024"))


## Weekday Frequencies by Class ##
wkdayclass <- weekday %>% count(ACTUAL_CLASS, sort  = F, name = "trxbyclass")
wkdayclass


#graph
v2 <- ggplot(wkdayclass, aes(x = as.factor(ACTUAL_CLASS), y = trxbyclass, fill = ACTUAL_CLASS,
                             data_id = ACTUAL_CLASS)) +
  ggtitle("Weekday Transactions (By Class)")+
  labs(y = "Transactions", x = "Vehicle Class")+
  scale_y_continuous(labels=function(n){format(n, scientific = FALSE)})+
  theme(legend.title=element_blank())+
  geom_bar_interactive(stat = "identity",
                       data_id = c("1", "2", "3", "4", "5", "6", "8"),
                       tooltip = c("64.91 %" = "280,874 Transactions <br> 90.48 % of Wkdy <br> 64.91 % of Total", 
                                   "3.06 %" = "13,242 Transactions <br> 4.27 % of Wkdy <br> 3.06 % of Total",
                                   ".62 %" = "2,703 Transactions <br> .87 % of Wkdy <br> .62 % of Total",
                                   ".40 %" = "1,734 Transactions <br> .56 % of Wkdy <br> .40 % of Total",
                                   "1.94 %" = "8,411 Transactions <br> 2.71 % of Wkdy <br> 1.94 % of Total",
                                   ".19 %" = "814 Transactions <br> .26 % of Wkdy <br> .19 % of Total",
                                   ".61 %" = "2,656 Transactions <br> .86 % of Wkdy <br> .61 % of Total")) +
  scale_fill_manual_interactive(
    values = c("1" = "mediumpurple1", "2" = "royalblue1", "3" = "tomato",
               "4" = "mediumspringgreen", "5" = "cadetblue1", "6" = "coral", "8" = "blueviolet"),
    data_id = c("1", "2","3","4","5","6","8"),
    labels = c("Class 1", "Class 2", "Class 3", "Class 4", "Class 5", "Class 6", "Class 8"),
    tooltip = c("1" = "280,874 Transactions <br> 90.48 % of Wkdy <br> 64.91 % of Total", 
                "2" = "13,242 Transactions <br> 4.27 % of Wkdy <br> 3.06 % of Total",
                "3" = "2,703 Transactions <br> .87 % of Wkdy <br> .62 % of Total",
                "4" = "1,734 Transactions <br> .56 % of Wkdy <br> .40 % of Total",
                "5" = "8,411 Transactions <br> 2.71 % of Wkdy <br> 1.94 % of Total",
                "6" = "814 Transactions <br> .26 % of Wkdy <br> .19 % of Total",
                "8" = "2,656 Transactions <br> .86 % of Wkdy <br> .61 % of Total"))+
  geom_text_interactive(aes(label = c("90.48% Wd\n64.91% All", "4.27% Wd\n3.06% All", ".87% Wd\n.62% All",
                                      ".56% wd\n.40% All", "2.71% Wd\n1.94% All",".26% Wd\n.19% All", ".86% Wd\n.61% All"), 
                            y = c(290000,23500,13000,12000,19000,11000,13000)), size = 2.6, check_overlap = T,
                        tooltip = c("64.91 %" = "280,874 Transactions <br> 90.48 % of Wkdy <br> 64.91 % of Total", 
                                    "3.06 %" = "13,242 Transactions <br> 4.27 % of Wkdy <br> 3.06 % of Total",
                                    ".62 %" = "2,703 Transactions <br> .87 % of Wkdy <br> .62 % of Total",
                                    ".40 %" = "1,734 Transactions <br> .56 % of Wkdy <br> .40 % of Total",
                                    "1.94 %" = "8,411 Transactions <br> 2.71 % of Wkdy <br> 1.94 % of Total",
                                    ".19 %" = "814 Transactions <br> .26 % of Wkdy <br> .19 % of Total",
                                    ".61 %" = "2,656 Transactions <br> .86 % of Wkdy <br> .61 % of Total"))


x2 <- girafe(ggobj = v2)
x2 <- girafe_options(x2,
                     opts_hover(css = girafe_css("fill:midnightblue")),
                     opts_hover_key(girafe_css("fill:midnightblue;stroke:orange")))
x2



percentallwday <- round(c(wkdayclass$trxbyclass[]/432707*100), 2)

percentaonlywday <- round(c(wkdayclass$trxbyclass[]/310434*100), 2) # % numbers for labels

x2



#############################################################################
################ Transaction frequency by date bar graph ####################
#data
txcountbydate<- Datatable %>% count(TX_DATE, sort = F,name = "TX_DATE_FREQ") 
txcountbydate$TX_DATE <- as.Date(txcountbydate$TX_DATE, tryFormats = c("mm/dd/yyyy", "%m/%d/%y", "m/d/yyyy"))
txcountbydate <- rbind(txcountbydate)
txcountbydate<- txcountbydate %>% arrange(format(txcountbydate$TX_DATE, "%m/%d/%y"))
txcountbydate

#graph
x3<- ggplot(data = txcountbydate, mapping = aes(TX_DATE, TX_DATE_FREQ, fill = TX_DATE_FREQ)) +
  geom_bar_interactive(stat = "identity",
                       data_id = c("2024-05-01", "2024-05-02", "2024-05-03", "2024-05-04", "2024-05-05", "2024-05-06", "2024-05-07", "2024-05-08", "2024-05-09", "2024-05-10",
                                   "2024-05-11", "2024-05-12", "2024-05-13", "2024-05-14", "2024-05-15", "2024-05-16", "2024-05-17", "2024-05-18", "2024-05-19", "2024-05-20",
                                   "2024-05-21", "2024-05-22", "2024-05-23", "2024-05-24", "2024-05-25", "2024-05-26", "2024-05-27", "2024-05-28", "2024-05-29", "2024-05-30",
                                   "2024-05-31", "2024-06-01", "2024-06-02", "2024-06-03", "2024-06-04", "2024-06-05", "2024-06-06", "2024-06-07", "2024-06-08", "2024-06-09",
                                   "2024-06-10", "2024-06-11", "2024-06-12", "2024-06-13", "2024-06-14", "2024-06-15", "2024-06-16", "2024-06-17", "2024-06-18", "2024-06-19"),
                       tooltip = c("2024-05-01", "2024-05-02", "2024-05-03<br>Friday", "2024-05-04", "2024-05-05", "2024-05-06", "2024-05-07", "2024-05-08", "2024-05-09", "2024-05-10<br>Friday",
                                   "2024-05-11", "2024-05-12", "2024-05-13", "2024-05-14", "2024-05-15", "2024-05-16", "2024-05-17<br>Friday", "2024-05-18", "2024-05-19", "2024-05-20",
                                   "2024-05-21", "2024-05-22", "2024-05-23", "2024-05-24<br>Friday", "2024-05-25", "2024-05-26", "2024-05-27", "2024-05-28", "2024-05-29", "2024-05-30",
                                   "2024-05-31<br>Friday", "2024-06-01", "2024-06-02", "2024-06-03", "2024-06-04", "2024-06-05", "2024-06-06", "2024-06-07<br>Friday", "2024-06-08", "2024-06-09",
                                   "2024-06-10", "2024-06-11", "2024-06-12", "2024-06-13", "2024-06-14<br>Friday", "2024-06-15", "2024-06-16", "2024-06-17", "2024-06-18", "2024-06-19"))+
  
  ggtitle("Transactions by Date")+
  labs(y = "Transactions", x = "Date of Trx") +
  guides(fill=guide_legend(title="Trx Count", reverse = T))+
  theme(axis.text.x = element_text(angle = 0))+
  scale_fill_continuous(trans = 'reverse')

x3 <- girafe(ggobj = x3)
x3 <- girafe_options(x3,
                     opts_hover(css = girafe_css("fill:gold")))
x3


#############################################################################
############### Transaction frequency by date line plot code ################



x3<- ggplot(data = txcountbydate, mapping = aes(TX_DATE, TX_DATE_FREQ)) +
  geom_line(stat = "identity", colour = "midnightblue")+
  geom_point_interactive(stat = "identity",
                         data_id = c("2024-05-01", "2024-05-02", "2024-05-03", "2024-05-04", "2024-05-05", "2024-05-06", "2024-05-07", "2024-05-08", "2024-05-09", "2024-05-10",
                                     "2024-05-11", "2024-05-12", "2024-05-13", "2024-05-14", "2024-05-15", "2024-05-16", "2024-05-17", "2024-05-18", "2024-05-19", "2024-05-20",
                                     "2024-05-21", "2024-05-22", "2024-05-23", "2024-05-24", "2024-05-25", "2024-05-26", "2024-05-27", "2024-05-28", "2024-05-29", "2024-05-30",
                                     "2024-05-31", "2024-06-01", "2024-06-02", "2024-06-03", "2024-06-04", "2024-06-05", "2024-06-06", "2024-06-07", "2024-06-08", "2024-06-09",
                                     "2024-06-10", "2024-06-11", "2024-06-12", "2024-06-13", "2024-06-14", "2024-06-15", "2024-06-16", "2024-06-17", "2024-06-18", "2024-06-19"),
                         tooltip = c(txcountbydate$TX_DATE_FREQ),
                         colour = "midnightblue")+
  
  ggtitle("Transactions by Date")+
  labs(y = "Transactions", x = "Date of Trx") +
  theme(axis.text.x = element_text(angle = 0))

x3 <- girafe(ggobj = x3)
x3 <- girafe_options(x3,
                     opts_hover(css = girafe_css("fill:yellow;stroke:purple")))
x3

######################################################
# Transaction frequency by date line plot code (dates tooltip with friday) ###

x3<- ggplot(data = txcountbydate, mapping = aes(TX_DATE, TX_DATE_FREQ)) +
  geom_line(stat = "identity", colour = "midnightblue")+
  geom_point_interactive(stat = "identity",
                         data_id = c("2024-05-01", "2024-05-02", "2024-05-03", "2024-05-04", "2024-05-05", "2024-05-06", "2024-05-07", "2024-05-08", "2024-05-09", "2024-05-10",
                                     "2024-05-11", "2024-05-12", "2024-05-13", "2024-05-14", "2024-05-15", "2024-05-16", "2024-05-17", "2024-05-18", "2024-05-19", "2024-05-20",
                                     "2024-05-21", "2024-05-22", "2024-05-23", "2024-05-24", "2024-05-25", "2024-05-26", "2024-05-27", "2024-05-28", "2024-05-29", "2024-05-30",
                                     "2024-05-31", "2024-06-01", "2024-06-02", "2024-06-03", "2024-06-04", "2024-06-05", "2024-06-06", "2024-06-07", "2024-06-08", "2024-06-09",
                                     "2024-06-10", "2024-06-11", "2024-06-12", "2024-06-13", "2024-06-14", "2024-06-15", "2024-06-16", "2024-06-17", "2024-06-18", "2024-06-19"),
                         tooltip = c("2024-05-01", "2024-05-02", "2024-05-03<br>Friday", "2024-05-04", "2024-05-05", "2024-05-06", "2024-05-07", "2024-05-08", "2024-05-09", "2024-05-10<br>Friday",
                                     "2024-05-11", "2024-05-12", "2024-05-13", "2024-05-14", "2024-05-15", "2024-05-16", "2024-05-17<br>Friday", "2024-05-18", "2024-05-19", "2024-05-20",
                                     "2024-05-21", "2024-05-22", "2024-05-23", "2024-05-24<br>Friday", "2024-05-25", "2024-05-26", "2024-05-27", "2024-05-28", "2024-05-29", "2024-05-30",
                                     "2024-05-31<br>Friday", "2024-06-01", "2024-06-02", "2024-06-03", "2024-06-04", "2024-06-05", "2024-06-06", "2024-06-07<br>Friday", "2024-06-08", "2024-06-09",
                                     "2024-06-10", "2024-06-11", "2024-06-12", "2024-06-13", "2024-06-14<br>Friday", "2024-06-15", "2024-06-16", "2024-06-17", "2024-06-18", "2024-06-19"),
                         colour = "midnightblue")+
  
  ggtitle("Transactions by Date")+
  labs(y = "Transactions", x = "Date of Trx") +
  theme(axis.text.x = element_text(angle = 0))

x3 <- girafe(ggobj = x3)
x3 <- girafe_options(x3,
                     opts_hover(css = girafe_css("fill:yellow;stroke:white")))
x3



