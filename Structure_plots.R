#Script for building structure admixture assignment figures
setwd("C:/Users/shelm/Dropbox/Pace/Structure/OF_anal")

library(ggplot2)
data <- read.csv("OF_assignments.csv", header=TRUE)
data$Pop <- as.factor(data$Pop)
ggplot(data, aes(fill=Pop, y=Prop, x=Label)) + 
  geom_bar(position="fill", stat = "identity")+
  theme_bw()+geom_col(width=.1)+ 
  labs(y= "Membership probability", x = "Colony")+
  scale_fill_manual(labels = c("H1/H2", "P. barbatus"),values= c("black", "palegreen4"), name="Population") +
  theme(text = element_text(size=20))

setwd("C:/Users/shelm/Dropbox/Pace/Structure/CT_anal")
data <- read.csv("CT_assignments.csv", header=TRUE)
data$Pop <- as.factor(data$Pop)
ggplot(data, aes(fill=Pop, y=Prop, x=Label)) + 
  geom_bar(position="fill", stat = "identity")+
  theme_bw()+geom_col(width=.1)+ 
  labs(y= "Membership probability", x = "Colony")+
  scale_fill_manual(labels = c("H1/H2", "P. barbatus"),values= c("black", "palegreen4"), name="Population") +
  theme(text = element_text(size=20))

setwd("C:/Users/shelm/Dropbox/Pace/Structure/ALP_anal")
data <- read.csv("ALP_assignments.csv", header=TRUE)
data$Pop <- as.factor(data$Pop)
ggplot(data, aes(fill=Pop, y=Prop, x=Label)) + 
  geom_bar(position="fill", stat = "identity")+
  theme_bw()+geom_col(width=.1)+ 
  labs(y= "Membership probability", x = "Colony")+
  scale_fill_manual(labels = c("H1/H2", "P. barbatus"),values= c("black", "palegreen4"), name="Population") +
  theme(text = element_text(size=20))

setwd("C:/Users/shelm/Dropbox/Pace/Structure/DC_anal")
data <- read.csv("DC_assignments.csv", header=TRUE)
data$Pop <- as.factor(data$Pop)
ggplot(data, aes(fill=Pop, y=Prop, x=Label)) + 
  geom_bar(position="fill", stat = "identity")+
  theme_bw()+geom_col(width=.1)+ 
  labs(y= "Membership probability", x = "Colony")+
  scale_fill_manual(labels = c("H1/H2", "P. rugosus"),values= c("black", "gray"), name="Population") +
  theme(text = element_text(size=20))

setwd("C:/Users/shelm/Dropbox/Pace/Structure/WS_anal")
data <- read.csv("WS_assignments.csv", header=TRUE)
data$Pop <- as.factor(data$Pop)
ggplot(data, aes(fill=Pop, y=Prop, x=Label)) + 
  geom_bar(position="fill", stat = "identity")+
  theme_bw()+geom_col(width=.1)+ 
  labs(y= "Membership probability", x = "Colony")+
  scale_fill_manual(labels = c("J1/J2", "P. rugosus"),values= c("darkred", "gray"), name="Population") +
  theme(text = element_text(size=20))

setwd("C:/Users/shelm/Dropbox/Pace/Structure/PC_reduced")
data <- read.csv("PC_assignments.csv", header=TRUE)
data$Pop <- as.factor(data$Pop)
ggplot(data, aes(fill=Pop, y=Prop, x=Label)) + 
  geom_bar(position="fill", stat = "identity")+
  theme_bw()+geom_col(width=.1)+ 
  labs(y= "Membership probability", x = "Colony")+
  scale_fill_manual(labels = c("H1/H2", "P. rugosus"),values= c("black", "gray"), name="Population") +
  theme(text = element_text(size=20))

