library(ggplot2)
library(gridExtra)
tooth_data <- data.frame(ToothGrowth)
tooth_data$dose <- as.factor(tooth_data$dose)
head(tooth_data)
str(tooth_data)
summary(tooth_data)

p <- ggplot(tooth_data, aes(dose,len)) + geom_boxplot(color="black",fill="orange")+
facet_grid(.~supp) + labs(title="Relationship Between Tooth Length and Vitamin C Dose")+
labs(x=" Vitamin C Dose(mg/day)", y="Tooth length") + theme(text = element_text(size=16))
print(p)

#Test if if OJ and VC delivery methods overall show a difference in tooth growth.
#Don't assume equal variances.  Two-tailed test.
t.test(tooth_data$len~tooth_data$supp)

#Perform the same test, but use the one tail test.
t.test(tooth_data$len~tooth_data$supp, alternative = "greater")

#Compare tooth growth for doses at 0.5 and 1 for the OJ group.  Does the
#1 dose group have larger teeth than the 0.5 dose group?
tooth_length_oj_dose1 <-tooth_data$len[tooth_data$dose==1 & tooth_data$supp =="OJ"]
tooth_length_oj_dose0p5 <-tooth_data$len[tooth_data$dose==0.5 & tooth_data$supp =="OJ"]
tooth_length_oj_dose2 <-tooth_data$len[tooth_data$dose==2 & tooth_data$supp =="OJ"]


t.test(tooth_length_oj_dose1,tooth_length_oj_dose0p5, alternative = "greater")
t.test(tooth_length_oj_dose2,tooth_length_oj_dose1, alternative = "greater")


#Compare tooth growth for doses at 0.5 and 1 for the OJ group.  Does the
#1 dose group have larger teeth than the 0.5 dose group?
tooth_length_vc_dose1 <-tooth_data$len[tooth_data$dose==1 & tooth_data$supp =="VC"]
tooth_length_vc_dose0p5 <-tooth_data$len[tooth_data$dose==0.5 & tooth_data$supp =="VC"]
tooth_length_vc_dose2 <-tooth_data$len[tooth_data$dose==2 & tooth_data$supp =="VC"]


t.test(tooth_length_vc_dose1,tooth_length_vc_dose0p5, alternative = "greater")
t.test(tooth_length_vc_dose2,tooth_length_vc_dose1, alternative = "greater")