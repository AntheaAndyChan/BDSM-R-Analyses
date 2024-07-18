# Useful settings
options(digits = 3)
options(scipen = 9999) # suppress scientific notation
library(tidyverse)
rm(list = ls()) 
dat<- readRDS("Final Cleaned Kink2.rds")
dat <- dat[complete.cases(dat),]

mlm1<-lm(cbind(ASSERT, ASSUME, POS, PBC_ln, RLN, IBA, AWARE_ln, RCUS_ln, LIKELY_ln) ~ 1, data = dat)
mlm5 <- update(mlm1, . ~ . +COMMUNITY_1+COMMUNITY_2+COMMUNITY_3+COMMUNITY_4+COMMUNITY_5+COMMUNITY_6)



# EFA?

EFA<-dat%>% dplyr::select(COMMUNITY_1:COMMUNITY_6)
EFA <- EFA[complete.cases(EFA),]
describe(EFA)

library(psych)
library(GPArotation)
set.seed(1234) # set seed for reproducability
fa.parallel(EFA, # perform parallel analysis
            fa = "fa",
            fm = "pa",
            show.legend = TRUE,
            main = "Scree Plot and Parallel Analysis")

polychoric_corr <- polychoric(EFA)$rho 


# efa_promax2 <- fa(polychoric_corr, # perform EFA with 6 factors
#                   nfactors=1,
#                   rotate="promax",
#                   fm="pa")
# print(efa_promax2,  cut = 0.4)
# 
# efa_promax2 <- fa(polychoric_corr, # perform EFA with 6 factors
#                   nfactors=2,
#                   rotate="promax",
#                   fm="pa")
# print(efa_promax2,  cut = 0.4)


efa_promax3 <- fa(polychoric_corr, # perform EFA with 6 factors
                  nfactors=3,
                  rotate="promax",
                  fm="pa")
print(efa_promax3,  cut = 0.4)
print(efa_promax3)
# ... been generally positive.”
# ... improved my sex life.”

# ... taught me to communicate my desires more effectively.”
# ... taught me to communicate my limits more effectively.”

# ... taught me the importance of consent.”
# ... provided opportunities for me to learn about consent.”

# Create new commun variables
# https://statisticsglobe.com/exploratory-factor-analysis-r

EFA<-EFA %>% rowwise()%>% mutate(COMM_Positive = mean(COMMUNITY_1,COMMUNITY_2, na.rm = TRUE),
                    COMM_Communicate = mean(COMMUNITY_3,COMMUNITY_4, na.rm = TRUE),
                    COMM_Growth = mean(COMMUNITY_5,COMMUNITY_6, na.rm = TRUE))
                    

describe(EFA[7:9])
hist(EFA$COMM_Positive)
hist(EFA$COMM_Communicate)
hist(EFA$COMM_Growth)

library(ltm)
cronbach.alpha(EFA[1:2])
cronbach.alpha(EFA[3:4])
cronbach.alpha(EFA[5:6])

rm(list = ls()) 
dat<- readRDS("Final Cleaned Kink2.rds")
dat<-dat %>% rowwise()%>% mutate(COMM_Positive = mean(COMMUNITY_1,COMMUNITY_2, na.rm = TRUE),
                                 COMM_Communicate = mean(COMMUNITY_3,COMMUNITY_4, na.rm = TRUE),
                                 COMM_Growth = mean(COMMUNITY_5,COMMUNITY_6, na.rm = TRUE))

dat<-dat%>% dplyr::select(-COMMUNITY_1:-COMMUNITY_6,-YOST_Wrong_ln:-YOST_RL, -YOST, -Single:-PARTNERS,-AGE_SEX)
miss_var_summary(dat, order=T)
dat<-dat%>% dplyr::select(-Women, -White)
dat <- dat[complete.cases(dat),] #215 remain!

mlm1<-lm(cbind(ASSERT, ASSUME, POS, PBC_ln, RLN, IBA, AWARE_ln, RCUS_ln, LIKELY_ln)  ~ 1, data = dat)
mlm2 <- update(mlm1, . ~ . + Gender+BDSMRole)
anova(mlm1, mlm2)
mlm3 <- update(mlm2, . ~ .  +Hetero)
anova(mlm2, mlm3)
mlm5 <- update(mlm3, . ~ . +YR_PRACTICE+YR_COMMUN)
anova(mlm3, mlm5)
mlm6 <- update(mlm5, . ~ . +COMM_Positive +COMM_Communicate+COMM_Growth)
anova(mlm5, mlm6)

summary(mlm6)





ANOVA<-anova(mlm1,mlm2,mlm3,mlm5,mlm6)
sink("anova2.txt");print(ANOVA);sink()
results<-summary(mlm6) 
coefs<-tidy(mlm6)
#std coefs
mlm<-lm(cbind(scale(ASSERT),scale(ASSUME), scale(POS), scale(PBC_ln), scale(RLN), scale(IBA), scale(AWARE_ln), scale(RCUS_ln), scale(LIKELY_ln)) ~   Hetero + Gender + BDSMRole+ scale(YR_PRACTICE) + scale(YR_COMMUN)+ scale(COMM_Positive) + scale(COMM_Communicate) + scale(COMM_Growth) , data=dat)
std.coefs<-tidy(mlm)$estimate

coefs<-cbind(std.coefs=std.coefs, coefs)  %>% 
    mutate(coef=paste(round(estimate,2),"(",round(std.error,2),")",sep=""),
           sig = case_when(p.value<.001 ~ "***",
                           p.value<.01 ~ "**",
                           p.value<.05 ~ "*",
                           T~""),
           beta=paste(round(std.coefs,2),sig,sep=""))%>% 
    dplyr::select(response,term,coef,beta)

#long to wide 
boop<-coefs %>% pivot_wider(names_from =response, values_from  = c(coef,beta))

# Anova results
table<-cbind("adj.r.squared","f","numdf","dendf")
for (i in 1:9){ 
    table<-rbind(table,
                 cbind(round(results[[i]]$adj.r.squared,3), 
                       round(results[[i]]$fstatistic[1],3), 
                       round(results[[i]]$fstatistic[2],3), 
                       round(results[[i]]$fstatistic[3],3))
    )
}
colnames(table)<-table[1,]
table<-table[-1,]
table<-as.data.frame(table)
table[,1]<-as.numeric(table[,1])
table[,2]<-as.numeric(table[,2])
table[,3]<-as.numeric(table[,3])
table[,4]<-as.numeric(table[,4])
table$p <- pf(table[,2],table[,3],table[,4],lower.tail=F)

table<-table %>% 
    mutate(sig= case_when(p<.001 ~ "p<.001",T~paste("p=",round(p,3))),
           Ftest=paste("F(",numdf,", ", dendf,") = ", f, ", ",sig,sep="")
    ) %>% dplyr::select(adj.r.squared,Ftest)


library(openxlsx)
BDSMResults <- createWorkbook("BDSMResults") 
addWorksheet(BDSMResults, "ANOVAs")
writeData(BDSMResults, sheet = 1, ANOVA) 
addWorksheet(BDSMResults, "coefficients")
writeData(BDSMResults, sheet = 2, boop) 
addWorksheet(BDSMResults, "modela")
writeData(BDSMResults, sheet = 3, table) 
saveWorkbook(BDSMResults, "MVR Results_newcomm_242024.xlsx", overwrite = TRUE) 
