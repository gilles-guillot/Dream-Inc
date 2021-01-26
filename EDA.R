library(tidyverse)
library(data.table)

source("./Comp_CI_diff_prop.R")
source("./comp_CI_prop.R")

## read csv data file 
label <- read_delim("../label.csv", delim = "|")
label
str(label)
label$treatment <- as.factor(label$treatment)
which(is.na(label$treatment))

outcome <- read_delim("../outcome.csv", delim = "|")
outcome
str(outcome)
which(is.na(outcome$failure))
#outcome$failure <- as.factor(outcome$failure)
#levels(outcome$failure)
table(outcome$failure)
which(outcome$failure==-1)
which(outcome$failure==100)
outcome[outcome$failure==-1 | outcome$failure==100,]
outcome$failure[outcome$failure==-1 | outcome$failure==100] <- NA
# levels(outcome$failure)
# outcome$failure = droplevels(outcome$failure)  

# if( sum(label$id != outcome$id) ==0 ){"Row ids are matching"}else
#   print("The  hiring committee has a poor sense of humor :)")

dat = tibble(id=label$id,treatment=label$treatment,failure=outcome$failure)
dat

table(dat$treatment,dat$failure)
pA = mean(dat$failure[dat$treatment=="1.4"],na.rm = TRUE)
pB = mean(dat$failure[dat$treatment=="1.5"],na.rm = TRUE)
pA*100
pB*100
(pA-pB)*100

fisher.test(dat$treatment,dat$failure)

CI_prop_A <- comp_CI_prop(dat$failure[dat$treatment=="1.4"] ,
                               alpha = 0.05, # 1-(nominal coverage)
                               method=c("normal","Wilson","Wilson_cc","CP"))
CI_prop_A

CI_prop_B <- comp_CI_prop(dat$failure[dat$treatment=="1.5"] ,
                               alpha = 0.05, # 1-(nominal coverage)
                               method=c("normal","Wilson","Wilson_cc","CP"))
CI_prop_B




## Cost analysis looking at two scenario no upgrade / upgrade
nrun <- seq(1,3e+6,1000)
Cu <- 43000
cf <- 2.32
Cu/(cf*(pA-pB))
C <- tibble(nrun=nrun,
            mean_update=Cu + cf*nrun*pB,
            min_update= Cu + cf*nrun*CI_prop_B$CI_CP[1],
            max_update= Cu + cf*nrun*CI_prop_B$CI_CP[2],
            mean_no_update= cf*nrun*pA,
            min_no_update= cf*nrun*CI_prop_A$CI_CP[1],
            max_no_update= cf*nrun*CI_prop_A$CI_CP[2],)
C <- C %>% setDT()


C <- melt(C, id=1, measure=patterns("^mean","^min","^max", cols=names(C)), 
     value.name=c("mean","min","max"), variable.name="decision")
C$decision = as.factor(C$decision)
levels(C$decision) <- c("update","no_update")
C  
  
p <- ggplot(C,aes(x=nrun,y=mean,min=min,ymax=max,group=decision,fill=decision,linetype=decision)) + 
  geom_line() +  scale_color_brewer(palette="Dark2") + 
  geom_ribbon(alpha=0.4) + 
  xlab("Number of runs") + ylab("Predicted cost")
p


## Cost analysis looking at the cost difference upgrade - no upgrade
CI_diff_prop <- Comp_CI_diff_prop(e=c(sum(dat$failure[dat$treatment=="1.5"],na.rm = TRUE),
                                      sum(dat$failure[dat$treatment=="1.4"],na.rm = TRUE)),
                                  n= c(length(na.omit(dat$failure[dat$treatment=="1.5"])), 
                                       length(na.omit(dat$failure[dat$treatment=="1.4"]))),
                                  alpha=0.1) 
CI_diff_prop

nrun <- seq(1,3e+6,1000)
Cu <- 43000
cf <- 2.32
Cu/(cf*(pA-pB))
C <- tibble(nrun=nrun,
            cost_diff = Cu + cf*nrun*(pB-pA),
            cost_diff_low = Cu + cf*nrun*CI_diff_prop$LL_ARR,
            cost_diff_up = Cu + cf*nrun*CI_diff_prop$UL_ARR)

p <- ggplot(C,aes(x=nrun,y=cost_diff,min=cost_diff_low,ymax=cost_diff_up)) + 
  geom_line(color = "#C4961A") +  
  geom_ribbon(alpha=0.2,fill = "#C4961A") + 
  xlab("Number of runs") + ylab("Predicted cost difference in $") + 
  scale_color_hue(l=40, c=35)
p







