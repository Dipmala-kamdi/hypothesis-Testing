
##########################Cutlets.mtw problemtatement ################################

cutlets <- read.csv("D:/excelr_DS/assignment/hypothesis Testing/Cutlets.csv")
View(cutlets)

attach(cutlets)

### Normality test 

shapiro.test(Unit.A)  #p-value = 0.32 >0.05 => follows normal distribution (accept null hypothesis)

shapiro.test(Unit.B)  #p-value = 0.5225 >0.05 => follows normal distribution (accept null hypothesis)

### Variance test

var.test(Unit.A, Unit.B)  #p-value = 0.3136>0.05 => equal variance

###   2 sample t test

t.test(Unit.A,Unit.B,alternative = "two.sided",conf.level = 0.95,correct = TRUE)
# mean is (7.019 & 6.964)
# null Hypothesis -> Equal means (7.019 & 6.964)
# Alternate Hypothesis -> Unequal Hypothesis
# p-value = 0.4723 > 0.05 accept null Hypothesis 

t.test(Unit.A,Unit.B,alternative = "greater",var.equal = T)

########################## LabTaT problemtatement ################################

lab <- read.csv("D:/excelr_DS/assignment/hypothesis Testing/LabTAT.csv")
View(lab)

attach(lab)

stack_lab <- stack(lab)
View(stack_lab)
attach(stack_lab)

###Normality test
library(nortest)
ad.test(stack_lab$values) ### p-value=0.05072 => follows normal distribution

### Variance test 
library(car)
#Test for equal Variance
leveneTest(stack_lab$values~stack_lab$ind, data = stack_lab)   

### One-way Anova 
Anova_results <- aov(values~ind,data = stack_lab)
summary(Anova_results)
# p-value = 2e-16 < 0.05 reject null hypothesis 

########################## buyer ratio problem statement ################################

buyer <- read.csv("D:/excelr_DS/assignment/hypothesis Testing/BuyerRatio.csv")
View(buyer)

summary(buyer)
str(buyer)

#stack(buyer)

data <- as.table(as.matrix(buyer))
#chisq.test(data)

chisq.test(buyer[-1])

# p-value= 0.6603 > 0.05 accept NULL hypothesis

########################## CustomerOrderForm problem statement ################################

cust <- read.csv("D:/excelr_DS/assignment/hypothesis Testing/Costomer+OrderForm.csv")
View(cust)

attach(cust)
colnames(cust)
table1 <- table(Phillippines,Indonesia,Malta,India)
table1
?prop.test
prop.test(x=c(20,206),n=c(480,740),conf.level = 0.95,correct = FALSE,alternative = "two.sided")

prop.test(x=c(20,206),n=c(480,740),conf.level = 0.95,correct = FALSE,alternative = "less")

# p-value= 2.2e-16 < 0.05 reject NULL hypothesis

##########################Fantaloons problemtatement ################################

fantaloons <- read.csv("D:/excelr_DS/assignment/hypothesis Testing/Faltoons.csv")
View(fantaloons)
attach(fantaloons)
table2 <- table(Weekdays,Weekend)
table2
?prop.test
prop.test(x=c(66,47),n=c(167,120),conf.level = 0.95,correct = FALSE,alternative = "two.sided")

prop.test(x=c(66,47),n=c(167,120),conf.level = 0.95,correct = FALSE,alternative = "less")

# p-value= 0.5242> 0.05 accept NULL hypothesis
