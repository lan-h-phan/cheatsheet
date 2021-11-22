####################################
# 1a. Assumptions: Constant Variance
####################################

# Check the constant variance assumption. This
# assumption asserts that the group variances 
# in the population are identical for each of the
# experimental groups. We will check this assumption
# in general by three methods. 1. Compute estimated
# group variances and compute the ratio of the 
# largest to the smallest. 2. Examine IQRs via 
# parallel boxplots. 3. Run a statistical test
# such as Levene's test of the null hypothesis
# that the group variances are identical.

# 0. Fit the full and restricted models.
lmF <- lm(bdi_EOT ~ ConditionF, data = dat2) #Confition F is the factor version of the categorical variable 
lmR <- lm(bdi_EOT ~ 1, data = dat2) #bdi_EOT is smoking end of trial

# 1. Compute estimated group variances.
by(data = dat2$bdi_EOT,
   INDICES = dat2$ConditionF,
   FUN = var, na.rm = TRUE)

# Compare to rule of thumb from p. 155 in MDK.
# Need group sample sizes.
table(dat2$Condition)
dat3 <- select(dat2, ConditionF, bdi_EOT)
dat3 <- na.omit(dat3)
table(dat3$ConditionF)

(mdk_ratio <- (109/56)*(129.2/46.3))
# Since the value of the ratio is larger than 4, 
# the rule of thumb suggests heterogeneity of 
# variance is problematic here.

# 2. Examine the boxplot of outcome by group.
boxplot(dat2$bdi_EOT ~ dat2$ConditionF,
        xlab = "Condition",
        ylab = "BDI Score at EOT")

# 3. Levene's test
library(car)
leveneTest(lmF)
# If Levene's test is significant then the assumption of constant variance is violated

Another test of the same null hypothesis that is 
# typically more powerful is called Bartlett's test.
bartlett.test(x = dat2$cig_EOT,
              g = dat2$ConditionF)

# Indeed, Bartlett's test rejects the null hypothesis
# of equal variances here. Thus, we should be cautious
# about interpreting the results of one-way ANOVA.
# Nevertheless, we will continue because, after all, 
# the goal of today is to run one-way ANOVA.

# Run the ANOVA omnibus test via general linear F test
# through the anova() function.
# Omnibus test: are there main difference across?
anova(lmR, lmF)

# Can implement the Welch correction by using the 
# oneway.test function with var.equal = FALSE.
oneway.test(formula = bdi_EOT ~ ConditionF,
            data = dat2, 
            na.action = "na.omit",
            var.equal = FALSE)

# Note that when using Welch's test, the p-value is .14, 
# which is about 3 times as large as the p-value from the
# unadjusted one-way ANOVA.


#############################
# 1b. Assumptions: Normality
#############################
# Check the normality assumption. This assumption 
# asserts that the distribution of the outcome (or, 
# identicall, the residuals) is normal within each
# experimental group and that the residuals, in 
# aggregate, are normally distributed. We can check 
# this by making group-specific histograms and by 
# examining a QQ plot of residuals.

# 1. Group-specific histograms.
# In base R:
# Now put all in same plot.
par(mfrow = c(1, 3))
hist(dat2$bdi_EOT[which(dat2$ConditionF == "SCBSCT")],
     breaks = 20)
hist(dat2$bdi_EOT[which(dat2$ConditionF == "SCBSCT-BA")],
     breaks = 20)
hist(dat2$bdi_EOT[which(dat2$ConditionF == "WL")],
     breaks = 20)
par(mfrow = c(1, 1))

# With ggplot2
ggplot(dat2, aes(x = bdi_EOT)) + 
  geom_histogram(bins = 20, color = "black", fill = "white") + 
  facet_wrap(~ConditionF)

# Create a QQ plot using the qqPlot() function in package car.
qqPlot(lmF)
# The assumptions of normality is violated when the blackdots don't fall within the blue shaded area

#Run the Shapiro test of normality
shapiro.test(x=dat3$bdi_EOT[dat3$ConditionF == "SCBSCT"])
shapiro.test(x=dat3$bdi_EOT[dat3$ConditionF == "SCBSCT-BA"])
shapiro.test(x=dat3$bdi_EOT[dat3$ConditionF == "WL"])
levels(dat3$ConditionF)


# In this case, it seems like both the constant variance
# and normality assumptions are violated. Recall, that 
# in cases where there are unequal group sizes, if the 
# smaller group is paired with the larger variance, the 
# ANOVA F test p-value will be too small. Here, we should
# not trust the p-value based on the one-way ANOVA because 
# of this pattern of violation.

#####################
# 2. One-way ANOVA
#####################

########################
# 3. Pairwise Contrasts
########################
# To run pairwise contrasts, we need to set up
# and test contrasts of the following form:
# psi1 = 1 mu1 - 1 mu2 + 0 mu3
# psi2 = 1 mu1 + 0 mu2 - 1 mu3
# psi3 = 0 mu1 + 1 mu2 - 1 mu3

# This can be done automatically using the emmeans 
# package. Install it if you don't have it already.
library(emmeans)
emm1 <- emmeans(object = lmF, #full model
                specs = ~ ConditionF) #breakout means by treatment group #right hand side of an equation
emm1
pairs(emm1, adjust = "none")

# Let's double check the calculations "by hand" for the
# second comparison of SCBSCT vs WL.
(psi_2_hat <- 1*mns[1] + 0*mns[2] - 1*mns[3])

# Get group sample sizes after deleting missing data.
table(dat2$ConditionF, is.na(dat2$bdi_EOT))

# Calculate the F statistic (and t statistic)
(F_obs <- (psi_2_hat^2 / (1^2/79 + (0)^2/95 + (-1)^2/46)) / 66.35)
t_obs <- (psi_2_hat / sqrt((1^2/79 + (0)^2/95 + (-1)^2/46))) / sqrt(66.35)

# Determine the p-value
(p_valF <- pf(q = F_obs, df1 = 1, df2 = 217, lower.tail = FALSE))
2*(p_valt <- pt(q = t_obs, df = 217, lower.tail = TRUE))

#######
# 5. Complex Contrasts
#######
# Test the null hypothesis that the average of the two 
# CBT groups have a lower mean BDI score at EOT as compared
# with the WL control group.
# psi4 = 1/2 mu1 + 1/2 mu2 - 1 mu3

# Again, this may be tested automatically in emmeans.
psi4 <- contrast(emm1, 
                 list(psi4 = c(1/2, 1/2, -1)))
psi4    
pt(-2.497, df = 217, lower.tail = TRUE)
# Check "by hand".
(psi_4_hat <- 0.5*mns[1] + 0.5*mns[2] - 1*mns[3])

# t statistic
(t_obs4 <- (psi_4_hat / sqrt((.5^2/79 + (.5)^2/95 + (-1)^2/46))) / sqrt(66.35))

# Determine the p-value
# The alternative hypothesis here is that the average BDI score
# of CBT groups is LESS THAN the mean WL control group score.
# Thus, we want the area in the lower tail for our p-value.
(p_val4 <- pt(q = t_obs4, df = 217, lower.tail = TRUE))
