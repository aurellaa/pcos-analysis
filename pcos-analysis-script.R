
# loading in the necessary packages
library(ggplot2)
library(dplyr)
library(cowplot)
library(readxl)
library(effsize)

# loading in the dataset
pcos_data <- read_excel("C:/Users/Christine Ong/Documents/pcos-analysis/PCOS_data_without_infertility.xlsx", sheet = 2)

# first looks at the dataset
head(pcos_data)
colnames(pcos_data)

# removing the spaces from the variable names to prevent coding errors
colnames(pcos_data) <- gsub(" ", "_", colnames(pcos_data))
colnames(pcos_data)

# only taking a subset of variables
pcos_data_subset <- pcos_data |>
  select(`PCOS_(Y/N)`,
         `Age_(yrs)`,
         `Weight_(Kg)`,
         `Height(Cm)`,
         BMI,
         `Cycle(R/I)`,
         `FSH(mIU/mL)`,
         `LH(mIU/mL)`,
         `FSH/LH`,
         `TSH_(mIU/L)`,
         `AMH(ng/mL)`,
         `PRL(ng/mL)`,
         `PRG(ng/mL)`,
         )

colnames(pcos_data_subset)

# recoding variables to suit the analysis
# renaming some of the column names for convenience purposes
pcos_df <- pcos_data_subset |> rename(pcos = `PCOS_(Y/N)`,
                           age = `Age_(yrs)`,
                           weight = `Weight_(Kg)`,
                           height = `Height(Cm)`,
                           bmi = BMI,
                           cycle = `Cycle(R/I)`,
                           fsh = `FSH(mIU/mL)`,
                           lh = `LH(mIU/mL)`,
                           fsh_lh_ratio = `FSH/LH`,
                           tsh = `TSH_(mIU/L)`,
                           amh = `AMH(ng/mL)`,
                           prl = `PRL(ng/mL)`,
                           prg = `PRG(ng/mL)`,
                           )
glimpse(pcos_df)

##############################################################################################


# correcting the data types of each variable
str(pcos_df)

pcos_df <- pcos_df |> 
  mutate(
    pcos = as.factor(pcos),  # Convert PCOS (Yes/No) to categorical
    cycle = as.factor(cycle),  # Regular/Irregular as categorical
    age = as.numeric(age),
    weight = as.numeric(weight),
    height = as.numeric(height),
    bmi = as.numeric(bmi),
    fsh = as.numeric(fsh),
    lh = as.numeric(lh),
    fsh_lh_ratio = as.numeric(fsh_lh_ratio),
    tsh = as.numeric(tsh),
    amh = as.numeric(amh),
    prl = as.numeric(prl),
    prg = as.numeric(prg),
  )

# an error was thrown. let's see what's happening...

pcos_df |> filter(is.na(as.numeric(amh)))

# unfortunately there seem to be some typographical errors in the data.
# for instance, row 307 amh is listed as "a". we should omit that.

# checking for any other NAs
colSums(is.na(pcos_df))

pcos_df <- na.omit(pcos_df)


#################################################################################################

# let's convert the binary variables values into being words instead
# of numerical values, for clarity.

# changing pcos values such that 0 = No, 1 = Yes
## checking for any possible data input errors
table(pcos_df$pcos)
pcos_df <- pcos_df |>
  mutate(pcos = ifelse(pcos == 0, "No", "Yes"))
table(pcos_df$pcos)

# changing cycle values such that 2 = "regular" and 4 = "irregular"
## checking for any possible data input errors
table(pcos_df$cycle)
## it seems that there is mistakenly one "5". we will get rid of this observation
pcos_df <- pcos_df |> filter(cycle != 5)
table(pcos_df$cycle)

# pcos_df will now be down to 539 observations

pcos_df <- pcos_df |> 
  mutate(cycle = ifelse(cycle == 2, "Regular", "Irregular"))

glimpse(pcos_df)

pcos_df <- pcos_df |> 
  mutate(
    pcos = as.factor(pcos),  # Convert PCOS (Yes/No) to categorical
    cycle = as.factor(cycle),  # Regular/Irregular as categorical
    age = as.numeric(age),
    weight = as.numeric(weight),
    height = as.numeric(height),
    bmi = as.numeric(bmi),
    fsh = as.numeric(fsh),
    lh = as.numeric(lh),
    fsh_lh_ratio = as.numeric(fsh_lh_ratio),
    tsh = as.numeric(tsh),
    amh = as.numeric(amh),
    prl = as.numeric(prl),
    prg = as.numeric(prg),
  )

pcos_df # the data is now clean and ready for analysis

##############################################################################################

# summary stats
summary(pcos_df)

##############################################################################################

# statistical testing

## research q.1 : Does BMI Differ Between Women With and Without PCOS? 

### first we want to visualise the data to ensure normal distribution

#### global theme
theme_set(
  theme_classic()
)

#### density plot

ggplot(pcos_df, aes(x=bmi, color = pcos)) +
  geom_density(lwd = 1) +
  labs(x = "BMI", y = "Density", title = "BMI Distribution by PCOS Status", color = "PCOS") +
  scale_color_manual(values = c("No" = "steelblue1", "Yes" = "sienna2"))

#### histogram faceted by pcos
ggplot(pcos_df, aes(x = bmi, fill = pcos)) +
  geom_histogram(alpha = 0.7, position = "identity", bins = 30) +
  facet_wrap(~ pcos) +
  labs(x = "BMI", y = "Count", fill = "PCOS", title = "BMI Distribution by PCOS Status") +
  scale_fill_manual(values = c("No" = "steelblue1", "Yes" = "sienna2"))

# checking for normality or non-normality with the shapiro wilk test
# if p < 0.05, the group is not normally distributed

shapiro.test(pcos_df$bmi[pcos_df$pcos == "No"])   # no pcos group
# W = 0.99272, p-value = 0.07472
# it is normally distributed!

shapiro.test(pcos_df$bmi[pcos_df$pcos == "Yes"])  # has pcos group
# W = 0.9839, p-value = 0.05312
# this is just barely above 0.05, but n is much larger than 30 so its reasonable for us to do a t-test


# welch t-test

t.test(bmi ~ pcos, data = pcos_df, var.equal = FALSE)

# from the results of the t-test...
# pvalue is < 0.05, meaning the difference in bmi between women with and without PCOS is statistically significant
# we reject the null hypothesis and conclude that bmi is significantly different between groups. yay!

# mean bmi for women with pcos is 23.75, and for women without pcos, it's 25.47.
# this suggests that on average, women with pcos have a higher bmi.

# 95% confidence interval of (-2.48, -0.96) means that we are 95% confident that the
# true difference in the means falls between -2.48 and -0.96. 
# and since the entire CI is negative and doesnt include 0, we can confirm that
# bmi is higher in pcos group.

# pvalue tells us that the difference is statistically significant-- but is the different meaningful in practice?
# let's do a cohen's d test!
cohen.d(pcos_df$bmi ~ pcos_df$pcos)

# the effect size is small (but almost moderate)

# visualising the difference
ggplot(pcos_df, aes(x = pcos, y = bmi, fill = pcos)) +
  geom_boxplot() +
  labs(title = "BMI Distribution by PCOS Status", x = "PCOS", y = "BMI", fill = "PCOS") +
  scale_fill_manual(values = c("No" = "steelblue1", "Yes" = "sienna2"))
# pcos group has a higher median BMI compared to the non pcos group.
# there are outliers in both groups

## final conclusion:
# women with pcos tend to have higher bmi compared to those without pcos, 
# but the effect size suggests the difference is small in practice.


##############################################################################################

## research q.2 : Are Irregular Periods Associated With Higher PCOS Risk?

# both variables are categorical. let's do a chi square test of independence.

# creating a contingency table
table(pcos_df$pcos, pcos_df$cycle)
# chi square test of independence assumes that the expected counts should be >= 5
# and thankfully they all are so we can continue

chisq.test(pcos_df$pcos, pcos_df$cycle)

# X-squared = 84.189, df = 1, p-value < 2.2e-16
# there is strong evidence that women with irregular periods 
# have a significantly higher likelihood of having PCOS compared to those with regular cycles.

# bar graph
ggplot(pcos_df, aes(x = cycle, fill = pcos)) +
  geom_bar(position = "fill", color = "black") +
  labs(x = "Menstrual Cycle Regularity", y = "Proportion", fill = "PCOS",
       title = "Proportion of Regular vs. Irregular Cycles by PCOS Status") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("No" = "steelblue1", "Yes" = "sienna2"))

# interpreting that bar graph:
# majority of women with irregular cycles have PCOS
# most women with regular cycles do not have PCOS, but there IS still a small proportion of people that do.

############################################################################################################

## research q.3 : Do Hormone Levels Differ Between PCOS and Non-PCOS Women?

hormones <- c("fsh", "lh", "fsh_lh_ratio", "tsh", "amh", "prl", "prg")

for (hormone in hormones) {
  print(hormone)
  print(shapiro.test(pcos_df[[hormone]]))
}

# fsh: W = 0.02379, p-value < 2.2e-16
# lh: W = 0.027288, p-value < 2.2e-16
# fsh-lh ratio: W = 0.049997, p-value < 2.2e-16
# tsh: W = 0.4139, p-value < 2.2e-16
# amh: W = 0.72465, p-value < 2.2e-16
# prl: W = 0.82024, p-value < 2.2e-16
# prg: W = 0.051056, p-value < 2.2e-16

# all p-values are < 0.05. none of the hormone levels are normally distributed.

# non-parametric test - the mann whitney u test
wilcoxon_results <- lapply(hormones, function(hormone) {
  test <- wilcox.test(pcos_df[[hormone]] ~ pcos_df$pcos)
  return(data.frame(Hormone = hormone, W = test$statistic, p_value = test$p.value))
})

wilcoxon_results_df <- do.call(rbind, wilcoxon_results)
print(wilcoxon_results_df)

# from the results of the mann whitney u test, it seems that
# fsh, fsh/lh ratio, amh,  are significant -- while lh, tsh, prl, and prg are not.

significant_hormones <- c("fsh", "fsh_lh_ratio", "amh")


# effect size cliffs delta
cliff.delta(pcos_df$fsh ~ pcos_df$pcos)
cliff.delta(pcos_df$fsh_lh_ratio ~ pcos_df$pcos)
cliff.delta(pcos_df$amh ~ pcos_df$pcos)

# pretty weak effect sizes...

# visualising the data in a boxplot

## right now each hormone is in a seperate column which isnt good
## reshaping the data for ggplot
pcos_long <- reshape2::melt(pcos_df, id.vars = "pcos", measure.vars = significant_hormones)

head(pcos_long)

# boxplot
ggplot(pcos_long, aes(x = pcos, y = value, fill = pcos)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Significant Hormone Levels by PCOS Status",
       x = "PCOS Status", y = "Hormone Level") +
  scale_fill_manual(values = c("No" = "steelblue1", "Yes" = "sienna2"))

## oooookay so there are some really extreme outliers throwing things off
# lets remove the outliers

# function for removing outliers
remove_outliers <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  x[x > (q3 + 1.5 * iqr)] <- NA  # Remove extreme values
  x[x < (q1 - 1.5 * iqr)] <- NA
  return(x)
}

# applying that function to our significant hormones
pcos_df$filtered_fsh <- remove_outliers(pcos_df$fsh)
pcos_df$filtered_fsh_lh_ratio <- remove_outliers(pcos_df$fsh_lh_ratio)
pcos_df$filtered_amh <- remove_outliers(pcos_df$amh)

glimpse(pcos_df)

pcos_long_reshape <- reshape2::melt(pcos_df, id.vars = "pcos",
                            measure.vars = c("filtered_fsh", "filtered_fsh_lh_ratio", "filtered_amh"))

head(pcos_long_reshape)

# just making sure results of the wilcoxon are similar..
wilcox.test(pcos_df$filtered_fsh ~ pcos_df$pcos)
wilcox.test(pcos_df$filtered_fsh_lh_ratio ~ pcos_df$pcos)
wilcox.test(pcos_df$filtered_amh ~ pcos_df$pcos)

# thankfully they are still significant!
# removing outliers actually made the results better.


# checking cliff's delta again
cliff.delta(pcos_df$filtered_fsh ~ pcos_df$pcos)
cliff.delta(pcos_df$filtered_fsh_lh_ratio ~ pcos_df$pcos)
cliff.delta(pcos_df$filtered_amh ~ pcos_df$pcos)

# yeah still small

#visualising the data
ggplot(pcos_long_reshape, aes(x = pcos, y = value, fill = pcos)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +  # Hide outliers
  facet_wrap(~variable, scales = "free") +
  labs(title = "Hormone Levels by PCOS Status (Outliers Removed)",
       x = "PCOS Status", y = "Hormone Level", fill = "PCOS") +
  scale_fill_manual(values = c("No" = "steelblue1", "Yes" = "sienna3"))

# results and conclusion:
## fsh lower in women with pcos than women without pcos
## fsh/lh ratio is lower in women with pcos than without pcos
## amh is higher in women with pcos

# however small-to-negligible effect sizes suggest that these differences may
# have limited clinical relevance.
