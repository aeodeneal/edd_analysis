""" EdD Quantitative Data Analysis """
""" Alexander Odeneal """



""" Import data and set up in useable format """
# sets working directory for this project. needs to be put in the console, doesn't seem to work here
setwd("/Users/aeodeneal/Library/CloudStorage/OneDrive-UniversityofStrathclyde/_Academic/01 EdD Strath/_THESIS/Data Generation/quant_data")

library(dplyr)

# import the data 
df11 <- read.csv('/Users/aeodeneal/Library/CloudStorage/OneDrive-UniversityofStrathclyde/_Academic/01 EdD Strath/_THESIS/Data Generation/quant_data/srqa11_all_data.csv', header = TRUE, sep = ',')
rownames(df11) <- df11$UI # sets UI as the index column, rather than arbitrary numbers
df11 <- data.frame(df11)


df10 <- read.csv('/Users/aeodeneal/Library/CloudStorage/OneDrive-UniversityofStrathclyde/_Academic/01 EdD Strath/_THESIS/Data Generation/quant_data/new_year10_all_data.csv', header = TRUE, sep = ',')
rownames(df10) <- df10$UI # sets UI as the index column, rather than arbitrary numbers
df10 <- data.frame(df10)

df10 <- df10 %>%
  rename(mean_targets = mean_target)

df <- rbind(df11, df10)
mean_target <- df$mean_target

df$ygroup <- ifelse(df$UI > 10999, "Year 11", "Year 10")
relocate(df, ygroup, .after = "UI")

""" Descriptive statistics """
# calculate motivation type score for each pupil, then RAI, then cbind to end of df11

### first round of data collection ###

# when calculating descriptive stats, include all of the data (e.g. stuff like mean, median for RAI). 
#do not remove blanks

# the next section of rows selects the appropriate questionnaire responses, and puts them in a vector
# then uses those to calculate a mean value by row 
avals1 <- select(df, c(A1a, A2a, A3a, A4a))
ameans1 = rowMeans(avals1, na.rm=TRUE)

evals1 <- select(df, c(E1a, E2a, E3a, E4a))
emeans1 = rowMeans(evals1, na.rm=TRUE)

nvals1 <- select(df, c(N1a, N2a, N3a, N4a))
nmeans1 = rowMeans(nvals1, na.rm=TRUE)

dvals1 <- select(df, c(D1a, D2a, D3a, D4a))
dmeans1 = rowMeans(dvals1, na.rm=TRUE)

ivals1 <- select(df, c(I1a, I2a, I3a, I4a))
imeans1 = rowMeans(ivals1, na.rm=TRUE)

# append the new set of values to the dataframe without creating a new dataframe 
df <- cbind(df, ameans1, emeans1, nmeans1, dmeans1, imeans1)

meansonly <- select(df, c(emeans1, nmeans1, dmeans1, imeans1)) # selects just the mean values
mot_type1 = colnames(meansonly)[max.col(meansonly)] # produces the col name for each row with the highest val
df <- cbind(df, mot_type1) # appends those colnames to the dataframe 

RAI1 = ((-2*emeans1) + (-1*nmeans1) + (1*dmeans1) + (2*imeans1))
round(RAI1, digits = 2)

df <- cbind(df, RAI1)

print("mean value for RAI1 is"); mean(RAI1, na.rm=TRUE)
print("range of values in RAI1 is (max, min):");range(RAI1, na.rm=TRUE)
print("standard deviation in RAI1 is:"); sd(RAI1,na.rm=TRUE)

### second round of data collection ###

avals2 <- select(df, c(A1b, A2b, A3b, A4b))
ameans2 <- rowMeans(avals2, na.rm=TRUE)

evals2 <- select(df, c(E1b, E2b, E3b, E4b))
emeans2 <- rowMeans(evals2, na.rm=TRUE)

nvals2 <- select(df, c(N1b, N2b, N3b, N4b))
nmeans2 <- rowMeans(nvals2, na.rm=TRUE)

dvals2 <- select(df, c(D1b, D2b, D3b, D4b))
dmeans2 <- rowMeans(dvals2, na.rm=TRUE)

ivals2 <- select(df, c(I1b, I2b, I3b, I4b))
imeans2 <- rowMeans(ivals2, na.rm=TRUE)

df <- cbind(df, ameans2, emeans2, nmeans2, dmeans2, imeans2)

meansonly2 <- select(df, c(emeans2, nmeans2, dmeans2, imeans2))
mot_type2 <- colnames(meansonly2)[max.col(meansonly2)]

df <- cbind(df, mot_type2)


RAI2 <- ((-2*emeans2) + (-1*nmeans2) + (1*dmeans2) + (2*imeans2))
round(RAI2, digits = 2)

df <- cbind(df, RAI2)

print("mean value for RAI2 is"); mean(RAI2, na.rm=TRUE)
print("range of values in RAI2 is (max, min):");range(RAI2, na.rm=TRUE)
print("standard deviation in RAI2 is:"); sd(RAI2,na.rm=TRUE)


### third round of data collection ###

avals3 <- select(df, c(A1c, A2c, A3c, A4c))
ameans3 <- rowMeans(avals3, na.rm=TRUE)

evals3 <- select(df, c(E1c, E2c, E3c, E4c))
emeans3 <- rowMeans(evals3, na.rm=TRUE)

nvals3 <- select(df, c(N1c, N2c, N3c, N4c))
nmeans3 <- rowMeans(nvals3, na.rm=TRUE)

dvals3 <- select(df, c(D1c, D2c, D3c, D4c))
dmeans3 <- rowMeans(dvals3, na.rm=TRUE)

ivals3 <- select(df, c(I1c, I2c, I3c, I4c))
imeans3 <- rowMeans(ivals3, na.rm=TRUE)

df <- cbind(df, ameans3, emeans3, nmeans3, dmeans3, imeans3)

meansonly3 <- select(df, c(emeans3, nmeans3, dmeans3, imeans3))
mot_type3 <- colnames(meansonly3)[max.col(meansonly3)]

df <- cbind(df, mot_type3)


RAI3 <- ((-2*emeans3) + (-1*nmeans3) + (1*dmeans3) + (2*imeans3))
round(RAI3, digits = 2)

df <- cbind(df, RAI3)

print("mean value for RAI3 is"); mean(RAI3, na.rm=TRUE)
print("range of values in RAI3 is (max, min):");range(RAI3, na.rm=TRUE)
print("standard deviation in RAI3 is:"); sd(RAI3,na.rm=TRUE)


""" Target Grade Affinity Indices """ 

tgvals1 <- df[,c("E5a", "N5a", "D5a", "I5a")] #selects the relevant columns from df
tgai1 <- ((-2*df$E5a) + (-1*df$N5a) + (df$D5a) + (2*df$I5a))
round(tgai1, digits=2)

print("mean value for TGAI1 is"); mean(tgai1, na.rm=TRUE)
print("range of values in TGAI1 is (max, min):");range(tgai1, na.rm=TRUE)
print("standard deviation in TGAI1 is:"); sd(tgai1,na.rm=TRUE)

tgvals2 <- df[,c("E5b", "N5b", "D5b", "I5b")]
tgai2 <- ((-2*df$E5b) + (-1*df$N5b) + (df$D5b) + (2*df$I5b))
round(tgai2, digits=2)

print("mean value for TGAI2 is"); mean(tgai2, na.rm=TRUE)
print("range of values in TGAI2 is (max, min):");range(tgai2, na.rm=TRUE)
print("standard deviation in TGAI2 is:"); sd(tgai2,na.rm=TRUE)

tgvals3 <- df[,c("E5c", "N5c", "D5c", "I5c")]
tgai3 <- ((-2*df$E5c) + (-1*df$N5c) + (df$D5c) + (2*df$I5c))
round(tgai3, digits=2)

print("mean value for TGAI3 is"); mean(tgai3, na.rm=TRUE)
print("range of values in TGAI3 is (max, min):");range(tgai3, na.rm=TRUE)
print("standard deviation in TGAI3 is:"); sd(tgai3,na.rm=TRUE)


df <- cbind(df, tgai1, tgai2, tgai3)


""" Visualisations """ 
# visualisations for descriptive statistics 
library(extrafont)
library(ggplot2)




# Data frame with motivation types
plot1 <- data.frame(mot_type1)
# Calculate the frequencies
motivation_counts <- as.data.frame(table(plot1))
colnames(motivation_counts) <- c("motivation_type", "frequency")
labels1 <- c("Identified", "External", "Intrinsic", "Introjected")

df_clean1 <- df %>% filter(!is.na(mot_type1))

df_proportion1 <- df_clean1 %>%
  group_by(ygroup, mot_type1) %>%
  summarise(count = n()) %>%
  group_by(ygroup) %>%
  mutate(proportion = count / sum(count))
         
ggplot(df_proportion1, aes(x = ygroup, y = proportion, fill = mot_type1)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent) +  # Convert y-axis to percentage
  labs(title = "Motivation type for students following first SRQ-A",
       x = "Year Group", 
       y = "Proportion") +
  scale_fill_manual(
    values = c("dmeans1" = "deepskyblue3", "emeans1" = "coral", "imeans1" = "darkseagreen4", "nmeans1" = "plum"),  # Custom colors
    labels = c("Identified", "Extrinsic", "Intrinsic", "Introjected")   # Custom legend labels
  ) +
  geom_text(aes(label = scales::percent(proportion, accuracy = 1)),  # Display proportion as percentage
            position = position_stack(vjust = 0.5),  # Place text in the middle of each segment
            color = "white") +  # Text color
  guides(fill = guide_legend(title = "Motivation Type")) +  # Change legend title
  theme_minimal()


# boxplot for RAI values 

ggplot(df, aes(x=ygroup, 
               y=RAI1, 
               fill=ygroup)) + 
  stat_boxplot(geom = 'errorbar') +
  geom_boxplot(show.legend=FALSE, 
               width=0.5) +
  scale_y_continuous(limits=c(-10,10)) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=4, show.legend = FALSE) +
  coord_flip()+
  labs(y = "Relative Autonomy Index - first SRQ-A",
       x = "Year Group") 





# Data frame with motivation types
df_clean2 <- df %>% filter(!is.na(mot_type2))

df_proportion2 <- df_clean2 %>%
  group_by(ygroup, mot_type2) %>%
  summarise(count = n()) %>%
  group_by(ygroup) %>%
  mutate(proportion = count / sum(count))

ggplot(df_proportion2, aes(x = ygroup, y = proportion, fill = mot_type2)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent) +  # Convert y-axis to percentage
  labs(title = "Motivation type for students following second SRQ-A",
       x = "Year Group", 
       y = "Proportion") +
  scale_fill_manual(
    values = c("dmeans2" = "deepskyblue3", "emeans2" = "coral", "imeans2" = "darkseagreen4", "nmeans2" = "plum"),  # Custom colors
    labels = c("Identified", "Extrinsic", "Intrinsic", "Introjected")   # Custom legend labels
  ) +
  geom_text(aes(label = scales::percent(proportion, accuracy = 1)),  # Display proportion as percentage
            position = position_stack(vjust = 0.5),  # Place text in the middle of each segment
            color = "white") +  # Text color
  guides(fill = guide_legend(title = "Motivation Type")) +  # Change legend title
  theme_minimal()

# code for producing box plot of RAI distributions for RAI2
ggplot(df, aes(x=ygroup, 
               y=RAI2, 
               fill=ygroup)) + 
  stat_boxplot(geom = 'errorbar') +
  geom_boxplot(show.legend=FALSE, 
               width=0.5, 
               ) +
  scale_y_continuous(limits=c(-10,10)) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=4, show.legend = FALSE) +
  coord_flip()+
  labs(y = "Relative Autonomy Index - second SRQ-A",
       x = "Year Group")



# Data frame with motivation types
df_clean3 <- df %>% filter(!is.na(mot_type3))

df_proportion3 <- df_clean3 %>%
  group_by(ygroup, mot_type3) %>%
  summarise(count = n()) %>%
  group_by(ygroup) %>%
  mutate(proportion = count / sum(count))

ggplot(df_proportion3, aes(x = ygroup, y = proportion, fill = mot_type3)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent) +  # Convert y-axis to percentage
  labs(title = "Motivation type for students following third SRQ-A",
       x = "Year Group", 
       y = "Proportion") +
  scale_fill_manual(
    values = c("dmeans3" = "deepskyblue3", "emeans3" = "coral", "imeans3" = "darkseagreen4", "nmeans3" = "plum"),  # Custom colors
    labels = c("Identified", "Extrinsic", "Intrinsic", "Introjected")   # Custom legend labels
  ) +
  geom_text(aes(label = scales::percent(proportion, accuracy = 1)),  # Display proportion as percentage
            position = position_stack(vjust = 0.5),  # Place text in the middle of each segment
            color = "white") +  # Text color
  guides(fill = guide_legend(title = "Motivation Type")) +  # Change legend title
  theme_minimal()


# code for producing box plot of RAI distributions for RAI3
ggplot(df, aes(x=ygroup, 
               y=RAI3, 
               fill=ygroup)) + 
  stat_boxplot(geom = 'errorbar') +
  geom_boxplot(show.legend=FALSE, 
               width=0.5) +
  scale_y_continuous(limits=c(-10,10)) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=4, show.legend = FALSE) +
  coord_flip()+
  labs(y = "Relative Autonomy Index - third SRQ-A",
       x = "Year Group") 


# box plots for TGAI for both year groups at each point in the year 

ggplot(df, aes(x=ygroup, 
               y=tgai1, 
               fill=ygroup)) + 
  stat_boxplot(geom = 'errorbar') +
  geom_boxplot(show.legend=FALSE, 
               width=0.5) +
  scale_y_continuous(limits=c(-10,10)) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=4, show.legend = FALSE) +
  coord_flip()+
  labs(y = "Target Grade Affinity Index - first SRQ-A",
       x = "Year Group") 

ggplot(df, aes(x=ygroup, 
               y=tgai2, 
               fill=ygroup)) + 
  stat_boxplot(geom = 'errorbar') +
  geom_boxplot(show.legend=FALSE, 
               width=0.5) +
  scale_y_continuous(limits=c(-10,10)) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=4, show.legend = FALSE) +
  coord_flip()+
  labs(y = "Target Grade Affinity Index - second SRQ-A",
       x = "Year Group") 

ggplot(df, aes(x=ygroup, 
               y=tgai3, 
               fill=ygroup)) + 
  stat_boxplot(geom = 'errorbar') +
  geom_boxplot(show.legend=FALSE, 
               width=0.5) +
  scale_y_continuous(limits=c(-10,10)) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=4, show.legend = FALSE) +
  coord_flip()+
  labs(y = "Target Grade Affinity Index - third SRQ-A",
       x = "Year Group") 



""" Inferential statistics """
# inferential statistics 

# when calculating t-tests for change over time, export a csv with everything done so far, create duplicate
# then use the duplicate to remove all the rows with any blanks in for calculations

library(dplyr)

# SPEARMANN RANK CORRELATION COEFFICIENT 
# RAI vs TGAI

cor(RAI1, tgai1, use = "pairwise.complete.obs", method="spearman") # 'use' arg omits NAs
cor(RAI2, tgai2, use = "pairwise.complete.obs", method="spearman")
cor(RAI3, tgai3, use = "pairwise.complete.obs", method="spearman")

RAI_mean_calc <- select(df, c("RAI1", "RAI2", "RAI3"))
RAI_mean <- rowMeans(RAI_mean_calc, na.rm=TRUE)
round(RAI_mean, digits=2)
df <- cbind(df, RAI_mean)

tgai_mean_calc
tgai_mean <- rowMeans(tgai_mean_calc, na.rm=TRUE)
df <- cbind(df, tgai_mean)

# spearman rank correlation coefficient 

cor(year11$RAI_mean, year11$tgai_mean, use = "pairwise.complete.obs", method="spearman")
summary(cor(year11$RAI_mean, year11$tgai_mean, use = "pairwise.complete.obs", method="spearman"))

cor(year10$RAI_mean, year10$tgai_mean, use = "pairwise.complete.obs", method="spearman")
summary(cor(year10$RAI_mean, year10$tgai_mean, use = "pairwise.complete.obs", method="spearman"))

ggplot(year11, aes(RAI_mean, tgai_mean)) +
  geom_point(alpha=0.5) +
  geom_smooth(method="lm", se=FALSE) +
  xlim(-8,10) +
  ylim(-5,8) +
  labs(x = "Mean Relative Autonomy Index (Y11)", y = "Mean Target Grade Affinity Index") 

ggplot(year10, aes(RAI_mean, tgai_mean)) +
  geom_point(alpha=0.5) +
  geom_smooth(method="lm", se=FALSE) +
  xlim(-8,10) +
  ylim(-5,8) +
  labs(x = "Mean Relative Autonomy Index (Y10)", y = "Mean Target Grade Affinity Index") 

lm(tgai_mean ~ RAI_mean, df)
summary(lm(tgai_mean ~ RAI_mean, df))
mean_value <- mean(tgai_mean, na.rm=TRUE)



year10 <- df[df$UI <= 11000, ]
year11 <- df[df$UI >= 10999, ]


# Ensure tgai_mean is numeric and remove NAs
year11$tgai_mean <- as.numeric(year11$tgai_mean)
year11 <- year11[!is.na(year11$tgai_mean), ]

# Calculate the mean value
mean_value11 <- mean(year11$tgai_mean, na.rm = TRUE)
mean_value10 <- mean(year10$tgai_mean, na.rm = TRUE)


# Plot the histogram
ggplot(year11, aes(x = tgai_mean)) + 
  geom_histogram(bins=17, color = "white", fill = "purple4", alpha = 0.7) + 
  geom_vline(aes(xintercept = mean_value11), color = "black", linetype = "dashed", size = 1) + 
  xlim(-8,8) +
  ylim(0,100) +
  labs(x = "Mean Target Grade Affinity Index (Year 11 Students)", y = "Density")

ggplot(year10, aes(x = tgai_mean)) + 
  geom_histogram(bins=17, color = "white", fill = "purple4", alpha = 0.7) + 
  geom_vline(aes(xintercept = mean_value10), color = "black", linetype = "dashed", size = 1) + 
  xlim(-8,8) +
  ylim(0,100) +
  labs(x = "Mean Target Grade Affinity Index (Year 10 Students)", y = "Density")



# TGAI vs mean target 

tgai_mean_calc <- select(df, c("tgai1", "tgai2", "tgai3"))
tgai_mean <- rowMeans(tgai_mean_calc, na.rm=TRUE)
round(tgai_mean, digits=2)
df <- cbind(df, tgai_mean)

year10 <- df[df$UI <= 11000, ]
year11 <- df[df$UI >= 10999, ]

cor(year11$mean_targets, year11$tgai_mean, use = "pairwise.complete.obs", method="spearman")
cor(year10$mean_targets, year10$tgai_mean, use = "pairwise.complete.obs", method="spearman")


ggplot(year11, aes(tgai_mean, year11$mean_targets)) +
  geom_point(alpha=0.5) +
  geom_smooth(method="lm", se=FALSE) +
  xlim(-6,8) +
  ylim(3,10) +
  labs(x = "Mean Target Grade Affinity Index (Y11)", y = "Mean Target Grade (all subjects)") 

ggplot(year10, aes(tgai_mean, year10$mean_targets)) +
  geom_point(alpha=0.5) +
  geom_smooth(method="lm", se=FALSE) +
  xlim(-6,8) +
  ylim(3,10) +
  labs(x = "Mean Target Grade Affinity Index (Y10)", y = "Mean Target Grade (all subjects)") 

# SINGLE LINEAR REGRESSIONS

summary(lm(tgai_mean ~ RAI_mean, df)) # lm = linear model; args: y ~ x, datasource

lm(tgai_mean ~ mean_targets, df)
summary(lm(tgai_mean ~ mean_targets, df))

# HYPOTHESIS TESTING FOR HYPOTHESES 3-4
# Note this requires only including rows that have data in SRQA1 and SRQA3

df_no_na <- df[!is.na(df$RAI1)&!is.na(df$RAI3),] # removes any RAI1 or RAI3 rows with NAs in them
# everything is before the , so acts on rows
# ! removes any rows with 'is.na' that applies to df$RAI1 etc


# multiple linear regression 

lm(tgai_mean ~ mean_targets + RAI_mean, df)
summary(lm(tgai_mean ~ mean_targets + RAI_mean, df))

# Hypothesis 3: H0 = TGAI does not change through year; H1 = TGAI increases 

diff <- df$tgai3-df$tgai1
ggplot(df, aes((tgai3-tgai1))) +
  geom_histogram(bins=10)

hyp_test_3 <- t.test(df_no_na$tgai1, 
                     df_no_na$tgai3, 
                     alternative="greater", 
                     mu=0,
                     paired=TRUE)
hyp_test_3

# Hypothesis 4: H0: RAI does not change through year; H1 = RAI increases 

diff <- df$rai3-df$rai1
ggplot(df, aes((RAI3-RAI1))) +
  geom_histogram(bins=10)

hyp_test_4 <- t.test(df_no_na$RAI1, 
                     df_no_na$RAI3, 
                     alternative="greater", 
                     mu=0,
                     paired=TRUE)
hyp_test_4


# change in motivation types

df_mot_type <- df[!is.na(df$mot_type1)&!is.na(df$mot_type3),] 
#above line creates df but removes any rows for students that didn't participate in first AND last SRQ-A

cont_table <- table(df$mot_type1, df$mot_type3)
install.packages("DescTools")
library(DescTools)
StuartMaxwellTest(cont_table)

# CRONBACH ALPHA - INTERNAL CONSISTENCY

df_cron <- df

# step 1: multiply all responses by appropriate weights. E=-2, N=-1, D=1, I=2. Same as RAI

# extrinsic motivation responses - all three SRQAs
df_cron$E1a_t <- df_cron$E1a *-2; df_cron$E2a_t <- df_cron$E2a *-2; df_cron$Eea_t <- df_cron$E3a *-2; df_cron$E4a_t <- df_cron$E4a *-2; df_cron$E5a_t <- df_cron$E5a *-2

df_cron$E1b_t <- df_cron$E1b *-2; df_cron$E2b_t <- df_cron$E2b *-2; df_cron$Eeb_t <- df_cron$E3b *-2; df_cron$E4b_t <- df_cron$E4b *-2; df_cron$E5b_t <- df_cron$E5b *-2

df_cron$E1c_t <- df_cron$E1c *-2; df_cron$E2c_t <- df_cron$E2c *-2; df_cron$Eec_t <- df_cron$E3c *-2; df_cron$E4c_t <- df_cron$E4c *-2; df_cron$E5c_t <- df_cron$E5c *-2



# introjected motivation responses - all three SRQAs
df_cron$N1a_t <- df_cron$N1a *-1; df_cron$N2a_t <- df_cron$N2a *-1; df_cron$N3a_t <- df_cron$N3a *-1; df_cron$N4a_t <- df_cron$N4a *-1; df_cron$N5a_t <- df_cron$N5a *-1

df_cron$N1b_t <- df_cron$N1b *-1; df_cron$N2b_t <- df_cron$N2b *-1; df_cron$N3b_t <- df_cron$N3b *-1; df_cron$N4b_t <- df_cron$N4b *-1; df_cron$N5b_t <- df_cron$N5b *-1

df_cron$N1c_t <- df_cron$N1c *-1; df_cron$N2c_t <- df_cron$N2c *-1; df_cron$N3c_t <- df_cron$N3c *-1; df_cron$N4c_t <- df_cron$N4c *-1; df_cron$N5c_t <- df_cron$N5c *-1



# identified motivation responses - all three SRQAs
df_cron$D1a_t <- df_cron$D1a *1; df_cron$D2a_t <- df_cron$D2a *1; df_cron$Dea_t <- df_cron$D3a *1; df_cron$D4a_t <- df_cron$D4a *1; df_cron$D5a_t <- df_cron$D5a *1

df_cron$D1b_t <- df_cron$D1b *1; df_cron$D2b_t <- df_cron$D2b *1; df_cron$Deb_t <- df_cron$D3b *1; df_cron$D4b_t <- df_cron$D4b *1; df_cron$D5b_t <- df_cron$D5b *1

df_cron$D1c_t <- df_cron$D1c *1; df_cron$D2c_t <- df_cron$D2c *1; df_cron$Dec_t <- df_cron$D3c *1; df_cron$D4c_t <- df_cron$D4c *1; df_cron$D5c_t <- df_cron$D5c *1


# intrinsic motivation responses - all three SRQAs
df_cron$I1a_t <- df_cron$I1a *2; df_cron$I2a_t <- df_cron$I2a *2; df_cron$Iea_t <- df_cron$I3a *2; df_cron$I4a_t <- df_cron$I4a *2; df_cron$I5a_t <- df_cron$I5a *2

df_cron$I1b_t <- df_cron$I1b *2; df_cron$I2b_t <- df_cron$I2b *2; df_cron$Ieb_t <- df_cron$I3b *2; df_cron$I4b_t <- df_cron$I4b *2; df_cron$I5b_t <- df_cron$I5b *2

df_cron$I1c_t <- df_cron$I1c *2; df_cron$I2c_t <- df_cron$I2c *2; df_cron$Iec_t <- df_cron$I3c *2; df_cron$I4c_t <- df_cron$I4c *2; df_cron$I5c_t <- df_cron$I5c *2


install.packages("ltm")
library(ltm)
cronbach.alpha(df_cron[,140:164], na.rm=TRUE)
cronbach.alpha(df_cron[,115:139], na.rm=TRUE)
cronbach.alpha(df_cron[,90:114], na.rm=TRUE)



