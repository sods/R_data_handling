########## Week 6 R script - BMS109 #############
#
####### Marta Milo 4th November 2019 #############

# Prepare work space to use ggplot2
library(ggplot2)
library(cowplot)
library(dplyr)

# add iris to your workspace
data(iris)

# Load data from file 
blood_data <- read.csv("Blood_osmosity.csv",stringsAsFactors = TRUE) 

#### BOX PLOT with ggpplot
ggplot_iris1<-ggplot(iris, aes(x = Species, y = Petal.Length/Petal.Width, fill = Species)) + 
  geom_boxplot() + 
  labs(x = "Species", y = "Petal Eccentricty", fill= "Species") +
  theme_classic()

ggplot_iris2<-ggplot(iris, aes(x = Species, y = Sepal.Length/Sepal.Width, fill=Species)) + 
  geom_boxplot() + 
  labs(x = "Species", y = "Sepal Eccentricty") +
  theme_classic()

plot_grid(ggplot_iris1, ggplot_iris2, labels = c("a)", "b)"))


# box plot for blood data
ggplot_bd1<-ggplot(blood_data, aes(x = Group, y = sample.A, fill = Group)) + 
  geom_boxplot() + 
  labs(x = "Group", y = "Blood Osmolarity", fill= "Group") +
  theme_classic()

ggplot_bd2<-ggplot(blood_data, aes(x = Group, y = sample.B, fill = Group)) + 
  geom_boxplot() + 
  labs(x = "Group", y = "Blood Osmolarity", fill= "Group") +
  theme_classic()
plot_grid(ggplot_bd1, ggplot_bd2, labels = c("a)", "b)"))

##### bar plots





# Using the mean

# step 1 no dplyr
sampleA.mean <- summarise(group_by(blood_data, Group), mean.SampleA = mean(sample.A))
sampleB.mean <- summarise(group_by(blood_data, Group), mean.SampleB = mean(sample.B))

mean.blood_data <- data.frame(blood_data$Group, sampleA.mean$mean.SampleA, sampleB.mean$mean.SampleB)
mean.blood_data


# step 1
bdA_stats <- 
  blood_data %>%
  group_by(Group) %>% 
  summarise(mean_bdA = mean(sample.A))

# step 2 
bd_sA<-ggplot(bdA_stats, aes(x = Group, y = mean_bdA)) + 
  geom_col(fill="lightblue") + 
  labs(y = "Blood Osmolarity (mg)", title = "Sample A")+
  theme_classic()


bdB_stats <- 
  blood_data %>%
  group_by(Group) %>% 
  summarise(mean_bdB = mean(sample.B))

# step 2 
bd_sB<-ggplot(bdB_stats, aes(x = Group, y = mean_bdB)) + 
  geom_col(fill="lightgreen") + 
  labs(y = "Blood Osmolarity (mg)", title = "Sample B")+
  theme_classic()

plot_grid(bd_sA, bd_sB, labels = c("a)", "b)"))

####### Adding error bars
bdA_stats <- 
  blood_data %>%
  group_by(Group) %>% 
  summarise(mean_bdA = mean(sample.A), 
            se = sd(sample.A) / sqrt(n())) # <- New calculation

bdB_stats <- 
  blood_data %>%
  group_by(Group) %>% 
  summarise(mean_bdB = mean(sample.B),
            se = sd(sample.A) / sqrt(n()))


####### plotting the data
bd_sA<-ggplot(bdA_stats, 
       aes(x = Group, y = mean_bdA, 
           ymin = mean_bdA - se, ymax = mean_bdA + se)) + 
  geom_col(fill = "lightblue", width = 0.7) + 
  geom_errorbar(width = 0.25) + 
  labs(y = "Blood Osmolarity (mg)", title = "Sample A")+
  theme_classic()

bd_sB<-ggplot(bdB_stats, 
              aes(x = Group, y = mean_bdB, 
                  ymin = mean_bdB - se, ymax = mean_bdB + se)) + 
  geom_col(fill = "lightblue", width = 0.7) + 
  geom_errorbar(width = 0.25) + 
  labs(y = "Blood Osmolarity (mg)", title = "Sample B")+
  theme_classic()

plot_grid(bd_sA, bd_sB, labels = c("a)", "b)"))

# histograms for blood data
ggplot(blood_data, aes(x=sample.A, color=Group)) +
  geom_histogram(fill="white", binwidth = 10)
# Overlaid histograms
ggplot(blood_data, aes(x=sample.A, color=Group)) +
  geom_histogram(fill="white", alpha=0.5, position="identity", binwidth = 10)

# semi transparent SAMPLEA
pA<-ggplot(blood_data, aes(x=sample.A, fill=Group, color=Group)) +
  geom_histogram(position="identity", alpha=0.5, binwidth = 5)+
  labs(x="Osmolarity", title="Sample A")+
  facet_grid(Group ~ .)

pB<-ggplot(blood_data, aes(x=sample.B, fill=Group, color=Group)) +
  geom_histogram(position="identity", alpha=0.5, binwidth = 5)+
  labs(x="Osmolarity", title="Sample B")+
  facet_grid(Group ~ .)

plot_grid(pA, pB, labels = c("a)", "b)"))


######### histograms with density ############
# semi transparent SAMPLEA
pA<-ggplot(blood_data, aes(x=sample.A, fill=Group, color=Group)) +
  geom_histogram(aes(y= ..density..), alpha=0.5, binwidth = 5)+
  geom_density(alpha=.2, fill="#FF6666")+  # Overlay with transparent density plot
  labs(x="Osmolarity", title="Sample A")+
  facet_grid(Group ~ .)

pB<-ggplot(blood_data, aes(x=sample.B, fill=Group, color=Group)) +
  geom_histogram(aes(y= ..density..), alpha=0.5, binwidth = 5)+
  geom_density(alpha=.2, fill="#FF6666")+  # Overlay with transparent density plot
  labs(x="Osmolarity", title="Sample B")+
  facet_grid(Group ~ .)

plot_grid(pA, pB, labels = c("a)", "b)"))




######### Chick data dot-line and bar plot

data(ChickWeight)

## Calculate the mean and standard errors for each diet at each time point
pltdata<- group_by(ChickWeight, Time, Diet) %>% 
  summarise(mn = mean(weight), se = sd(weight)/sqrt(n()))

## Plot the means over time - remembering to colour by the diet
plta <- ggplot(pltdata, aes(x=Time, y = mn, colour = Diet)) + 
  geom_point() + 
  geom_line() + ## function for adding lines to our plot
  theme_classic() + 
  labs(y = "Mean weight (g)", x = "Time (days)")
plta

######## bar plot
## Filter the summary data to only include the final weights
pltdata2 <- ungroup(pltdata) %>% 
  filter(Time==max(Time))
## Make a bar plot of the means and standard errors
pltb <- ggplot(pltdata2, aes(x=Diet, y = mn, ymin = mn-se, ymax = mn+se)) + 
  geom_col(fill = 'cornflowerblue', colour = "black") + 
  geom_errorbar(width = 0.3) + 
  labs(y = "Final weight (g)") + 
  theme_classic()
pltb

plot_grid(plta,pltb,labels= c("a)","b)"))


# histograms
ggplot(df, aes(x=weight, color=sex)) +
  geom_histogram(fill="white")
# Overlaid histograms
ggplot(df, aes(x=weight, color=sex)) +
  geom_histogram(fill="white", alpha=0.5, position="identity")
# semi transparent
p<-ggplot(df, aes(x=weight, fill=sex, color=sex)) +
  geom_histogram(position="identity", alpha=0.5)





