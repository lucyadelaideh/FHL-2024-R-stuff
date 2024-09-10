setwd("C:/Users/lucya/OneDrive/Documents/Friday Harbor/Year 2/Data")
#Color Pallette -----
{"Persian green":"#17a398","Lapis Lazuli":"#175676","Raspberry rose":"#aa4465","Safety orange":"#ec7505","Forest green":"#548c2f"}

#Colorblind safe color pallet: 
("#332288", "#117733","#44aa99","#88CCEE","#DDCC77","#CC6677","#AA4499","#882255")

#read in the data 
#Short-Term pH analysis ------
Short_Term_pH <- read.csv("~/Friday Harbor/Year 2/Data/Short_Term_pH.csv", stringsAsFactors=TRUE)
View(Short_Term_pH)
library(dplyr)

#to test for normality
shapiro.test(Short_Term_pH$Intracellular.pH)

#Short Term pH Graph-----
library(ggplot2)

ggplot(Short_Term_pH, aes(x = Organization, y = Intracellular.pH, fill = Treatment)) + 
  geom_boxplot() + theme_classic() + labs(x = 'Treatment', y = 'Intracellular pH (Scale 1-7)', title = "Short Term pH Analysis") +
  scale_x_discrete("Treatment", labels =c('Past','Present','Control','Future')) + 
  scale_fill_manual(values = c("#117733", "#cc6677","#332288", "#44aa99"))+
  annotate("text", x=c(1), y = c(5), label = c("a"), size = 5)+
  annotate("text", x=c(2), y = c(1.6), label = c("b"), size = 5)+
  annotate("text", x=c(3), y = c(2.1), label = c("b"), size = 5)+
  annotate("text", x=c(4), y = c(5.6), label = c("d"), size = 5)


#NOT NORMAL SO NEED TO DO A NON_PARAMETRIC TEST
#Statistical Analysis -----
#Do non-parametric t-test with a bonferroni correction 

library(dplyr)
group_by(Short_Term_pH, Treatment) %>%
  summarise(
    count = n(),
    mean = mean(Intracellular.pH, na.rm = TRUE),
    sd = sd(Intracellular.pH, na.rm = TRUE),
    median = median(Intracellular.pH, na.rm = TRUE),
    IQR = IQR(Intracellular.pH, na.rm = TRUE)
  )

# ANOVA and then Post-hoc test
kruskal.test(Intracellular.pH~Treatment,data=Short_Term_pH)
pairwise.t.test(Short_Term_pH$Intracellular.pH, Short_Term_pH$Treatment, paired=FALSE, p.adjust.method="bonferroni")

#Transforming the Data 

Log_ph <- log(PH_values$Intracellular.pH)

shapiro.test(Log_ph)

#Long-Term pH -----
Long_Term_pH <- read.csv("~/Friday Harbor/Year 2/Data/Long_Term_pH.csv", stringsAsFactors=TRUE)
library(dplyr)
View(Long_Term_pH)

#to test for normality
shapiro.test(PH_values$Intracellular.pH)

#Graph-----

# Successful attempt to reorder x-axis
ggplot(Long_Term_pH, aes(x = Organization, y = Intracellular.pH, fill = Treatment)) + 
  geom_boxplot() + theme_classic() + 
  labs(x = 'Treatment', y = 'Intracellular pH (Scale 1-7)', title = "Long Term pH Analysis") + 
  scale_x_discrete("Treatment", labels =c('Past','Control','Future')) + 
  scale_fill_manual(values = c("#117733", "#cc6677","#332288"))+
  annotate("text", x=c(1), y = c(6.1), label = c("a"), size = 5)+
  annotate("text", x=c(2), y = c(2.1), label = c("b"), size = 5)+
  annotate("text", x=c(3), y = c(6.1), label = c("a"), size = 5)


#NOT NORMAL SO NEED TO DO A NON_PARAMETRIC TEST
#Statistical Analysis -----
#Do non-parametric t-test with a bonferroni correction 

library(dplyr)
group_by(Long_Term_pH, Treatment) %>%
  summarise(
    count = n(),
    mean = mean(Intracellular.pH, na.rm = TRUE),
    sd = sd(Intracellular.pH, na.rm = TRUE),
    median = median(Intracellular.pH, na.rm = TRUE),
    IQR = IQR(Intracellular.pH, na.rm = TRUE)
  )

# ANOVA and then Post-hoc test
kruskal.test(Intracellular.pH~Treatment,data=Long_Term_pH)
pairwise.t.test(Long_Term_pH$Intracellular.pH, Long_Term_pH$Treatment, paired=FALSE, p.adjust.method="bonferroni")

#Grouped pH Box-Plot -----

pH_Combined <- read.csv("~/Friday Harbor/Year 2/Data/pH_Combined.csv", stringsAsFactors=TRUE)
View(pH_Combined)

#Grouped Scatterplot
ggplot(pH_Combined, aes(x=Treatment,y=Intracellular.pH,group=Timeframe,colour=Timeframe),
)+geom_point(position = position_dodge(width = 0.5), size = 3)

#Combined Box_Plot
library(ggpattern)
ggplot(pH_Combined, aes(x = Treatment, y = Intracellular.pH, fill = Treatment, color = Timeframe)) + 
  geom_boxplot() + theme_classic() + 
  labs(x = 'Treatment', y = 'Intracellular pH (Scale 1-7)', title = "Timeframe pH Analysis") + 
  scale_x_discrete("Treatment", labels =c('Control', 'Past','Future')) + 
  scale_fill_manual(values = c("#117733","#cc6677","#332288")) + scale_color_manual(values = c("#117733","#44aa99","#DDCC77"))+
  annotate("text", x=c(1), y = c(2.2), label = c("a"), size = 5)+
  annotate("text", x=c(1.82), y = c(6.1), label = c("b"), size = 5)+
  annotate("text", x=c(2.2), y = c(5.7), label = c("c"), size = 5)+
  annotate("text", x=c(2.8), y = c(6.1), label = c("d"), size = 5)+
  annotate("text", x=c(3.2), y = c(5.2), label = c("dc"), size =5) + 
  geom_boxplot_pattern(aes(pattern = Timeframe, pattern_fill = Timeframe )) + 
  scale_pattern_fill_manual(values=c("Long Term" = "stripe", "Short Term" = "crosshatch"))


#Grouped Statistics ------
#Past Comparison
Past_Comparison <- read.csv("~/Friday Harbor/Year 2/Data/Past_Comparison_pH.csv", stringsAsFactors=TRUE)
View(Past_Comparison)

library(ggpattern)

#Adding pattern: 

ggplot(Past_Comparison, aes(x = Treatment, y = Intracellular.pH, fill = Treatment, color = Timeframe)) + 
  geom_boxplot() + theme_classic() + 
  labs(x = 'Treatment', title = "Past pH Analysis") + ylab('Intracellular pH (Scale 1-7)') +
  scale_x_discrete("Treatment", labels =c('Past')) + 
  scale_fill_manual(values = c("#332288")) + scale_color_manual(values = c("black","black"))+
  geom_boxplot_pattern(aes(pattern = Timeframe, pattern_fill = Timeframe ), pattern_density = .3, outlier.shape = NA) +
  scale_pattern_alpha_manual(values = c("Long Term" = "crosshatch", "Short Term" = "stripe")) +  
  scale_pattern_fill_manual(values = c("Long Term" = "black", "Short Term" = "black")) +
  annotate("text", x=c(.82), y = c(6.1), label = c("a"), size = 5)+
  annotate("text", x=c(1.19), y = c(5.2), label = c("b"), size = 5) 



# ANOVA and then Post-hoc test
kruskal.test(Intracellular.pH~Timeframe,data=Past_Comparison)
pairwise.t.test(Past_Comparison$Intracellular.pH, Past_Comparison$Timeframe, paired=FALSE, p.adjust.method="bonferroni")

#Future Comparison

Future_Comparison <- read.csv("~/Friday Harbor/Year 2/Data/Future_Comparison_pH.csv", stringsAsFactors=TRUE)
View(Future_Comparison)

library(dplyr)
group_by(Future_Comparison, Time.Frame) %>%
  summarise(
    count = n(),
    mean = mean(Intracellular.pH, na.rm = TRUE),
    sd = sd(Intracellular.pH, na.rm = TRUE),
    median = median(Intracellular.pH, na.rm = TRUE),
    IQR = IQR(Intracellular.pH, na.rm = TRUE)
  )

ggplot(Future_Comparison, aes(x = Treatment, y = Intracellular.pH, fill = Treatment, color = Timeframe)) + 
  geom_boxplot() + theme_classic() + 
  labs(x = 'Treatment', title = "Future pH Analysis") + ylab('Intracellular pH (Scale 1-7)') +
  scale_x_discrete("Treatment", labels =c('Future')) + 
  scale_fill_manual(values = c("#cc6677")) + scale_color_manual(values = c("black","black"))+
  geom_boxplot_pattern(aes(pattern = Timeframe, pattern_fill = Timeframe ), pattern_density = .3, outlier.shape = NA) +
  scale_pattern_alpha_manual(values = c("Short Term" = "stripe", "Long Term" = "crosshatch")) +  
  scale_pattern_fill_manual(values = c("Long Term" = "black", "Short Term" = "black"))+
  annotate("text", x=c(.82), y = c(5.92), label = c("a"), size = 5)+
  annotate("text", x=c(1.19), y = c(5.45), label = c("b"), size = 5)

# ANOVA and then Post-hoc test
kruskal.test(Intracellular.pH~Timeframe,data=Future_Comparison)
pairwise.t.test(Future_Comparison$Intracellular.pH, Future_Comparison$Timeframe, paired=FALSE, p.adjust.method="bonferroni")


#MTS Analysis -----
library(ggplot2)
library(dplyr)
#Short Term MTS ------
Short_Term_MTS_na <- read.csv("~/Friday Harbor/Year 2/Data/Short_Term_MTS.csv", stringsAsFactors = TRUE)
Short_Term_MTS <- Short_Term_MTS_na %>% na.omit()
View(Short_Term_MTS)
#Long-Term MTS -----
library(dplyr)
Long_Term_MTS_na <- read.csv("~/Friday Harbor/Year 2/Data/Long_Term_MTS.csv", stringsAsFactors = TRUE)
Long_Term_MTS <- Long_Term_MTS_na %>% na.omit()
View(Long_Term_MTS)

#Grouped MTS -----
MTS_Combined <- read.csv("~/Friday Harbor/Year 2/Data/MTS_Combined.csv", stringsAsFactors = TRUE)
View(MTS_Combined)
#Modulus Analysis -----
#Short Term modulus ----

# ANOVA and then Post-hoc test
kruskal.test(Modulus_Mpa~Treatment,data=Short_Term_MTS)
pairwise.t.test(Short_Term_MTS$Modulus_Mpa, Short_Term_MTS$Treatment, paired=FALSE, p.adjust.method="bonferroni")

#Modulus graph 
library(ggplot2)
ggplot(Short_Term_MTS, aes(x =Organization, y = Modulus_Mpa, fill = Treatment)) + 
  geom_boxplot() + theme_classic() + 
  labs(x = 'Treatment', y = 'Elastic Modulus (MPa)', title = "Short Term Elastic Modulus Analysis") + 
  scale_x_discrete("Treatment", labels =c('Control','Past', 'Present','Future')) + 
  scale_fill_manual(values = c("#117733", "#cc6677","#332288", "#44aa99"))

#Long Term Modulus ----

# ANOVA and then Post-hoc test
kruskal.test(Modulus_Mpa~Treatment,data=Long_Term_MTS)
Long_Stats <- pairwise.t.test(Long_Term_MTS$Modulus_Mpa, Long_Term_MTS$Treatment, paired=FALSE, p.adjust.method="bonferroni")

#Long-term Modulus Graph

library(ggplot2)
library(ggsignif)
ggplot(Long_Term_MTS, aes(x =Treatment, y = Modulus_Mpa, fill = Treatment)) + 
  geom_boxplot() + 
  theme_classic() + 
  labs(x = 'Treatment', y = 'Elastic Modulus (MPa)', title = "Long Term Elastic Modulus Analysis") + 
  scale_x_discrete("Treatment", labels =c('Control','Past','Future')) + 
  scale_fill_manual(values = c("#117733", "#cc6677","#332288"))

#Long Term Modulus graph with significance letters: NOT WORKING

ggplot(Long_Term_MTS, aes(x =Treatment, y = Modulus_Mpa, fill = Treatment)) + 
  geom_boxplot() + 
  geom_signif(
    comparisons = list(c('Control', 'Past'), c('Control', 'Future'), c('Past', 'Future')), # Specify the comparisons
    map_signif_level = TRUE, # Automatically map significance levels to asterisks
    test = "kruskal.test") # Perform t-test



#Grouped Modulus Statistics ------

#Past Comparison
Past_Comparison_MTS <- read.csv("~/Friday Harbor/Year 2/Data/Past_Comparison_MTS.csv", stringsAsFactors=TRUE)
View(Past_Comparison_MTS)

kruskal.test(Modulus_Mpa~Timeframe,data=Past_Comparison_MTS)
pairwise.t.test(Past_Comparison_MTS$Modulus_Mpa, Past_Comparison_MTS$Timeframe, paired=FALSE, p.adjust.method="bonferroni")

#Future Comparison

Future_Comparison_MTS <- read.csv("~/Friday Harbor/Year 2/Data/Future_Comparison_MTS.csv", stringsAsFactors=TRUE)
View(Future_Comparison_MTS)

kruskal.test(Modulus_Mpa~Timeframe,data=Future_Comparison_MTS)
pairwise.t.test(Future_Comparison_MTS$Modulus_Mpa, Future_Comparison_MTS$Timeframe, paired=FALSE, p.adjust.method="bonferroni")

#Combined modulus graph: 
MTS_Combined <- read.csv("~/Friday Harbor/Year 2/Data/MTS_Combined.csv", stringsAsFactors = TRUE)
View(MTS_Combined)

ggplot(MTS_Combined, aes(x = Treatment, y = Modulus_Mpa, fill = Timeframe)) + 
  geom_boxplot() + theme_classic() + 
  labs(x = 'Treatment', y = 'Elastic Modulus (MPa)', title = "Timeframe Elastic Modulus Analysis") + 
  scale_x_discrete("Treatment", labels =c('Control','Past','Future')) + 
  scale_fill_manual(values = c("#117733","#F2AF29", "#028090"))

# ANOVA and then Post-hoc test
kruskal.test(Intracellular.pH~Time.frame,data=Past_Comparison)
pairwise.t.test(Past_Comparison$Intracellular.pH, Past_Comparison$Time.frame, paired=FALSE, p.adjust.method="bonferroni")

#Strain @ Break Analysis -----
#Long Term Strain @ Break ----
# ANOVA and then Post-hoc test
kruskal.test(Strain_at_Break_mm.mm~Treatment,data=Long_Term_MTS)
pairwise.t.test(Long_Term_MTS$Strain_at_Break_mm.mm, Long_Term_MTS$Treatment, paired=FALSE, p.adjust.method="bonferroni")

#Long-term Strain @ Break Graph

library(ggplot2)
ggplot(Long_Term_MTS, aes(x =Organization, y = Strain_at_Break_mm.mm, fill = Treatment)) + 
  geom_boxplot() + 
  theme_classic() + 
  labs(x = 'Treatment', y = 'Strain At Break (mm/mm)', title = "Long Term Strain At Break Analysis") + 
  scale_x_discrete("Treatment", labels =c('Past','Control','Future')) + 
  scale_fill_manual(values = c("#117733", "#cc6677","#332288"))+ 
  annotate("text", x=c(1), y = c(.43), label = c("a"), size = 5)+
  annotate("text", x=c(2), y = c(.22), label = c("b"), size = 5)+
  annotate("text", x=c(3), y = c(.31), label = c("c"), size = 5)

#Short-Term Strain @Break ------
# ANOVA and then Post-hoc test
kruskal.test(Strain_at_Break_mm.mm~Treatment,data=Short_Term_MTS)
pairwise.t.test(Short_Term_MTS$Strain_at_Break_mm.mm, Short_Term_MTS$Treatment, paired=FALSE, p.adjust.method="bonferroni")

#Short term Strain @ Break Graph

library(ggplot2)
ggplot(Short_Term_MTS, aes(x =Organization, y = Strain_at_Break_mm.mm, fill = Treatment)) + 
  geom_boxplot() + 
  theme_classic() + 
  labs(x = 'Treatment', y = 'Strain At Break (mm/mm)', title = "Short Term Strain At Break Analysis") + 
  scale_x_discrete("Treatment", labels =c('Past', 'Present', 'Control','Future')) + 
  scale_fill_manual(values = c("#117733", "#cc6677","#332288", "#44aa99"))+
  annotate("text", x=c(1), y = c(.47), label = c("a"), size = 5)+
  annotate("text", x=c(2), y = c(.25), label = c("b"), size = 5)+
  annotate("text", x=c(3), y = c(.22), label = c("c"), size = 5)+ 
  annotate("text", x=c(4), y = c(.33), label = c("d"), size = 5) 
#Combined statistics strain at break  ----

#Past Comparison -----
library(ggpattern)
kruskal.test(Strain_at_Break_mm.mm~Timeframe,data= Past_Comparison_MTS)
pairwise.t.test(Past_Comparison_MTS$Strain_at_Break_mm.mm, Past_Comparison_MTS$Timeframe, paired=FALSE, p.adjust.method="bonferroni")

ggplot(Past_Comparison_MTS, aes(x = Treatment, y = Strain_at_Break_mm.mm, fill = Treatment, color = Timeframe)) + 
  geom_boxplot() + theme_classic() + 
  labs(x = 'Treatment', title = "Past Toughness Analysis") + ylab(bquote('Toughness' (J/m^2))) +
  scale_x_discrete("Treatment", labels =c('Past')) + 
  scale_fill_manual(values = c("#332288")) + scale_color_manual(values = c("black","black"))+
  scale_color_manual(values = c("black","black"))+
  geom_boxplot_pattern(aes(pattern = Timeframe, pattern_fill = Timeframe ), pattern_density = .3, outlier.shape = NA) +
  scale_pattern_alpha_manual(values = c("Short Term" = "stripe", "Long Term" = "crosshatch")) +  
  scale_pattern_fill_manual(values = c("Long Term" = "black", "Short Term" = "black"))


#Future Comparison -----
kruskal.test(Strain_at_Break_mm.mm~Timeframe,data=Future_Comparison_MTS)
pairwise.t.test(Future_Comparison_MTS$Strain_at_Break_mm.mm, Future_Comparison_MTS$Timeframe, paired=FALSE, p.adjust.method="bonferroni")

ggplot(Future_Tensile, aes(x = organization, y = toughnessCalc, fill = Treatment, color = Timeframe)) + 
  geom_boxplot() + theme_classic() + 
  labs(x = 'Treatment', title = "Future Toughness Analysis") + ylab(bquote('Toughness' (J/m^2))) +
  scale_x_discrete("Treatment", labels =c('Future')) + 
  scale_fill_manual(values = c("#cc6677")) + scale_color_manual(values = c("black","black"))+
  geom_boxplot_pattern(aes(pattern = Timeframe, pattern_fill = Timeframe ), pattern_density = .3, outlier.shape = NA) +
  scale_pattern_alpha_manual(values = c("Short Term" = "stripe", "Long Term" = "crosshatch")) +  
  scale_pattern_fill_manual(values = c("Long Term" = "black", "Short Term" = "black"))+
  annotate("text", x=c(.82), y = c(.31), label = c("a"), size = 5)+
  annotate("text", x=c(1.19), y = c(.25), label = c("a"), size = 5)

#Peak Load Analysis -----
#Short_Term Peak Load graph ------
ggplot(Short_Term_MTS, aes(x =Organization, y = Peak_Load_N, fill = Treatment)) + 
  geom_boxplot() + 
  theme_classic() + 
  labs(x = 'Treatment', y = 'Peak Load (N)', title = "Short Term Peak Load Analysis") + 
  scale_x_discrete("Treatment", labels =c('Control','Past', 'Present','Future')) + 
  scale_fill_manual(values = c("#117733", "#cc6677","#332288", "#44aa99"))+
  annotate("text", x=c(1), y = c(9.3), label = c("a"), size = 5)+
  annotate("text", x=c(2), y = c(5.3), label = c("b"), size = 5)+
  annotate("text", x=c(3), y = c(5.2), label = c("b"), size = 5)+ 
  annotate("text", x=c(4), y = c(5.5), label = c("b"), size = 5) 

#Short Term Peak Load Statistics ----

kruskal.test(Peak_Load_N~Treatment,data=Short_Term_MTS)
pairwise.t.test(Short_Term_MTS$Peak_Load_N, Short_Term_MTS$Treatment, paired=FALSE, p.adjust.method="bonferroni")

#Long Term Peak Load Graph ----

ggplot(Long_Term_MTS, aes(x =Organization, y = Peak_Load_N, fill = Treatment)) + 
  geom_boxplot() + 
  theme_classic() + 
  labs(x = 'Treatment', y = 'Peak Load (N)', title = "Long Term Peak Load Analysis") + 
  scale_x_discrete("Treatment", labels =c('Control','Past','Future')) + 
  scale_fill_manual(values = c("#117733", "#cc6677","#332288"))+ 
  annotate("text", x=c(1), y = c(9.3), label = c("a"), size = 5)+
  annotate("text", x=c(2), y = c(7.6), label = c("b"), size = 5)+
  annotate("text", x=c(3), y = c(6.8), label = c("c"), size = 5)

#Long Term Peak Load Statistics -----
kruskal.test(Peak_Load_N~Treatment,data=Long_Term_MTS)
pairwise.t.test(Long_Term_MTS$Peak_Load_N, Long_Term_MTS$Treatment, paired=FALSE, p.adjust.method="bonferroni")

#Grouped Peak Load Analysis -----
#Grouped Peak Load Graph ----
ggplot(MTS_Combined, aes(x = Treatment, y = Peak_Load_N, fill = Treatment, color = Timeframe)) + 
  geom_boxplot() + theme_classic() + 
  labs(x = 'Treatment', y = 'Peak Load (N)', title = "Timeframe Peak Load Analysis") + 
  scale_x_discrete("Treatment", labels =c('Control','Past','Future')) + 
  scale_fill_manual(values = c("#117733","#cc6677","#332288")) + scale_color_manual(values = c("#117733","#44aa99","#DDCC77"))+
  annotate("text", x=c(1), y = c(9.5), label = c("a"), size = 5)+
  annotate("text", x=c(1.82), y = c(7.7), label = c("b"), size = 5)+
  annotate("text", x=c(2.2), y = c(5.5), label = c("c"), size = 5)+
  annotate("text", x=c(2.8), y = c(7), label = c("d"), size = 5)+
  annotate("text", x=c(3.2), y = c(5.7), label = c("dc"), size =5)




ggplot(MTS_Combined, aes(x=Treatment, y = Peak_Load_N, fill = Timeframe, color=Treatment))
#Grouped Peak Load Statistics -----
#Past Comparison
Past_Comparison_MTS <- read.csv("~/Friday Harbor/Year 2/Data/Past_Comparison_MTS.csv", stringsAsFactors=TRUE)
View(Past_Comparison_MTS)

kruskal.test(Peak_Load_N~Timeframe,data=Past_Comparison_MTS)
pairwise.t.test(Past_Comparison_MTS$Peak_Load_N, Past_Comparison_MTS$Timeframe, paired=FALSE, p.adjust.method="bonferroni")

#Future Comparison

Future_Comparison_MTS <- read.csv("~/Friday Harbor/Year 2/Data/Future_Comparison_MTS.csv", stringsAsFactors=TRUE)
View(Future_Comparison_MTS)

kruskal.test(Peak_Load_N~Timeframe,data=Future_Comparison_MTS)
pairwise.t.test(Future_Comparison_MTS$Peak_Load_N, Future_Comparison_MTS$Timeframe, paired=FALSE, p.adjust.method="bonferroni")

#Peak Stress Analysis ------
#SHort Term Peak Stress Stats: 
View(Short_Term_MTS)
kruskal.test(Peak_Stress_Mpa~Treatment,data=Short_Term_MTS)
pairwise.t.test(Short_Term_MTS$Peak_Stress_Mpa, Short_Term_MTS$Treatment, paired=FALSE, p.adjust.method="bonferroni")

#Short Term Peak Stress Graph ----
ggplot(Short_Term_MTS, aes(x =Organization, y = Peak_Stress_Mpa, fill = Treatment)) + 
  geom_boxplot() + 
  theme_classic() + 
  labs(x = 'Treatment', y = 'Peak Stress (MPa)', title = "Short Term Peak Stress Analysis") + 
  scale_x_discrete("Treatment", labels =c('Past', 'Present','Control','Future')) + 
  scale_fill_manual(values = c("#117733", "#cc6677","#332288", "#44aa99")) + 
  annotate("text", x=c(1), y = c(.35), label = c("b"), size = 5)+
  annotate("text", x=c(2), y = c(.3), label = c("b"), size = 5)+
  annotate("text", x=c(3), y = c(.56), label = c("a"), size = 5)+ 
  annotate("text", x=c(4), y = c(.35), label = c("b"), size = 5) 

#Long Term PEak Stress Graph ----
kruskal.test(Peak_Stress_MPa~Treatment,data=Long_Term_MTS)
pairwise.t.test(Long_Term_MTS$Peak_Stress_MPa, Long_Term_MTS$Treatment, paired=FALSE, p.adjust.method="bonferroni")

View(Long_Term_MTS)
ggplot(Long_Term_MTS, aes(x =Organization, y = Peak_Stress_MPa, fill = Treatment)) + 
  geom_boxplot() + 
  theme_classic() + 
  labs(x = 'Treatment', y = 'Peak Stress (MPa)', title = "Long Term Peak Stress Analysis") + 
  scale_x_discrete("Treatment", labels =c('Past', 'Control','Future')) + 
  scale_fill_manual(values = c("#117733","#cc6677","#332288")) + 
  annotate("text", x=c(1), y = c(.49), label = c("a"), size = 5)+
  annotate("text", x=c(2), y = c(.56), label = c("a"), size = 5)+
  annotate("text", x=c(3), y = c(.41), label = c("b"), size = 5)
  


#Tensile Toughness Analysis ----

#Short term Tensile toughness -----
#Short Term Tensile Toughness statistics
Short_Term_Tensile <- read.csv("~/Friday Harbor/Year 2/Data/SHort_Term_Tensile.csv", stringsAsFactors=TRUE)
View(Short_Term_Tensile)

kruskal.test(toughnessCalc~Treatment,data=Short_Term_Tensile)
pairwise.t.test(Short_Term_Tensile$toughnessCalc, Short_Term_Tensile$Treatment, paired=FALSE, p.adjust.method="bonferroni")
#Short Term Tensile Toughness graph ----
ggplot(Short_Term_Tensile, aes(x = organization, y = toughnessCalc, fill = Treatment)) + 
  geom_boxplot() + theme_classic() + 
  labs(x = 'Treatment', title = "Short Term Tensile Toughness Analysis") + ylab(bquote('Toughness' (J/m^2))) +
  scale_x_discrete("Treatment", labels =c('Control','Past','Present','Future')) + 
  scale_fill_manual(values = c("#117733","#cc6677","#332288","#44aa99")) +
  annotate("text", x=c(1), y = c(.44), label = c("a"), size = 5)+
  annotate("text", x=c(2), y = c(.23), label = c("b"), size = 5)+
  annotate("text", x=c(3), y = c(.23), label = c("c"), size = 5)+
  annotate("text", x=c(4), y = c(.25), label = c("d"), size = 5)

#Long term Tensile toughness -----
#Long Term Tensile Toughness statistics ------
Long_Term_Tensile <- read.csv("~/Friday Harbor/Year 2/Data/Long_Term_Tensile.csv", stringsAsFactors=TRUE)
View(Long_Term_Tensile)

kruskal.test(toughnessCalc~Treatment,data=Long_Term_Tensile)
pairwise.t.test(Long_Term_Tensile$toughnessCalc, Long_Term_Tensile$Treatment, paired=FALSE, p.adjust.method="bonferroni")
#Long Term Tensile Toughness graph ----
ggplot(Long_Term_Tensile, aes(x = organization, y = toughnessCalc, fill = Treatment)) + 
  geom_boxplot() + theme_classic() + 
  labs(x = 'Treatment', title = "Long Term Tensile Toughness Analysis") + ylab(bquote('Toughness' (J/m^2))) +
  scale_x_discrete("Treatment", labels =c('Control','Past','Future')) + 
  scale_fill_manual(values = c("#117733","#cc6677","#332288","#44aa99")) +
  annotate("text", x=c(1), y = c(.44), label = c("a"), size = 5)+
  annotate("text", x=c(2), y = c(.36), label = c("b"), size = 5)+
  annotate("text", x=c(3), y = c(.31), label = c("c"), size = 5)
#Grouped Tensile Toughness Graph -----
Combined_Tensile <- read.csv("~/Friday Harbor/Year 2/Data/Tensile_Combined.csv", stringsAsFactors=TRUE)
View(Combined_Tensile)

ggplot(Combined_Tensile, aes(x = organization, y = toughnessCalc, fill = Treatment, color = Timeframe)) + 
  geom_boxplot() + theme_classic() + 
  labs(x = 'Treatment', title = "Timeframe Toughness Analysis") + ylab(bquote('Toughness' (J/m^2))) +
  scale_x_discrete("Treatment", labels =c('Control','Past','Future')) + 
  scale_fill_manual(values = c("#117733","#cc6677","#332288")) + scale_color_manual(values = c("#117733","#44aa99","#DDCC77"))

#Past Comparison -----
Past_Tensile <- read.csv("~/Friday Harbor/Year 2/Data/Past_Tensile.csv", stringsAsFactors=TRUE)
View(Past_Tensile)
kruskal.test(toughnessCalc~Timeframe,data=Past_Tensile)
pairwise.t.test(Past_Tensile$toughnessCalc, Past_Tensile$Timeframe, paired=FALSE, p.adjust.method="bonferroni")

ggplot(Past_Tensile, aes(x = organization, y = toughnessCalc, fill = Treatment, color = Timeframe)) + 
  geom_boxplot() + theme_classic() + 
  labs(x = 'Treatment', title = "Past Toughness Analysis") + ylab(bquote('Toughness' (J/m^2))) +
  scale_x_discrete("Treatment", labels =c('Past')) + 
  scale_fill_manual(values = c("#332288")) + scale_color_manual(values = c("#44aa99","#DDCC77"))+
  annotate("text", x=c(.82), y = c(.35), label = c("a"), size = 5)+
  annotate("text", x=c(1.19), y = c(.23), label = c("b"), size = 5) +scale_color_manual(values = c("black","black"))+
  geom_boxplot_pattern(aes(pattern = Timeframe, pattern_fill = Timeframe ), pattern_density = .3, outlier.shape = NA) +
  scale_pattern_alpha_manual(values = c("Short Term" = "stripe", "Long Term" = "crosshatch")) +  
  scale_pattern_fill_manual(values = c("Long Term" = "black", "Short Term" = "black"))


#Future Comparison -----
Future_Tensile <- read.csv("~/Friday Harbor/Year 2/Data/Future_Tensile.csv", stringsAsFactors=TRUE)
View(Future_Tensile)
kruskal.test(toughnessCalc~Timeframe,data=Future_Tensile)
pairwise.t.test(Future_Tensile$toughnessCalc, Future_Tensile$Timeframe, paired=FALSE, p.adjust.method="bonferroni")

ggplot(Future_Tensile, aes(x = organization, y = toughnessCalc, fill = Treatment, color = Timeframe)) + 
  geom_boxplot() + theme_classic() + 
  labs(x = 'Treatment', title = "Future Toughness Analysis") + ylab(bquote('Toughness' (J/m^2))) +
  scale_x_discrete("Treatment", labels =c('Future')) + 
  scale_fill_manual(values = c("#cc6677")) + scale_color_manual(values = c("black","black"))+
  geom_boxplot_pattern(aes(pattern = Timeframe, pattern_fill = Timeframe ), pattern_density = .3, outlier.shape = NA) +
  scale_pattern_alpha_manual(values = c("Short Term" = "stripe", "Long Term" = "crosshatch")) +  
  scale_pattern_fill_manual(values = c("Long Term" = "black", "Short Term" = "black"))+
  annotate("text", x=c(.82), y = c(.31), label = c("a"), size = 5)+
  annotate("text", x=c(1.19), y = c(.25), label = c("a"), size = 5)



#ANOVA GRAPH: CASSANDRA ------

ggplot(Long_Term_pH,aes(x=factor(Treatment),y=Intracellular.pH))+
  geom_point(aes(color=factor(Treatment)))+
  stat_summary(geom = "errorbar", 
               fun.y = mean,
               aes(ymin = 0, ymax = 7),
               width=0.75,size=1)+
  stat_summary(geom = "errorbar",
               fun.data=mean_se,
               width=0.2)+
  scale_color_manual(values=c('red','blue','green'),
                     labels=c('','Eggs','toast'),
                     name=c('Something'))+                       #Change the color and labels for your categories.
  labs(x = "Breakfast", y = "How far the car runs") +                 #Label your axes
  annotate("text",x=c(2),y=c(35),label=c("*"),size=15)+               #Add a denoter of statistical significance
  theme_bw()+                                                         #Change the "theme" to a prettier one.
  theme(panel.grid.major = element_blank(),                           #Take away the grid in the plot
        panel.grid.minor = element_blank(),               
        legend.text=element_text(size=18),                            #Change the size of the legend text (not the title)
        axis.title.y=element_text(vjust=5),                           #Change the position of the y axis title
        axis.title.x=element_blank(),                                 #Get rid of your x axis title
        axis.text.x=element_blank(),                                  #Get rid of you x axis labels
        text=element_text(size=40, family="serif"),                   #Make the rest of the font bigger and Times New Roman
        plot.margin=unit(c(1,1,0.5,1.1),"cm"))                        #Change some margins so that things fit nicely.

#No Longer Used -----
#omit Na values
PH_values <- Short_Term_pH %>% na.omit()
View(PH_values)

#Create data
Intracellular_pH <- PH_values$Intracellular.pH
Treatment <- factor(PH_values$Treatment, levels = c("Past", "Present", "Future")) 

#DOESNT WORK WITH TIES
WILCOX_PH <- wilcox.test(PH_values$Intracellular.pH, y = NULL,
                         alternative = c("two.sided"),
                         mu = 0, paired = FALSE, exact = NULL, correct = TRUE,
                         conf.int = FALSE, conf.level = 0.95,
                         tol.root = 1e-4, digits.rank = Inf, p.adjust.method = "bonferroni", as.numeric(Treatment))

