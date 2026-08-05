

pacman::p_load("tidyverse","dplyr","haven","readxl",
               "ggsci","RColorBrewer","reshape2","psych",
               "ggplot2","gt","gtsummary","writexl","summarytools",
               "here" )

# Loaded the data 


Data_P <-read.csv(here("Data","heart.csv"))



# Understanding the data set 
glimpse(Data_P)# this every column and the data type 

colnames(Data_P)

dim(Data_P)# shows the number of rows and colmns

summary(Data_P)# Gives Statistical Summary

str(Data_P)


# Checking the missing values 

 Data_P |> 
   summarise(across(everything(),~sum(is.na(.))))




# Renaming the columns 
  Data_P_Edited <- Data_P |> 
    rename(
      Age=age,
      Sex=sex,
      ChestPainType=cp,
      RestingBP=trestbps,
      Cholesterol=chol,
      FastingBloodSugar=fbs,
      RestECG=restecg,
      MaxHeartRate=thalach,
      ExerciseAngina=exang,
      STDepression=oldpeak,
      STSlope=slope,
      MajorVessels=ca,
      Thalessemia=thal,
      HeartDisease=target
    )


#Converting Variables to factors 
  Data_P_Edited <- Data_P_Edited |> 
     mutate(
       Sex=factor(
         Sex,
         levels = c(0,1),
         labels = c("Female","Male"),
         exclude = NA
         
       ),
       HeartDisease = factor(
         HeartDisease,
         levels = c(0,1),
         labels = c("No","Yes")
       ),
       FastingBloodSugar=factor(
         FastingBloodSugar,
         levels = c(0,1),
         labels = c("≤120 mg/dL",">120 mg/dL"),
         exclude = NA
       ),
       ExerciseAngina=factor(
         ExerciseAngina,
         levels = c(0,1),
         labels = c("No","Yes"),
         exclude = NA
       ),
       ChestPainType=factor(
         ChestPainType,
         levels = c(0,1,2,3),
         labels = c("Typical Angina",
                    "Atypical Angina",
                    "Non-anginal Pain",
                    "Asymptomatic"),
         exclude = NA
         
       ),
       RestECG=factor(
         RestECG,
         levels = c(0,1,2),
         labels = c("Normal",
                    "ST-T Wave Abnormality",
                    "Left Ventricular Hypertrophy"),
         exclude = NA
       ),
       STSlope=factor(
         STSlope,
         levels = c(0,1,2),
         labels = c(
           "Upsloping",
           "Flat",
           "Downsloping"
         ),
         exclude = NA
       ),
       Thalessemia=factor(
         Thalessemia,
         levels = c(0,1,2,3),
         labels = c("Unknown",
                    "Normal",
                    "Fixed Defect",
                    "Reversible Defect"),
         exclude = NA
       )
   
     )
  
  

#Business Question 1
#What is the gender distribution?
  Data_P_Edited |> 
    count(Sex)
  
  #Percentage
  Data_P_Edited |> 
    count(Sex) |> 
    mutate(
      Percentage = round(n/sum(n)*100,2)
    )
  
  
#Business Question 2
  
#How many patients have heart disease?

   
Data_P_Edited |>  
  count(HeartDisease)
  
  
# With Percentage 

Data_P_Edited |> 
  count(HeartDisease) |> 
  mutate(percentage=
           round(n/sum(n)*100),2)
  
  


#Business Question 3

#Average age
Data_P_Edited |> 
  summarise(
    AverageAge=mean(Age),
    MedianAge=median(Age),
    Minimum=min(Age),
    Maximum= max(Age)
  )






#Business Question 4

#Average Cholesterol

Data_P_Edited |> 
  summarise(
    AverageCholestrol=mean(Cholesterol),
    Median=median(Cholesterol),
    Maximum=max(Cholesterol)
  )


#Business Question 5

#How many males have heart disease?


Data_P_Edited |> 
  group_by(Sex,HeartDisease) |> 
  summarise(
    Patients=n()
  )


#Business Question 6 

#Average Age by Heart Disease

Data_P_Edited %>%
  group_by(HeartDisease) %>%
  summarise(
    MeanAge=mean(Age),
    SDAge=sd(Age),
    Patients=n()
  )



#Business Question 7

#Average Cholesterol by Heart Disease

Data_P_Edited |> 
  group_by(HeartDisease) |>
  summarise(
    MeanCholesterol=mean(Cholesterol),
    Patients=n()
  )



#Business Question 8

#Chest Pain vs Heart Disease

Data_P_Edited |> 
  count(ChestPainType,HeartDisease)



#Business Question 9

#Exercise Angina vs Heart Disease

Data_P_Edited |> 
  count(ExerciseAngina,HeartDisease)


#Business Question 10

#Average Maximum Heart Rate


Data_P_Edited |> 
  group_by(HeartDisease) |>
  summarise(
    MeanHeartRate=mean(MaxHeartRate)
  )
  


library(gt)



Gender_Table <- Data_P_Edited |> 
  count(Sex) |> 
  mutate(
    Percentage=round(n/sum(n)*100,2)
  ) |> 
  gt() |> 
  tab_header(
    title = "Gender Distribution of Patients",
    subtitle= "Heart Disease Dataset"
  )
  

Gender_Table



gtsave(
  Gender_Table,
  "Gender_Distribution.html"
)


summary(Data_P_Edited)

library(gtsummary)

tbl_summary(Data_P_Edited)



Summary_Table <- Data_P_Edited |> 
  tbl_summary(
    by= HeartDisease
  )
  

Summary_Table

Data_P_Edited |> 
  group_by(Sex,HeartDisease) |>
  summarise(
    Patients=n()
  )





Male_HD_Table <- Data_P_Edited |> 
  group_by(
    Sex,HeartDisease
  ) |> 
  gt() |> 
  tab_header(
    title = "Heart Disease by Gender"
  )
  
Male_HD_Table




Cholesterol_HD_Table <- Data_P_Edited |>
  group_by(HeartDisease) |> 
  summarise(
    Mean_Cholesterol=mean(Cholesterol),
    Sd_Cholesterol=sd(Cholesterol),
    Minimum= min(Cholesterol),
    Maximum=max(Cholesterol),
    Patients=n(),
    .groups = "drop"
  ) |> 
  gt() |> 
  tab_header(
    title = "Average Cholesterol by Heart Disease"
  )
  
Cholesterol_HD_Table



Age_HD_Table <- Data_P_Edited |>
  group_by(HeartDisease) |>
  summarise(
    Mean_Age = mean(Age),
    Median_Age = median(Age),
    SD_Age = sd(Age),
    Min_Age = min(Age),
    Max_Age = max(Age),
    Patients = n(),
    .groups = "drop"
  ) |>
  gt() |>
  tab_header(
    title = "Age Summary by Heart Disease Status",
    subtitle = "Heart Disease Analysis"
  )


Age_HD_Table

ChestPainTable <- Data_P_Edited |> 
  count(ChestPainType,HeartDisease) |> 
  rename(Patients=n) |> 
  group_by(HeartDisease) |> 
  mutate(
    Percentage=round(Patients/sum(Patients)*100,2)) |> 
  ungroup() |> 
  gt() |> 
  tab_header(title = "Chest Pain Type by Heart Disease Status",
             subtitle = "Counts and Percentage of Patients")


ChestPainTable


ExerciseAngina_HD_Table <- Data_P_Edited |> 
  count(ExerciseAngina,HeartDisease) |> 
  pivot_wider(
    names_from = HeartDisease,
    values_from = n,
    values_fill = 0,
    
  ) |> 
  gt() |> 
  tab_header(
    title = "Exercise Angina by Heart Disease"
  )
  
ExerciseAngina_HD_Table


# Vidualization


Gender_Plot <- Data_P_Edited |> 
  ggplot(
    aes(
      x=Sex,fill = Sex
    )
  )+geom_bar()+
  labs(
    title = "Gender Distribution",
    x="Gender",
    y="No.Patients",
    fill="Gender"
  )+
  theme_classic()+
  scale_fill_brewer(palette = "Set1")+
  theme(
    plot.title = element_text(hjust = 0.5,size = 23,face = "bold"),
    axis.title.x = element_text(size=12,face = "bold"),
    axis.title.y = element_text(size = 12,face = "bold"),
    legend.title = element_text(size = 12,face = "bold",hjust = 0.5)
  )
  
  
Gender_Plot 

ggsave(
  "Gender_Plot_bar.jpeg",
  plot = Gender_Plot,
  dpi = 900,
  limitsize = FALSE,
  units = "in",
  width = 11.5,
  height = 25.5,
  path = "F:\\RLanguage\\Project\\Project_01\\Plots"
)



Heart_Plot <- Data_P_Edited |> 
  ggplot(
    aes(x=HeartDisease,
        fill = HeartDisease)
  )+geom_bar()+
  labs(
    title = "Heart Distribution",
    x="Heart Disease",
    y="Patients"
  )+
  theme_classic()+
  scale_fill_brewer(palette = "Set1")+
  theme(
    plot.title = element_text(hjust = 0.5,size = 23,face = "bold"),
    axis.title.x = element_text(size=12,face = "bold"),
    axis.title.y = element_text(size = 12,face = "bold"),
    legend.title = element_text(size = 12,face = "bold",hjust = 0.5)
  )

Heart_Plot 


Age_Plot <- Data_P_Edited |> 
  ggplot(
    aes(
      Age
    )
  )+geom_histogram(
    bins = 20,
    fill = "blue",
    colour = "black"
    
  )+
  labs(
    title = "Age Distribution"
  )+theme_classic()+
  theme(
    plot.title = element_text(hjust = 0.5,size = 23,face = "bold"),
    axis.title.x = element_text(size=12,face = "bold"),
    axis.title.y = element_text(size = 12,face = "bold"),
    
  )
  
Numerical_Data <- Data_P_Edited |> 
  select(
    Age,
    RestingBP,
    Cholesterol,
    MaxHeartRate,
    STDepression,
    MajorVessels
  )





Correlation_Matrix <- cor(Numerical_Data)

correlation_long <- reshape2::melt(Correlation_Matrix)

ggplot(correlation_long,
       aes(
         x = Var1,
         y = Var2,
         fill = value
       )) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2))) +
  scale_fill_gradient2(
    low = "orange",
    mid = "white",
    high = "red",
    midpoint = 0
  ) +
  theme_minimal()+labs(
    title = "Correlation HeatMap",
    x="Clinical Variables",
    y="Clinical Variables"
  )+theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5,size=23,face = "bold")
  )



ggplot(
  Data_P_Edited,
  aes(
    HeartDisease,
    Cholesterol,
    fill = HeartDisease
  )
) +
  
  geom_violin(trim = FALSE) +
  
  geom_boxplot(width = 0.15) +
  
  theme_minimal()



ggplot(
  Data_P_Edited,
  aes(
    Age,
    Cholesterol,
    colour = HeartDisease
  )
) +
  
  geom_point(size = 2, alpha = 0.7) +
  
  geom_smooth(method = "lm", se = FALSE) +
  
  theme_minimal()
 


Exercise_Data <- Data_P_Edited |>
  count(ExerciseAngina, HeartDisease)




ggplot(
  Exercise_Data,
  aes(
    x = HeartDisease,
    y = ExerciseAngina,
    fill = n
  )
) +
  
  geom_tile(color = "white") +
  
  geom_text(aes(label = n),
            size = 6) +
  
  scale_fill_viridis_c() +
  
  labs(
    title = "Exercise Angina vs Heart Disease",
    fill = "Patients"
  ) +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5,size = 23,face = "bold"),
    axis.title.x = element_text(size=12,face = "bold"),
    axis.title.y = element_text(size = 12,face = "bold"),
    legend.title = element_text(size = 12,face = "bold",hjust = 0.5)
  )


ChestPain_Heatmap <- Data_P_Edited |>
  count(ChestPainType, HeartDisease)




ggplot(
  ChestPain_Heatmap,
  aes(
    x = HeartDisease,
    y = ChestPainType,
    fill = n
  )
) +
  
  geom_tile(color = "white") +
  
  geom_text(aes(label = n),
            color = "white",
            size = 5) +
  
  scale_fill_viridis_c() +
  labs(
    title = "Chest Pain Type vs Heart Disease",
    subtitle = "Distribution of patients by chest pain category",
    x = "Heart Disease Status",
    y = "Chest Pain Type",
    fill = "Number of\nPatients"
  )+theme_minimal()+
  scale_fill_gradient2(
    low = "darkorange",
    mid = "darkgray",
    high = "#D7191C",
    midpoint = 150
  )+ theme(
    plot.title = element_text(hjust = 0.5,size = 23,face = "bold"),
    axis.title.x = element_text(size=12,face = "bold"),
    axis.title.y = element_text(size = 12,face = "bold"),
    legend.title = element_text(size = 12,face = "bold",hjust = 0.5)
  )

#Density Plot

install.packages("GGally")

library(GGally)

GGally::ggpairs(
  Numrical_Data
)

ggplot(
  Data_P_Edited,
  aes(
    x = Cholesterol,
    fill = HeartDisease
  )
)+
  geom_density(alpha=.5)+
  labs(
    x="Cholesterol Level",
    y="Density",
    title = "Cholesterol Distribution by Heart Disease"
  )+
  theme_classic()+
  theme(
    plot.title = element_text(hjust = 0.5,size = 23,face = "bold"),
    axis.title.x = element_text(size=12,face = "bold"),
    axis.title.y = element_text(size = 12,face = "bold"),
    legend.title = element_text(size = 12,face = "bold",hjust = 0.5)
  )+scale_fill_brewer(palette = "Set1")


  


#Faceted Histogram

library(ggplot2)


ggplot(
  Data_P_Edited,
  aes(Age)
)+geom_histogram(
  bins = 20,
  fill = "darkblue",
  color = "green"
)+facet_wrap(~HeartDisease)+
  labs(
    x="Age",
    y="Count",
    title = "Histogram Count vs Age"
  )+
  theme(
    plot.title = element_text(hjust = 0.5,size = 23,face = "bold"),
    axis.title.x = element_text(size=12,face = "bold"),
    axis.title.y = element_text(size = 12,face = "bold"),
    legend.title = element_text(size = 12,face = "bold",hjust = 0.5)
  )

#Stacked Percentage Chart 

Data_P_Edited |>
  ggplot(
    aes(
      x=ChestPainType,
      fill = HeartDisease
    )
  )+geom_bar(
    position = "fill"
  )+scale_y_continuous(labels = scales::percent)+
  labs(
    title = "Percenage of HreatDiesease by Chest Pain Type",
    x="Chest Pain Type",
    y="Percetage of Patients"
  )+  theme(
    plot.title = element_text(hjust = 0.5,size = 13,face = "bold"),
    axis.title.x = element_text(size=12,face = "bold"),
    axis.title.y = element_text(size = 12,face = "bold"),
    legend.title = element_text(size = 12,face = "bold",hjust = 0.5)
  )



#Bubble Plot

ggplot(Data_P_Edited,
  aes(
    x=Age,
    y=Cholesterol,
    size = MaxHeartRate,
    colour=HeartDisease
  
  )
)+geom_point(alpha = .6)+
  labs(
    x="Age(Years)",
    y="Cholesterol (ml/dl)", 
    title= "Age Vs Cholesterol by Heart Disease ",
    subtitle="Bubble Size Represent the Heart Rate ",
    size= "Maximum Heart Rate ",
    colour="HeartRate"
  )+theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5,size = 13,face = "bold"),
    axis.title.x = element_text(size=12,face = "bold"),
    axis.title.y = element_text(size = 12,face = "bold"),
    legend.title = element_text(size = 12,face = "bold",hjust = 0.5)
  )

#T_test
#Statistical Tests

T_test_1<- t.test(
  Cholesterol~HeartDisease,
  data = Data_P_Edited
)


T_test_1


T_Test_2 <- t.test(
  Age~HeartDisease,
  data = Data_P_Edited
)


T_Test_2 


#Chiq_Test
#Chest Pain
Chiq_Test_1 <- chisq.test(
  Data_P_Edited$ChestPainType,
  Data_P_Edited$HeartDisease
)
Chiq_Test_1 



#Exercise Angina

chiq_Test_2 <- chisq.test(
  Data_P_Edited$ExerciseAngina,
  Data_P_Edited$HeartDisease
)

chiq_Test_2


#Anova Test


Cholesterol_ANOVA <- aov(
  Cholesterol~ChestPainType,
  data = Data_P_Edited
)


summary(Cholesterol_ANOVA)



HeartDisease_ANOVA <- aov(
  MaxHeartRate~ChestPainType,
  data = Data_P_Edited
)

summary(HeartDisease_ANOVA)



#Visualize ANOVA

ggplot(Data_P_Edited,
  aes(
  x=ChestPainType,
  y=Cholesterol,
  fill=ChestPainType
))+geom_boxplot()+
  labs(
    x="Chest Pain Type",
    y="Cholesterol (ml/dl)",
    title = "Cholesterol by Chest Pain Type"
  )+theme_minimal()+
  theme(
    axis.title.x = element_text(hjust = 0.5,face="bold"),
    plot.title = element_text(hjust = 0.5,size = 13,face = "bold"),
    axis.title.y = element_text(size = 12,face = "bold"),
    legend.title = element_text(size = 12,face = "bold",hjust = 0.5)
  )


#Correlation Table

round(Correlation_Matrix,2)


# psych

psych::describe(
  Numerical_Data
)

# Summary tools 
dfSummary(
  Data_P_Edited
)









