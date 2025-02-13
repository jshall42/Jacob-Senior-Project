#ChatGPT helped build -Jacob
#It mostly just error checked for me like guiding me
# Load necessary libraries
library(tidyverse)
library(dplyr)

# Read in the existing dataset and clean it
existing_data <- read.csv("Data_set.csv")
existing_data <- existing_data %>% select(-X, -Age_Group)

# Set seed for reproducibility
set.seed(123)

generate_participant_data <- function() {
  # Generate the participant data
  participant_data <- data.frame(
    
    id = sample(10000:99999, 1),
    Basic_Demos.Age = sample(5:22, 1),
    Basic_Demos.Sex = sample(0:1, 1),
    
    # CGAS (Children's Global Assessment Scale)
    CGAS.CGAS_Score = sample(50:55, 1),  # Moderate score range for CGAS (50-70)
    
    # Physical Measures
    Physical.BMI = round(runif(1, 22, 30), 1),  # Moderate BMI range (e.g., overweight but not extreme)
    Physical.Height = sample(58:69, 1),  # Height in inches
    Physical.Weight = sample(100:150, 1),  # Moderate weight range (not too low or high)
    Physical.Waist_Circumference = sample(28:32, 1),  # Moderate waist circumference
    Physical.Diastolic_BP = sample(70:75, 1),  # Slightly higher normal diastolic BP
    Physical.HeartRate = sample(70:700, 1),  # Normal but slightly elevated heart rate
    Physical.Systolic_BP = sample(110:120, 1),  # Normal systolic BP but on the higher side
    
    # Fitness
    Fitness_Endurance.Max_Stage = sample(6:10, 1),  # Moderate stage on the treadmill (not too low or high)
    Fitness_Endurance.Time_Mins = sample(15:20, 1),  # Moderate time on treadmill (around 15-20 minutes)
    Fitness_Endurance.Time_Sec = sample(30:50, 1),  # Moderate seconds
    
    # FitnessGram Child (FGC)
    FGC.FGC_CU = sample(75:110, 1),  # Moderate curl-up performance
    FGC.FGC_CU_Zone = sample(0:1, 1),  # Zone (either needs improvement or healthy)
    FGC.FGC_GSND = round(runif(1, 40, 70), 1),  # Moderate grip strength (non-dominant hand)
    FGC.FGC_GSND_Zone = sample(2:3, 1),  # Grip strength zone (Normal to strong)
    FGC.FGC_GSD = round(runif(1, 40, 70), 1),  # Moderate grip strength (dominant hand)
    FGC.FGC_GSD_Zone = sample(2:3, 1),  # Grip strength zone (Normal to strong)
    FGC.FGC_PU = sample(15:30, 1),  # Moderate push-up performance
    FGC.FGC_PU_Zone = sample(0:1, 1),  # Zone (either needs improvement or healthy)
    FGC.FGC_SRL = round(runif(1, 15, 30), 1),  # Moderate Sit & Reach (left side)
    FGC.FGC_SRL_Zone = sample(0:1, 1),  # Zone (either needs improvement or healthy)
    FGC.FGC_SRR = round(runif(1, 15, 30), 1),  # Moderate Sit & Reach (right side)
    FGC.FGC_SRR_Zone = sample(0:1, 1),  # Zone (either needs improvement or healthy)
    FGC.FGC_TL = sample(25:35, 1),  # Moderate trunk lift performance
    FGC.FGC_TL_Zone = sample(0:1, 1),  # Zone (either needs improvement or healthy)
    
    # Bio-electric Impedance Analysis (BIA)
    BIA.BIA_Activity_Level_num = sample(3:4, 1),  # Moderate to high activity level
    BIA.BIA_BMC = round(runif(1, 25, 35), 1),  # Moderate Bone Mineral Content
    BIA.BIA_BMI = round(runif(1, 22, 30), 1),  # Moderate BMI
    BIA.BIA_BMR = sample(1500:1600, 1),  # Basal Metabolic Rate (moderate)
    BIA.BIA_DEE = sample(6000:6500, 1),  # Daily Energy Expenditure (moderate activity)
    BIA.BIA_ECW = round(runif(1, 25, 35), 1),  # Extracellular Water (moderate)
    BIA.BIA_FFM = round(runif(1, 25, 35), 1),  # Fat Free Mass (moderate)
    BIA.BIA_FFMI = round(runif(1, 55, 70), 1),  # Fat Free Mass Index (moderate)
    BIA.BIA_FMI = round(runif(1, 180, 250), 1),  # Fat Mass Index (moderate)
    BIA.BIA_Fat = round(runif(1, 20, 30), 1),  # Body Fat Percentage (moderate)
    BIA.BIA_Frame_num = sample(1:3, 1),  # Body frame size (small, medium, or large)
    BIA.BIA_ICW = round(runif(1, 5, 8), 1),  # Intracellular Water (moderate)
    BIA.BIA_LDM = round(runif(1, 3, 6), 1),  # Lean Dry Mass (moderate)
    BIA.BIA_LST = round(runif(1, 4, 6), 1),  # Lean Soft Tissue (moderate)
    BIA.BIA_SMM = round(runif(1, 5, 7), 1),  # Skeletal Muscle Mass (moderate)
    BIA.BIA_TBW = round(runif(1, 5, 7), 1),  # Total Body Water (moderate)
    
    # Physical Activity Questionnaire (Adolescents)
    PAQ_A.PAQ_A_Total = sample(18:20, 1),  # Moderate activity summary score
    
    # Physical Activity Questionnaire (Children)
    PAQ_C.PAQ_C_Total = sample(18:20, 1),  # Moderate activity summary score
    
    # Parent-Child Internet Addiction Test (PCIAT) responses (Biasing scores towards 2-4 for moderate to severe scores)
    PCIAT.PCIAT_01 = sample(3:4, 1),
    PCIAT.PCIAT_02 = sample(3:4, 1),
    PCIAT.PCIAT_03 = sample(3:4, 1),
    PCIAT.PCIAT_04 = sample(3:5, 1),
    PCIAT.PCIAT_05 = sample(3:5, 1),
    PCIAT.PCIAT_06 = sample(3:5, 1),
    PCIAT.PCIAT_07 = sample(3:5, 1),
    PCIAT.PCIAT_08 = sample(3:5, 1),
    PCIAT.PCIAT_09 = sample(3:5, 1),
    PCIAT.PCIAT_10 = sample(3:5, 1),
    PCIAT.PCIAT_11 = sample(3:5, 1),
    PCIAT.PCIAT_12 = sample(3:5, 1),
    PCIAT.PCIAT_13 = sample(3:5, 1),
    PCIAT.PCIAT_14 = sample(3:5, 1),
    PCIAT.PCIAT_15 = sample(3:5, 1),
    PCIAT.PCIAT_16 = sample(3:5, 1),
    PCIAT.PCIAT_17 = sample(3:5, 1),
    PCIAT.PCIAT_18 = sample(3:5, 1),
    PCIAT.PCIAT_19 = sample(3:5, 1),
    PCIAT.PCIAT_20 = sample(3:5, 1),
    
    # SDS (Self-Reported Depression Scale)
    SDS.SDS_Total_Raw = sample(20:40, 1),  # Moderate raw score
    SDS.SDS_Total_T = sample(40:60, 1),  # Moderate T-Score
    
    # Pre-intake Education History: Computer/Internet usage hours per day
    PreInt_EduHx.computerinternet_hoursday = round(runif(2, 0, 3), 1)
  )
  
  # Ensure that there are no NAs in PCIAT.PCIAT_* columns
  pciat_columns <- grep("PCIAT.PCIAT_", names(participant_data))
  participant_data[pciat_columns] <- lapply(participant_data[pciat_columns], function(x) ifelse(is.na(x), 0, x))
  
  # Calculate PCIAT.PCIAT_Total as the sum of PCIAT.PCIAT_01 to PCIAT.PCIAT_20, ensuring no NA values are considered
  pciat_total_score <- rowSums(participant_data[, pciat_columns], na.rm = TRUE)
  
  # Assign the calculated PCIAT.PCIAT_Total score to the dataframe
  participant_data$PCIAT.PCIAT_Total <- pciat_total_score
  
  # Categorize PCIAT severity based on PCIAT.Total score (adjusted ranges)
  participant_data <- participant_data %>%
    mutate(
      sii = case_when(
        is.na(PCIAT.PCIAT_Total) ~ NA_real_,  # If PCIAT.PCIAT_Total is NA, return NA for sii
        PCIAT.PCIAT_Total <= 30 ~ 0,  # None
        PCIAT.PCIAT_Total >= 31 & PCIAT.PCIAT_Total <= 49 ~ 1,  # Mild
        PCIAT.PCIAT_Total >= 50 & PCIAT.PCIAT_Total <= 79 ~ 2,  # Moderate
        PCIAT.PCIAT_Total > 80 ~ 3   # Severe
      )
    )
  
  return(participant_data)
}

# Generate 2000 new participants with adjusted bias
new_participants_data <- do.call(rbind, replicate(2000, generate_participant_data(), simplify = FALSE))

# Add the new generated data to the existing data frame
updated_data <- rbind(existing_data, new_participants_data)

# View the first few rows of the updated data
head(updated_data)

# Filter out rows where sii is NA
updated_data <- updated_data %>% filter(!is.na(sii))

# Write the updated data to a new CSV file
write.csv(updated_data, "updated_data.csv", row.names = FALSE)

# Summary of the updated dataset
summary(updated_data)

# Count how many have sii == 0, sii == 1, sii == 2, and sii == 3
count_sii_0 <- updated_data %>%
  filter(sii == 0) %>%
  nrow()

count_sii_1 <- updated_data %>%
  filter(sii == 1) %>%
  nrow()

count_sii_2 <- updated_data %>%
  filter(sii == 2) %>%
  nrow()

count_sii_3 <- updated_data %>%
  filter(sii == 3) %>%
  nrow()

# Print the count of each severity category
print(count_sii_0)
print(count_sii_1)
print(count_sii_2)
print(count_sii_3)


