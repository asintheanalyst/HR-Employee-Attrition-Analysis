# 1. Load the library
library(tidyverse)

# 2. Define the path (I fixed the slashes for you)
file_path <- "D:/Employee_attriition_project/WA_Fn-UseC_-HR-Employee-Attrition.csv"

# 3. Check if file exists before trying to load
if(file.exists(file_path)) {
  
  # Load the data
  hr_data <- read_csv(file_path)
  
  # Success Message
  print("SUCCESS! The data is loaded.")
  print(head(hr_data))
  
} else {
  
  # Failure Message
  print("ERROR: R cannot find the file.")
  print("Please check: Is the file name 'WA_Fn-UseC_-HR-Employee-Attrition.csv' correct?")
  print("Most IBM files are named 'WA_Fn-UseC-HR-Employee-Attrition.sv' (without the underscore after UseC).")
  
}
# This assumes you successfully imported the data as 'hr_data' using the steps above
library(tidyverse)
library(ggplot2)
library(dplyr)

# Calculate counts first to make labeling easy
attrition_counts <- hr_data %>%
  count(Attrition) %>%
  mutate(prop = n / sum(n)) # Calculate percentage

ggplot(attrition_counts, aes(x = Attrition, y = n, fill = Attrition)) +
  geom_col(width = 0.6) + # Thinner, cleaner bars
  
  # Add the Number AND Percentage on top of the bars
  geom_text(aes(label = paste0(n, " (", scales::percent(prop), ")")), 
            vjust = -0.5, fontface = "bold") +
  
  # Professional Colors: Grey for "Stay", Red for "Leave" (Action color)
  scale_fill_manual(values = c("No" = "lightgrey", "Yes" = "#FF6F61")) +
  
  labs(title = "Overall Attrition Overview",
       subtitle = "What percentage of the workforce has left?",
       x = "Status",
       y = "Employee Count") +
  
  theme_minimal() +
  theme(legend.position = "none", # Hide legend (colors explain themselves)
        plot.title = element_text(face = "bold", size = 16))

#for age distribution
ggplot(hr_data, aes(x = Age, fill = Attrition)) +
  geom_density(alpha = 0.6) + # 60% transparent
  
  # Custom Colors
  scale_fill_manual(values = c("No" = "grey80", "Yes" = "#FF6F61")) +
  
  labs(title = "Age Distribution: Who is Leaving?",
       subtitle = "Interpretation: A peak on the left indicates higher turnover among youth.",
       x = "Employee Age",
       y = "Density (Frequency)") +
  
  theme_minimal() +
  
  # Move legend to top for easier reading
  theme(legend.position = "top",
        plot.title = element_text(face = "bold", size = 16))

# Salary Analysis (MonthlyIncome)

# 1. Calculate the Median Salary for each group first
# We do this so we can print the exact number on the chart
salary_stats <- hr_data %>%
  group_by(Attrition) %>%
  summarise(median_salary = median(MonthlyIncome, na.rm = TRUE))

# 2. Plot the enhanced visual
ggplot(hr_data, aes(x = Attrition, y = MonthlyIncome, fill = Attrition)) +
  
  # A. The Violin (The background shape)
  # This shows where the "mass" of employees sits salary-wise
  geom_violin(trim = FALSE, alpha = 0.4, color = NA) +
  
  # B. The Boxplot (The statistical core)
  # We make it narrow (width = 0.2) to fit inside the violin
  geom_boxplot(width = 0.2, color = "black", alpha = 0.9, outlier.shape = NA) +
  
  # C. The Median Labels (The Text)
  # This prints "$5,000" etc. directly above the box
  geom_text(data = salary_stats, 
            aes(y = median_salary, label = scales::dollar(median_salary)), 
            vjust = -0.8, fontface = "bold", size = 5) +
  
  # Colors & Formatting
  scale_y_continuous(labels = scales::label_dollar()) +
  scale_fill_manual(values = c("No" = "#4E79A7", "Yes" = "#FF6F61")) + # Blue vs. Red
  
  labs(title = "Income Gap Analysis",
       subtitle = "Comparing the median salary of those who leave vs. those who stay",
       x = "Attrition Status",
       y = "Monthly Income") +
  
  theme_minimal() +
  theme(
    legend.position = "none", # Hide legend as labels explain it
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.major.x = element_blank() # Remove vertical lines for a cleaner look
  )

 # stress status managemen

stress_stats <- hr_data %>%
  group_by(JobRole) %>%
  summarise(avg_satisfaction = mean(JobSatisfaction, na.rm = TRUE)) %>%
  arrange(avg_satisfaction) # Sort from lowest to highest

# 2. The Catchy Lollipop Plot
ggplot(stress_stats, aes(x = reorder(JobRole, avg_satisfaction), y = avg_satisfaction, color = avg_satisfaction)) +
  
  # The Stick
  geom_segment(aes(xend = JobRole, yend = 0), size = 1) +
  # The Lollipop Head
  geom_point(size = 5) +
  
  # Color Gradient: Red (Low Score) to Green (High Score)
  scale_color_gradient(low = "#E41A1C", high = "#4DAF4A") +
  
  # Clean up axis (Scale is 1 to 4)
  scale_y_continuous(limits = c(0, 4)) +
  coord_flip() +
  
  labs(title = "Which Roles are the Most Stressed?",
       subtitle = "Lower satisfaction score (Red) indicates higher stress",
       x = "", # Remove redundant label
       y = "Avg Satisfaction Score (1-4)") +
  
  theme_minimal() +
  theme(legend.position = "none", # Hide legend (colors explain themselves)
        panel.grid.major.y = element_blank(), # Remove messy horizontal lines
        plot.title = element_text(face = "bold", size = 14))

# THE BURNOUT STATUS
# 1. Calculate the exact % of people who leave in each group
overtime_stats <- hr_data %>%
  group_by(OverTime) %>%
  summarise(Attrition_Rate = mean(Attrition == "Yes"))

# 2. The Crisp Plot
ggplot(overtime_stats, aes(x = OverTime, y = Attrition_Rate, fill = OverTime)) +
  geom_col(width = 0.6) +
  
  # Add the big % label on top
  geom_text(aes(label = scales::percent(Attrition_Rate, accuracy = 1)), 
            vjust = -0.5, fontface = "bold", size = 5) +
  
  # Colors: Grey for No Overtime, Red for Yes Overtime
  scale_fill_manual(values = c("No" = "lightgrey", "Yes" = "#E41A1C")) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.4)) + # Set limit to make room for text
  
  labs(title = "The Burnout Effect",
       subtitle = "Attrition rate for employees who work overtime vs. those who don't",
       x = "Works Overtime?",
       y = "Attrition Rate") +
  
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 14),
        panel.grid.major.x = element_blank())

# LOYALTY ANALYSIS: Years at Company
library(ggplot2)

ggplot(hr_data, aes(x = YearsAtCompany, fill = Attrition)) +
  
  # 1. The smooth density curves
  geom_density(alpha = 0.7, color = NA) + # No outline looks more modern
  
  # 2. Professional Colors
  scale_fill_manual(values = c("No" = "lightgrey", "Yes" = "#FF6F61")) +
  
  # 3. Add a "Call out" text box directly on the chart
  # (Adjust x and y numbers slightly if the text isn't perfectly placed for your data)
  annotate("text", x = 2, y = 0.08, label = "Danger Zone:\nHigh turnover in\nYears 0-2", 
           color = "#FF6F61", fontface = "bold", hjust = 0) +
  
  labs(title = "The Loyalty Curve",
       subtitle = "At what stage of employment do people leave?",
       x = "Years at Company",
       y = "Density (Probability)") +
  
  theme_minimal() +
  theme(
    legend.position = "top", 
    panel.grid.minor = element_blank(), # Remove small grid lines
    plot.title = element_text(face = "bold", size = 16)
  )

# Stagnation Analysis: Years Since Last Promotion
library(ggplot2)
library(dplyr)

# 1. Calculate the Median first for the labels
promo_stats <- hr_data %>%
  group_by(Attrition) %>%
  summarise(median_years = median(YearsSinceLastPromotion, na.rm = TRUE))

# 2. The Enhanced Green Plot
ggplot(hr_data, aes(x = Attrition, y = YearsSinceLastPromotion, fill = Attrition)) +
  
  # A. The Jitter Points (The background dots)
  # This shows the actual employees. We make them faint green so they don't distract.
  geom_jitter(color = "#4E8F6F", width = 0.2, alpha = 0.2) +
  
  # B. The Boxplot (The main summary)
  # We make it partially transparent to see the dots behind it
  geom_boxplot(width = 0.5, outlier.shape = NA, alpha = 0.8) +
  
  # C. The Median Labels
  geom_text(data = promo_stats, 
            aes(y = median_years, label = paste(median_years, "Years")), 
            vjust = -0.8, fontface = "bold", size = 5) +
  
  # D. The Green Color Palette
  # "No" = Pale Mint Green, "Yes" = Deep Forest Green
  scale_fill_manual(values = c("No" = "#A8D5BA", "Yes" = "#2E8B57")) +
  
  labs(title = "Stagnation Analysis",
       subtitle = "Comparison of years since last promotion (Dots = Individual Employees)",
       x = "Attrition Status",
       y = "Years Since Last Promotion") +
  
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.major.x = element_blank() # Clean vertical lines
  )

#GENDER ANALYSIS
# 1. Prepare the data first (Calculate percentages)
# This allows us to put the exact labels on the bars
gender_stats <- hr_data %>%
  group_by(Gender, Attrition) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count))

# 2. The Black & Orange Plot
ggplot(gender_stats, aes(x = Gender, y = percentage, fill = Attrition)) +
  
  # The Bars
  geom_col(width = 0.6) +
  
  # Add Labels inside the bars
  geom_text(aes(label = scales::percent(percentage, accuracy = 1)), 
            position = position_stack(vjust = 0.5), # Center text in the block
            color = "white", fontface = "bold", size = 5) +
  
  # The Requested Colors: Black (No) and Orange (Yes)
  scale_fill_manual(values = c("No" = "#000000", "Yes" = "#FF8C00")) +
  
  # Clean Axis (0% to 100%)
  scale_y_continuous(labels = scales::percent_format()) +
  
  labs(title = "Attrition by Gender",
       subtitle = "Is one gender leaving faster than the other?",
       x = "Gender",
       y = "Percentage") +
  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.major.x = element_blank(), # Remove vertical grid lines
    legend.position = "top" # Put the legend clearly at the top
  )
ls()
hr_data <-WA_Fn.UseC_.HR.Employee.Attrition
exists("hr_data")

library(readr) # Needed to fix data types automatically

# 1. Set the column names to be the values in the first row
colnames(hr_data) <- as.character(hr_data[1, ])

# 2. Remove the first row (since it's now the header)
hr_data <- hr_data[-1, ]

# 3. Automatically fix data types (converts text numbers back to real numbers)
hr_data <- type_convert(hr_data)

# 4. Check the names again to confirm
glimpse(hr_data)

# This puts quotes around names so you can see spaces
dput(names(hr_data))


# STEP 1: Create the column (KEEP THIS!)
hr_data <- hr_data %>% 
  mutate(
    tenure_group = case_when(
      YearsAtCompany <= 1 ~ "0-1 years",
      YearsAtCompany <= 3 ~ "1-3 years",
      YearsAtCompany <= 5 ~ "3-5 years",
      YearsAtCompany <= 10 ~ "5-10 years",
      TRUE ~ "10+ years"
    )
  )

# STEP 2: Fix the order (RUN THIS NEXT)
hr_data$tenure_group <- factor(hr_data$tenure_group, 
                               levels = c("0-1 years", "1-3 years", "3-5 years", "5-10 years", "10+ years"))

# STEP 3: Analyze (RUN THIS LAST)
tenure_stats <- hr_data %>%
  group_by(tenure_group) %>%
  summarise(
    count = n(),
    attrition_rate = mean(Attrition == "Yes")
  )
print(tenure_stats)

library(scales) # Needed for formatting percentages

ggplot(tenure_stats, aes(x = tenure_group, y = attrition_rate)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = percent(attrition_rate, accuracy = 1)), 
            vjust = -0.5, size = 3.5) + # Adds the % numbers on top of bars
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Attrition Risk by Tenure",
    subtitle = "New hires (0-1 years) are the most likely to leave",
    x = "Years at Company",
    y = "Attrition Rate"
  ) +
  theme_minimal()

# --- STEP 1: SETUP & LIBRARIES ---
# install.packages("caret") # Only run this once if not installed
library(caret)
library(dplyr)
library(ggplot2)

# Ensure 'Attrition' is a Factor
hr_data$Attrition <- as.factor(hr_data$Attrition)

# --- STEP 2: PREPARE DATA & SPLIT ---
# We select ALL variables we might want for both the math and the plot
# (Adding JobRole here because it's great for the plot)
model_data <- hr_data %>% 
  select(Attrition, Age, MonthlyIncome, OverTime, DistanceFromHome, YearsAtCompany, JobRole)

set.seed(123)
index <- createDataPartition(model_data$Attrition, p = 0.7, list = FALSE)
train_data <- model_data[index, ]
test_data  <- model_data[-index, ]

# --- STEP 3: TRAIN THE MODEL ---
# We train ONE model named 'final_model'
final_model <- glm(Attrition ~ ., data = train_data, family = "binomial")

# --- STEP 4: CHECK ACCURACY (The Math) ---
predictions_prob <- predict(final_model, test_data, type = "response")
predictions_class <- ifelse(predictions_prob > 0.5, "Yes", "No")

# Print the accuracy results
print("--- MODEL ACCURACY RESULTS ---")
confusionMatrix(as.factor(predictions_class), test_data$Attrition)

# --- STEP 5: VISUALIZE RISKS (The Catchy Plot) ---
# Extract the odds ratios from the SAME model we just built
risk_factors <- exp(coef(final_model)) 
risk_data <- data.frame(Variable = names(risk_factors), Risk = risk_factors)

# Clean up: Remove intercept, keep only high risks, and take top 10
plot_data <- risk_data %>% 
  filter(Variable != "(Intercept)", Risk > 1) %>%
  arrange(desc(Risk)) %>%
  head(10)

# The Plot
ggplot(plot_data, aes(x = reorder(Variable, Risk), y = Risk)) +
  geom_col(fill = "#FF6F61", width = 0.7) + 
  coord_flip() +
  geom_text(aes(label = round(Risk, 1)), hjust = -0.2, size = 5, fontface = "bold") +
  labs(title = "Top Risk Factors: Who is most likely to leave?",
       subtitle = "Interpretation: A score of 3.5 means they are 3.5x more likely to quit.",
       x = "",
       y = "Risk Factor (Odds Ratio)") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        plot.title = element_text(face = "bold", size = 16))