# Load required libraries
library(ggplot2)
library(GGally)
library(reshape2)
library(corrplot)
library(randomForest)
library(caret)

# Load dataset
data <- read.csv("C:/Users/USER/Downloads/Normalized_StressLevelToPerformance.csv")

# ---- Figure 1: Bar chart of students reporting high negative conditions (>0.7 threshold) ----
factors <- c("anxiety_level", "sleep_quality", "living_conditions", "study_load", "bullying")
neg_counts <- sapply(factors, function(v) sum(data[[v]] > 0.7))
bar_data <- data.frame(Factor = factors, Count = neg_counts)
p1 <- ggplot(bar_data, aes(x = reorder(Factor, Count), y = Count, fill = Factor)) +
  geom_bar(stat = "identity") + coord_flip() +
  labs(title = "Figure 1: Number of Students Reporting High Negative Conditions", x = "Factor", y = "Number of Students") +
  theme_minimal() + theme(legend.position = "none")


ggsave("C:/Users/USER/Downloads/Figure1_BarChart.png", p1, width = 7, height = 5)

# ---- Figure 2: Pair plot of key psychological and performance variables ----
key_vars <- data[, c("anxiety_level", "self_esteem", "depression", "sleep_quality", "academic_performance")]
p2 <- ggpairs(key_vars,
              upper = list(continuous = wrap("points", alpha=0.3, size=1)),
              diag = list(continuous = "barDiag"),
              lower = list(continuous = wrap("density", alpha=0.5))) +
  ggtitle("Figure 2: Pair Plot of Psychological and Performance Variables")

ggsave("C:/Users/USER/Downloads/Figure2_PairPlot.png", p2, width = 8, height = 8)

# ---- Figure 3: Original corrplot correlation heatmap ----
cor_mat <- cor(key_vars)

png("C:/Users/USER/Downloads/Figure3_CorrelationHeatmap.png", width = 700, height = 700)
corrplot(cor_mat, method = "color", addCoef.col = "black", number.cex = 0.7,
         tl.cex = 0.8, tl.col = "black", title = "Figure 3: Correlation Heatmap", mar = c(0,0,2,0))
dev.off()

# ---- Figure 4: Boxplots of key variables ----
melted_vars <- melt(key_vars)
p4 <- ggplot(melted_vars, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  labs(title = "Figure 4: Boxplots of Psychological and Academic Variables", x = "Variable", y = "Value") +
  theme_minimal() + theme(legend.position = "none")

ggsave("C:/Users/USER/Downloads/Figure4_Boxplots.png", p4, width = 7, height = 5)

# ---- Feature importance plotting function ----
plot_feature_importance <- function(predictors, title, color) {
  formula <- as.formula(paste("stress_level ~", paste(predictors, collapse = "+")))
  rf_model <- randomForest(formula, data = data, importance = TRUE)
  imp <- importance(rf_model, type = 1)
  imp_df <- data.frame(Feature = rownames(imp), Importance = imp[,1])
  p <- ggplot(imp_df, aes(x = reorder(Feature, Importance), y = Importance, fill = color)) +
    geom_bar(stat = "identity") + coord_flip() +
    labs(title = title, x = "Feature", y = "Mean Decrease Accuracy") +
    theme_minimal() + theme(legend.position = "none")
  return(p)
}

# ---- Figures 5 to 9: Feature importance by factor group ----
psych_vars <- c("anxiety_level", "self_esteem", "depression", "mental_health_history")
p5 <- plot_feature_importance(psych_vars, "Figure 5: Psychological Factors Importance on Stress", "skyblue")
ggsave("C:/Users/USER/Downloads/Figure5_PsychImportance.png", p5, width = 7, height = 5)

phys_vars <- c("headache", "blood_pressure", "sleep_quality", "breathing_problem")
p6 <- plot_feature_importance(phys_vars, "Figure 6: Physiological Factors Importance on Stress", "orange")
ggsave("C:/Users/USER/Downloads/Figure6_PhysImportance.png", p6, width = 7, height = 5)

env_vars <- c("noise_level", "living_conditions", "safety", "basic_needs")
p7 <- plot_feature_importance(env_vars, "Figure 7: Environmental Factors Importance on Stress", "forestgreen")
ggsave("C:/Users/USER/Downloads/Figure7_EnvImportance.png", p7, width = 7, height = 5)

acad_vars <- c("study_load", "academic_performance", "teacher_student_relationship", "future_career_concerns")
p8 <- plot_feature_importance(acad_vars, "Figure 8: Academic Factors Importance on Stress", "purple")
ggsave("C:/Users/USER/Downloads/Figure8_AcadImportance.png", p8, width = 7, height = 5)

soc_vars <- c("social_support", "peer_pressure", "extracurricular_activities", "bullying")
p9 <- plot_feature_importance(soc_vars, "Figure 9: Social Factors Importance on Stress", "red")
ggsave("C:/Users/USER/Downloads/Figure9_SocImportance.png", p9, width = 7, height = 5)

# ---- Predictive Modeling: Random Forest classification for high academic performers ----
data$high_performer <- ifelse(data$academic_performance >= quantile(data$academic_performance, 0.75), 1, 0)
set.seed(123)
train_index <- createDataPartition(data$high_performer, p = 0.7, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

rf_classifier <- randomForest(as.factor(high_performer) ~ anxiety_level + depression + self_esteem + sleep_quality + study_load + bullying + basic_needs + teacher_student_relationship, 
                              data = train_data, importance = TRUE)

pred <- predict(rf_classifier, test_data)
conf_mat <- confusionMatrix(pred, as.factor(test_data$high_performer))

model_accuracy <- conf_mat$overall['Accuracy']
model_kappa <- conf_mat$overall['Kappa']

var_imp <- importance(rf_classifier, type = 1)
var_imp_df <- data.frame(Feature = rownames(var_imp), Importance = var_imp[,1])
p10 <- ggplot(var_imp_df, aes(x = reorder(Feature, Importance), y = Importance, fill = Importance)) +
  geom_bar(stat = "identity") + coord_flip() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Figure 10: Variable Importance in Predicting High Academic Performance", x = "Feature", y = "Mean Decrease Accuracy") +
  theme_minimal() + theme(legend.position = "none")

ggsave("C:/Users/USER/Downloads/Figure10_ModelVarImportance.png", p10, width = 7, height = 5)

# ---- Display all figures one by one ----
print(p1)
print(p2)
# Display corrplot heatmap (base R plot)
corrplot(cor_mat, method = "color", addCoef.col = "black", number.cex = 0.7,
         tl.cex = 0.8, tl.col = "black", title = "Figure 3: Correlation Heatmap", mar = c(0,0,2,0))
print(p4)
print(p5)
print(p6)
print(p7)
print(p8)
print(p9)
print(p10)

# ---- Print model evaluation results ----
print(conf_mat)
cat("\nModel Accuracy:", round(model_accuracy, 4), "\n")
cat("Model Kappa:", round(model_kappa, 4), "\n")


