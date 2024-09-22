setwd("C:/Users/Administrator/OneDrive/help/评分模型")
# 加载必要的库
library(survival)
library(survminer)

# 读取数据
data <- read.csv("cox_imputed_iterative.csv", encoding="ANSI")

# 创建生存对象
surv_object <- Surv(time = data$DGF_time, event = data$DGF)

# 选择要分析的变量（排除DGF和DGF_time）
variables <- names(data)[!names(data) %in% c("DGF", "DGF_time")]

# 单因素Cox分析
univariate_results <- lapply(variables, function(var) {
  formula <- as.formula(paste("surv_object ~", var))
  model <- coxph(formula, data = data)
  coef <- summary(model)$coefficients
  conf <- summary(model)$conf.int
  data.frame(
    variable = var,
    HR = round(conf[1], 3),
    CI_95L = round(conf[3], 3),
    CI_95H = round(conf[4], 3),
    p_value = round(coef[5], 4)
  )
})

# 将结果整合到一个数据框中
univariate_df <- do.call(rbind, univariate_results)

# 选择p值小于0.1的变量
significant_vars <- univariate_df[univariate_df$p_value < 0.1 & !is.na(univariate_df$p_value), ]

# 按p值排序
significant_vars <- significant_vars[order(significant_vars$p_value), ]

# 打印显著变量的结果
print(significant_vars[, c("variable", "HR", "CI_95L", "CI_95H", "p_value")])



# 构建多因素Cox模型的公式univariate_df <- do.call(rbind, univariate_results)

# 选择p值小于0.1的变量
significant_vars <- univariate_df$variable[univariate_df$p_value < 0.1 & !is.na(univariate_df$p_value)]

# 构建多因素Cox模型的公式
multivariate_formula <- as.formula(paste("surv_object ~", paste(significant_vars, collapse = " + ")))

# 执行多因素Cox分析
multivariate_model <- coxph(multivariate_formula, data = data)

# 提取多因素分析结果
multivariate_summary <- summary(multivariate_model)
multivariate_results <- data.frame(
    variable = row.names(multivariate_summary$coefficients),
    HR = round(multivariate_summary$conf.int[, "exp(coef)"], 3),
    CI_95L = round(multivariate_summary$conf.int[, "lower .95"], 3),
    CI_95H = round(multivariate_summary$conf.int[, "upper .95"], 3),
    p_value = round(multivariate_summary$coefficients[, "Pr(>|z|)"], 4)
)

# 按p值排序
multivariate_results <- multivariate_results[order(multivariate_results$p_value), ]

# 打印多因素分析结果
print(multivariate_results)

#p小于0.1的变量
final_significant_vars <- multivariate_results[multivariate_results$p_value < 0.1, ]

# 按p值排序
final_significant_vars <- final_significant_vars[order(final_significant_vars$p_value), ]

# 打印最终显著变量的结果
print(final_significant_vars)