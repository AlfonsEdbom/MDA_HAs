library(readxl)

data = read_excel("Knee.xlsx")

data = data[,c(2, 5, 51,53,55,57)]

data = subset(data, I_Q_conNm_weight != "M" & I_Q_eccNm_weight != "M"
              & I_H_conNm_weight != "M" & I_H_eccNm_weight != "M")

data = as.matrix(data)
data = matrix(as.numeric(data), ncol = ncol(data))
data = data.frame(data)


names(data) <- c("Gender", "Group", "Q_Con", "Q_Ecc", "H_Con", "H_Ecc")
data$Gender= as.factor(data$Gender)
data$Group= as.factor(data$Group)

m_F <- manova(cbind(Q_Con, Q_Ecc, H_Con, H_Ecc) ~ Gender * Group, data = data)
m_R <- manova(cbind(Q_Con, Q_Ecc, H_Con, H_Ecc) ~ Gender + Group, data = data)

summary(m_F, test="Wilks")
summary(m_R, test="Wilks")


# need to do anova for only gender and only group effect?
anova_Q_Con = aov(data$Q_Con~data$Group * data$Gender)
summary(anova_Q_Con)

anova_Q_Ecc = aov(data$Q_Ecc~data$Group * data$Gender)
summary(anova_Q_Ecc)

anova_H_Con = aov(data$H_Con~data$Group * data$Gender)
summary(anova_H_Con)

anova_H_Ecc = aov(data$H_Ecc~data$Group * data$Gender)
summary(anova_H_Ecc)

# Dont need this

F_det <- det(cov(m_F$residuals))
R_det <- det(cov(m_R$residuals))


F_det/R_det
