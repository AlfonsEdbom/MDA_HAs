library(readxl)

data = read_excel("Knee.xlsx")

data = data[,c(2, 5, 51,53,55,57)]

data = subset(data, I_Q_conNm_weight != "M" & I_Q_eccNm_weight != "M"
              & I_H_conNm_weight != "M" & I_H_eccNm_weight != "M")

data = as.matrix(data)
data = matrix(as.numeric(data), ncol = ncol(data))
data = data.frame(data)
names(data) <- c("Gender", "Group", "Q_Con", "Q_Ecc", "H_Con", "H_Ecc")


m_F <- manova(cbind(Q_Con, Q_Ecc, H_Con, H_Ecc) ~ Gender * Group, data = data)
m_R <- manova(cbind(Q_Con, Q_Ecc, H_Con, H_Ecc) ~ Gender + Group, data = data)


F_det <- det(cov(m_F$residuals))
R_det <- det(cov(m_R$residuals))


F_det/R_det 

summary(m_F, test="Wilks")
