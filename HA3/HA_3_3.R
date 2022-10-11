library(readxl)

data = read_excel("Knee.xlsx")


# Get the knee strength data for the different groups
group_1 = data[which(data[, 5]==1),51:58]
group_2 = data[which(data[, 5]==2),51:58]
group_3 = data[which(data[, 5]==3),51:58]

# Remove rows containing missing data
group_1 <- subset(group_1, I_Q_conNm_weight != "M" & C_Q_conNm_weight != "M"
                     & I_Q_eccNm_weight != "M" & C_Q_eccNm_weight != "M"
                     & I_H_conNm_weight != "M" & C_H_conNm_weight != "M"
                     & I_H_eccNm_weight != "M" & C_H_eccNm_weight != "M")

group_2 <- subset(group_2, I_Q_conNm_weight != "M" & C_Q_conNm_weight != "M"
                     & I_Q_eccNm_weight != "M" & C_Q_eccNm_weight != "M"
                     & I_H_conNm_weight != "M" & C_H_conNm_weight != "M"
                     & I_H_eccNm_weight != "M" & C_H_eccNm_weight != "M")

group_3 <- subset(group_3, I_Q_conNm_weight != "M" & C_Q_conNm_weight != "M"
                     & I_Q_eccNm_weight != "M" & C_Q_eccNm_weight != "M"
                     & I_H_conNm_weight != "M" & C_H_conNm_weight != "M"
                     & I_H_eccNm_weight != "M" & C_H_eccNm_weight != "M")


# Convert into numerical matrix
df1 = as.matrix(knee_no_m1)
df1 = matrix(as.numeric(df1), ncol = ncol(df1))

df2 = as.matrix(knee_no_m2)
df2 = matrix(as.numeric(df2), ncol = ncol(df2))

df3 = as.matrix(knee_no_m3)
df3 = matrix(as.numeric(df3), ncol = ncol(df3))