
setwd("C:/Users/Markus/uni-hohenheim.de/Robert Jung - 520KOneDrive/MA_Lehre/MA_EMBE/MA_EMBE_2122/MA_EconometricMethods_WiSe2122/01_ExerciseSheet_No01")

library(AER)

data("CASchools")
head(CASchools)

dat <- CASchools[,c("students","teachers","read","math")]
head(dat)

write.table(dat,
            file = "CASchools_01_data.txt",
            sep = ",",
            row.names = FALSE)