library(ggplot2)
library(readxl)

df <- read_excel("C:\\Users\\USER\\Desktop\\Data\\CleanedData\\Cleaned_SEA_Cancer.xlsx")

ggplot(data = df, aes(x = Cancer, y = Number, fill = Cancer))+
  geom_bar(stat = "identity")+
  coord_flip()+
  labs(title = "Southeast Asia Cancer Cases in 2020",
       x = "Types of Cancers", y = "Number of Cases")

summary(df$Number)
