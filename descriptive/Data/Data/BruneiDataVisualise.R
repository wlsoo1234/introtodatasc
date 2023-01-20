library(ggplot2)
library(readxl)

df <- read_excel("C:\\Users\\USER\\Desktop\\Data\\CleanedData\\Cleaned_Brunei_Data.xlsx")

ggplot(data = df, aes(x = Cancer, y = Number, fill = Cancer))+
  geom_bar(stat = "identity")+
  coord_flip()+
  labs(title = "Brunei Cancers Cases in 2020",
       x = "Types of Cancers", 
       y = "Number of Cases")

summary(df$Number)

