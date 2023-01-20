library(ggplot2)
library(readxl)

df <- read_excel("C:\\Users\\USER\\Desktop\\Data\\CleanedData\\Top10_Indonesia_Data.xlsx")

ggplot(data = df, aes(x = "", y = -Number, 
                      fill = reorder(Cancer, -Number)))+
  geom_bar(stat = "identity", color = "black")+
  #geom_text(aes(label = Cancer),
  #position = position_stack(vjust = 0.5),
  #show.legend = FALSE)+
  labs(title = "Top 10 Cancers in Indonesia in 2020")+
  coord_polar("y")+
  theme_void()
