# Simeon's work 

library(ggplot2)
library(tidyr)

df <- data.frame(group = c("No hit", "Bacteria", "Archae", "Eukaryotic", "Viruses"),
                 value = c(13, 9, 0, 2, 0))

df$percentage <- df$value / sum(df$value) * 100


ggp1 <- ggplot(df, aes(x = "", y = percentage, fill = group)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(fill = "Group", x = NULL, y = NULL, title = "Percentage of Each Group") +
  theme_void() +
  geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5))+
  theme(plot.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent"))


ggp1

df2 <- data.frame(group = c("No hit", "Bacteria", "Archae", "Eukaryotic", "Viruses"),
                  value = c(20, 10, 4, 3, 6))

df2$percentage <- df2$value / sum(df2$value) * 100


ggp2 <- ggplot(df2, aes(x = "", y = percentage, fill = group)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(fill = "Group", x = NULL, y = NULL, title = "Percentage of Each Group") +
  theme_void() +
  geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5))+
  theme(plot.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent"))


ggp1 + ggp2 
df2 <- data.frame(group = c("No hit", "Bacteria", "Archae", "Eukaryotic", "Viruses"),
                 value1 = c(13, 9, 0, 2, 0),
                 value2 = c(20, 10, 4, 3, 6))

df2$percentage1 <- df2$value1 / sum(df2$value1) * 100
df2$percentage2 <- df2$value2 / sum(df2$value2) * 100

df2_long <- df2 %>%
  pivot_longer(cols = starts_with("value"), names_to = "variable", values_to = "value") %>%
  mutate(percentage = value / sum(value) * 100)


ggplot(df2_long, aes(x = "", y = percentage, fill = variable)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(fill = "Variable", x = NULL, y = NULL, title = "Comparison of value1 and value2") +
  theme_void()



df <- data.frame(group = c("No hit", "Bacteria", "Archae", "Eukaryotic", "Viruses"),
                 value1 = c(13, 9, 0, 2, 0),
                 value2 = c(20, 10, 4, 3, 6))

df_long <- df %>%
  pivot_longer(cols = starts_with("value"), names_to = "variable", values_to = "value") %>%
  mutate(percentage = value / sum(value) * 100)

ggplot(df_long, aes(x = "", y = percentage, fill = as.factor(variable))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  facet_wrap(~ group) +
  labs(fill = "Variable", x = NULL, y = NULL, title = "Comparison of value1 and value2 for each group") +
  theme_void()





library(ggplot2)
library(tidyr)

df <- data.frame(group = c("No hit", "Bacteria", "Archae", "Eukaryotic", "Viruses"),
                 value1 = c(13, 9, 0, 2, 0),
                 value2 = c(20, 10, 4, 3, 6))

df_long <- df %>%
  pivot_longer(cols = starts_with("value"), names_to = "variable", values_to = "value") %>%
  mutate(percentage = value / sum(value) * 100)

ggplot(df_long, aes(x = "", y = percentage, fill = as.factor(variable))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  facet_wrap(~ group) +
  labs(fill = "Variable", x = NULL, y = NULL, title = "Comparison of value1 and value2 for each group") +
  theme_void() +
  geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5))




library(ggplot2)

df <- data.frame(group = c("No hit", "Bacteria", "Archae", "Eukaryotic", "Viruses"),
                 value = c(13, 9, 0, 2, 0))

df$percentage <- df$value / sum(df$value) * 100

ggplot(df, aes(x = "", y = percentage, fill = group)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(fill = "Group", x = NULL, y = NULL, title = "Percentage of Each Group") +
  theme_void() +
  theme(plot.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent"))







