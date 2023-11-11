library(tidyverse)
library(sjPlot)

Data <- read.table("./Data/eye-tracking_Jafarnejad.tsv", sep = "\t", header = T)

# correcting AOI name
Data$AOI <- str_replace_all(Data$AOI, "package  7 Right", "package 7 Right")

# correcting variable names
Dataa <- Data |> mutate(TFD = Total_duration_of_fixations) |> select(-starts_with("Total_duration_of_fixations"))


# Recordings to be removed
Data1 <- Dataa |> subset(Recording != "Recording1" &
                         Recording != "Recording2" &
                         Recording != "Recording24" &
                         Recording != "Recording3"&
                         Recording != "Recording45")

# Recordings 1, 2, and 3 were pilot tests. Recording 45 and 24 were excluded due to software error during the experiment.

# removing training trials
Data2 <- Data1 |> subset(AOI != "Whole Test" &
                           AOI != "package left Test" &
                           AOI != "package Right Test" &
                           AOI != "None Test")


# removing entire recording metrics
Data3 <- Data2 |> subset(TOI != "Entire Recording")

# xtabs(~AOI + Recording, Data2) |> View()
#xtabs(~ Recording, Data) |> View()

# Creating sub_id
rle <- rle(Data3$Recording)
Data3$sub_id <- rep(seq_along(rle$lengths), rle$lengths)

# Creating ch_id
rle <- rle(Data3$Media)
Data3$chid <- rep(seq_along(rle$lengths), rle$lengths)

# Normalizing
# Data4 <- Data3 |> group_by(chid) |> mutate(TFD.n.wh = Total_duration_of_whole_fixations / subset(Data3, Data3$AOI == "Whole")[,20])

# removing extra variables

Data4 <- Data3 |> select(-c("Re.reading_duration",
                            "Last_key_press",
                            "Regression.path_duration",
                            "First.pass_regression",
                            "Selective_regression.path_duration",
                            "First.pass_duration",
                            "First.pass_first_fixation_duration",
                            "Number_of_units",
                            "Text_unit_type",
                            "AOI_string",
                            "Sentence_index",
                            "Word_index", "Character_index",
                            "Timeline",
                            "TOI",
                            "AOI_at_interval_end",
                            "Time_to_first_mouse_click_and_release",
                            "Interval",
                            "Number_of_mouse_clicks_and_releases", "Time_from_first_fixation_to_mouse_click_and_release"))

# remove whole AOI from last AOI viewed

Data4$Last_AOI_viewed <- gsub(",Whole", "", Data4$Last_AOI_viewed)


# keep metrics for whole AOI separately
whole.aoi <- Data4 |> filter(AOI == "Whole") |> select(c(6:43, 45))

colnames(whole.aoi) <- paste(colnames(whole.aoi), "_whole_screen", sep = "")

# Removing whole AOI
Data5 <- subset(Data4, Data4$AOI != "Whole")

Data5 <- Data5 |> mutate(choice = Number_of_mouse_clicks)

# add whole aoi info
Data6 <- left_join(Data5, whole.aoi, by = (c("chid" = "chid_whole_screen")))

# specify the location of each alternative

Data6 <- Data6 |> mutate(loc = ifelse(str_detect(AOI ,"left"), "left",
                                      ifelse(str_detect(AOI ,"Right"), "right", "none")))

# specify the ID of each alternative

Data6 <- Data6 |> mutate(alt_id = extract_numeric(AOI))
Data7 <- Data6[,c(44 ,2:3, 45, 4, 85:86, 46, 6:43, 47:84)]


# add the questionnaire data

# add roundbess and spherity metrics

# remove participants which chose none option in more than 25 percent of the trials

# remove trials in which no valid alternative has been chosen

shape.res <- read.csv("./Data/resultsNorm.csv", header = T)


Data8 <- left_join(Data7, shape.res, by = c("alt_id" = "packageNum"))

Data9 <- Data8 |> filter(loc != "none")

Data9 <- Data9[,c(1:7, 85:90, 8:84)]
Data10 <- Data9 |> mutate(areaNorm = round(areaNorm, 4),
                          perimeterNorm = round(perimeterNorm, 4),
                          ConvexAreaNorm = round(ConvexAreaNorm, 4),
                          EccentricityNorm = round(EccentricityNorm, 4),
                          ExtentNorm = round(ExtentNorm, 4),
                          roundnessNorm = round(roundnessNorm, 4),
                          choice = factor(choice),
                          sub_id = factor(sub_id),
                          alt_id = factor(alt_id),
                          chid = factor(chid),
                          Media = factor(Media),
                          choice = ifelse(Data9$choice == 1, T, F))

Data11 <- Data10 |> mutate(NoF_n = Number_of_fixations /
                             Number_of_fixations_whole_screen,
                           APD_n = Average_pupil_diameter /
                             Average_pupil_diameter_whole_screen,
                           TDoWF_n = Total_duration_of_whole_fixations /                         Total_duration_of_whole_fixations_whole_screen,
                           NoWF_n = Number_of_whole_fixations /
                             Number_of_whole_fixations_whole_screen,
                           AWFPD_n = `Average_whole.fixation_pupil_diameter` /                         `Average_whole.fixation_pupil_diameter_whole_screen`,
                           TDoV_n = Total_duration_of_Visit /
                             Total_duration_of_Visit_whole_screen,
                           TFD_n = TFD / TFD_whole_screen,
                            NoF_n_t = Number_of_fixations /
                             Time_to_first_mouse_click_whole_screen,
                           TFD_n_t = TFD /
                             Time_to_first_mouse_click_whole_screen,
                           TDoWF_n_t = Total_duration_of_whole_fixations /
                             Time_to_first_mouse_click_whole_screen,
                           NoWF_n_t = Number_of_whole_fixations /
                              Time_to_first_mouse_click_whole_screen,
                           NoV_n_t = Number_of_Visits /
                               Time_to_first_mouse_click_whole_screen)

# glm <- glm(choice ~ Total_duration_of_whole_fixations + areaNorm + perimeterNorm + ConvexAreaNorm + EccentricityNorm + ExtentNorm + roundnessNorm + areaNorm:TFD + perimeterNorm:TFD + ConvexAreaNorm:TFD + EccentricityNorm:TFD + roundnessNorm:TFD, family = "binomial", data = Data10)
#
# sjPlot::tab_model(glm)

# choice.df <- xtabs(~sub_id + alt_id + choice, Data11) |>
#   as.data.frame() |> filter(choice == TRUE) |> mutate(prob= Freq / 72) |>
#   select(-"choice", -"Freq")

choice.df2 <- xtabs(~Participant + alt_id + choice, Data11) |>
  as.data.frame() |> filter(choice == TRUE) |> mutate(prob= Freq / 72) |>
  select(-"choice", -"Freq")

choice.df2 |> group_by(alt_id) |> summarise(Mean = mean(prob),
                                            Median = median(prob),
                                            SD = sd(prob)) |>
  mutate(Mean = round(Mean, 2),
         Median = round(Median, 2),
         SD = round(SD, 2)) |> write.csv("./exports/package_preference.csv")

# choice.df <- choice.df |> mutate(sub_id = as.numeric(sub_id),
#                                  alt_id = as.numeric(alt_id))
#
# Data11 <- Data11 |> mutate(sub_id = as.numeric(sub_id),
#                                  alt_id = as.numeric(alt_id))

Data12 <- left_join(Data11, choice.df2, by = c("Participant" = "Participant",                                               "alt_id" = "alt_id"))

gender <- read.csv("./Data/gender.csv")
Data12 <- left_join(Data12, gender, by = "Participant")

alt.prob <- Data12 |> group_by(choice) |> ggplot()+
  aes(x= reorder(alt_id, prob), y = prob, group = alt_id)+ geom_violin(aes(fill = alt_id), trim = FALSE) +
  geom_boxplot(width = 0.08, position = position_dodge(width = 0.9))+
  scale_fill_manual(values = c("#ADD8E6", "#87CEEB", "#0000FF", "#F0F8FF", "#1E90FF", "#4169E1", "#87CEFA", "#00008B", "#00BFFF")) +
  labs(x = "Alternative ID", y = "Choice probability", fill = "Alternative ID") +
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = round(seq(min(Data12$prob),
                                        max(Data12$prob), by = 0.05),2))

ggsave("./exports/alt.prob.png",alt.prob,
       width = 6,
       height = 3)

area.choice <- Data12 |> group_by(choice) |> ggplot()+
  aes(x= choice, y = areaNorm, group = choice, fill = choice)+
  geom_boxplot(notch = T)

perimeter.choice <- Data12 |> group_by(choice) |> ggplot()+
  aes(x= choice, y = perimeterNorm, group = choice, fill = choice)+
  geom_boxplot(notch = T)

ConvexArea.choice <- Data12 |> group_by(choice) |> ggplot()+
  aes(x= choice, y = ConvexAreaNorm, group = choice, fill = choice)+
  geom_boxplot(notch = T)

Eccentricity.choice <- Data12 |> group_by(choice) |> ggplot()+
  aes(x= choice, y = EccentricityNorm, group = choice, fill = choice)+
  geom_boxplot(notch = T)

Extent.choice <- Data12 |> group_by(choice) |> ggplot()+
  aes(x= choice, y = ExtentNorm, group = choice, fill = choice)+
  geom_boxplot(notch = T)

roundness.choice <- Data12 |> group_by(choice) |> ggplot()+
  aes(x= choice, y = roundnessNorm, group = choice, fill = choice)+
  geom_boxplot(notch = T)

shape.choice <- ggpubr::ggarrange(area.choice,
                                  perimeter.choice,
                                  ConvexArea.choice,
                                  Eccentricity.choice,
                                  Extent.choice,
                                  roundness.choice,
                                  labels = c("A", " B",
                                             "C", " D",
                                             "E", "F"),
                                  ncol = 3, nrow = 2)

ggsave("./exports/shape.choice.png", shape.choice,
       width = 10,
       height = 6)


area.prob <- Data12 |> mutate(gender = factor(gender))  |>
  ggplot(aes(x= areaNorm, y = prob, color = gender)) +
  geom_smooth(method = "glm")+
  scale_color_discrete(labels = c("Female", "Male")) +
  labs(x = "Normalized Area", y = "Choice probability", color = "Gender") +
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = round(seq(min(Data12$prob),
                                        max(Data12$prob), by = 0.01),3))

perimeter.prob <- Data12 |> mutate(gender = factor(gender)) |>  ggplot(aes(x= perimeterNorm, y = prob, color = gender)) +
  geom_smooth(method = "glm")+
  scale_color_discrete(labels = c("Female", "Male")) +
  labs(x = "Normalized Perimeter", y = "Choice probability", color = "Gender") +
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = round(seq(min(Data12$prob),
                                        max(Data12$prob), by = 0.01),3))

ConvexArea.prob <- Data12 |> mutate(gender = factor(gender)) |>  ggplot(aes(x= ConvexAreaNorm, y = prob, color = gender)) +
  geom_smooth(method = "glm")+
  scale_color_discrete(labels = c("Female", "Male")) +
  labs(x = "Normalized Convex Area", y = "Choice probability", color = "Gender") +
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = round(seq(min(Data12$prob),
                                        max(Data12$prob), by = 0.01),3))

Eccentricity.prob <- Data12 |> mutate(gender = factor(gender)) |>  ggplot(aes(x= EccentricityNorm, y = prob, color = gender)) +
  geom_smooth(method = "glm")+
  scale_color_discrete(labels = c("Female", "Male")) +
  labs(x = "Normalized Eccentricity", y = "Choice probability", color = "Gender") +
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = round(seq(min(Data12$prob),
                                        max(Data12$prob), by = 0.01),3))

Extent.prob <- Data12 |> mutate(gender = factor(gender)) |>  ggplot(aes(x= ExtentNorm, y = prob, color = gender)) +
  geom_smooth(method = "glm")+
  scale_color_discrete(labels = c("Female", "Male")) +
  labs(x = "Normalized Extent", y = "Choice probability", color = "Gender") +
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = round(seq(min(Data12$prob),
                                        max(Data12$prob), by = 0.01),3))

roundness.prob <- Data12 |> mutate(gender = factor(gender)) |>  ggplot(aes(x= roundnessNorm, y = prob, color = gender)) +
  geom_smooth(method = "glm")+
  scale_color_discrete(labels = c("Female", "Male")) +
  labs(x = "Normalized Roundness", y = "Choice probability", color = "Gender") +
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = round(seq(min(Data12$prob),
                                        max(Data12$prob), by = 0.01),3))

shape.prob <- ggpubr::ggarrange(area.prob,
                                  perimeter.prob,
                                  ConvexArea.prob,
                                  Eccentricity.prob,
                                  Extent.prob,
                                  roundness.prob,
                                  labels = c("A", " B",
                                             "C", " D",
                                             "E", "F"),
                                  ncol = 3, nrow = 2,
                                common.legend = TRUE, legend = "bottom")

  ggsave("./exports/shape.prob.png", shape.prob,
       width = 10,
       height = 6)

area.DoWF <- Data12 |> mutate(gender = factor(gender)) |>  ggplot(aes(x= areaNorm, y = Average_duration_of_whole_fixations, color = gender)) +
  geom_smooth(method = "glm")+
  scale_color_discrete(labels = c("Female", "Male")) +
  labs(x = "Normalized Area", y = "Average Duration of Whole Fixations (ms)", color = "Gender")

perimeter.DoWF <- Data12 |> mutate(gender = factor(gender)) |>  ggplot(aes(x= perimeterNorm, y = Average_duration_of_whole_fixations, color = gender)) +
  geom_smooth(method = "glm")+
  scale_color_discrete(labels = c("Female", "Male")) +
  labs(x = "Normalized Perimeter", y = "Average Duration of Whole Fixations (ms)", color = "Gender")

ConvexArea.DoWF <- Data12 |>  mutate(gender = factor(gender)) |>
  ggplot(aes(x= ConvexAreaNorm, y = Average_duration_of_whole_fixations, color = gender)) +
  geom_smooth(method = "glm")+
  scale_color_discrete(labels = c("Female", "Male")) +
  labs(x = "Normalized Convex Area", y = "Average Duration of Whole Fixations (ms)", color = "Gender")

Eccentricity.DoWF <- Data12  |> mutate(gender = factor(gender)) |>  ggplot(aes(x= EccentricityNorm, y = Average_duration_of_whole_fixations, color = gender)) +
  geom_smooth(method = "glm")+
  scale_color_discrete(labels = c("Female", "Male")) +
  labs(x = "Normalized Eccentricity", y = "Average Duration of Whole Fixations (ms)", color = "Gender")

Extent.DoWF <- Data12  |> mutate(gender = factor(gender)) |>  ggplot(aes(x= ExtentNorm, y = Average_duration_of_whole_fixations, color = gender)) +
  geom_smooth(method = "glm")+
  scale_color_discrete(labels = c("Female", "Male")) +
  labs(x = "Normalized Extent", y = "Average Duration of Whole Fixations (ms)", color = "Gender")

roundness.DoWF <- Data12  |> mutate(gender = factor(gender)) |>  ggplot(aes(x= roundnessNorm, y = Average_duration_of_whole_fixations, color = gender)) +
  geom_smooth(method = "glm")+
  scale_color_discrete(labels = c("Female", "Male")) +
  labs(x = "Normalized Roundness", y = "Average Duration of Whole Fixations (ms)", color = "Gender")

shape.DoWF <- ggpubr::ggarrange(area.DoWF,
                                  perimeter.DoWF,
                                  ConvexArea.DoWF,
                                  Eccentricity.DoWF,
                                  Extent.DoWF,
                                  roundness.DoWF,
                                  labels = c("A", " B",
                                             "C", " D",
                                             "E", "F"),
                                  ncol = 3, nrow = 2,
                                common.legend = TRUE, legend = "bottom")

ggsave("./exports/shape.DoWF.png", shape.DoWF,
       width = 12,
       height = 8)



area.AWFPD_n <- Data12 |> mutate(gender = factor(gender)) |>  ggplot(aes(x= areaNorm, y = AWFPD_n, color = gender)) +
  geom_smooth(method = "glm")+
  scale_color_discrete(labels = c("Female", "Male")) +
  labs(x = "Normalized Area", y = "Normalized AWFPD*", color = "Gender")

perimeter.AWFPD_n <- Data12 |> mutate(gender = factor(gender)) |>  ggplot(aes(x= perimeterNorm, y = AWFPD_n, color = gender)) +
  geom_smooth(method = "glm")+
  scale_color_discrete(labels = c("Female", "Male")) +
  labs(x = "Normalized Perimeter", y = "Normalized AWFPD*", color = "Gender")


ConvexArea.AWFPD_n <- Data12 |> mutate(gender = factor(gender)) |>  ggplot(aes(x= ConvexAreaNorm, y = AWFPD_n, color = gender)) +
  geom_smooth(method = "glm")+
  scale_color_discrete(labels = c("Female", "Male")) +
  labs(x = "Normalized Convex Area", y = "Normalized AWFPD*", color = "Gender")


Eccentricity.AWFPD_n <- Data12 |> mutate(gender = factor(gender)) |>  ggplot(aes(x= EccentricityNorm, y = AWFPD_n, color = gender)) +
  geom_smooth(method = "glm")+
  scale_color_discrete(labels = c("Female", "Male")) +
  labs(x = "Normalized Eccentricity", y = "Normalized AWFPD*", color = "Gender")


Extent.AWFPD_n <- Data12 |> mutate(gender = factor(gender)) |>  ggplot(aes(x= ExtentNorm, y = AWFPD_n, color = gender)) +
  geom_smooth(method = "glm")+
  scale_color_discrete(labels = c("Female", "Male")) +
  labs(x = "Normalized Extent", y = "Normalized AWFPD*", color = "Gender")


roundness.AWFPD_n <- Data12 |> mutate(gender = factor(gender)) |>  ggplot(aes(x= roundnessNorm, y = AWFPD_n, color = gender, color = gender)) +
  geom_smooth(method = "glm")+
  scale_color_discrete(labels = c("Female", "Male")) +
  labs(x = "Normalized Roundness", y = "Normalized AWFPD*", color = "Gender")


shape.AWFPD_n <- ggpubr::ggarrange(area.AWFPD_n,
                                  perimeter.AWFPD_n,
                                  ConvexArea.AWFPD_n,
                                  Eccentricity.AWFPD_n,
                                  Extent.AWFPD_n,
                                  roundness.AWFPD_n,
                                  labels = c("A", " B",
                                             "C", " D",
                                             "E", "F"),
                                  ncol = 3, nrow = 2,
                                common.legend = TRUE, legend = "bottom")

ggsave("./exports/shape.AWFPD_n.png", shape.AWFPD_n,
       width = 10,
       height = 6)


area.NoWF_n_t <- Data12 |> mutate(NoWF_n_t = scale(NoWF_n_t)) |> mutate(gender = factor(gender)) |>  ggplot(aes(x= areaNorm, y = NoWF_n_t, color = gender)) +
  geom_smooth(method = "glm")+
  scale_color_discrete(labels = c("Female", "Male")) +
  labs(x = "Normalized Area", y = "Normalized Number of Whole Fixations", color = "Gender")

perimeter.NoWF_n_t <- Data12 |> mutate(NoWF_n_t = scale(NoWF_n_t)) |>  mutate(gender = factor(gender)) |>  ggplot(aes(x= perimeterNorm, y = NoWF_n_t, color = gender)) +
  geom_smooth(method = "glm")+
  scale_color_discrete(labels = c("Female", "Male")) +
  labs(x = "Normalized Perimeter", y = "Normalized Number of Whole Fixations", color = "Gender")


ConvexArea.NoWF_n_t <- Data12 |> mutate(NoWF_n_t = scale(NoWF_n_t)) |> mutate(gender = factor(gender)) |>  ggplot(aes(x= ConvexAreaNorm, y = NoWF_n_t, color = gender)) +
  geom_smooth(method = "glm")+
  scale_color_discrete(labels = c("Female", "Male")) +
  labs(x = "Normalized Convex Area", y = "Normalized Number of Whole Fixations", color = "Gender")


Eccentricity.NoWF_n_t <- Data12 |> mutate(NoWF_n_t = scale(NoWF_n_t)) |> mutate(gender = factor(gender)) |>  ggplot(aes(x= EccentricityNorm, y = NoWF_n_t, color = gender)) +
  geom_smooth(method = "glm")+
  scale_color_discrete(labels = c("Female", "Male")) +
  labs(x = "Normalized Eccentricity", y = "Normalized Number of Whole Fixations", color = "Gender")


Extent.NoWF_n_t <- Data12 |> mutate(NoWF_n_t = scale(NoWF_n_t)) |> mutate(gender = factor(gender)) |>  ggplot(aes(x= ExtentNorm, y = NoWF_n_t, color = gender)) +
  geom_smooth(method = "glm")+
  scale_color_discrete(labels = c("Female", "Male")) +
  labs(x = "Normalized Extent", y = "Normalized Number of Whole Fixations", color = "Gender")


roundness.NoWF_n_t <- Data12 |>mutate(NoWF_n_t = scale(NoWF_n_t)) |>  mutate(gender = factor(gender)) |>  ggplot(aes(x= roundnessNorm, y = NoWF_n_t, color = gender, color = gender)) +
  geom_smooth(method = "glm")+
  scale_color_discrete(labels = c("Female", "Male")) +
  labs(x = "Normalized Roundness", y = "Normalized Number of Whole Fixations", color = "Gender")


shape.NoWF_n_t <- ggpubr::ggarrange(area.NoWF_n_t,
                                  perimeter.NoWF_n_t,
                                  ConvexArea.NoWF_n_t,
                                  Eccentricity.NoWF_n_t,
                                  Extent.NoWF_n_t,
                                  roundness.NoWF_n_t,
                                  labels = c("A", " B",
                                             "C", " D",
                                             "E", "F"),
                                  ncol = 3, nrow = 2,
                                common.legend = TRUE, legend = "bottom")

ggsave("./exports/shape.NoWF_n_t.png", shape.NoWF_n_t,
       width = 12,
       height = 8)


area.TDoWF_n_t <- Data12 |> mutate(TDoWF_n_t = scale(TDoWF_n_t)) |> mutate(gender = factor(gender)) |>  ggplot(aes(x= areaNorm, y = TDoWF_n_t, color = gender)) +
  geom_smooth(method = "glm")+
  scale_color_discrete(labels = c("Female", "Male")) +
  labs(x = "Normalized Area", y = "Normalized TDoWF*", color = "Gender")

perimeter.TDoWF_n_t <- Data12 |> mutate(TDoWF_n_t = scale(TDoWF_n_t)) |>  mutate(gender = factor(gender)) |>  ggplot(aes(x= perimeterNorm, y = TDoWF_n_t, color = gender)) +
  geom_smooth(method = "glm")+
  scale_color_discrete(labels = c("Female", "Male")) +
  labs(x = "Normalized Perimeter", y = "Normalized TDoWF*", color = "Gender")


ConvexArea.TDoWF_n_t <- Data12 |> mutate(TDoWF_n_t = scale(TDoWF_n_t)) |> mutate(gender = factor(gender)) |>  ggplot(aes(x= ConvexAreaNorm, y = TDoWF_n_t, color = gender)) +
  geom_smooth(method = "glm")+
  scale_color_discrete(labels = c("Female", "Male")) +
  labs(x = "Normalized Convex Area", y = "Normalized TDoWF*", color = "Gender")


Eccentricity.TDoWF_n_t <- Data12 |> mutate(TDoWF_n_t = scale(TDoWF_n_t)) |> mutate(gender = factor(gender)) |>  ggplot(aes(x= EccentricityNorm, y = TDoWF_n_t, color = gender)) +
  geom_smooth(method = "glm")+
  scale_color_discrete(labels = c("Female", "Male")) +
  labs(x = "Normalized Eccentricity", y = "Normalized TDoWF*", color = "Gender")


Extent.TDoWF_n_t <- Data12 |> mutate(TDoWF_n_t = scale(TDoWF_n_t)) |> mutate(gender = factor(gender)) |>  ggplot(aes(x= ExtentNorm, y = TDoWF_n_t, color = gender)) +
  geom_smooth(method = "glm")+
  scale_color_discrete(labels = c("Female", "Male")) +
  labs(x = "Normalized Extent", y = "Normalized TDoWF*", color = "Gender")


roundness.TDoWF_n_t <- Data12 |>mutate(TDoWF_n_t = scale(TDoWF_n_t)) |>  mutate(gender = factor(gender)) |>  ggplot(aes(x= roundnessNorm, y = TDoWF_n_t, color = gender, color = gender)) +
  geom_smooth(method = "glm")+
  scale_color_discrete(labels = c("Female", "Male")) +
  labs(x = "Normalized Roundness", y = "Normalized TDoWF*", color = "Gender")


shape.TDoWF_n_t <- ggpubr::ggarrange(area.TDoWF_n_t,
                                  perimeter.TDoWF_n_t,
                                  ConvexArea.TDoWF_n_t,
                                  Eccentricity.TDoWF_n_t,
                                  Extent.TDoWF_n_t,
                                  roundness.TDoWF_n_t,
                                  labels = c("A", " B",
                                             "C", " D",
                                             "E", "F"),
                                  ncol = 3, nrow = 2,
                                common.legend = TRUE, legend = "bottom")

ggsave("./exports/shape.TDoWF_n_t.png", shape.TDoWF_n_t,
       width = 10,
       height = 6)


area.TDoV_n <- Data12 |> mutate(TDoV_n = scale(TDoV_n)) |> mutate(gender = factor(gender)) |>  ggplot(aes(x= areaNorm, y = TDoV_n, color = gender)) +
  geom_smooth(method = "glm")+
  scale_color_discrete(labels = c("Female", "Male")) +
  labs(x = "Normalized Area", y = "Normalized TDoV*", color = "Gender")

perimeter.TDoV_n <- Data12 |> mutate(TDoV_n = scale(TDoV_n)) |>  mutate(gender = factor(gender)) |>  ggplot(aes(x= perimeterNorm, y = TDoV_n, color = gender)) +
  geom_smooth(method = "glm")+
  scale_color_discrete(labels = c("Female", "Male")) +
  labs(x = "Normalized Perimeter", y = "Normalized TDoV*", color = "Gender")


ConvexArea.TDoV_n <- Data12 |> mutate(TDoV_n = scale(TDoV_n)) |> mutate(gender = factor(gender)) |>  ggplot(aes(x= ConvexAreaNorm, y = TDoV_n, color = gender)) +
  geom_smooth(method = "glm")+
  scale_color_discrete(labels = c("Female", "Male")) +
  labs(x = "Normalized Convex Area", y = "Normalized TDoV*", color = "Gender")


Eccentricity.TDoV_n <- Data12 |> mutate(TDoV_n = scale(TDoV_n)) |> mutate(gender = factor(gender)) |>  ggplot(aes(x= EccentricityNorm, y = TDoV_n, color = gender)) +
  geom_smooth(method = "glm")+
  scale_color_discrete(labels = c("Female", "Male")) +
  labs(x = "Normalized Eccentricity", y = "Normalized TDoV*", color = "Gender")


Extent.TDoV_n <- Data12 |> mutate(TDoV_n = scale(TDoV_n)) |> mutate(gender = factor(gender)) |>  ggplot(aes(x= ExtentNorm, y = TDoV_n, color = gender)) +
  geom_smooth(method = "glm")+
  scale_color_discrete(labels = c("Female", "Male")) +
  labs(x = "Normalized Extent", y = "Normalized TDoV*", color = "Gender")


roundness.TDoV_n <- Data12 |>mutate(TDoV_n = scale(TDoV_n)) |>  mutate(gender = factor(gender)) |>  ggplot(aes(x= roundnessNorm, y = TDoV_n, color = gender, color = gender)) +
  geom_smooth(method = "glm")+
  scale_color_discrete(labels = c("Female", "Male")) +
  labs(x = "Normalized Roundness", y = "Normalized TDoV*", color = "Gender")


shape.TDoV_n <- ggpubr::ggarrange(area.TDoV_n,
                                  perimeter.TDoV_n,
                                  ConvexArea.TDoV_n,
                                  Eccentricity.TDoV_n,
                                  Extent.TDoV_n,
                                  roundness.TDoV_n,
                                  labels = c("A", " B",
                                             "C", " D",
                                             "E", "F"),
                                  ncol = 3, nrow = 2,
                                common.legend = TRUE, legend = "bottom")

ggsave("./exports/shape.TDoV_n.png", shape.TDoV_n,
       width = 10,
       height = 6)


area.NoV_n_t <- Data12 |> mutate(NoV_n_t = scale(NoV_n_t)) |> mutate(gender = factor(gender)) |>  ggplot(aes(x= areaNorm, y = NoV_n_t, color = gender)) +
  geom_smooth(method = "glm")+
  scale_color_discrete(labels = c("Female", "Male")) +
  labs(x = "Normalized Area", y = "Normalized Number of Visits", color = "Gender")

perimeter.NoV_n_t <- Data12 |> mutate(NoV_n_t = scale(NoV_n_t)) |>  mutate(gender = factor(gender)) |>  ggplot(aes(x= perimeterNorm, y = NoV_n_t, color = gender)) +
  geom_smooth(method = "glm")+
  scale_color_discrete(labels = c("Female", "Male")) +
  labs(x = "Normalized Perimeter", y = "Normalized Number of Visits", color = "Gender")


ConvexArea.NoV_n_t <- Data12 |> mutate(NoV_n_t = scale(NoV_n_t)) |> mutate(gender = factor(gender)) |>  ggplot(aes(x= ConvexAreaNorm, y = NoV_n_t, color = gender)) +
  geom_smooth(method = "glm")+
  scale_color_discrete(labels = c("Female", "Male")) +
  labs(x = "Normalized Convex Area", y = "Normalized Number of Visits", color = "Gender")


Eccentricity.NoV_n_t <- Data12 |> mutate(NoV_n_t = scale(NoV_n_t)) |> mutate(gender = factor(gender)) |>  ggplot(aes(x= EccentricityNorm, y = NoV_n_t, color = gender)) +
  geom_smooth(method = "glm")+
  scale_color_discrete(labels = c("Female", "Male")) +
  labs(x = "Normalized Eccentricity", y = "Normalized Number of Visits", color = "Gender")


Extent.NoV_n_t <- Data12 |> mutate(NoV_n_t = scale(NoV_n_t)) |> mutate(gender = factor(gender)) |>  ggplot(aes(x= ExtentNorm, y = NoV_n_t, color = gender)) +
  geom_smooth(method = "glm")+
  scale_color_discrete(labels = c("Female", "Male")) +
  labs(x = "Normalized Extent", y = "Normalized Number of Visits", color = "Gender")


roundness.NoV_n_t <- Data12 |>mutate(NoV_n_t = scale(NoV_n_t)) |>  mutate(gender = factor(gender)) |>  ggplot(aes(x= roundnessNorm, y = NoV_n_t, color = gender, color = gender)) +
  geom_smooth(method = "glm")+
  scale_color_discrete(labels = c("Female", "Male")) +
  labs(x = "Normalized Roundness", y = "Normalized Number of Visits", color = "Gender")


shape.NoV_n_t <- ggpubr::ggarrange(area.NoV_n_t,
                                  perimeter.NoV_n_t,
                                  ConvexArea.NoV_n_t,
                                  Eccentricity.NoV_n_t,
                                  Extent.NoV_n_t,
                                  roundness.NoV_n_t,
                                  labels = c("A", " B",
                                             "C", " D",
                                             "E", "F"),
                                  ncol = 3, nrow = 2,
                                common.legend = TRUE, legend = "bottom")

ggsave("./exports/shape.NoV_n_t.png", shape.NoV_n_t,
       width = 10,
       height = 6)

area.ADoV_n_t <- Data12 |> mutate(ADoV_n_t = Average_duration_of_Visit / Time_to_first_mouse_click) |> mutate(ADoV_n_t = scale(ADoV_n_t)) |> mutate(gender = factor(gender)) |>  ggplot(aes(x= areaNorm, y = ADoV_n_t, color = gender)) +
  geom_smooth(method = "glm")+
  scale_color_discrete(labels = c("Female", "Male")) +
  labs(x = "Normalized Area", y = "Normalized ADoV*", color = "Gender")

perimeter.ADoV_n_t <- Data12 |> mutate(ADoV_n_t = Average_duration_of_Visit / Time_to_first_mouse_click) |> mutate(ADoV_n_t = scale(ADoV_n_t)) |>  mutate(gender = factor(gender)) |>  ggplot(aes(x= perimeterNorm, y = ADoV_n_t, color = gender)) +
  geom_smooth(method = "glm")+
  scale_color_discrete(labels = c("Female", "Male")) +
  labs(x = "Normalized Perimeter", y = "Normalized ADoV*", color = "Gender")


ConvexArea.ADoV_n_t <- Data12 |> mutate(ADoV_n_t = Average_duration_of_Visit / Time_to_first_mouse_click) |> mutate(ADoV_n_t = scale(ADoV_n_t)) |> mutate(gender = factor(gender)) |>  ggplot(aes(x= ConvexAreaNorm, y = ADoV_n_t, color = gender)) +
  geom_smooth(method = "glm")+
  scale_color_discrete(labels = c("Female", "Male")) +
  labs(x = "Normalized Convex Area", y = "Normalized ADoV*", color = "Gender")


Eccentricity.ADoV_n_t <- Data12 |> mutate(ADoV_n_t = Average_duration_of_Visit / Time_to_first_mouse_click) |> mutate(ADoV_n_t = scale(ADoV_n_t)) |> mutate(gender = factor(gender)) |>  ggplot(aes(x= EccentricityNorm, y = ADoV_n_t, color = gender)) +
  geom_smooth(method = "glm")+
  scale_color_discrete(labels = c("Female", "Male")) +
  labs(x = "Normalized Eccentricity", y = "Normalized ADoV*", color = "Gender")


Extent.ADoV_n_t <- Data12 |> mutate(ADoV_n_t = Average_duration_of_Visit / Time_to_first_mouse_click) |> mutate(ADoV_n_t = scale(ADoV_n_t)) |> mutate(gender = factor(gender)) |>  ggplot(aes(x= ExtentNorm, y = ADoV_n_t, color = gender)) +
  geom_smooth(method = "glm")+
  scale_color_discrete(labels = c("Female", "Male")) +
  labs(x = "Normalized Extent", y = "Normalized ADoV*", color = "Gender")


roundness.ADoV_n_t <- Data12 |> mutate(ADoV_n_t = Average_duration_of_Visit / Time_to_first_mouse_click) |>mutate(ADoV_n_t = scale(ADoV_n_t)) |>  mutate(gender = factor(gender)) |>  ggplot(aes(x= roundnessNorm, y = ADoV_n_t, color = gender, color = gender)) +
  geom_smooth(method = "glm")+
  scale_color_discrete(labels = c("Female", "Male")) +
  labs(x = "Normalized Roundness", y = "Normalized ADoV*", color = "Gender")


shape.ADoV_n_t <- ggpubr::ggarrange(area.ADoV_n_t,
                                  perimeter.ADoV_n_t,
                                  ConvexArea.ADoV_n_t,
                                  Eccentricity.ADoV_n_t,
                                  Extent.ADoV_n_t,
                                  roundness.ADoV_n_t,
                                  labels = c("A", " B",
                                             "C", " D",
                                             "E", "F"),
                                  ncol = 3, nrow = 2,
                                common.legend = TRUE, legend = "bottom")

ggsave("./exports/shape.ADoV_n_t.png", shape.ADoV_n_t,
       width = 10,
       height = 6)


table(Data8$loc)
alt.loc.df <- Data8 |> filter(choice == T)
alt.choice <- xtabs(~loc , alt.loc.df)
alt.loc.no_none.df <- Data8 |> filter(choice == T)|> filter(loc != "none")
alt.loc.df2 <- table(alt.loc.no_none.df$loc)
prop.test(alt.loc.df2)
alt.choice.sub <- xtabs(~Participant + loc , alt.loc.df) |> as.data.frame()
alt.choice.sub$perc <- alt.choice.sub$Freq / 72
alt.choice.sub.none <- alt.choice.sub |> dplyr::filter(loc == "none")
max(alt.choice.sub.none$perc)


shape.NoF_n <- glm(NoF_n ~ areaNorm + perimeterNorm + ConvexAreaNorm + EccentricityNorm + ExtentNorm + roundnessNorm + gender + areaNorm:gender + perimeterNorm:gender + ConvexAreaNorm:gender + EccentricityNorm:gender + ExtentNorm:gender + roundnessNorm:gender, data = Data12, family = "gaussian")

shape.TDoWF_n <- glm(TDoWF_n ~ areaNorm + perimeterNorm + ConvexAreaNorm + EccentricityNorm + ExtentNorm + roundnessNorm + gender + areaNorm:gender + perimeterNorm:gender + ConvexAreaNorm:gender + EccentricityNorm:gender + ExtentNorm:gender + roundnessNorm:gender, data = Data12, family = "gaussian")

shape.NoWF_n <- glm(NoWF_n ~ areaNorm + perimeterNorm + ConvexAreaNorm + EccentricityNorm + ExtentNorm + roundnessNorm + gender + areaNorm:gender + perimeterNorm:gender + ConvexAreaNorm:gender + EccentricityNorm:gender + ExtentNorm:gender + roundnessNorm:gender, data = Data12, family = "gaussian")

# shape.NoWF_n <- glm(NoWF_n ~ areaNorm + perimeterNorm + ConvexAreaNorm + EccentricityNorm + ExtentNorm + roundnessNorm + gender + areaNorm:gender + perimeterNorm:gender + ConvexAreaNorm:gender + EccentricityNorm:gender + ExtentNorm:gender + roundnessNorm:gender, data = Data12, family = "gaussian")

shape.TDoV_n <- glm(TDoV_n ~ areaNorm + perimeterNorm + ConvexAreaNorm + EccentricityNorm + ExtentNorm + roundnessNorm + gender + areaNorm:gender + perimeterNorm:gender + ConvexAreaNorm:gender + EccentricityNorm:gender + ExtentNorm:gender + roundnessNorm:gender, data = Data12, family = "gaussian")

shape.APD_n <- glm(APD_n ~ areaNorm + perimeterNorm + ConvexAreaNorm + EccentricityNorm + ExtentNorm + roundnessNorm + gender + areaNorm:gender + perimeterNorm:gender + ConvexAreaNorm:gender + EccentricityNorm:gender + ExtentNorm:gender + roundnessNorm:gender, data = Data12, family = "gaussian")

shape.AWFPD_n <- glm(AWFPD_n ~ areaNorm + perimeterNorm + ConvexAreaNorm + EccentricityNorm + ExtentNorm + roundnessNorm + gender + areaNorm:gender + perimeterNorm:gender + ConvexAreaNorm:gender + EccentricityNorm:gender + ExtentNorm:gender + roundnessNorm:gender, data = Data12, family = "gaussian")

shape.Average_whole_fixation_pupil_diameter <- glm(`Average_whole.fixation_pupil_diameter` ~ areaNorm + perimeterNorm + ConvexAreaNorm + EccentricityNorm + ExtentNorm + roundnessNorm + gender + areaNorm:gender + perimeterNorm:gender + ConvexAreaNorm:gender + EccentricityNorm:gender + ExtentNorm:gender + roundnessNorm:gender, data = Data12, family = "gaussian")

shape.ADoWF <- glm(Average_duration_of_whole_fixations ~ areaNorm + perimeterNorm + ConvexAreaNorm + EccentricityNorm + ExtentNorm + roundnessNorm + gender + areaNorm:gender + perimeterNorm:gender + ConvexAreaNorm:gender + EccentricityNorm:gender + ExtentNorm:gender + roundnessNorm:gender, data = Data12, family = "gaussian")

Data13 <- Data12 |> mutate(ADoV_n_t = Average_duration_of_Visit / Time_to_first_mouse_click)

shape.ADoV_n_t <- glm(ADoV_n_t ~ areaNorm + perimeterNorm + ConvexAreaNorm + EccentricityNorm + ExtentNorm + roundnessNorm + gender + areaNorm:gender + perimeterNorm:gender + ConvexAreaNorm:gender + EccentricityNorm:gender + ExtentNorm:gender + roundnessNorm:gender, data = Data13, family = "gaussian")

shape.NoV_n_t <- glm(NoV_n_t ~ areaNorm + perimeterNorm + ConvexAreaNorm + EccentricityNorm + ExtentNorm + roundnessNorm + gender + areaNorm:gender + perimeterNorm:gender + ConvexAreaNorm:gender + EccentricityNorm:gender + ExtentNorm:gender + roundnessNorm:gender, data = Data12, family = "gaussian")



sjPlot::tab_model(shape.NoWF_n, shape.ADoWF, shape.TDoWF_n, shape.NoV_n_t, shape.ADoV_n_t,
          shape.TDoV_n, show.ci = F, show.r2 = F, file = "./exports/att.shape.gender.html")


shape.choice <- glm(choice ~ areaNorm + perimeterNorm + ConvexAreaNorm + EccentricityNorm + ExtentNorm + roundnessNorm + gender + areaNorm:gender + perimeterNorm:gender + ConvexAreaNorm:gender + EccentricityNorm:gender + ExtentNorm:gender + roundnessNorm:gender, data = Data12, family = "binomial")


tab_model(shape.choice, show.ci = F, show.r2 = F, show.se = T, file = "./exports/shape.choice.gender.html")

att.choice <- lm(choice ~ NoWF_n + TDoWF_n + TDoV_n + AWFPD_n + gender + NoWF_n:gender + TDoWF_n:gender + TDoV_n:gender + AWFPD_n:gender,data = Data12, family = binomial(link = "logit"), na.action("na.omit"))

tab_model(att.choice, show.ci = F, show.r2 = T, show.se = T,
          file = "./exports/att.choice.gender.html")

att.prob <- glm(prob ~ NoWF_n + TDoWF_n + TDoV_n + AWFPD_n + gender + NoWF_n:gender + TDoWF_n:gender + TDoV_n:gender + AWFPD_n:gender,data = Data12, family = "gaussian", na.action("na.omit"))

tab_model(att.prob, show.ci = F, show.r2 = T, show.se = T,
          file = "./exports/att.prob.gender.html")

