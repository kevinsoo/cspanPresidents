library(tidyverse)
library(ggthemes)

# read
df <- read.csv(file="presidents_qualtrics.csv")
df$User <- c(1:nrow(df))
user <- df %>% select(User, Age:GovernmentViews)

# get individual clusters
wt <- df %>% select(User, FactPercent1, Similarity1, Quality1, Commonalities1)
wt$Group <- "War-timers"
as <- df %>% select(User, FactPercent2, Similarity2, Quality2, Commonalities2)
as$Group <- "All-stars"
exp <- df %>% select(User, FactPercent3, Similarity3, Quality3, Commonalities3)
exp$Group <- "Expansionists"
sd <- df %>% select(User, FactPercent4, Similarity4, Quality4, Commonalities4)
sd$Group <- "Social Democrats"
unr <- df %>% select(User, FactPercent5, Similarity5, Quality5, Commonalities5)
unr$Group <- "Unremarkables"
gop <- df %>% select(User, FactPercent6, Similarity6, Quality6, Commonalities6)
gop$Group <- "Republicans"
oth <- df %>% select(User, FactPercent7, Similarity7, Quality7, Commonalities7)
oth$Group <- "The Others"

# rename clusters
colnames(wt) <- c("User", "FactPercent", "Similarity", "Quality", "Commonalities", "Group")
colnames(as) <- c("User", "FactPercent", "Similarity", "Quality", "Commonalities", "Group")
colnames(exp) <- c("User", "FactPercent", "Similarity", "Quality", "Commonalities", "Group")
colnames(sd) <- c("User", "FactPercent", "Similarity", "Quality", "Commonalities", "Group")
colnames(unr) <- c("User", "FactPercent", "Similarity", "Quality", "Commonalities", "Group")
colnames(gop) <- c("User", "FactPercent", "Similarity", "Quality", "Commonalities", "Group")
colnames(oth) <- c("User", "FactPercent", "Similarity", "Quality", "Commonalities", "Group")

# combine data
df <- rbind(wt, as, exp, sd, unr, gop, oth)
qualtrics.df <- merge(user, df)
qualtrics.df$Group <- factor(qualtrics.df$Group, levels=c("War-timers", "All-stars", "Expansionists", "Social Democrats", "Unremarkables", "Republicans", "The Others"))
save(qualtrics.df, file="qualtricsData.Rda")

# plot data
qual <- qualtrics.df %>% group_by(Group) %>% 
    summarise(N=n(), Sim = mean(Similarity), SimSE = sd(Similarity)/sqrt(N), 
              Fame = mean(FactPercent), FameSE = sd(FactPercent)/sqrt(N))

# plot basic data
qual$Group <- as.factor(qual$Group)
qual$Group <- factor(qual$Group, levels=c("War-timers", "All-stars", "Expansionists", "Social Democrats", "Unremarkables", "Republicans", "The Others"))

