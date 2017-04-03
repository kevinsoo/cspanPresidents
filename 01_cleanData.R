# load libraries
library(tidyverse)
library(ggthemes)
library(lsa)

# load data
potus <- read.csv("cspan_2017_presidents.csv")
potus$President <- factor(potus$President, levels=unique(potus$President[order(potus$Number)]))

# get points and ranks
potus.pts <- potus %>% select(Number:Rank, 
                              PublicPersuasion, CrisisLeadership, EconomicManagement, MoralAuthority, InternationalRelations,
                              AdministrativeSkills, RelationsWithCongress, VisionAgenda, PursuedEqualJustice, PerformanceAtTime)
potus.rnk <- potus %>% select(Number:Rank, 
                              PublicPersuasion_Rank, CrisisLeadership_Rank, EconomicManagement_Rank, 
                              MoralAuthority_Rank, InternationalRelations_Rank, AdministrativeSkills_Rank,
                              RelationsWithCongress_Rank, VisionAgenda_Rank, PursuedEqualJustice_Rank, PerformanceAtTime_Rank)

# long format
potus.pts <- potus.pts %>% gather(Attribute, Score, PublicPersuasion:PerformanceAtTime) %>% arrange(Number, Attribute)
potus.rnk <- potus.rnk %>% gather(Attribute, AttributeRank, PublicPersuasion_Rank:PerformanceAtTime_Rank) %>% arrange(Number, Attribute)
potus.df <- data.frame(potus.pts, select(potus.rnk, AttributeRank))

# get summary stats
potus.sd <- potus.df %>% group_by(President) %>% summarise(OverallSD = sd(Score))
potus.df <- merge(potus.df, potus.sd)
potus <- merge(potus, potus.sd)

# visualize
potus.df %>% arrange(Number) %>%
    ggplot(aes(x=President, y=Score, group=Attribute)) +
    geom_line(aes(color=Attribute), alpha=.5) +
    geom_point(aes(color=Attribute)) + 
    theme_minimal() +
    scale_color_ptol(labels=c("Administrative skills", "Crisis leadership", "Economic management", "International relations", 
                              "Moral authority", "Performance within\ncontext of times", "Public persuasion",
                              "Pursued equal\njustice for all", "Relations with Congress", "Vision/Setting agenda")) +
    theme(axis.text.x  = element_text(angle=60, vjust=0.5)) +
    labs(title="Presidential Attributes Across Presidents", caption="Note: Grover Cleveland is only rated once")


potus.df %>% arrange(Number) %>%
    ggplot(aes(x=President, y=Overall/10, group=NA)) +
    geom_line(color="gray38", linetype="dashed") +
    geom_errorbar(aes(ymax=Overall/10+OverallSD, ymin=Overall/10-OverallSD), width=.2, size=.4) +
    geom_point(size=2.5) + 
    theme_minimal() +
    scale_color_ptol() +
    theme(axis.text.x  = element_text(angle=60, vjust=0.5)) +
    labs(title="Overall Scores Across Presidents", 
         subtitle="Error bars indicate SD of attributes", 
         caption="Note: Grover Cleveland is only rated once",
         y="Overall")

# compute distance matrix
potus.pts <- potus.pts %>% select()


d <- dist(potus.pts)
clust <- hclust(d)
plot(clust)







# matrix for computing distances
potusMatrix <- matrix(rep(NA, nrow(potus)^2), nrow=nrow(potus), ncol=nrow(potus), byrow=T)
colnames(potusMatrix) <- potus$President
rownames(potusMatrix) <- potus$President

# get data (drop ranks)
stats.df <- potus %>% select(PublicPersuasion, CrisisLeadership, EconomicManagement, MoralAuthority, InternationalRelations, AdministrativeSkills, RelationsWithCongress, VisionAgenda, PursuedEqualJustice, PerformanceAtTime)

# calculate cosine similarity for each potus
for (i in 1:nrow(stats.df)) {
    for (j in 1:nrow(stats.df)) {
        a <- as.numeric(stats.df[i,]) # feature vector for player i
        b <- as.numeric(stats.df[j,]) # feature vector for player j
        potusMatrix[i,j] <- cosine(a, b) # cosine similarity between players i and j
    }
}
