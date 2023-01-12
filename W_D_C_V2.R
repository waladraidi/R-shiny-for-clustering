  # vuln <- "Pysical"
  #  clust <- "1"
  #  df <- W_D_data_prep_V2()

W_D_C_V2 <- function(df,vuln,clust){
  set.seed(4)
  
  QLD_SA2_SHP <- read_sf("sQLD.shp")
  
  df.0 <- st_drop_geometry(df)
  dummy <- which(names(df.0) == paste0("Scale",vuln))
  Kmeans <<- kmeans(df.0[,dummy],4,nstart = 5)
  
  clusteridx <<- kmeans(df.0[,dummy], centers = sort(kmeans(df.0[,dummy], 4)$centers))
  
  df <- mutate(df,Clusters = as.factor( clusteridx$cluster)) %>% filter(Clusters == clust)
  
  ### Plot 1:
  df.1 <- data.frame(df$Australia,df$Preschool,df$English,df$Indigenous)
  colnames(df.1) <- c("Australia","Preschool","English","Indigenous")
  df.1<- gather(df.1)
  colnames(df.1)[1]<- "Covariate"
  colnames(df.1)[2]<- "Proportion"
  options(repr.plot.width = 10, repr.plot.height = 0.75) 
  p1 <- ggplot(df.1, aes(x = Covariate, y = Proportion)) +
    geom_boxplot()+scale_x_discrete(name = "Covariate") +
    scale_y_continuous(name = "Proportion") + theme_bw()
 
  round_preserve_sum <- function(x, digits = 0) {
    up <- 10 ^ digits
    x <- x * up
    y<- floor(x)
    indices <- tail(order(x-y), round(sum(x)) - sum(y))
    y[indices] <- y[indices] + 1
    y / up
  } 
  ### Plot 2:
  df.2 <- st_drop_geometry(df)
  PERC <- df.2 %>% dplyr::count(Remoteness) %>%
    mutate(perc = round_preserve_sum( `n` / sum(`n`),2)) %>% arrange(perc) %>%
    mutate(labels = 100*(perc))
  
  p2 <- ggplot(PERC, aes(x = "", y = perc, fill = Remoteness)) +
    geom_col() +
    geom_text(aes(x=1.65, label = labels),
              position = position_stack(vjust = 0.5), size = 4) +
    coord_polar(theta = "y") +
    geom_col(width = 1, color = 1) +
    theme_void()
  
  ### Plot 3:
  PERC2<- df.2 %>% dplyr::count(IRSD)%>%
    mutate(perc =round_preserve_sum( `n` / sum(`n`),2)) %>% arrange(perc) %>%
    mutate(labels = 100*(perc))
  
  p3 <- ggplot(PERC2, aes(x = "", y = perc, fill = IRSD)) +
    geom_col() +
    geom_text(aes(x=1.65, label = labels),
              position = position_stack(vjust = 0.5), size = 4) +
    coord_polar(theta = "y") +
    geom_col(width = 1, color = 1) +
    theme_void()
  
  ### Plot 4:
  p4<- ggplot() +
    xlab("Longitude") +
    ylab("Latitude") +
    theme_void() +
    theme(legend.position = "right") +
    labs(fill='Cluster Number') +
    geom_sf(data = df, aes(fill=Clusters,), color=NA) +
    geom_sf(data=QLD_SA2_SHP, fill=NA) +
    xlim(135, 155)
  
  ## Outputs declaration:
  OP <- list()
  
  ## KMeans Summary:
  df.5 <- st_drop_geometry(df) # Specific dataset for Kmeans summary.
  dummy2 <- which(names(df.5) == paste0(vuln))
  
  AVG<- df.5%>%
    summarise(mean= (mean(df.5[,dummy2])))
  AVG$mean%>% round (4)
  
  MED<- df.5%>%
    summarise(median= (median(df.5[,dummy2])))
  MED$median%>% round (4)
  
  RANGE<- df.5%>%
    summarise(RANGE= (range(df.5[,dummy2])))
  RANGE$RANGE%>% round (4)
  
  OP$KM <- kmeans(df.0[,dummy], centers = sort(kmeans(df.0[,dummy], 4)$centers))$size[as.numeric(clust)]
  OP$AVG <- AVG$mean
  OP$MED <- MED$median
  OP$RANGE <- RANGE$RANGE
  
  ### Plot(s):
  PLOT <- ggarrange(p1,p2,p3,p4,
                    ncol = 2, nrow = 2,legend="bottom")
  # PLOT<- grid.arrange(p1,p2,p3,p4, ncol = 2, nrow=2,
  #                      widths=c(2.3, 2.3,0.8, 0.8))
  OP$PLOT <- PLOT
  return(OP)
}

