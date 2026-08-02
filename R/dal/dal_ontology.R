# Ontology Data Access Layer ##################################################
# Description: Central interface for accessing ontology-based reference data 
#              used in financial markets.
#
# Responsibilities:
#   - Load and manage ontology reference data.
#   - Provide access to B3 stock indexes.
#   - Provide access to market sectors.
#   - Provide access to similarity scores matrix.
#   - Support ontology queries.
#
# Author:
# Created:
# Last Modified:
###############################################################################


#
# technical explanation
################################################################################

library(R6)
library(dplyr)
library(tidyr)

if(!exists("data_mart_path")) data_mart_path <- here::here("data","mart")
data_ontology_path = here::here(data_mart_path,"ontology.rds")

# Ontology Data Access Layer Class #########################################
DAL_Ontology <- R6::R6Class(
  "DAL_Ontology",
  private = list(
    .path = NULL,
    similarity_list = list(),
    b3_index_list = list(),
    assets_list = list()
  ),
  public = list(
    initialize = function(path=data_ontology_path) {
      private$.path <- path
      self$reload()
      self$healthcheck()
    },     void = function() { cat("") },
    healthcheck = function() {
      cat("ontology healthcheck validation...")
      cat(paste0("\n"  ,length(unique(self$assets()$df$sector_screening))," sectors "))
      cat(paste0("and ",length(unique(self$assets()$df$asset))," assets "))
      cat(paste0("\nindexes updated at ",self$b3_index()$IBOV_updated," "))
      cat(paste0("(IBOV~60%=",length(unique(self$b3_index()$IBOV_60$asset)),") "))
      cat(paste0("(IBOV~90%=",length(unique(self$b3_index()$IBOV_90$asset)),") ")) 
      cat(paste0("(IBOV="    ,length(unique(self$b3_index()$IBOV$asset   )),") "))
      cat(paste0("(SMLL="    ,length(unique(self$b3_index()$SMLL$asset   )),") "))
      cat("\n")
    },
    reload = function() {
      data <- readRDS(private$.path)
      private$assets_list$df <- data$assets
      private$b3_index_list <- data$b3_index
      private$similarity_list$matrix_long <- data$similarity_matrix %>%
        mutate(asset1 = rownames(.)) %>%
        pivot_longer(cols = -asset1, names_to = "asset2", values_to = "score") %>%
        filter(asset1 < asset2) |>
        ungroup()
      private$similarity_list$matrix <- private$similarity_list$matrix_long %>% # matrix_long_with_sectors
        inner_join(private$assets_list$df |> select(asset,sector_screening), by = c("asset1" = "asset")) %>%
        rename(sector1 = sector_screening) %>%
        inner_join(private$assets_list$df |> select(asset,sector_screening), by = c("asset2" = "asset")) %>%
        rename(sector2 = sector_screening) %>%
        mutate(relationship = ifelse(sector1==sector2, "Within Same Sector", "Cross-Sector"))
      
      sim_mat_compl <- data.frame(
        asset1      =private$similarity_list$matrix$asset2,
        asset2      =private$similarity_list$matrix$asset1,
        score       =private$similarity_list$matrix$score,
        sector1     =private$similarity_list$matrix$sector2,
        sector2     =private$similarity_list$matrix$sector1,
        relationship=private$similarity_list$matrix$relationship
      )
      private$similarity_list$full_matrix_df <- private$similarity_list$matrix |> rbind(sim_mat_compl) |> arrange(asset1,asset2)
        
      private$similarity_list$sector_matrix_df <- private$similarity_list$matrix %>%
          mutate(
            CleanSector1 = pmin(sector1, sector2),
            CleanSector2 = pmax(sector1, sector2)
          ) %>%
          group_by(sector1=CleanSector1, sector2=CleanSector2) %>%
          summarise(avg_score = mean(score), .groups = 'drop')
      private$similarity_list$asset_membership_matrix_df  <- private$similarity_list$matrix |> filter(relationship=="Within Same Sector") |> arrange(     score)  
      private$similarity_list$asset_cross_matrix_df       <- private$similarity_list$matrix |> filter(relationship=="Cross-Sector")       |> arrange(desc(score)) 
      private$similarity_list$sector_membership_matrix_df <- private$similarity_list$sector_matrix_df |> filter(sector1==sector2) |> group_by(sector=sector1) |> summarise(avg_score=mean(avg_score)) |> arrange(desc(avg_score)) 
      private$similarity_list$sector_cross_matrix_df      <- private$similarity_list$sector_matrix_df |> filter(sector1!=sector2) |> arrange(desc(avg_score)) 
      
      
    },
    assets = function() { private$assets_list },
    b3_index = function() { private$b3_index_list },
    similarity = function() { private$similarity_list },
    get_assets = function() { sort(unique(self$assets()$df$asset)) },
    get_sectors = function() { sort(unique(self$assets()$df$sector_screening)) },
    get_assets_df = function() { self$assets()$df },        # Returns the Assets Data Frame
    get_b3_index =function() { self$b3_index() },           # Returns B3 Indexes data
    get_index_df = function(index_name="IBOV") { tibble::as_tibble(self$b3_index()[[index_name]]) |> arrange(desc(share)) |> inner_join(self$assets()$df |> select(asset,sector_screening),by="asset") },
    get_IBOV_60_df = function()                { tibble::as_tibble(self$b3_index()$IBOV_60      ) |> arrange(desc(share)) |> inner_join(self$assets()$df |> select(asset,sector_screening),by="asset") },
    get_IBOV_90_df = function()                { tibble::as_tibble(self$b3_index()$IBOV_90      ) |> arrange(desc(share)) |> inner_join(self$assets()$df |> select(asset,sector_screening),by="asset") },
    get_IBOV_df    = function()                { tibble::as_tibble(self$b3_index()$IBOV         ) |> arrange(desc(share)) |> inner_join(self$assets()$df |> select(asset,sector_screening),by="asset") },
    get_SMLL_df    = function()                { tibble::as_tibble(self$b3_index()$SMLL         ) |> arrange(desc(share)) |> inner_join(self$assets()$df |> select(asset,sector_screening),by="asset") },
    get_similarity = function() { self$similarity() },      # Returns the Similarity data
    get_similarity_asset_matrix_df = function() { self$similarity()$matrix },
    get_similarity_asset_df = function(asset_input,n=15) {
      # self$similarity()$matrix |>
      # filter(if_any(c(asset1, asset2), ~ . == toupper(asset_input))) |>
      self$similarity()$full_matrix_df |>
        filter(asset1==toupper(asset_input)) |> 
        arrange(desc(score)) |> head(n)
    },
    # get_similarity_sector_matrix_df = function(){
    #   self$similarity()$matrix %>%
    #     mutate(
    #       CleanSector1 = pmin(sector1, sector2),
    #       CleanSector2 = pmax(sector1, sector2)
    #     ) %>%
    #     group_by(sector1=CleanSector1, sector2=CleanSector2) %>%
    #     summarise(avg_score = mean(score), .groups = 'drop')
    # },
    # get_similarity_asset_membership_matrix_df  = function(){ self$get_similarity_asset_matrix_df()  |> filter(relationship=="Within Same Sector") |> arrange(     score)  },
    # get_similarity_asset_cross_matrix_df       = function(){ self$get_similarity_asset_matrix_df()  |> filter(relationship=="Cross-Sector")       |> arrange(desc(score)) },
    # get_similarity_sector_membership_matrix_df = function(){ self$get_similarity_sector_matrix_df() |> filter(sector1==sector2) |> group_by(sector=sector1) |> summarise(avg_score=mean(avg_score)) |> arrange(desc(avg_score)) },
    # get_similarity_sector_cross_matrix_df      = function(){ self$get_similarity_sector_matrix_df() |> filter(sector1!=sector2) |> arrange(desc(avg_score)) }, 
    # get_similarity_sector_cross_matrix_df      = function(){ self$get_similarity_sector_matrix_df() |> filter(relationship=="Cross-Sector") |> arrange(desc(avg_score)) },
    # get_similarity_sector_membership_matrix_df = function(){ self$get_similarity_sector_matrix_df() |> filter(relationship=="Within Same Sector") |> group_by(sector=sector1) |> summarise(avg_score=mean(avg_score)) |> arrange(desc(avg_score)) },
    similarity_validation_distribution = function(){
      cat("##### Similarity scores distribution \n")
      cat(paste0("            .1 : ",quantile(self$similarity()$matrix$score,.1 ))) # 0.41 | with underline: 0.45
      cat(paste0("  ...   median : ",quantile(self$similarity()$matrix$score,.5 ))) # 0.53 | with underline: 0.56
      cat(paste0("  ...      .95 : ",quantile(self$similarity()$matrix$score,.95))) # 0.75 | with underline: 0.75
      cat("\n")
    },
    similarity_validation = function(){
      self$similarity_validation_distribution()
      # cat("\n          ##### Same sector lowest similarity\n")   ; print(self$similarity()$matrix |> filter(relationship=="Within Same Sector") |> arrange(     score)  |> head(3) |> as.data.frame())
      # cat("\n          ##### Cross-sector highest similarity\n") ; print(self$similarity()$matrix |> filter(relationship=="Cross-Sector")       |> arrange(desc(score)) |> head(3) |> as.data.frame())
      cat("\n          ##### Same sector lowest similarity get_similarity()$asset_membership_matrix_df()\n")   ; print(self$similarity()$asset_membership_matrix_df |> head(3) |> as.data.frame())
      cat("\n          ##### Cross-sector highest similarity get_similarity_asset_cross_matrix_df()\n")      ; print(self$similarity()$asset_cross_matrix_df      |> head(3) |> as.data.frame())
      cat("\n          ##### Similarity data integrity \n")      ; print(self$similarity()$matrix |> group_by(relationship) |> summarise(avg_score=mean(score,na.rm=T)) |> as.data.frame())
      cat("\n          ##### Highest sectors membership similarity get_similarity_sector_membership_matrix_df()\n") ; print(self$similarity()$sector_membership_matrix_df |> head(5) |> as.data.frame())
      cat("\n          ##### Highest cross-sectors similarity get_similarity_sector_cross_matrix_df()\n")           ; print(self$similarity()$sector_cross_matrix_df      |> head(5) |> as.data.frame())
      
    },
    
    # dataset = function() { private$ds },      # Returns the Arrow Dataset (lazy)
    # all = function() {                # Returns the complete dataset as a tibble
    #   private$ds %>%
    #     arrange(date) %>%
    #     collect()
    # }, 
    # asset = function(asset) {                           # Returns a single asset
    #   private$ds %>%
    #     filter(asset == !!asset) %>%
    #     arrange(date) %>%
    #     collect()
    # },
    # assets = function(asset_vector) {                          # Multiple assets
    #   private$assets
    # },
    
    #
    # new_functions_here = function(inputs) {
    #   
    # },
    #
    
    query = function() { private$ds }                    # Generic query builder
  )
)











# Developer playground #########################################################
# (not executed when sourced)
# to initiate source this file
if(F) {
  
  ontology <- DAL_Ontology$new()
  ontology$get_assets()
  ontology$get_sectors()
  ontology$get_b3_index()$IBOV_60
  ontology$get_IBOV_60_df()$asset
  ontology$get_IBOV_df()
  
  ontology$get_similarity_asset_df("cxse3",n=5) |> select(-c(sector1))
  
  ontology$get_similarity()$sector_membership_matrix_df
  ontology$get_similarity()$full_matrix_df
  
  
  # sectors share plots ########################################################
  library(tidyverse) ; library(treemapify)
  fill_color="gray"
  title_text="IBOV"
  sectors_df <- ontology$get_IBOV_df() |> group_by(sector_screening) |> 
    mutate(share_calc=share) %>% reframe(share=sum(share_calc,na.rm=T),biggest_asset=ifelse(any(!is.na(share_calc)),asset[which.max(share_calc)],""),biggest_share=ifelse(any(!is.na(share_calc)),share_calc[which.max(share_calc)],0)) %>% arrange(desc(share)) #|> filter(!is.na(sector_screening),share>0)
  sectors_df %>% 
    ggplot(aes(area=share,subgroup=sector_screening))+geom_treemap(aes(alpha=share),fill=fill_color)+
    geom_treemap_text(aes(label=sector_screening),size=18,alpha=.8,place="top")+#scale_color_identity()+
    geom_treemap_text(aes(label=sprintf("%.1f%%",share         )),size=20,colour="black",alpha=.8,place="top",padding.y=grid::unit( 8,"mm"))+
    geom_treemap_text(aes(label=biggest_asset                  ),size=10,colour="black",alpha=.5,place="top",padding.y=grid::unit(17,"mm"))+
    geom_treemap_text(aes(label=sprintf("%.1f%%",biggest_share)),size=10,colour="black",alpha=.5,place="top",padding.y=grid::unit(20,"mm"))+
    labs(title=if(is.na(title_text) || title_text == "") NULL else title_text)+
    theme_void()+theme(plot.title=element_text(color="white"),plot.background=element_rect(fill="gray13",colour="gray13"),legend.position="none")
  
  
  
  ontology$similarity_validation()
  # similarity scores plots ####################################################
  library(tidyverse)
  midpoint_data <- quantile(ontology$similarity()$matrix$score,.5)
  ontology$similarity()$matrix |> 
    ggplot(aes(x=score))+geom_vline(xintercept=midpoint_data,linetype=2,color="tomato",alpha=.5)+
      geom_density(fill = "#4682B4", color = "#1C39BB", alpha = 0.6, linewidth = 0.8) +
      labs(x = "Similarity Score", y = "Density") + theme_minimal(base_size = 12)
  ontology$similarity_validation_distribution()
  # sector heatmap
  ontology$similarity()$sector_matrix_df |> 
    ggplot(aes(x = sector1, y = sector2, fill = avg_score)) +
      geom_tile(color = "white", lwd = 0.5, linetype = 1) +
      scale_fill_gradient2(low = "tomato", mid = "gray89", high = "blue", midpoint = midpoint_data) +
      labs(
        title = "Macro-Sector Semantic Similarity Heatmap",
        subtitle = "Average embedding scores aggregated across industries",
        x = "Sector A",
        y = "Sector B",
        fill = "Avg Score"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  
  # ggraph looks like sky constellations
  library(igraph)
  edges <- ontology$similarity()$sector_matrix_df |> filter(avg_score > 0.638)
  g <- graph_from_data_frame(edges)
  # plot(g)
  library(ggraph)
  library(tidygraph)
  ggraph(g, layout = "fr") +
    geom_edge_link(aes(width = avg_score),alpha=.2) +
    geom_node_point(size = 5) +
    geom_node_text(aes(label = name),vjust=2)
  
  sector_summary <- ontology$similarity()$sector_matrix_df
  # Other advanced plots
  sectors <- sort(unique(c(sector_summary$sector1, sector_summary$sector2)))
  mat <- matrix(
    NA,
    nrow = length(sectors),
    ncol = length(sectors),
    dimnames = list(sectors, sectors)
  )
  for(i in seq_len(nrow(sector_summary))) {
    mat[
      sector_summary$sector1[i],
      sector_summary$sector2[i]
    ] <- 1 - sector_summary$avg_score[i]
    mat[
      sector_summary$sector2[i],
      sector_summary$sector1[i]
    ] <- 1 - sector_summary$avg_score[i]
  }
  dim(mat)
  sum(is.na(mat))
  hc <- hclust(as.dist(mat),method = "ward.D2")
  # plot(hc)
  library(ggdendro)
  ddata <- dendro_data(as.dendrogram(hc))
  label_df <- label(ddata) %>% as_tibble()
  ggplot(segment(ddata),aes(x = x,y = y)) +
    geom_segment(aes(xend = xend,yend = yend)) +
    geom_label(data=label_df,aes(label=label),fill=NA,hjust=0,nudge_y=(.01),label.size=NA,label.r=unit(0,"pt"))+# scale_fill_identity()+
    ylim(1,-2)+coord_flip()
  
}




# What is ontology? ############################################################
# An ontology is a structured representation of knowledge 
# that defines the concepts in a particular domain, the properties that describe them, and the relationships between them. 
# Rather than simply storing data, an ontology captures the meaning 
# and organization of that data, enabling consistent classification, querying, and reasoning. 
# In the context of financial markets, an ontology might define entities 
# such as companies, sectors, industries, stock indexes, 
# and financial instruments, along with relationships like 
#   *belongs to sector*, 
#   *is a constituent of index*, or 
#   *is similar to*. 
# This provides a shared, well-defined framework that allows applications 
# and analysts to interpret and use reference data consistently 
# across different systems and analyses.
# 
# read.me ######################################################################
# The R Ontology Data Access Layer class provides a structured interface
#  for retrieving and managing ontology-based reference data 
#  used in financial markets. 
# It acts as a central access point 
#  for information about market sectors, stock indexes, 
#  and the grouping relationships that classify companies 
#  participating in the stock market. 
# By abstracting the underlying data sources, 
#  the class makes it easier for analysts and applications to query consistent, 
#  well-organised market ontology data, support company classification, 
#  and maintain a clear mapping between entities, their sector affiliations, 
#  and index memberships.



    #                                 +----------------------+
    #                                 |   Data Lake          |
    #                                 |  Parquet / Arrow     |
    #                                 |  RDS/CSV/JSON/TXT    |
    #                                 +----------+-----------+
    #                                           |
    #                                           |
    #                               +----------v-----------+
    #                               | quantlake (R package)|
    #                               |                      |
    #                               | DAL_Quantamental     |
    #                               | DAL_Ontology         |
    #                               | DAL_Cointegration    |
    #                               +----+-----------+-----+
    #                                     |           |
    #                     +--------------+           +---------+
    #                   |                                     |
    #         +--------v---------+                  +--------v--------+
    #         | R notebooks      |                  | REST API        |
    #         | Examples         |                  | plumber         |
    #         | Feature research |                  |                 |
    #         +------------------+                  +-----------------+

# How to use 
# Main data available after you DAL_Ontology$new()
# get_assets()
# get_sectors()
# get_b3_index()
# get_similarity()
# 
if(!exists("healthcheck_validation")) healthcheck_validation <- T ; if(healthcheck_validation) DAL_Ontology$new()$void()
