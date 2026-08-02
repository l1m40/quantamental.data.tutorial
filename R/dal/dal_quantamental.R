# Quantamental Data Access Layer ###############################################
# Description: Provides structured access to quantamental datasets 
#              used for financial analysis and investment research.
#
# Responsibilities:
#  - Provide a unified interface for accessing quantamental datasets.
#  - Load and manage daily fundamental ratios time series.
#  - Support efficient querying of large datasets through Apache Arrow.
#  - Retrieve asset-level valuation, profitability, and financial ratios.
#  - Provide access to consolidated financial statements and derived metrics.
#  - Enable retrieval of historical daily observations for individual or multiple assets.
#  - Expose a consistent schema across quantitative and fundamental data sources.
#  - Support selective data extraction to minimise memory usage.
#  - Validate dataset availability and integrity through health checks.
#  - Abstract the underlying storage format from analytical applications.
#  - Facilitate reproducible financial research by providing a stable data access API.
#  - Support downstream quantitative models, factor construction, and investment research.
#
# Author:
# Created:
# Last Modified:
###############################################################################

#
#
# I should write and small explanation here
################################################################################

library(R6)
library(arrow)
library(dplyr)

if(!exists("data_mart_path")) data_mart_path <- here::here("data","mart")
data_quantamental_path = here::here(data_mart_path,"quantamental")

# Quantamental Data Access Layer Class #########################################
DAL_Quantamental <- R6::R6Class(
  "DAL_Quantamental",
  private = list(
    ds = NULL,
    path = NULL
  ),
  public = list(
    initialize = function(path=data_quantamental_path) {
      private$path <- path
      private$ds <- arrow::open_dataset(path)
      self$healthcheck()
    },     void = function() { cat("") },
    healthcheck = function() {
      cat("quantamental healthcheck validation...")
      cat("\n")
    },
    dataset = function() { private$ds },      # Returns the Arrow Dataset (lazy)
    all = function() {                # Returns the complete dataset as a tibble
      private$ds %>%
        arrange(date) %>%
        collect()
    }, 
    asset = function(asset) {                           # Returns a single asset
      private$ds %>%
        filter(asset == !!asset) %>%
        arrange(date) %>%
        collect()
    },
    assets = function(asset_vector) {                          # Multiple assets
      private$ds %>%
        filter(asset %in% !!asset_vector) %>%
        arrange(asset, date) %>%
        collect()
    },
    
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
  
  dal <- DAL_Quantamental$new()
  start_time=Sys.time() ; cat("Load all data ") ; df <- dal$all() ; difftime(Sys.time(), start_time, units = "secs")
  glimpse(df |> tail(10))
  start_time=Sys.time() ; df <- dal$asset("VALE3") ; difftime(Sys.time(), start_time, units = "secs")
  df |> tail(2) |> t()
  
  library(tidyverse)
  ggplot(df,aes(date,close))+geom_line() # close = price in daily timeframe
  
  df |> #filter(year(date)>=2025) |> 
    ggplot(aes(date,pe))+
    geom_hline(yintercept=0,linetype=2,color="tomato",alpha=.5)+
    geom_line()+ylab("P/L")+xlab(df$asset[1])+
    geom_line(aes(y=Net_Profit_12M/max(Net_Profit_12M,na.rm=T)*max(pe,na.rm=T)),color="gold4",alpha=.3)+
    #geom_line(aes(y=         close/max(         close,na.rm=T)*max(pe,na.rm=T)),color="gold2",alpha=.5)+
    theme_minimal()
  
  
  
  dal$query() |>  # Custom Arrow query
    # filter(asset %in% c("INTB3","GMAT3","xSUZB3","xCEAB3","xPRIO3"),year(date)>=2022) |> 
    # filter(asset %in% c("INTB3","VAMO3","TOTS3"),year(date)>=2022) |> 
    filter(asset %in% c("CXSE3","PSSA3","BBSE3","xIRBR3","xHAPV3","xQUAL3"),year(date)>=2022) |> 
    select(asset,date,close,pe,Net_Profit_12M) |> 
    collect() |> 
    ggplot(aes(date,pe))+
    geom_hline(yintercept=0,linetype=2,color="tomato",alpha=.5)+
    geom_line()+ylab("P/L")+
    geom_line(aes(y=Net_Profit_12M/max(Net_Profit_12M,na.rm=T)*max(pe,na.rm=T)),color="gold4",alpha=.3)+
    geom_line(aes(y=         close/max(         close,na.rm=T)*max(pe,na.rm=T)),color="gold2",alpha=.3)+
    facet_grid(cols=vars(asset))+
    theme_minimal()
  
  
  
}






# What is quantamental? ########################################################
# Quantamental refers to the combination of 
# quantitative and fundamental investment approaches. 
# It integrates market data, statistical models, and systematic analysis 
# with company fundamentals such as 
# financial statements, earnings, valuation ratios, 
# and business characteristics to support investment research and decision-making.
# 
# read.me ######################################################################
# The Quantamental Data Access Layer provides a structured interface 
# for retrieving, querying, and managing daily quantitative 
# and fundamental datasets used in financial research and investment analysis. 
# It abstracts the underlying storage technology 
# while enabling efficient access to historical prices, 
# valuation multiples, financial ratios, 
# and consolidated financial statement metrics through a consistent API.
#
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
# Main data available after you DAL_Quantamental$new()
# dal$query() |>  # Custom Arrow query
#   filter(asset %in% c("INTB3"),year(date)>=2022) |> 
#   select(asset,date,close,pe,Net_Profit_12M) |> 
#   collect() |> 
#   head(10)
# 
if(!exists("healthcheck_validation")) healthcheck_validation <- T ; if(healthcheck_validation) DAL_Quantamental$new()$void()
