###############################################################################
###############################################################################
################# TOBACCO MICROSIMULATION - ECUADOR ###########################
###############################################################################
###############################################################################
### 1. INPUTS PARAMETER 
### 2. LOAD AND GENERATE SYNTHETIC DATASET  : Generate synthetic dataset 
### 3. SIMULATION : MC simulations :  Simulate change in tab. cons.
### 4. EXPORT : Export main outputs aggregated per scenario.strata.iteration
### 5. ANALYSE : Summarize results per scenario.strata
rm(list = ls())
# Set seed for reproducibility
set.seed(20150406)

# Package load
pkg <- c("readstata13","tidyr","tidyverse","readr","dplyr","ggthemes",
         "pbapply","parallel","gmodels")
lapply(pkg, require, character.only = TRUE)

#setwd GENERAL LOCATION
root  <- "~/Tobacco_uSim Ec/" #Base data STEPS
sroot <- "~/Tobacco_uSim Ec/lbecea/" #Outputs 
oroot <- sroot #Simulation data repos
#Gradiente de Escenarios: 3.4 -> 6.8
simname_grad <- paste0("_s",sprintf("%.1f",seq(3.4+0.1,6.8,by=0.1)),"d")
STaxVaSpePos_grad <- seq(3.4+0.1,6.8,by=0.1)
#Gen Seeds for each scenario
seeds <- round(runif(length(simname_grad), min = 1, max = 100000)*100) 

# # Short version :
# # PriceRet = 6.2 1.MINI:7.03 (exc=3.9), 2.MEDIO:7.85 (exc=4.4) BASE: 9.49 (exc=5.4)
# simname_grad <- simname_grad[STaxVaSpePos_grad %in% c(3.9,4.4,5.4)]
# STaxVaSpePos_grad <- STaxVaSpePos_grad[STaxVaSpePos_grad %in% c(3.9,4.4,5.4)]

# Number of simulations
NSimFi = 1000

# Loop over scenarios
for(i in 1:length(simname_grad)){
  #Set Seed And timer 
  set.seed(seeds[i])
  start_time_total <- Sys.time() # Timer setup
  # Assign Scenario:
  simname <- simname_grad[i]
  STaxVaSpePos <- STaxVaSpePos_grad[i]
  # DESAGREGACIONES
  Desag <- c("CSex","HIQuin","CVRegion","Nacional")
  #Random allocation function: arguments: data, population filter, 
  #variable to assign, secondary filter for assignment 
  randalloc <- function(data,fltr,modvar,prob,fltr2=NA){
    data$RanPop <- data$RanPro <- data$RanU <- NA
    data$RanU[fltr] <- runif(sum(fltr))
    # Sort by random number (descending)
    data <- data[order(-data$RanU), ]
    # Calculate cumulative population and proportion
    data$RanPop[!is.na(data$RanU)] <- cumsum(data$CFex[!is.na(data$RanU)])
    data$RanTot <- sum(data$CFex[!is.na(data$RanU)],na.rm = T)
    data$RanPro[!is.na(data$RanU)] <- 
      data$RanPop[!is.na(data$RanU)] / data$RanTot[!is.na(data$RanU)]
    # Quitting decision: filtered or not
    if(is.na(fltr2[1])) {
      data[which(data$RanPro <= prob),modvar] <- 1
    }else{
      data[which(fltr2 & data$RanPro <= prob),modvar] <- 1
    }
    # Update main dataset
    data <- data[order(data$idper),]
    # Clean up temporary variables
    data$RanTot <- data$RanPop <- data$RanPro <- data$RanU <- NULL
    return(data)
  }
  #############################################################################
  ##########################  I - INPUTS  #####################################
  #############################################################################
  # CONVENTIONS (Prefixes and Suffixes)
  # Pre: Pre  Tax increase | Pos: Post Tax increase
  ################### PRICE MODEL  STRUCTURAL - S (PRICES)
  ##### Taxes
  ## Values
  # Specific (excise) pre tax
  STaxVaSpePre <- 3.4
  # AdValorem (excise)
  STaxVaAdvPre <- 0
  STaxVaAdvPos <- STaxVaAdvPre
  # Value Added Vat
  STaxVaVatPre <- 0.15
  STaxVaVatPos <- STaxVaVatPre
  # Total
  STaxVaTotPre <- STaxVaSpePre + STaxVaAdvPre + STaxVaVatPre
  STaxVaTotPos <- STaxVaSpePos + STaxVaAdvPos + STaxVaVatPos
  ## Rates
  # AdValorem (excise)
  STaxRaAdvPre <- 1
  STaxRaAdvPos <- STaxRaAdvPre
  # Value Added Vat
  STaxRaVatPre <- 0.15
  STaxRaVatPos <- STaxRaVatPre
  # Pass-Through Specific (excise)
  STaxPassSpePre <- 1.2
  STaxPassSpePos <- STaxPassSpePre
  ## Implicit Baseline Prices
  # Ibp Vat
  STaxIbpVatPre <- STaxVaVatPre/STaxRaVatPre
  STaxIbpVatPos <- STaxVaVatPos/STaxRaVatPos
  # Ibp AdValorem
  STaxIbpAdvPre <- STaxVaAdvPre/STaxRaAdvPre
  STaxIbpAdvPos <- STaxVaAdvPos/STaxRaAdvPos
  
  ##### Exwork
  # Manufacturing Cost
  SExwManCoPre <- 0.25
  SExwManCoPos <- SExwManCoPre
  # Manufacturing Profit Rate
  SExwMinProRaPre <- 0.2
  SExwMinProRaPos <- SExwMinProRaPre
  # Manufacturing Profit
  SExwManProPre <- SExwManCoPre*SExwMinProRaPre + (STaxPassSpePre-1)*STaxVaSpePre
  SExwManProPos <- SExwManCoPos*SExwMinProRaPos + (STaxPassSpePos-1)*STaxVaSpePos
  # Price Exwork with no taxes (excise)
  SExwPriNoExciPre <- SExwManCoPre + SExwManProPre
  SExwPriNoExciPos <- SExwManCoPos + SExwManProPos
  # Profit Rate
  SExwProRaPre <- (SExwPriNoExciPre - SExwManCoPre) / SExwManCoPre
  SExwProRaPos <- (SExwPriNoExciPos - SExwManCoPos) / SExwManCoPos
  
  ##### Price
  # Price Exwork with Taxes
  SPriPrExwTaxPre <- SExwPriNoExciPre + (STaxVaSpePre + STaxVaAdvPre + STaxVaVatPre)
  SPriPrExwTaxPos <- SExwPriNoExciPos + (STaxVaSpePos + STaxVaAdvPos + STaxVaVatPos)
  # Distribution Margin
  SPriDisMaPre <- 0.18
  SPriDisMaPos <- SPriDisMaPre
  # Retail Margin
  SPriRetMaPre <- 0.19
  SPriRetMaPos <- SPriRetMaPre
  # Retail Price
  SPriPrRetaPre <- SPriPrExwTaxPre*(1 + SPriDisMaPre + SPriRetMaPre)
  SPriPrRetaPos <- SPriPrExwTaxPos*(1 + SPriDisMaPos + SPriRetMaPos)
  # Rounded
  SPriPrRetaPreRou <- round(SPriPrRetaPre, 2)
  SPriPrRetaPosRou <- round(SPriPrRetaPos, 2)
  # Retail Price Increase
  SPriceIncrease <- (SPriPrRetaPos - SPriPrRetaPre)/SPriPrRetaPre
  
  ## Impact on Elasticities ~ half of elasticity for quitting, half for intensity
  # At the extensive margin
  SImpaElasExtensive <- 0.5
  # At the intensive margin (based on DEICS)
  SImpaElasIntensive <- -0.4524638
  # Increase Impact Extensive Margin Youth
  CQuitYouth <- 2
  
  ######### ELASTICITIES
  ## Quintiles/terciles
  HIQuin <- 1:5
  ## Elasticities  
  # Linear interpolation: flat using ref. Ec no.1
  SPriElas <- rep(-0.899, 5)
  # Create elasticities data frame
  elasticities_df <- data.frame(
    HIQuin = HIQuin,
    SPriElas = SPriElas
  )
  ## Probability of Quitting
  # Consumption - Quitting Proportion
  elasticities_df$CQuitProp <- 
    - elasticities_df$SPriElas * SImpaElasExtensive * SPriceIncrease
  # Consumption - Reduction on smoking intensity on those who remain smoking
  elasticities_df$CIntenProp <- 
    - elasticities_df$SPriElas * SImpaElasIntensive * SPriceIncrease
  
  ################### CONSUMPTION
  # Function to adjust number of smokers 
  AdjNumSmo <- function(df, lower_age, upper_age, sex, adj_factor) {
    df <- stps
    df <- df[order(df$idper),]
    # 1. Define target group
    fltr <- df$CAge >= lower_age &  df$CAge <= upper_age & df$CSex == sex
    fltrnonsm <- fltr & df$SFumaSino == 0
    fltrsmokr <- fltr & df$SFumaSino == 1
    # 2. Calculate number of new smokers to add (Total weighted smokers)*adj_factor
    nsmo <- sum(df$CFex[fltrsmokr]) * (adj_factor - 1)
    #message("Extra Smokers: ", nsmo)
    # 4. Assign random uniform values to non-smokers
    df$RanU <- NA
    df$RanU[fltrnonsm] <- 8 + runif(sum(fltrnonsm))
    # Term (+8) to avoid confusion between random numbers and probs 
    # (proportions and cutoff values)
    # 5. Sort by RanU descending and compute cumulative sum of CFex
    df <- df[order(df$RanU,decreasing = T),]
    df$RanPop <- NA
    df$RanPop[!is.na(df$RanU)] = cumsum(df$CFex[!is.na(df$RanU)])
    # 6. Flag new smokers (RanW)
    df$RanW <- 0
    df$RanW[!is.na(df$RanPop) & df$RanPop <= nsmo] <- 1 
    # 7. Update smoking status
    df$SFumaSino[df$RanW == 1] <- 1
    # 9. Impute median frequency and intensity for new smokers
    df <- df[order(df$idper),] #Reorder to calc medians
    df$SFumaFrec[df$RanW == 1] <- median(df$SFumaFrec[fltrsmokr], na.rm = TRUE) 
    df$SFumaCuan[df$RanW == 1] <- median(df$SFumaCuan[fltrsmokr], na.rm = TRUE)
    #!!!!! Imputar usand la distribucion, no la mediana de cada grupo
    # 10. Clean up temporary variables
    df <- df[,-grep("Ran",names(df))] 
    return(df)
  }
  
  ################### IMPORTS
  # Cigarette packs of 20 according to import records [REFREF]
  SCigaImpo <- 508.3
  # ILLICIT TRADE (PRE-TAX)
  SIlliTraPre <- 0.51
  
  ################### HEALTH
  # Smokers deaths caused by tobacco
  HDeathTob <- 0.5
  # Shares of Averted deaths by NCD
  HShareHear <- 0.46694529
  HShareStro <- 0.20801672
  HShareCopd <- 0.21238602
  HShareCanc <- 0.11265198
  
  # Second-Hand Smoke Shares of Averted deaths by NCD
  HShsShareHear <- 0.39574816
  HShsShareStro <- 0.108160262
  HShsShareCopd <- 0.412755519
  HShsShareCanc <- 0.083336059
  # RISK REDUCTION
  risk_reduction_df <- data.frame(
    CAgeGroup = c(4:14),
    HRiskRedu = c(0.968811722, 0.947662258, 0.92098104, 0.892470978, 
                  0.865640471, 0.836784324, 0.794983743, 0.729006299, 
                  0.628344949, 0.499176461, 0.364361418)
  )
  ## LIFE YEARS
  # Life Years Gained (base points for interpolation)
  life_years_base <- data.frame(
    HAgeLiYe = c(0, 15, 25, 45, 65, 105),
    HLiYeGain = c(0.00, 10.00, 9.00, 6.00, 3.00, 0.00)
  )
  ## Cubic spline interpolated values
  cubic_spline_df <- data.frame(
    CAge = 0:117,
    HAgeLiYe = c(
      0, 0.90168, 1.79706, 2.67985, 3.54376, 4.38248, 5.18973, 5.95922, 
      6.68464, 7.35970, 7.97810, 8.53356, 9.01977, 9.43045, 9.75929, 10.00000,
      10.14911, 10.21446, 10.20669, 10.13645, 10.01442, 9.85122, 9.65753, 
      9.44399, 9.22127, 9.00000, 8.78894, 8.58918, 8.39992, 8.22034, 8.04962,
      7.88695, 7.73152, 7.58252, 7.43914, 7.30056, 7.16596, 7.03454, 6.90549,
      6.77798, 6.65122, 6.52438, 6.39665, 6.26722, 6.13527, 6.00000, 5.86080,
      5.71788, 5.57169, 5.42264, 5.27117, 5.11771, 4.96268, 4.80651, 4.64964,
      4.49248, 4.33548, 4.17906, 4.02364, 3.86966, 3.71755, 3.56774, 3.42064,
      3.27670, 3.13635, 3.00000, 2.86800, 2.74034, 2.61689, 2.49754, 2.38219,
      2.27073, 2.16304, 2.05902, 1.95854, 1.86151, 1.76781, 1.67732, 1.58995,
      1.50557, 1.42408, 1.34536, 1.26930, 1.19580, 1.12474, 1.05601, 0.98950,
      0.92510, 0.86270, 0.80219, 0.74345, 0.68637, 0.63085, 0.57678, 0.52403,
      0.47251, 0.42209, 0.37268, 0.32415, 0.27640, 0.22932, 0.18279, 0.13670,
      0.09095, 0.04542, rep(0.00000, 13)
    )
  )
  
  ################### HEALTHCARE
  # Healthcare Utilization Gradient
  healthcare_gradient_df <- data.frame(
    HIQuin = 1:5,
    EUtiGrad = c(1.002717, 1.092391, 1.000000, 1.058424, 1.173913)
  )
  # Probability of utilization
  EUti <- 0.8
  # Calculate probability of using healthcare
  healthcare_gradient_df$EUtiGrad <- EUti * 
    healthcare_gradient_df$EUtiGrad
  ## COSTS (USD per year per person - Annual)
  ECost1 <- 6674.148 # Heart disease
  ECost2 <- 5602 # Stroke  
  ECost3 <- 5736.213 # COPD
  ECost4 <- 41424.914 # Cancer
  ## Out Of Pocket expenditure
  ECopayReim <- 1-0.34 # Cover by public expense
  # Proportion of healthcare expenditures as Out Of Pocket
  ECopayProOop <- 1-ECopayReim
  #ENIGHUR 2011: Gasto total 
  # Gradient normalized expenditure 
  gsq <- (c(5.70, 6.80, 7.50, 7.50, 8.20) *4.761905)/100
  healthcare_gradient_df$ECopayProOop <- gsq

  ################### POVERTY
  PLinPobMon <- 91.43 #0.28 # Monetary poverty line: Mensual per capita
  PLinPobExt <- 51.53 #0.127 # Indigence or extreme poverty line:  Mensual/capita
  PGasCat <- 0.1 # Catastrophic expenditure
  
  ################### Print key results
  cat("=== KEY MODEL PARAMETERS ===\n")
  cat("Retail Price Pre-Tax:", SPriPrRetaPre, "\n")
  cat("Retail Price Post-Tax:", SPriPrRetaPos, "\n") 
  cat("Price Increase:", 
      round(SPriceIncrease*100, 2), "%\n")
  cat("Quit Proportion (Q1):", 
      round(elasticities_df$CQuitProp[1]*100, 2), "%\n")
  cat("Intensity Reduction (Q1):", 
      round(elasticities_df$CIntenProp[1]*100, 2), "%\n")
  save.image(paste0(root,"Input.RData"))
  #############################################################################
  ##########################  II- SYNTHETIC DATASET ###########################
  #############################################################################
  ################### LOAD RAW DB AND PROCESS #################################
  stps <- read.dta13(paste0(root,"base construida 2.dta"),nonint.factors = TRUE)

  #Edad Sexo education
  stps$CAge <- stps$age
  stps$CAgeGroup <- as.numeric(as.factor(trunc(stps$age/5)))+3
  stps$CSex <- as.numeric(stps$sexn)
  stps$CEduYe  <- stps$c4
  stps$CParente <- NA
  stps$n <- 1
  
  # Geo
  stps$CVDepar <- stps$prov <- (trunc(stps$psu/10^10)) #Provincia
  stps$CVCabece <- stps$cant <- (trunc(stps$psu/10^8)) #Canton
  stps$parr <-  (trunc(stps$psu/10^6)) #Canton
  stps$CVRegion <- car::Recode(
    as.numeric(stps$prov),"1:6=1;10=1;11=1;17=1;18=1;23=1;
    7:9=2;12=2;13=2;24=2;90=2;14:16=3;19=3;21=3;22=3;20=3")#Region
  stps$CVRegion <- factor(stps$CVRegion, levels = c(1, 2, 3),
                          labels = c("Sierra", "Costa", "Oriente"))
  
  ###############################################################################
  ################### GENERATE SYNTHETIC DATASET ################################
  RSynS500 <-
    pblapply(1:NSimFi,FUN = function(x){
      #Some Parralell loadings
      library(dplyr)
      library(tidyr)
      load(paste0(root,"Input.RData"))
      # Adjust smoking intensity
      df <- stps_exp
      df <- AdjNumSmo(df,10, 16, 1, 2.75)
      df <- AdjNumSmo(df,10, 16, 2, 4.46)
      df <- AdjNumSmo(df,17, 21, 1, 1.56)
      df <- AdjNumSmo(df,17, 21, 2, 4.73)
      df <- AdjNumSmo(df,22, 26, 1, 1.31)
      df <- AdjNumSmo(df,22, 26, 2, 3.23)
      df <- AdjNumSmo(df,27, 31, 1, 1.41)
      df <- AdjNumSmo(df,27, 31, 2, 2.91)
      df <- AdjNumSmo(df,32, 35, 1, 1.22)
      df <- AdjNumSmo(df,32, 35, 2, 2.51)
      df <- AdjNumSmo(df,36, 49, 1, 1.14)
      df <- AdjNumSmo(df,36, 49, 2, 2.01)
      df <- AdjNumSmo(df,50, 64, 1, 1.07)
      df <- AdjNumSmo(df,50, 64, 2, 1.69)
      # Household-level smoking indicator: useless for steps
      df$SFumaHSino <- df$SFumaSino
      # Save final version
      return(df)
    })
  write_rds(RSynS500,file =  paste0(oroot,"RSynS",simname, ".rds"))
  ###############################################################################
  ##########################  III- SIMULATIONS ##################################
  ###############################################################################
  #RSynS500 <- read_rds(file =  paste0(oroot,"RSynS",simname, ".rds"))
  # Parralellize
  numCores <- detectCores()
  cl <- parallel::makeCluster(numCores)
  # Main processing loop
  SSimS500 <- pblapply(RSynS500,cl=cl,FUN = function(s){
    #Some Parallel loadings
    library(dplyr)
    library(tidyr)
    load(paste0(root,"Input.RData"))
    
    ################# LOAD SYNTHETIC DATASET
    synthetic_data <- s
    #synthetic_data <- readRDS(paste0(sroot,"RSynS", s, ".rds"))
    synthetic_data <- synthetic_data %>%  rename(HIQuin = RIngQuin)
    # ELASTICITIES & PROBABILITY OF QUITTING
    data <- merge(synthetic_data, elasticities_df, by = "HIQuin", all.x = TRUE)
    # Adjust Probability of quitting for young population
    data$CQuitProp[data$CAgeGroup <= 5] <- 
      data$CQuitProp[data$CAgeGroup <= 5] * CQuitYouth
    
    ################# CONSUMPTION
    ############## QUITTING: Did ____ quit after the tax?
    data$CQuit <- ifelse(data$SFumaSino == 1, 0, NA)
    # Process each income quintile
    for (i in 1:5) {
      data <- data[order(data$idper),]
      # Probability of being selected (same for all in the group)
      smokers_quintile <- 
        data$SFumaSino == 1 & data$HIQuin == i & 
        !is.na(data$SFumaSino) & !is.na(data$HIQuin)
      
      if (sum(smokers_quintile, na.rm = TRUE) > 0) {
        data <- randalloc(data, smokers_quintile, "CQuit",data$CQuitProp)
      }
    }
    # REDUCTION IN SMOKING INTENSITY
    data$SFumaCuanPos <- ifelse(data$CQuit == 0, 
                                data$SFumaCuan * (1 + data$CIntenProp), 
                                NA)
    ################# HEALTH
    # DEATHS PRE TAX
    data$HDiePreTax <- ifelse(data$SFumaSino == 1, 0, NA)
    # Random allocation for pre-tax deaths
    smokers <- data$SFumaSino == 1 & !is.na(data$SFumaSino)
    if (sum(smokers, na.rm = TRUE) > 0) {
      data <- randalloc(data,smokers,"HDiePreTax",HDeathTob)
    }
    # DEATHS FROM SECOND-HAND SMOKE (SHS) PRE-TAX
    # Number of smokers in the household
    data <- data %>%
      group_by(id_hog) %>%
      mutate(SFumaSmoHh = sum(SFumaSino, na.rm = TRUE)) %>%
      ungroup()
    # Identify smokers in non-single-person HH where not all members smoke
    data$SmoNonUni <- ifelse(data$SFumaSino == 1 & data$RCanPerso < 2, 0,
                             ifelse(data$SFumaSino == 1 & data$RCanPerso >= 2 & 
                                      data$SFumaSmoHh < data$RCanPerso, 1, NA))
    
    TSmoNonUni <- sum(data$CFex[data$SmoNonUni == 1], na.rm = TRUE)
    #cat("# Smokers in non-single-person HHs:", TSmoNonUni, "\n")
    
    # Number of deaths due to SHS
    TDeShsPre <- TSmoNonUni / 56.1
    # Identify population exposed to SHS
    data <- data %>%
      group_by(id_hog) %>%
      mutate(SExpoShs0 = sum(SmoNonUni, na.rm = TRUE)) %>%
      ungroup()
    data$SExpoShs1 <- ifelse(!is.na(data$SExpoShs0) & data$SExpoShs0 > 0 & 
                               !is.na(data$SFumaSino) & data$SFumaSino == 0, 1, NA)
    data$HDiePreShs <- ifelse(data$SExpoShs1 == 1, 0, NA)
    # Random allocation of SHS deaths
    exposed <- data$SExpoShs1 == 1 & !is.na(data$SExpoShs1)
    if (sum(exposed, na.rm = TRUE) > 0) {
      data <- randalloc(data,exposed,"SExpoShs1",TDeShsPre)
    }
    
    # Clean up temporary variables
    data$SFumaSmoHh <- NULL
    data$SmoNonUni <- NULL
    data$SExpoShs0 <- NULL
    data$SExpoShs1 <- NULL
    
    # DEATHS POST-TAX
    # Load risk reduction data
    data <- merge(data, risk_reduction_df, by = "CAgeGroup", all.x = TRUE)
    
    # Apply risk reduction only to smokers
    data$HRiskRedu[data$SFumaSino != 1] <- NA
    
    # Death probability after quitting
    data$HQuitDie <- ifelse(data$CQuit == 1, 0, NA)
    
    # Process each age group
    for (k in 1:21) {
      quitters_age <- data$CQuit == 1 & 
        data$CAgeGroup == k & !is.na(data$CQuit) & !is.na(data$CAgeGroup)
      if (sum(quitters_age, na.rm = TRUE) > 0) {
        data <- randalloc(data,quitters_age,"HQuitDie",(1-data$HRiskRedu),
                          fltr2= (data$CQuit==1  & data$HDiePreTax==1))
      }
    }
    ####### SHS deaths post-tax
    data$HQuitDieShs <- ifelse(data$HDiePreShs == 1, 1, NA)
    # Calculate number of smokers in household post-tax
    data <- data %>%
      group_by(id_hog) %>%
      mutate(TSmoHh = sum(SFumaSino == 1 & CQuit == 0, na.rm = TRUE)) %>%
      ungroup()
    # No SHS death if all smokers in household quit
    data$HQuitDieShs[data$HQuitDieShs == 1 & data$TSmoHh == 0] <- 0
    # SHS deaths averted
    data$HDeathShsAver <- ifelse(data$HDiePreShs == 1, 0, NA)
    data$HDeathShsAver[data$HDiePreShs == 1 & data$HQuitDieShs == 0] <- 1
    
    ####### LIFE YEARS GAINED
    data <- merge(data, cubic_spline_df, by = "CAge", all.x = TRUE)
    
    # Apply only to smokers
    data$HAgeLiYe[data$SFumaSino != 1] <- NA
    
    # Life years gained for quitters
    data$HLiYeGainPosTaxQuit <- ifelse(data$CQuit == 1, data$HAgeLiYe, NA)
    
    # **** DEATHS AVERTED
    data$HDiePosTax <- ifelse(!is.na(data$HDiePreTax), 0, NA)
    data$HDiePosTax[data$HQuitDie == 1 | (data$CQuit == 0 & data$HDiePreTax == 1)] <- 1
    data$HDeathAver <- ifelse(data$HDiePreTax == 1, 0, NA)
    data$HDeathAver[data$HDiePreTax == 1 & data$HQuitDie == 0] <- 1
    
    ################# CAUSE OF DEATH (NCD) FOR SMOKERS AVERTED
    # Initialize NCD death averted variable
    data$HNcdDeaAver <- NA
    # Random allocation for NCDs among those with deaths averted
    deaths_averted <- data$HDeathAver == 1 & !is.na(data$HDeathAver)
    if (sum(deaths_averted, na.rm = TRUE) > 0) {
      data$RanU <- NA
      data$RanU[deaths_averted] <- runif(sum(deaths_averted))
      temp_data <- data[deaths_averted, ]
      temp_data <- temp_data[order(-temp_data$RanU), ]
      temp_data$RanPop <- cumsum(temp_data$CFex)
      total_deaths <- sum(temp_data$CFex)
      
      # Heart Disease
      temp_data$RanPro1 <- temp_data$RanPop / total_deaths
      temp_data$HNcdDeaAver[temp_data$RanPro1 <= HShareHear] <- 1
      # Stroke
      remaining <- temp_data$HDeathAver==1 & is.na(temp_data$HNcdDeaAver)
      temp_data$RanPop2[remaining] <- cumsum(temp_data$CFex[remaining])
      temp_data$RanPro2 <- ifelse(remaining, temp_data$RanPop2 /total_deaths, NA)
      temp_data$HNcdDeaAver[remaining & temp_data$RanPro2 <= HShareStro] <- 2
      
      # COPD
      remaining <- is.na(temp_data$HNcdDeaAver)
      if (sum(remaining, na.rm = TRUE) > 0) {
        temp_data$RanPop3[remaining] <- cumsum(temp_data$CFex[remaining])
        temp_data$RanPro3 <- ifelse(remaining, temp_data$RanPop3 / total_deaths, NA)
        temp_data$HNcdDeaAver[remaining &
                                temp_data$RanPro3 <= HShareCopd] <- 3
      }
      
      # Cancer  
      remaining <- is.na(temp_data$HNcdDeaAver)
      if (sum(remaining, na.rm = TRUE) > 0) {
        temp_data$RanPop4[remaining] <- cumsum(temp_data$CFex[remaining])
        temp_data$RanPro4 <- ifelse(remaining, temp_data$RanPop4 / total_deaths, NA)
        temp_data$HNcdDeaAver[remaining &
                                temp_data$RanPro4 <= HShareCanc] <- 4
      }
      
      # Handle rounding errors
      remaining <- is.na(temp_data$HNcdDeaAver)
      if (sum(remaining, na.rm = TRUE) > 0) {
        n_remaining <- sum(remaining)
        ce1 <- ceiling(HShareCanc * n_remaining)
        ce2 <- ceiling(HShareCopd * n_remaining)
        ce3 <- ceiling(HShareStro * n_remaining)
        temp_data$xmiss <- ifelse(remaining, 1:sum(remaining), NA)
        temp_data$HNcdDeaAver[remaining & temp_data$xmiss <= ce1] <- 1
        temp_data$HNcdDeaAver[remaining & is.na(temp_data$HNcdDeaAver) & temp_data$xmiss <= ce1 + ce2] <- 2
        temp_data$HNcdDeaAver[remaining & is.na(temp_data$HNcdDeaAver) & temp_data$xmiss <= ce1 + ce2 + ce3] <- 3
        temp_data$HNcdDeaAver[remaining & is.na(temp_data$HNcdDeaAver)] <- 4
      }
      # Update main dataset
      data <- merge(
        data[,-grep("HNcdDeaAver",names(data))],
        temp_data[,c("idper","HNcdDeaAver")],all.x=T)
    }
    
    ################# CAUSE OF DEATH (NCD) FOR SECOND HAND SMOKE
    data$HNcdShsDeaAver <- NA
    shs_deaths_averted <- data$HDeathShsAver == 1 & !is.na(data$HDeathShsAver)
    if (sum(shs_deaths_averted, na.rm = TRUE) > 0) {
      data$RanU <- NA
      data$RanU[shs_deaths_averted] <- runif(sum(shs_deaths_averted))
      temp_data <- data[shs_deaths_averted, ]
      temp_data <- temp_data[order(-temp_data$RanU), ]
      temp_data$RanPop <- cumsum(temp_data$CFex)
      total_deaths <- sum(temp_data$CFex)
      
      # Heart Disease
      temp_data$RanPro1 <- temp_data$RanPop / total_deaths
      temp_data$HNcdShsDeaAver[temp_data$RanPro1 <= HShsShareHear] <- 1
      
      # Stroke
      remaining <- temp_data$HDeathAver==1 & is.na(temp_data$HNcdShsDeaAver)
      temp_data$RanPop2[remaining] <- cumsum(temp_data$CFex[remaining])
      temp_data$RanPro2 <- ifelse(remaining, temp_data$RanPop2 /total_deaths, NA)
      temp_data$HNcdShsDeaAver[remaining & temp_data$RanPro2 <= HShsShareStro] <- 2
      
      # COPD
      remaining <- is.na(temp_data$HNcdShsDeaAver)
      if (sum(remaining, na.rm = TRUE) > 0) {
        temp_data$RanPop3[remaining] <- cumsum(temp_data$CFex[remaining])
        temp_data$RanPro3 <- ifelse(remaining, temp_data$RanPop3 / total_deaths, NA)
        temp_data$HNcdShsDeaAver[remaining &
                                   temp_data$RanPro3 <= HShsShareCopd] <- 3
      }
      
      # Cancer  
      remaining <- is.na(temp_data$HNcdShsDeaAver)
      if (sum(remaining, na.rm = TRUE) > 0) {
        temp_data$RanPop4[remaining] <- cumsum(temp_data$CFex[remaining])
        temp_data$RanPro4 <- ifelse(remaining, temp_data$RanPop4 / total_deaths, NA)
        temp_data$HNcdShsDeaAver[remaining &
                                   temp_data$RanPro4 <= HShsShareCanc] <- 4
      }
      
      # Handle rounding errors
      remaining <- is.na(temp_data$HNcdShsDeaAver)
      if (sum(remaining, na.rm = TRUE) > 0) {
        n_remaining <- sum(remaining)
        ce1 <- ceiling(HShsShareCanc * n_remaining)
        ce2 <- ceiling(HShsShareCopd * n_remaining)
        ce3 <- ceiling(HShsShareStro * n_remaining)
        temp_data$xmiss <- ifelse(remaining, 1:sum(remaining), NA)
        temp_data$HNcdShsDeaAver[remaining & temp_data$xmiss <= ce1] <- 1
        temp_data$HNcdShsDeaAver[remaining & is.na(temp_data$HNcdShsDeaAver) & temp_data$xmiss <= ce1 + ce2] <- 2
        temp_data$HNcdShsDeaAver[remaining & is.na(temp_data$HNcdShsDeaAver) & temp_data$xmiss <= ce1 + ce2 + ce3] <- 3
        temp_data$HNcdShsDeaAver[remaining & is.na(temp_data$HNcdShsDeaAver)] <- 4
      }
      
      # Update main dataset
      data <- merge(
        data[,-grep("HNcdShsDeaAver",names(data))],
        temp_data[,c("idper","HNcdShsDeaAver")],all.x=T)  
    }
    
    ################# HEALTHCARE
    # Load healthcare utilization gradient
    data <- merge(data, healthcare_gradient_df, by = "HIQuin", all.x = TRUE)
    data$EUtiGrad[is.na(data$HNcdDeaAver) & is.na(data$HNcdShsDeaAver)] <- NA
    
    # Healthcare utilization
    healthcare_candidates <- !is.na(data$HNcdDeaAver) | !is.na(data$HNcdShsDeaAver)
    data$EUtiHCare <- ifelse(as.numeric(healthcare_candidates) == 1, 0, NA)
    
    if (sum(healthcare_candidates, na.rm = TRUE) > 0) {
      data <- randalloc(data,healthcare_candidates,"EUtiHCare",data$EUtiGrad)
    }
    
    ###### COSTS OF TREATMENT
    data$ECostTotal <- NA
    
    # Assign costs based on NCD type and utilization
    fltr1 <- which((data$HNcdDeaAver == 1 | data$HNcdShsDeaAver == 1) & 
                     data$EUtiHCare == 1)
    fltr2 <- which((data$HNcdDeaAver == 2 | data$HNcdShsDeaAver == 2) & 
                     data$EUtiHCare == 1)
    fltr3 <- which((data$HNcdDeaAver == 3 | data$HNcdShsDeaAver == 3) & 
                     data$EUtiHCare == 1)
    fltr4 <- which((data$HNcdDeaAver == 4 | data$HNcdShsDeaAver == 4) & 
                     data$EUtiHCare == 1)
    data$ECostTotal[fltr1] <- data$EUtiHCare[fltr1] * ECost1/12
    data$ECostTotal[fltr2] <- data$EUtiHCare[fltr2] * ECost2/12
    data$ECostTotal[fltr3] <- data$EUtiHCare[fltr3] * ECost3/12
    data$ECostTotal[fltr4] <- data$EUtiHCare[fltr4] * ECost4/12
    
    # Out of Pocket and Out of Health System costs
    data$ECostOop <- data$ECopayProOop * data$ECostTotal
    data$ECostOohs <- (1 - (data$ECopayProOop)) * data$ECostTotal
    
    ################# POVERTY
    ###### HOUSEHOLDS CURRENTLY IN POVERTY
    data$PHouPobMon <- ifelse(!is.na(data$UMicroIngPer), 0, NA)
    data$PHouPobMon[!is.na(data$UMicroIngPer) & 
                      data$UMicroIngPer < PLinPobMon] <- 1
    
    data$PHouPobExt <- ifelse(!is.na(data$UMicroIngPer), 0, NA)
    data$PHouPobExt[!is.na(data$UMicroIngPer) & 
                      data$UMicroIngPer < PLinPobExt] <- 1
    
    ###### HHs PUSHED INTO POVERTY AVERTED
    data <- data %>%
      group_by(id_hog) %>%
      mutate(ECostOopH = sum(ECostOop, na.rm = TRUE)) %>%
      ungroup()
    
    data$PAverPobMon <- ifelse(data$PHouPobMon == 0, 0, NA)
    data$PAverPobMon[
      data$PAverPobMon == 0 & 
        (data$UMicroIngTot - data$ECostOopH)/data$UCanPerso < PLinPobMon] <- 1
    
    ###### HHs PUSHED INTO CATASTROPHIC EXPENDITURE AVERTED
    data$PPropGasCat <- data$ECostOopH / data$UMicroIngTot
    data$PAverGasCas <- ifelse(!is.na(data$UMicroIngTot), 0, NA)
    data$PAverGasCas[!is.na(data$PPropGasCat) & data$PPropGasCat > PGasCat] <- 1
    
    ################# Final calculations and ordering
    data$Nal <- 1
    # Save final simulation results
    data <- data[order(data$idper),]
    return(data)
  })
  total_time <- Sys.time() - start_time_total
  parallel::stopCluster(cl)
  cat("Total scenario simulation ",simname, " completed in:",total_time)
  rm(RSynS500)
  write_rds(SSimS500,file =  paste0(oroot,"SSimS",NSimFi,simname, ".rds"))
  ###############################################################################
  ##########################  IV- OUTPUTS #######################################
  ###############################################################################
  #SSimS500 <- read_rds(file =  paste0(oroot,"SSimS",NSimFi,simname,".rds"))
  # Add iteration count var
  SSimS500 <- lapply(1:NSimFi,function(x) {
    SSimS500[[x]]$iteration <- x
    return(SSimS500[[x]])
  })
  # Parralellize
  numCores <- detectCores()
  cl <- makeCluster(numCores)
  # Main processing loop
  RSimS <- pblapply(SSimS500,cl=cl,FUN = function(s){
    #Some Parrallel loadings
    library(dplyr)
    library(tidyr)
    load(paste0(root,"Input.RData"))
    # Process each disaggregation level
    data <- s
  
    ################# CONSUMPTION
    ###### NUMBER OF CIGARETTES
    data$Cig = data$CFex * data$SFumaCuan * (30 * 12) / 20
    data$CigPos = NA
    data$CigPos[data$SFumaSino == 1 & data$CQuit == 0] = 
      data$CFex[data$SFumaSino == 1 & data$CQuit == 0] * 
      data$SFumaCuanPos[data$SFumaSino == 1 & data$CQuit == 0] * (30 * 12) / 20
    data$Nacional <- "Nacional"
    Desag <- c("CSex","HIQuin","CVRegion","Nacional")
    RStrat <- lapply(Desag,FUN = function(d){
      ##### Population data
      population_data <- data %>%
        # Grouping var 
        group_by(.data[[d]]) %>%
        summarise(
          ##### POPULATION
          CPopulation = sum(CFex) / 10^6,
          # NUMBER OF SMOKERS
          # Pre-Tax smokers
          CPreTaxSmok = sum(CFex[SFumaSino == 1], na.rm = TRUE) / 10^6,
          # Post-Tax smokers
          CPosTaxSmok = sum(CFex[SFumaSino == 1 & CQuit == 0],
                            na.rm = TRUE)/10^6,
          ##### CONSUMPTION
          # Smoking Intensity Pre-Tax (median)
          CPreInteMedian = weighted.mean(SFumaCuan, CFex, na.rm = TRUE),
          # Smoking Intensity Post-Tax (median)
          CPosInteMedian = weighted.mean(SFumaCuanPos, CFexTab, na.rm = TRUE),
          # Pre-Tax cigarettes smoked
          CPreTaxCigSmo = sum(Cig, na.rm = TRUE) / 10^6,
          # Post-Tax cigarettes smoked
          CPosTaxCigSmo = sum(CigPos[SFumaSino == 1 & CQuit == 0],
                              na.rm = TRUE) / 10^6, 
          .groups = 'drop'
          ) 
      
      # Reduction due to intensity (intensive margin)
      reduction_intensive <- data %>%
        filter(SFumaSino == 1 & CQuit == 0) %>%
        mutate(Cig = CFex * SFumaCuan * (30 * 12) / 20 - 
                 CFex * SFumaCuanPos * (30 * 12) / 20) %>%
        group_by(.data[[d]]) %>%
        summarise(CPrePosCigSmoInDif = sum(Cig, na.rm = TRUE) / 10^6, 
                  .groups = 'drop')
      
      # Reduction due to quitting (extensive margin)
      reduction_extensive <- data %>%
        filter(SFumaSino == 1 & CQuit == 1) %>%
        mutate(Cig = CFex * SFumaCuan * (30 * 12) / 20) %>%
        group_by(.data[[d]]) %>%
        summarise(CPrePosCigSmoExDif = sum(Cig, na.rm = TRUE) / 10^6, 
                  .groups = 'drop')
      
      # Total cigarettes paying taxes
      cigarettes_total_pre <- data %>%
        group_by(.data[[d]]) %>%
        summarise(CPreTaxCigTot = SCigaImpo, .groups = 'drop')
      
      cigarettes_total_pos <- data %>%
        mutate(
          CigPre = ifelse(SFumaSino == 1, CFex * SFumaCuan * (30 * 12) / 20, 0),
          CigPos = ifelse(SFumaSino == 1 & CQuit == 0, 
                          CFex * SFumaCuanPos * (30 * 12) / 20, 0)
        ) %>%
        group_by(.data[[d]]) %>%
        summarise(
          TCigPre = sum(CigPre, na.rm = TRUE),
          TCigPos = sum(CigPos, na.rm = TRUE),
          CPosTaxCigTot = SCigaImpo - (TCigPre - TCigPos) / 1000000,
          .groups = 'drop'
        ) %>%
        select(all_of(d), CPosTaxCigTot)
      
      # Illicit Trade
      illicit_trade <- data %>%
        mutate(TIlli = (CFex * SFumaCuan * (30 * 12) / 20) * SIlliTraPre) %>%
        group_by(.data[[d]]) %>%
        summarise(CPrePosCigIllTra = sum(TIlli, na.rm = TRUE) / 1000000,
                  .groups = 'drop'
                  )
      
      ###############  HEALTH
      # Deaths Pre-Tax
      deaths_pretax <- data %>%
        filter(HDiePreTax == 1) %>%
        group_by(.data[[d]]) %>%
        summarise(HDeathPreTax = sum(CFex, na.rm = TRUE) / 1000,
                  .groups = 'drop')
      
      # Deaths from Second-Hand Smoke Pre-Tax: NO RESULTS 
      deaths_shs_pretax <- data %>%
        filter(HDiePreShs == 1) %>%
        group_by(.data[[d]]) %>%
        summarise(HDeathPreShs = sum(CFex, na.rm = TRUE) / 1000, 
                  .groups = 'drop')
      
      # Life Years Gained
      life_years_gained <- data %>%
        mutate(LiYe = CFex * HLiYeGainPosTaxQuit) %>%
        group_by(.data[[d]]) %>%
        summarise(HLiYearGainPosTaxQuit = sum(LiYe, na.rm = TRUE) / 10^6,
                  .groups = 'drop')
      
      # Deaths of quitters who still die
      deaths_quit_still_die <- data %>%
        filter(HQuitDie == 1) %>%
        group_by(.data[[d]]) %>%
        summarise(HDeathPosTaxQuit = sum(CFex, na.rm = TRUE) / 1000, 
                  .groups = 'drop')
      
      # Deaths of those who keep smoking
      deaths_keep_smoking <- data %>%
        filter(HDiePosTax == 1) %>%
        group_by(.data[[d]]) %>%
        summarise(HDeathPosTaxSmokQuit = sum(CFex, na.rm = TRUE) / 1000,
                  .groups = 'drop')
      
      # Deaths averted
      deaths_averted <- data %>%
        filter(HDeathAver == 1) %>%
        group_by(.data[[d]]) %>%
        summarise(HDeathsAver = sum(CFex, na.rm = TRUE) / 1000,
                  .groups = 'drop')
      
      # Deaths averted by NCD type
      data$HNcdDeaAverNm <- car::Recode(
        data$HNcdDeaAver, "1='HDeathsAverHear';2='HDeathsAverStro';
        3='HDeathsAverCopd';4='HDeathsAverCanc'"
      )
      ncd_deaths <- data %>%
        group_by(.data[[d]],HNcdDeaAverNm) %>%
        summarise(val= sum(CFex, na.rm = TRUE) / 1000, .groups = 'drop') %>%
        drop_na() %>% 
        spread(key=HNcdDeaAverNm,value = val,drop = T)
      
      # SHS Deaths averted
      shs_deaths_averted <- data %>%
        filter(HDeathShsAver == 1) %>%
        group_by(.data[[d]]) %>%
        summarise(HDeathsShsAv = sum(CFex, na.rm = TRUE) / 1000, 
                  .groups = 'drop')
      
      ############### HEALTHCARE
      # Total Healthcare Costs Averted by NCD type
      data$healthcare_costsNm <-  car::Recode(
        data$HNcdDeaAver, "1='ECostHeartAver';2='ECostStroAver';
        3='ECostCopdAver';4='ECostCancAver'"
      )
      cost_data <- data %>%
        mutate(CosTot = CFex * ECostTotal * 12) %>%
        group_by(.data[[d]],healthcare_costsNm) %>%
        summarise(Cost = sum(CosTot, na.rm = TRUE), .groups = 'drop') %>%
        drop_na() %>% 
        spread(key=healthcare_costsNm,value = Cost,drop = T)
      
      # Out-of-Pocket Costs Averted
      data$oop_costsNm <-  car::Recode(
        data$HNcdDeaAver, "1='ECostHeartOopAver';2='ECostStroOopAver';
        3='ECostCopdOopAver';4='ECostCancOopAver'"
      )
      oop_data <- data %>%
        mutate(CosTot = CFex * ECostOop * 12) %>%
        group_by(.data[[d]],oop_costsNm) %>%
        summarise(Cost= sum(CosTot, na.rm = TRUE), .groups = 'drop') %>%
        drop_na() %>% 
        spread(key=oop_costsNm,value = Cost,drop = T)
      
      # Out-of-Health-System Costs Averted
      data$oohs_costsNm <- car::Recode(
        data$HNcdDeaAver, "1='ECostHeartOohsAver';2='ECostStroOohsAver';
        3='ECostCopdOohsAver';4='ECostCancOohsAver'"
      )
      oohs_data <- data %>%
        mutate(CosTot = CFex * ECostOohs * 12) %>%
        group_by(.data[[d]],oohs_costsNm) %>%
        summarise(Cost= sum(CosTot, na.rm = TRUE), .groups = 'drop') %>%
        drop_na() %>% 
        spread(key=oohs_costsNm,value = Cost,drop = T)
      
      ############### POVERTY
      # People averted from poverty
      poverty_averted <- data %>%
        filter(PAverPobMon == 1) %>%
        group_by(.data[[d]]) %>%
        summarise(PAverPovCostOop = sum(CFex, na.rm = TRUE) / 1000, 
                  .groups = 'drop')
      
      # People averted from catastrophic expenditure
      catastrophic_averted <- data %>%
        filter(PAverGasCas == 1) %>%
        group_by(.data[[d]]) %>%
        summarise(PAverCatasCostOop = sum(CFex, na.rm = TRUE) / 1000,
                  .groups = 'drop')
      
      ############### TAX REVENUE
      # Tax Revenue from cigarettes smoked in Ecuador
      tax_revenue_pre_smo <- data %>%
        mutate(Con = (CFex * SFumaCuan * (30 * 12) / 20)) %>%
        group_by(.data[[d]]) %>%
        summarise(
          TCon = sum(Con, na.rm = TRUE),
          ConNetIll = TCon * (1 - SIlliTraPre),
          TRevPreSpeSmo = (ConNetIll * STaxVaSpePre) / 10^6,
          .groups = 'drop'
        ) %>%
        select(all_of(d), TRevPreSpeSmo)
      
      tax_revenue_pos_smo <- data %>%
        mutate(
          ConsPre = (CFex * SFumaCuan * (30 * 12) / 20),
          ConsPos = ifelse(SFumaSino == 1 & CQuit == 0, 
                           (CFex * SFumaCuanPos * (30 * 12) / 20), 0)
        ) %>%
        group_by(.data[[d]]) %>%
        summarise(
          TConsPre = sum(ConsPre, na.rm = TRUE),
          ConsPreIlli = TConsPre * SIlliTraPre,
          TConsPos = sum(ConsPos, na.rm = TRUE),
          #MODIF: Uso de cantidad total illi pos
          ConsPosIlli = TConsPos * SIlliTraPre,
          ConsNetIll = TConsPos - ConsPosIlli,
          TRevPosSpeSmo = (ConsNetIll * STaxVaSpePos) / 10^6,
          .groups = 'drop'
        ) %>%
        select(all_of(d), TRevPosSpeSmo)
      
      # Tax Revenue from imports
      # tax_revenue_imports_pre <- data %>%
      #   group_by(.data[[d]]) %>%
      #   summarise(
      #     TRev = (SCigaImpo * 1000000) * STaxVaSpePre,
      #     TRevPreSpeCol = TRev / 10^6,
      #     .groups = 'drop'
      #   ) %>%
      #   select(all_of(d), TRevPreSpeCol) #%>%
      #   #slice(1)  # Take first row since constant
      # 
      # tax_revenue_imports_pos <- data %>%
      #   mutate(
      #     CigPre = ifelse(SFumaSino == 1, CFex * SFumaCuan * 
      #        (30 * 12) / 20, 0),
      #     CigPos = ifelse(SFumaSino == 1 & CQuit == 0, CFex * SFumaCuanPos * 
      #        (30 * 12) / 20, 0)
      #   ) %>%
      #   group_by(.data[[d]]) %>%
      #   summarise(
      #     TCigPre = sum(CigPre, na.rm = TRUE),
      #     TCigPos = sum(CigPos, na.rm = TRUE),
      #     TRevPosSpeCol = ((SCigaImpo * 1000000) - (TCigPre - TCigPos)) * 
      #      STaxVaSpePos / 1000000000000,
      #     .groups = 'drop'
      #   ) %>%
      #   select(all_of(d), TRevPosSpeCol)
      
      ############### INDUSTRY PROFITS
      industry_profits_pre <- data %>%
        mutate(Prof = (CFex * SFumaCuan * (30 * 12) / 20) * SExwManProPre) %>%
        group_by(.data[[d]]) %>%
        summarise(TProfPreSmo = sum(Prof, na.rm = TRUE) / 10^6, 
                  .groups = 'drop')
      
      industry_profits_pos <- data %>%
        filter(SFumaSino == 1 & CQuit == 0) %>%
        mutate(
          Prof = (CFex * SFumaCuanPos * (30 * 12) / 20) * SExwManProPos) %>%
        group_by(.data[[d]]) %>%
        summarise(TProfPosSmo = sum(Prof, na.rm = TRUE) / 10^6, 
                  .groups = 'drop')
      
      ############### DEVELOPMENT
      # Total Years of Education lost Averted
      education_total <- data %>%
        filter(HDeathAver == 1) %>%
        mutate(YeEduEx = CFex * CEduYe) %>%
        group_by(.data[[d]]) %>%
        summarise(TEduYeAve = sum(YeEduEx, na.rm = TRUE) / 10^6, 
                  .groups = 'drop')
      
      # Median Years of Education Averted
      education_median <- data %>%
        filter(HDeathAver == 1) %>%
        group_by(.data[[d]]) %>%
        summarise(TEduYeMedi = median(rep(CEduYe, CFexTab), na.rm = TRUE), 
                  .groups = 'drop')
      
      # Average Years of Education Averted
      education_average <- data %>%
        filter(HDeathAver == 1) %>%
        group_by(.data[[d]]) %>%
        summarise(TEduYeAver = weighted.mean(CEduYe, CFexTab, na.rm = TRUE), 
                  .groups = 'drop')
      
      ############### JOIN AND SAVE
      res <- merge(population_data,reduction_intensive)
      res <- merge(res,reduction_extensive,all.x=T)
      res <- merge(res,cigarettes_total_pre,all.x=T)
      res <- merge(res,cigarettes_total_pos,all.x=T)
      res <- merge(res,illicit_trade,all.x=T)
      res <- merge(res,deaths_pretax,all.x=T)
      res <- merge(res,deaths_shs_pretax,all.x=T)
      res <- merge(res,life_years_gained,all.x=T)
      res <- merge(res,deaths_quit_still_die,all.x=T)
      res <- merge(res,deaths_keep_smoking,all.x=T)
      res <- merge(res,deaths_averted,all.x=T)
      res <- merge(res,ncd_deaths,all.x=T)
      res <- merge(res,shs_deaths_averted,all.x=T)
      res <- merge(res,cost_data,all.x=T)
      res <- merge(res,oop_data,all.x=T)
      res <- merge(res,oohs_data,all.x=T)
      res <- merge(res,poverty_averted,all.x=T)
      res <- merge(res,catastrophic_averted,all.x=T)
      res <- merge(res,tax_revenue_pre_smo,all.x=T)
      res <- merge(res,tax_revenue_pos_smo,all.x=T)
      res <- merge(res,industry_profits_pre,all.x=T)
      res <- merge(res,industry_profits_pos,all.x=T)
      res <- merge(res,education_total,all.x=T)
      res <- merge(res,education_median,all.x=T)
      res <- merge(res,education_average,all.x=T)
      #Strata & Iteration
      names(res)[1] <- "value"
      res$Strata <- d
      res$Iteration <- data$iteration[1]
      return(res)
    })
    RStrat <- do.call("rbind",RStrat)
    # Add scenario characteristics
    RStrat$SPriPrRetaPre=SPriPrRetaPre
    RStrat$SPriPrRetaPos=SPriPrRetaPos
    RStrat$STaxVaSpePre=STaxVaSpePre
    RStrat$STaxVaSpePos=STaxVaSpePos
    return(RStrat)
  })
  parallel::stopCluster(cl)
  #Export outputs 
  write_rds(RSimS,file =  paste0(sroot,"RSimS",NSimFi,simname,".rds"))
  end_time <- Sys.time() # Timing for simulation
  rm(SSimS500)
  rm(RSimS)
  cat("Simulation ",simname," completed in",end_time- start_time_total)
  ###############################################################################
}
###############################################################################
##########################  V- ANALYSE ########################################
###############################################################################

###### Loadings
load(paste0(root,"Input.RData"))
simfile <- list.files(sroot,pattern = "RSimS1000.*_s[0-9]")

###### Results variables 
vnms <- c(
  "value","Strata","Iteration","CPopulation","CPreTaxSmok",
  "CPosTaxSmok","CPreInteMedian",
  "CPosInteMedian","CPreTaxCigSmo","CPosTaxCigSmo","CPrePosCigSmoInDif",
  "CPrePosCigSmoExDif","CPreTaxCigTot","CPosTaxCigTot","CPrePosCigIllTra",
  "HDeathPreTax","HDeathPreShs","HLiYearGainPosTaxQuit","HDeathPosTaxQuit",
  "HDeathPosTaxSmokQuit","HDeathsAver","HDeathsAverCanc",
  "HDeathsAverCopd","HDeathsAverHear","HDeathsAverStro","HDeathsShsAv",
  "ECostCancAver","ECostCopdAver","ECostHeartAver","ECostStroAver",
  "ECostCancOopAver","ECostCopdOopAver","ECostHeartOopAver","ECostStroOopAver",
  "ECostCancOohsAver","ECostCopdOohsAver","ECostHeartOohsAver",
  "ECostStroOohsAver","PAverPovCostOop","PAverCatasCostOop","TRevPreSpeSmo",
  "TRevPosSpeSmo","TProfPreSmo","TProfPosSmo","TEduYeAve","TEduYeMedi",
  "TEduYeAver","SPriPrRetaPre","SPriPrRetaPos","STaxVaSpePre","STaxVaSpePos")

# Function to fill . agregate 
flcol <- function(x) {
  if(any(!vnms %in% names(x))) x[,vnms[!vnms %in% names(x)]] <- NA
  return(x)
}
# Eval 95% confint
mci <- function(x) {
  x <- as.numeric(x)
    cires <- ci(x,na.rm=T)
    if(!is.na(cires[2])  & cires[2]<0)  cires[2] <- 0 
    cires <- paste0(round(cires[1],3)," [",round(cires[2],3),
                    " - ",round(cires[3],3),"]")
  return(cires)
}



######## Join results 
tres <- split(simfile,simfile)
for(i in simfile){ 
  Res <- read_rds(file =  paste0(sroot,i))
  Res <- lapply(Res,flcol)
  Res <- do.call(rbind,Res)
  Res[,grep("ECos",names(Res))] <-   Res[,grep("ECos",names(Res))]/10^6 
  idvars <- grep("Strata|Itera|value",names(Res))
  ixvars <- (1:ncol(Res))[
    -c(idvars,grep("HDeathPreShs|HDeathsShsAv",names(Res)))]
  Res <- Res[,c(idvars,ixvars)]
  suppressWarnings(   
    tabz <- Res %>% 
    select(-Iteration) %>%
    group_by(Strata,value) %>%
    summarise_all(list(mci)) %>%
      ungroup()
  )
  tabz$scenario <- gsub(".rds|RSimS.*_","",i)
  tres[[i]] <- tabz
}
tab1 <- do.call(rbind,tres)

####### Put labels and build res table 
RTab <- tribble(
  ~ System,~SDG_group,~SDG,~Units,~Before_tax,~Prefered_Increase,
  "Society" ,"Heatlh SDG3", "Tobacco tax","(USD per 20-stick pack)","STaxVaSpePre", "STaxVaSpePos",
  "Society" ,"Heatlh SDG3", "Price","(USD per 20-stick pack)","SPriPrRetaPre", "SPriPrRetaPos",
  "Society" ,"Heatlh SDG3", "Smokers ","(million)","CPreTaxSmok","CPosTaxSmok",
  "Society" ,"Heatlh SDG3", "Smoking Intensity","(mean cigarettes per day)","CPreInteMedian","CPosInteMedian",
  "Society" ,"Heatlh SDG3", "Cigarettes smoked","(million 20-Sticks packs)","CPreTaxCigSmo","CPosTaxCigSmo",
  "Society" ,"Heatlh SDG3","Years of life Gained from quitting Post-Tax","(million years of life)",NA,"HLiYearGainPosTaxQuit",
  "Society" ,"Heatlh SDG3","Deaths of smokers Pre-tax/Averted by the tax","(thousand people)","HDeathPreTax","HDeathsAver",
  "Society" ,"Heatlh SDG3","Deaths of smokers Averted: NCD: Heart Disease","(thousand people)",NA,"HDeathsAverHear",
  "Society" ,"Heatlh SDG3","Deaths of smokers Averted: NCD: Stroke","(thousand people)",NA,"HDeathsAverStro",
  "Society" ,"Heatlh SDG3","Deaths of smokers Averted: NCD: COPD","(thousand people)",NA,"HDeathsAverCopd",
  "Society" ,"Heatlh SDG3","Deaths of smokers Averted: NCD: Cancer","(thousand people)",NA,"HDeathsAverCanc",
  "Society" ,"Healthcare expenditure","Total Healthcare Costs Averted: Heart Disease","(Million USD$/year)",NA,"ECostHeartAver",
  "Society" ,"Healthcare expenditure","Total Healthcare Costs Averted: Stroke","(Million USD$/year)",NA,"ECostStroAver",
  "Society" ,"Healthcare expenditure","Total Healthcare Costs Averted: COPD","(Million USD$/year)",NA,"ECostCopdAver",
  "Society" ,"Healthcare expenditure","Total Healthcare Costs Averted: Cancer","(Million USD$/year)",NA,"ECostCancAver",
  "Society" ,"Healthcare expenditure","OOP Healthcare Costs Averted: Heart Disease","(Million USD$/year)",NA,"ECostHeartOopAver",
  "Society" ,"Healthcare expenditure","OOP Healthcare Costs Averted: Stroke","(Million USD$/year)",NA,"ECostStroOopAver",
  "Society" ,"Healthcare expenditure","OOP Healthcare Costs Averted: COPD","(Million USD$/year)",NA,"ECostCopdOopAver",
  "Society" ,"Healthcare expenditure","OOP Healthcare Costs Averted: Cancer","(Million USD$/year)",NA,"ECostCancOopAver",
  "Society" ,"Gender SDG5","Women Smokers","(Million)","CPreTaxSmokWom","CPosTaxSmokWom",
  "Society" ,"Gender SDG5","Men Smokers","(Million)","CPreTaxSmokMen","CPosTaxSmokMen",
  "Society" ,"Gender SDG5","Women Smoking Intensity","(mean n cig/smoker)","CPreInteMedianWom","CPosInteMedianWom",
  "Society" ,"Gender SDG5","Men Smoking Intensity","(mean n cig/smoker)","CPreInteMedianMen","CPosInteMedianMen",
  "Economy" ,"Domestic Ressources SDG17","Tax Revenues licit cigarettes smoked","(Million USD$/year)","TRevPreSpeSmo","TRevPosSpeSmo",
  "Economy" ,"Domestic Ressources SDG17","Industry Profits from cigarettes smoked","(million USD)","TProfPreSmo","TProfPosSmo",
  "Economy" ,"Education SDG4","Total Years of Education lost Averted","(million years)",NA,"TEduYeAve",
  "Economy" ,"Education SDG4","Average Years of Education lost Averted","(years)",NA,"TEduYeAver",
  "Economy" ,"Poverty","People Averted from Poverty caused by PPO Healthcare","(thousand people)",NA,"PAverPovCostOop",
  "Economy" ,"Poverty","People Averted from Catasthrophic Expenditure caused by OOP","(thousand people)",NA,"PAverCatasCostOop"
  )
RTab$Medium_Increase <- RTab$Low_Increase <-  RTab$Prefered_Increase
RTab <- as.data.frame(RTab)

#### Populate table :
fltrsc1 <- tab1$Strata=="Nacional" & tab1$scenario=="s5.4d" # High Increase
fltrsc2 <- tab1$Strata=="Nacional" & tab1$scenario=="s4.4d" # Medium increase
fltrsc3 <- tab1$Strata=="Nacional" & tab1$scenario=="s3.9d" # Low Increase

RTab$Before_tax[!is.na(RTab$Before_tax)] <- 
  as.character(tab1[fltrsc1,RTab$Before_tax[!is.na(RTab$Before_tax)]])
RTab$Prefered_Increase[!is.na(RTab$Prefered_Increase)] <-  
  as.character(tab1[fltrsc1,RTab$Prefered_Increase[!is.na(RTab$Prefered_Increase)]])
RTab$Medium_Increase[!is.na(RTab$Medium_Increase)] <-  
  as.character(tab1[fltrsc2,RTab$Medium_Increase[!is.na(RTab$Medium_Increase)]])
RTab$Low_Increase[!is.na(RTab$Low_Increase)] <-  
  as.character(tab1[fltrsc3,RTab$Low_Increase[!is.na(RTab$Low_Increase)]])
# ADD EXICSE  & PRICE INCREASE
RTab[which(RTab$SDG=="Tobacco tax"),5:8] <- 
  gsub("\\[.*\\]","",RTab[which(RTab$SDG=="Tobacco tax"),5:8])
RTab[which(RTab$SDG=="Price"),5:8] <- 
  gsub("\\[.*\\]","",RTab[which(RTab$SDG=="Price"),5:8])
RTab$Before_tax[is.na(RTab$Before_tax)] <- "-"

############### Export results 
writexl::write_xlsx(list(RTab=RTab,Indicadores_completos=tab0),
                    path = paste0(root,"ResultsTotalv4.4.xlsx"))

