#' Export food-web interactions to the models. Returns food-web settings.
#'
#' Export predation settings for zooplankton, fish, etc.
#' Food-sources related to biogeochemistry that are controlled by a single model
#' parameter, e.g lEatPOM in WET, are not handled by this function and should
#' instead be specified by the respective parameters. Coupling-related settings
#' are handled by set_coupling
#'
#'@param config_file character; name of LakeEnsemblR_WQ config file
#'@param folder path; location of config_file
#'
#'@examples
#'
#'@importFrom configr read.config write.config
#'@importFrom glmtools read_nml write_nml
#'
#'@export

# Reminder: for GOTM-Selmaprotbas and GOTM-WET, it is essential that the
# set_coupling function is also run (after incorporating user input) to
# let these food-web settings work! 

export_food_web_interactions <- function(config_file, folder){
  
  # Read config file as a list
  lst_config <- read.config(file.path(folder, config_file)) 
  
  models_coupled <- lst_config[["models"]]
  wq_models <- strsplit(models_coupled, "-")
  wq_models <- sapply(wq_models, function (x) tolower(x[length(x)]))
  
  # Allowed food-web structure:
  # - Zooplankton and zoobenthos can feed on phytoplankton
  # - Fish can feed on zooplankton, zoobenthos, and other fish groups
  
  # Load the food-web settings from the config file
  for(i in c("phytoplankton", "zooplankton", "zoobenthos", "fish")){
    if(lst_config[[i]][["use"]]){
      assign(paste0(i, "_groups"), names(lst_config[[i]][["groups"]]))
      assign(paste0(i, "_prey"), lapply(lst_config[[i]][["groups"]], "[[", "prey"))
    }else{
      assign(paste0(i, "_groups"), NULL)
      assign(paste0(i, "_prey"), NULL)
    }
  }
  
  # Make sure the input is correct - see helpers.R
  quality_check_food_web(phy_gr = phytoplankton_groups, zoop_gr = zooplankton_groups,
                         zoob_gr = zoobenthos_groups, fish_gr = fish_groups,
                         zoop_pr = zooplankton_prey, zoob_pr = zoobenthos_prey,
                         fish_pr = fish_prey)
  
  for(i in seq_len(length(models_coupled))){
    if(wq_models[i] == "selmaprotbas"){
      wq_config <- read.config(file.path(folder,
                                         lst_config[["config_files"]][[models_coupled[i]]]))

      # Only zooplankton: number of prey, prey preference, and
      # prey nutrient concentrations
      for(j in zooplankton_groups){
        wq_config[["instances"]][[j]][["parameters"]][["nprey"]] <-
          length(zooplankton_prey[[j]])
        
        wq_config[["instances"]][[j]][["parameters"]][["pref{prey_num}"]] <- NULL
        wq_config[["instances"]][[j]][["parameters"]][["prey_rfn{prey_num}"]] <- NULL
        wq_config[["instances"]][[j]][["parameters"]][["prey_rfr{prey_num}"]] <- NULL
        wq_config[["instances"]][[j]][["parameters"]][["prey_rfs{prey_num}"]] <- NULL

        # Default values
        default_pref <- LakeEnsemblR_WQ_dictionary[LakeEnsemblR_WQ_dictionary$module == "zooplankton" &
                                                    LakeEnsemblR_WQ_dictionary$model == "selmaprotbas" &
                                                    LakeEnsemblR_WQ_dictionary$parameter == "pref{prey_num}",
                                                   "default"]
        default_rfn <- LakeEnsemblR_WQ_dictionary[LakeEnsemblR_WQ_dictionary$module == "zooplankton" &
                                                   LakeEnsemblR_WQ_dictionary$model == "selmaprotbas" &
                                                   LakeEnsemblR_WQ_dictionary$parameter == "prey_rfn{prey_num}",
                                                  "default"]
        default_rfr <- LakeEnsemblR_WQ_dictionary[LakeEnsemblR_WQ_dictionary$module == "zooplankton" &
                                                   LakeEnsemblR_WQ_dictionary$model == "selmaprotbas" &
                                                   LakeEnsemblR_WQ_dictionary$parameter == "prey_rfr{prey_num}",
                                                  "default"]
        default_rfs <- LakeEnsemblR_WQ_dictionary[LakeEnsemblR_WQ_dictionary$module == "zooplankton" &
                                                   LakeEnsemblR_WQ_dictionary$model == "selmaprotbas" &
                                                   LakeEnsemblR_WQ_dictionary$parameter == "prey_rfs{prey_num}",
                                                  "default"]
        
        for(k in seq_len(length(zooplankton_prey[[j]]))){
          wq_config[["instances"]][[j]][["parameters"]][[paste0("pref", k)]] <- as.numeric(default_pref)
          wq_config[["instances"]][[j]][["parameters"]][[paste0("prey_rfn", k)]] <- as.numeric(default_rfn)
          wq_config[["instances"]][[j]][["parameters"]][[paste0("prey_rfr", k)]] <- as.numeric(default_rfr)
          wq_config[["instances"]][[j]][["parameters"]][[paste0("prey_rfs", k)]] <- as.numeric(default_rfs)
        }
      }
      write.config(wq_config,
             file.path(folder,
                       lst_config[["config_files"]][[models_coupled[i]]]),
             write.type = "yaml")
    }else if(wq_models[i] == "wet"){
      wq_config <- read.config(file.path(folder,
                                         lst_config[["config_files"]][[models_coupled[i]]]))
      
      # Zooplankton
      for(j in zooplankton_groups){
        wq_config[["instances"]][[j]][["parameters"]][["nPrey"]] <-
          length(zooplankton_prey[[j]])
        
        wq_config[["instances"]][[j]][["parameters"]][["cPref{prey_num}"]] <- NULL
        wq_config[["instances"]][[j]][["parameters"]][["prey_suffix{prey_num}"]] <- NULL
        wq_config[["instances"]][[j]][["parameters"]][["lSi{prey_num}"]] <- NULL
        
        # Default values
        default_pref <- LakeEnsemblR_WQ_dictionary[LakeEnsemblR_WQ_dictionary$module == "zooplankton" &
                                                     LakeEnsemblR_WQ_dictionary$model == "wet" &
                                                     LakeEnsemblR_WQ_dictionary$parameter == "cPref{prey_num}",
                                                   "default"]
        default_lsi <- LakeEnsemblR_WQ_dictionary[LakeEnsemblR_WQ_dictionary$module == "zooplankton" &
                                                    LakeEnsemblR_WQ_dictionary$model == "wet" &
                                                    LakeEnsemblR_WQ_dictionary$parameter == "lSi{prey_num}",
                                                  "default"]
        for(k in seq_len(length(zooplankton_prey[[j]]))){
          wq_config[["instances"]][[j]][["parameters"]][[paste0("cPref", k)]] <- as.numeric(default_pref)
          wq_config[["instances"]][[j]][["parameters"]][[paste0("prey_suffix", k)]] <- "W"
          wq_config[["instances"]][[j]][["parameters"]][[paste0("lSi", k)]] <- as.logical(default_lsi)
        }
      }
      
      # Zoobenthos
      for(j in zoobenthos_groups){
        wq_config[["instances"]][[j]][["parameters"]][["nPrey"]] <-
          length(zoobenthos_prey[[j]])
        
        wq_config[["instances"]][[j]][["parameters"]][["cPref{prey_num}"]] <- NULL
        wq_config[["instances"]][[j]][["parameters"]][["prey_suffix{prey_num}"]] <- NULL
        wq_config[["instances"]][[j]][["parameters"]][["lSi{prey_num}"]] <- NULL
        
        # Default values
        default_pref <- LakeEnsemblR_WQ_dictionary[LakeEnsemblR_WQ_dictionary$module == "zoobenthos" &
                                                     LakeEnsemblR_WQ_dictionary$model == "wet" &
                                                     LakeEnsemblR_WQ_dictionary$parameter == "cPref{prey_num}",
                                                   "default"]
        default_lsi <- LakeEnsemblR_WQ_dictionary[LakeEnsemblR_WQ_dictionary$module == "zoobenthos" &
                                                    LakeEnsemblR_WQ_dictionary$model == "wet" &
                                                    LakeEnsemblR_WQ_dictionary$parameter == "lSi{prey_num}",
                                                  "default"]
        for(k in seq_len(length(zoobenthos_prey[[j]]))){
          wq_config[["instances"]][[j]][["parameters"]][[paste0("cPref", k)]] <- as.numeric(default_pref)
          wq_config[["instances"]][[j]][["parameters"]][[paste0("prey_suffix", k)]] <- "S"
          wq_config[["instances"]][[j]][["parameters"]][[paste0("lSi", k)]] <- as.logical(default_lsi)
        }
      }
      
      # Fish
      ## Many additional settings, including time spent on different feeding strategies and 
      ## different parameter names depending on the prey module
      
      for(j in fish_groups){
        # Prey types
        prey_zoop <- fish_prey[[j]][grepl("zooplankton/", fish_prey[[j]])]
        prey_zoob <- fish_prey[[j]][grepl("zoobenthos/", fish_prey[[j]])]
        prey_fish <- fish_prey[[j]][grepl("fish/", fish_prey[[j]])]
        
        # Default fraction of time feeding -> equal fractions if any prey present
        small_number <- 1E-10
        total_feeding_techniques <- (length(prey_zoop) > 0L) + (length(prey_zoob) > 0L) +
                                    (length(prey_fish) > 0L) + small_number
        
        frac_zooplanktivory <- round(((length(prey_zoop) > 0L) + small_number) / total_feeding_techniques, 5L)
        frac_benthivory <- round(((length(prey_zoob) > 0L) + small_number) / total_feeding_techniques, 5L)
        frac_piscivory <- round(((length(prey_fish) > 0L) + small_number) / total_feeding_techniques, 5L)
        
        wq_config[["instances"]][[j]][["parameters"]][["nZOO"]] <- length(prey_zoop)
        wq_config[["instances"]][[j]][["parameters"]][["fFishZoo"]] <- frac_zooplanktivory
        wq_config[["instances"]][[j]][["parameters"]][["nBEN"]] <- length(prey_zoob)
        wq_config[["instances"]][[j]][["parameters"]][["fFishBen"]] <- frac_benthivory
        wq_config[["instances"]][[j]][["parameters"]][["nPISC"]] <- length(prey_fish)
        wq_config[["instances"]][[j]][["parameters"]][["fFishPisc"]] <- frac_piscivory
        
        wq_config[["instances"]][[j]][["parameters"]][["ZOOpref{prey_num}"]] <- NULL
        wq_config[["instances"]][[j]][["parameters"]][["BENpref{prey_num}"]] <- NULL
        wq_config[["instances"]][[j]][["parameters"]][["PISCpref{prey_num}"]] <- NULL
        
        # Default values
        default_zoopref <- LakeEnsemblR_WQ_dictionary[LakeEnsemblR_WQ_dictionary$module == "fish" &
                                                        LakeEnsemblR_WQ_dictionary$model == "wet" &
                                                        LakeEnsemblR_WQ_dictionary$parameter == "ZOOpref{prey_num}",
                                                      "default"]
        default_benpref <- LakeEnsemblR_WQ_dictionary[LakeEnsemblR_WQ_dictionary$module == "fish" &
                                                        LakeEnsemblR_WQ_dictionary$model == "wet" &
                                                        LakeEnsemblR_WQ_dictionary$parameter == "BENpref{prey_num}",
                                                      "default"]
        default_piscpref <- LakeEnsemblR_WQ_dictionary[LakeEnsemblR_WQ_dictionary$module == "fish" &
                                                         LakeEnsemblR_WQ_dictionary$model == "wet" &
                                                         LakeEnsemblR_WQ_dictionary$parameter == "PISCpref{prey_num}",
                                                       "default"]
        
        for(k in seq_len(length(fish_prey[[j]]))){
          wq_config[["instances"]][[j]][["parameters"]][[paste0("ZOOpref", k)]] <- as.numeric(default_zoopref)
          wq_config[["instances"]][[j]][["parameters"]][[paste0("BENpref", k)]] <- as.numeric(default_benpref)
          wq_config[["instances"]][[j]][["parameters"]][[paste0("PISCpref", k)]] <- as.numeric(default_piscpref)
        }
      }
      
      write.config(wq_config,
                   file.path(folder,
                             lst_config[["config_files"]][[models_coupled[i]]]),
                   write.type = "yaml")
      
    }else if(wq_models[i] == "aed2"){
      
      # Zooplankton only
      if(!lst_config[["zooplankton"]][["use"]]) next
      
      loc <- file.path(folder,
                       lst_config[["config_files"]][[models_coupled[i]]])
      zoo_config <- read_nml(file.path(paste0(sub("\\.nml.*", "", loc), "_zoop_pars.nml")))
      
      # Until this default value has not yet been set:
      default_pref <- 1.0
      # default_pref <- LakeEnsemblR_WQ_dictionary[LakeEnsemblR_WQ_dictionary$module == "zooplankton" &
      #                                              LakeEnsemblR_WQ_dictionary$model == "aed2" &
      #                                              LakeEnsemblR_WQ_dictionary$parameter == "Pzoo_prey",
      #                                            "default"]
      
      unique_prey <- unique(unlist(zooplankton_prey))
      aed2_prey <- gsub("phytoplankton/", "PHY_", unique_prey)
      
      zoo_config[["zoop_params"]][["zoop_params%num_prey"]] <- rep(length(unique_prey), length(zooplankton_groups))
      
      for(j in seq_len(length(unique_prey))){
        zoo_config[["zoop_params"]][[paste0("zoop_params%prey(", j, ")%zoop_prey")]] <- rep(aed2_prey[j], length(zooplankton_groups))
        zoo_config[["zoop_params"]][[paste0("zoop_params%prey(", j, ")%Pzoo_prey")]] <- rep(default_pref, length(zooplankton_groups))
        
        # If a zooplankton group doesn't predate on a prey, set pref to 0
        for(k in seq_len(length(zooplankton_groups))){
          if(!(unique_prey[j] %in% zooplankton_prey[[k]])){
            zoo_config[["zoop_params"]][[paste0("zoop_params%prey(", j, ")%Pzoo_prey")]][k] <- 0
          }
        }
      }
      
      write_nml(zoo_config, file.path(file.path(paste0(sub("\\.nml.*", "", loc), "_zoop_pars.nml"))))
    }else if(wq_models[i] == "mylake" | wq_models[i] == "pclake"){
      # Nothing needs to be done, because MyLake does not support higher trophic levels than phytoplankton,
      # and PCLake always has a single zooplankton group that always predates on three phytoplankton groups -
      # if there are less groups, the growth rates of the remaining groups should be set to 0. 
    }
    
  }
  
  return(list(phytoplankton_groups = phytoplankton_groups,
              zooplankton_groups = zooplankton_groups,
              zoobenthos_groups = zoobenthos_groups,
              fish_groups = fish_groups,
              zooplankton_prey = zooplankton_prey,
              zoobenthos_prey = zoobenthos_prey,
              fish_prey = fish_prey))
}
