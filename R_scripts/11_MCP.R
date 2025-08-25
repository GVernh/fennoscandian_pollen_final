#- LIBRARIES ----
libs <- c("ggplot2","ecp", "mcp", "dplyr", "ggpubr", "purrr")

installed_libs <- libs %in% rownames(
  installed.packages())

if (any(installed_libs == F)) {
  install.packages(
    libs[!installed_libs]
  )
}

invisible(lapply(
  libs,
  library,
  character.only = T
))
rm(list = setdiff(ls(), "relative_abun"))

#disable scientific notation
options(scipen=999)

dir.create(file.path("./Results/", "multiple_change_point"), showWarnings = FALSE)

data_names = list.files(path= "./Processed_data/Full_datasets/",
              pattern = "_abun.csv",
               full.names = T)

data_id = list.files(path= "./Processed_data/Full_datasets/",
                        pattern = "_abun.csv")

# Set Bayes factor threshold for mcp acceptance
BF_thresh <- 10

# Prepare models

model5 = list(LCC~1+age, ~1+age, ~1+age, ~1+age, ~1+age, ~1+age)
model4 = list(LCC~1+age, ~1+age, ~1+age, ~1+age, ~1+age)
model3 = list(LCC~1+age, ~1+age, ~1+age, ~1+age)
model2 = list(LCC~1+age, ~1+age, ~1+age)
model1 = list(LCC~1+age, ~1+age)

# Run Models

for (y in 1:length(data_id)) {
  all_data<- read.csv(paste0(data_names[y]))
  identifier <- data_id[y]
  cp_df <- data.frame(matrix(ncol = 9, nrow = 0))
  colnames(cp_df) <- c("name", "mean", "lower", "upper", "Rhat", "n.eff", "BF", "ID", "CP_N")
  
    for (i in 9:length(all_data)) {
      rep = 0
      tag <- names(all_data[i])
      
      data = all_data %>%
        select(yearsBP, tag)
      colnames(data) <- c("age","LCC")
      
      repeat{ print(tag)
        fit_mcp = mcp(model5, data = data, par_x = "age", adapt = 200000, sample = "both", iter = 200000, cores = 3)
        
        
        tab = summary(fit_mcp)
        rhat_check <- max(tab$Rhat)
        rep = rep +1
        print(rep)
        if(rhat_check < 1.1) {break
        }
        else if (rep ==10) {break
        }
      }
      if (rhat_check < 1.1) { hyp_test = hypothesis(fit_mcp, c(paste0("cp_1=",tab[7,2]),
                                                               paste0("cp_2=",tab[8,2]),
                                                               paste0("cp_3=",tab[9,2]),
                                                               paste0("cp_4=",tab[10,2]),
                                                               paste0("cp_5=",tab[11,2])))
      
      cp_tab <- subset(tab, name %in% c("cp_1", "cp_2", "cp_3", "cp_4", "cp_5"))
      CP_N <-5
      }
      
      else if (rhat_check >=1.1) { 
        rep = 0
        repeat{ print(tag)
        fit_mcp = mcp(model4, data = data, par_x = "age", adapt = 200000, sample = "both", iter = 200000, cores = 3)
        
        
        tab = summary(fit_mcp)
        rhat_check <- max(tab$Rhat)
        rep = rep +1
        print(rep)
        if(rhat_check < 1.1) {break
        }
        else if (rep ==10) {break
        }
      }
      if (rhat_check < 1.1) { hyp_test = hypothesis(fit_mcp, c(paste0("cp_1=",tab[6,2]),
                                                               paste0("cp_2=",tab[7,2]),
                                                               paste0("cp_3=",tab[8,2]),
                                                               paste0("cp_4=",tab[9,2])))
      
      cp_tab <- subset(tab, name %in% c("cp_1", "cp_2", "cp_3", "cp_4"))
      CP_N <-4
      }
      
      else if (rhat_check >=1.1) { 
        rep = 0
        repeat { print(tag)
        fit_mcp = mcp(model3, data = data, par_x = "age", adapt = 200000, sample = "both", iter = 200000, cores = 3)
        
        
        tab = summary(fit_mcp)
        rhat_check <- max(tab$Rhat)
        rep = rep +1
        print(rep)
        if(rhat_check < 1.1) {break
        }
        else if (rep ==10) {break
        }
      }
      if (rhat_check < 1.1) { hyp_test = hypothesis(fit_mcp, c(paste0("cp_1=",tab[5,2]),
                                                               paste0("cp_2=",tab[6,2]),
                                                               paste0("cp_3=",tab[7,2])))
                              
      cp_tab <- subset(tab, name %in% c("cp_1", "cp_2", "cp_3"))
      CP_N <-3
      }
        
        else if (rhat_check >= 1.1) {
          rep = 0
          repeat{ print(tag)
            fit_mcp = mcp(model2, data = data, par_x = "age", adapt = 200000, sample = "both", iter = 200000, cores = 3)
            
            
            tab = summary(fit_mcp)
            rhat_check <- max(tab$Rhat)
            rep = rep +1
            print(rep)
            if(rhat_check < 1.1) {break
            }
            else if (rep ==10) {break
            }
          }
          if (rhat_check < 1.1) { hyp_test = hypothesis(fit_mcp, c(paste0("cp_1=",tab[4,2]),
                                                                   paste0("cp_2=",tab[6,2])))
          
          cp_tab <- subset(tab, name %in% c("cp_1", "cp_2"))
          CP_N <-2
          }
        }
      }
    }
      
      if (rhat_check >= 1.1) {
        rep = 0
        repeat{ print(tag)
          fit_mcp = mcp(model1, data = data, par_x = "age", adapt = 200000, sample = "both", iter = 200000, cores = 3)
          
          
          tab = summary(fit_mcp)
          rhat_check <- max(tab$Rhat)
          rep = rep +1
          print(rep)
          if(rhat_check < 1.1) {break
          }
          else if (rep ==10) {break
          }
        }
      hyp_test = hypothesis(fit_mcp, paste0("cp_1=",tab[3,2]))
        
      cp_tab <- subset(tab, name %in% "cp_1")
      CP_N <-1
      }
      
      cp_tab$BF <- hyp_test$BF
      cp_tab$ID <- tag
      cp_tab$CP_N <- CP_N
      cp_df <- rbind(cp_df, cp_tab)
    }
  write.csv(cp_df, paste0("./Results/multiple_change_point/", identifier))
}
