CallSites_N = function(df){
  pol15_1 <-  subset(df, dataset_ID == "20")
  pol15_2 <-  subset(df, dataset_ID == "317")
  pol15_3 <-  subset(df,dataset_ID == "720")
  pol15_4 <-  subset(df,dataset_ID == "4257")
  pol15_5 <-  subset(df,dataset_ID == "4286")
  pol15_6 <-  subset(df,dataset_ID == "4372")
  pol15_7 <-  subset(df,dataset_ID == "4468")
  pol15_8 <-  subset(df,dataset_ID == "20034")
  pol15_9 <-  subset(df,dataset_ID == "20279")
  pol15_10 <-  subset(df,dataset_ID == "20285")
  pol15_11 <-  subset(df,dataset_ID == "20293")
  pol15_12 <-  subset(df,dataset_ID == "44941")
  pol15_13 <-  subset(df,dataset_ID == "45311")
  pol15_14 <-  subset(df,dataset_ID == "45636")
  pol15_15 <-  subset(df,dataset_ID == "45639")
  pol15_16 <-  subset(df,dataset_ID == "45642")
  pol15_17 <-  subset(df,dataset_ID == "24757")
  pol15_18 <-  subset(df,dataset_ID == "4169")
  pol15_all <- rbind(pol15_1,
                     pol15_2,
                     pol15_3,
                     pol15_4,
                     pol15_5,
                     pol15_6,
                     pol15_7,
                     pol15_8,
                     pol15_9,
                     pol15_10,
                     pol15_11,
                     pol15_12,
                     pol15_13,
                     pol15_14,
                     pol15_15,
                     pol15_16,
                     pol15_17,
                     pol15_18)
}

#CallSites_SW
CallSites_SW = function(df){
  pol1_1 <-  subset(df, dataset_ID == "20050")
  pol1_2 <-  subset(df, dataset_ID == "45704")
  pol1_3 <-  subset(df,dataset_ID == "45707")
  pol1_4 <-  subset(df,dataset_ID == "45710")
  pol1_5 <-  subset(df,dataset_ID == "45713")
  pol1_6 <-  subset(df,dataset_ID == "45716")
  pol1_7 <-  subset(df,dataset_ID == "45719")
  pol1_8 <-  subset(df,dataset_ID == "45722")
  pol1_9 <-  subset(df,dataset_ID == "45725")
  pol1_10 <-  subset(df,dataset_ID == "45728")
  pol1_11 <-  subset(df,dataset_ID == "45731")
  pol1_all <- rbind(pol1_1,
                    pol1_2,
                    pol1_3,
                    pol1_4,
                    pol1_5,
                    pol1_6,
                    pol1_7,
                    pol1_8,
                    pol1_9,
                    pol1_10,
                    pol1_11)
}

#CallSites_MW
CallSites_MW = function(df){
  pol2_1 <-  subset(df, dataset_ID == "977")
  pol2_2 <-  subset(df, dataset_ID == "20042")
  pol2_3 <-  subset(df,dataset_ID == "20046")
  pol2_4 <-  subset(df,dataset_ID == "45331")
  pol2_5 <-  subset(df,dataset_ID == "45345")
  pol2_6 <-  subset(df,dataset_ID == "45347")
  pol2_7 <-  subset(df,dataset_ID == "45349")
  pol2_all <- rbind(pol2_1,
                    pol2_2,
                    pol2_3,
                    pol2_4,
                    pol2_5,
                    pol2_6,
                    pol2_7)
}

#CallSites_MM
CallSites_MM = function(df){
  pol47_1 <-  subset(df, dataset_ID == "19906")
  pol47_2 <-  subset(df, dataset_ID == "19909")
  pol47_3 <-  subset(df,dataset_ID == "19913")
  pol47_4 <-  subset(df,dataset_ID == "20018")
  pol47_5 <-  subset(df, dataset_ID == "21790")
  pol47_6 <-  subset(df, dataset_ID == "45351")
  pol47_7 <-  subset(df,dataset_ID == "45698")
  pol47_8 <-  subset(df,dataset_ID == "45701")
  pol47_all <- rbind(pol47_1,
                     pol47_2,
                     pol47_3,
                     pol47_4,
                     pol47_5,
                     pol47_6,
                     pol47_7,
                     pol47_8)
}

#CallSites_M
CallSites_M = function(df){
  pol47_1 <-  subset(df, dataset_ID == "19906")
  pol47_2 <-  subset(df, dataset_ID == "19909")
  pol47_3 <-  subset(df,dataset_ID == "19913")
  pol47_4 <-  subset(df,dataset_ID == "20018")
  pol47_5 <-  subset(df, dataset_ID == "21790")
  pol47_6 <-  subset(df, dataset_ID == "45351")
  pol47_7 <-  subset(df,dataset_ID == "45698")
  pol47_8 <-  subset(df,dataset_ID == "45701")
  pol2_1 <-  subset(df, dataset_ID == "977")
  pol2_2 <-  subset(df, dataset_ID == "20042")
  pol2_3 <-  subset(df,dataset_ID == "20046")
  pol2_4 <-  subset(df,dataset_ID == "45331")
  pol2_5 <-  subset(df,dataset_ID == "45345")
  pol2_6 <-  subset(df,dataset_ID == "45347")
  pol2_7 <-  subset(df,dataset_ID == "45349")
  pol47_all <- rbind(pol47_1,
                     pol47_2,
                     pol47_3,
                     pol47_4,
                     pol47_5,
                     pol47_6,
                     pol47_7,
                     pol47_8,
                     pol2_1,
                     pol2_2,
                     pol2_3,
                     pol2_4,
                     pol2_5,
                     pol2_6,
                     pol2_7)
}

#CallSites_SE
CallSites_SE = function(df){
  pol911_1 <-  subset(df, dataset_ID == "4092")
  pol911_2 <-  subset(df, dataset_ID == "4133")
  pol911_3 <-  subset(df,dataset_ID == "4156")
  pol911_4 <-  subset(df,dataset_ID == "4168")
  pol911_5 <-  subset(df, dataset_ID == "4259")
  pol911_6 <-  subset(df, dataset_ID == "4393")
  pol911_7 <-  subset(df,dataset_ID == "4420")
  pol911_8 <-  subset(df,dataset_ID == "4472")
  pol911_9 <-  subset(df, dataset_ID == "4539")
  pol911_10 <-  subset(df, dataset_ID == "4543")
  pol911_11 <-  subset(df,dataset_ID == "4017")
  pol911_12 <-  subset(df,dataset_ID == "3928")
  pol911_all <- rbind(pol911_1,
                      pol911_2,
                      pol911_3,
                      pol911_4,
                      pol911_5,
                      pol911_6,
                      pol911_7,
                      pol911_8,
                      pol911_9,
                      pol911_10,
                      pol911_11,
                      pol911_12)
}

#CallSites_SM
CallSites_SM = function(df){
  pol36_1 <-  subset(df, dataset_ID == "12")
  pol36_2 <-  subset(df, dataset_ID == "1438")
  pol36_3 <-  subset(df,dataset_ID == "4403")
  pol36_4 <-  subset(df,dataset_ID == "45329")
  pol36_all <- rbind(pol36_1,
                     pol36_2,
                     pol36_3,
                     pol36_4)
}

#CallSites_S
CallSites_S = function(df){
  pol911_1 <-  subset(df, dataset_ID == "4092")
  pol911_2 <-  subset(df, dataset_ID == "4133")
  pol911_3 <-  subset(df,dataset_ID == "4156")
  pol911_4 <-  subset(df,dataset_ID == "4168")
  pol911_5 <-  subset(df, dataset_ID == "4259")
  pol911_6 <-  subset(df, dataset_ID == "4393")
  pol911_7 <-  subset(df,dataset_ID == "4420")
  pol911_8 <-  subset(df,dataset_ID == "4472")
  pol911_9 <-  subset(df, dataset_ID == "4539")
  pol911_10 <-  subset(df, dataset_ID == "4543")
  pol911_11 <-  subset(df,dataset_ID == "4017")
  pol911_12 <-  subset(df,dataset_ID == "3928")
  pol36_1 <-  subset(df, dataset_ID == "12")
  pol36_2 <-  subset(df, dataset_ID == "1438")
  pol36_3 <-  subset(df,dataset_ID == "4403")
  pol36_4 <-  subset(df,dataset_ID == "45329")
  pol911_all <- rbind(pol911_1,
                      pol911_2,
                      pol911_3,
                      pol911_4,
                      pol911_5,
                      pol911_6,
                      pol911_7,
                      pol911_8,
                      pol911_9,
                      pol911_10,
                      pol911_11,
                      pol911_12,
                      pol36_1,
                      pol36_2,
                      pol36_3,
                      pol36_4)
}
