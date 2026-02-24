source("constants.R")
source("helperFunctions.R")

# To do: named list of all 500 runs, transpose rows sand columns.

raw_data <- read.csv("Datasets/dfp_nyc_pre_election_2021_final_v2_e5_0020040_micro.csv")

posterior <- preprocess_dfp(raw_data, weighted = 1)

# Probs is a 500 x 807 matrix. Each row corresponds to one sample of 
# probabilities from the Dirichlet posterior given the 807 unique rankings
probs <- sample_probs(probs_size = probs_size, posterior = posterior)

rcvOutputs <- run_rcv(sample_probs = probs, candidates = candidates)

# Sort 
sorted_weighted_rankings <- culled_weighted_sum_rankings %>%
  arrange(desc(total))

# Ignore below, just all potentially helpful columns of poll.
#"rank_preference_1"                                                                                               
#"rank_preference_2"                                                                                               
#"rank_preference_3"                                                                                               
#"rank_preference_4"                                                                                               
#"rank_preference_5"                                                                                               
#"rank_preference_sure_1"                                                                                          
#"rank_preference_sure_2"                                                                                          
#"rank_preference_sure_3"                                                                                          
#"rank_preference_sure_4"                                                                                          
#"rank_preference_sure_5"                                                                                          
#"rcv_result_after_0"                                                                                              
#"rcv_result_after_1"                                                                                              
#"rcv_result_after_2"                                                                                              
#"rcv_result_after_3"                                                                                              
#"rcv_result_after_4"                                                                                              
#"rcv_result_after_5"                                                                                              
#"rcv_result_after_6"                                                                                              
#"rcv_result_after_7"                                                                                              
#"rcv_result_after_8"                                                                                              
#"rcv_result_after_8_alternative" 
#"dfp_nyc_2021_mayor_ballot"                                                                                       
#"dfp_nyc_2021_mayor_ballot_merged"                                                                                
#"dfp_nyc_2021_mayor_ballot_push"                                                                                  
#"dfp_nyc_2021_mayor_ballot_second_choice"                                                                         
#"dfp_nyc_2021_mayor_ballot_second_choice_voted"                                                                   
#"dfp_nyc_2021_mayor_ballot_third_choice"                                                                          
#"dfp_nyc_2021_mayor_ballot_third_choice_voted"                                                                    
#"dfp_nyc_2021_mayor_ballot_voted"