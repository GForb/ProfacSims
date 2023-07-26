# Simualtion tasks can happen:
#
# Once per simulation
#   once per scenario
#     once per rep
#       once per data set
#         once per model
#           once per per evaluation
#
# simulation_set_up_tasks
# repeat_scenarios(scenario_args)
#   scenario_set_up_tasks
#   do_reps()
#     rep_set_up
#     do_modelling()
#       model_set_up
#       do_evaluation
#         evaluation_set_up
#         evaluate(mdoel)

same test data for all models needs to go in rep set update
evaluate needs to be able to take a list of test_data to test on different datasets
