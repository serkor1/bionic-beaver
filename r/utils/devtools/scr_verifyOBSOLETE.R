# # script: scr_verify
# # objective: create a framework for verifying data integrity
# # based on the model functions
# # date: 2022-09-18
# # author: Serkan Korkmaz
# 
# .model_data <- function(
#     model = 1
# ) {
#   
#   
#   # 1) Load Data
#   data_list <- preload_data(
#     developper_mode = FALSE
#   )
#   
#   # 2) Load parameters
#   load_parameters <- .gen_option(
#     data_list = data_list
#   )
#   
#   # 3) Generate Lookup-table
#   lookup <- .gen_lookup(data_list)
#   
#   
#   
#   
#   
#   
#   
#   return(
#     list(
#       data_list  = data_list[[model]],
#       parameters = flatten(map(load_parameters, function(x){x[model]})),
#       lookup     = lookup[[model]]
#     )
#   )
#   
#   
# }
# 
# 
# 
# 
# .model1_checksum <- function(
#     input,
#     do_aggregate = FALSE
# ) {
#   
#   
#   # Generate options; ####
#   assignment_grid <- data.table(
#     CJ(
#       control = unlist(
#         input$parameters$assignment
#       ),
#       intervention = unlist(
#         input$parameters$assignment
#       )
#     )
#   )[
#     control != intervention
#   ]
#   
#   if (do_aggregate) {
#     
#     do_chars <- sample(
#       unlist(input$parameters$chars),
#       replace = TRUE,
#       size = 4
#     )
#     
#   } else {
#     
#     do_chars <- NULL
#     
#   }
#   
#   
#   data_list <- map(
#     1:nrow(assignment_grid),
#     function(i) {
#       
#       # extract assignment
#       control <- assignment_grid$control[i]
#       intervention <- assignment_grid$intervention[i]
#       
#       
#       data <- spread(
#         grind(
#           input$data_list,
#           intervention = intervention,
#           control      = control,
#           allocators   = unlist(input$parameters$outcome),
#           chars        = do_chars
#         )
#       )
#       
#       
#       data <- list(
#         data = map(
#         data,
#         function(element) {
#           
#           element[
#             ,
#             .(
#               x,
#               allocator,
#               control,
#               intervention
#             )
#             ,
#           ]
#           
#         }
#       ),
#       control = control,
#       intervention = intervention
#       )
#       
#     }
#   )
#   
#   
#   
#   # data_list <- rbindlist(map(
#   #   data_list,
#   #   function(x) {
#   # 
#   #     data.table(
#   #       intervention = x$intervention,
#   #       control      = x$control,
#   #       checksum = digest::digest(
#   #         x$data,
#   #         algo = 'md5'
#   #       )
#   #     )
#   # 
#   # 
#   #   }
#   # ))
#   
#   
#   return(
#     data_list
#   )
#   
# }
# 
# .input_data <- function(
#     model = 1
# ) {
#   
#   
#   .load_data(
#     fifelse(
#       model == 1, 
#       'input/data/model1/',
#       'input/data/model2/'
#     )
#   )
#   
#   
# }
# 
# 
# 
# # 
# # 
# # test_data <- .load_data(
# #   'input/data/model1/'
# # )
# # 
# # 
# # test_data <- (map(
# #   test_data,
# #   function(data){
# #     
# #     
# #     setnames(
# #       data,
# #       old = 'year',
# #       new = 'x'
# #     )
# #     data <- data[
# #       type == 0 & assignment %chin% c('Cancer_Prostata', 'Diabetes II_Med Komplikationer')
# #       ,
# #       .(
# #         outcome = mean(qty, na.rm = TRUE)
# #       )
# #       ,
# #       by = .(
# #         x,
# #         allocator,
# #         assignment
# #       )
# #       
# #     ]
# #     
# #     
# #     data[
# #       ,
# #       assignment := fcase(
# #         assignment %chin% 'Cancer_Prostata', 'intervention',
# #         assignment %chin% 'Diabetes II_Med Komplikationer', 'control'
# #       )
# #       ,
# #     ]
# #     
# #     
# #     data <- dcast(
# #       data,
# #       formula = x + allocator ~ assignment,
# #       value.var = 'outcome'
# #     )
# #     
# #     # data.table(
# #     #   control = 'Diabetes II_Med Komplikationer',
# #     #   intervention = 'Cancer_Prostata',
# #     #   checksum = digest::digest(
# #     #     data,
# #     #     algo = 'md5'
# #     #   )
# #     # )
# #     
# #     
# #     
# #   }
# # ))
# 
# 
# 
# 
# 
# 
# verify_data <- function(model = 1) {
#   
#   
#   model_data <- .model_data(model = model)
#   input_data <- .input_data(model = model)
#   
#   
#   # parameters
#   assignment_dt <- data.table(
#     CJ(
#       intervention = unlist(model_data$parameters$assignment),
#       control   = unlist(model_data$parameters$assignment)
#     )
#   )[
#     !(
#       intervention == control
#     )
#   ]
#   
#   chars <- sample(
#     unlist(model_data$parameters$chars),
#     replace = TRUE,
#     size = 4
#     )
#   
  # get_col = str_split(get_var, '_',simplify = TRUE)[,1]
  # get_val = str_split(get_var, '_', simplify = TRUE)[,2]
#   
#   
#   
  # data_checksums <- rbindlist(
  #   map(
  #     input_data,
  #     function(data) {
  # 
  #       data <- data[
  #         data[
  # 
  #           ,
  #           Reduce(
  #             `&`,
  #             lapply(
  #               .SD,
  #               `%chin%`,
  #               get_val
  #             )
  #           ),
  #           .SDcols = get_col
  #         ]
  #       ]
  # 
  #       setnames(
  #         data,
  #         old = 'year',
  #         new = 'x'
  #       )
  # 
  # 
  # 
  #       data <- data[
  #         type == 0 & assignment %chin% c(assignment_dt$intervention[1], assignment_dt$control[2])
  #         ,
  #         .(
  #           outcome = mean(qty, na.rm = TRUE)
  #         )
  #         ,
  #         by = .(
  #           x,
  #           allocator,
  #           assignment
  #         )
  # 
  #       ]
  # 
  # 
  #       data[
  #         ,
  #         assignment := fcase(
  #           assignment %chin% assignment_dt$intervention[1], 'intervention',
  #           assignment %chin% assignment_dt$control[2], 'control'
  #         )
  #         ,
  #       ]
  # 
  # 
  #       data <- dcast(
  #         data,
  #         formula = x + allocator ~ assignment,
  #         value.var = 'outcome'
  #       )
  # 
  #       data.table(
  #         control = assignment_dt$control[2],
  #         intervention = assignment_dt$intervention[1],
  #         checksum = digest::digest(
  #           data,
  #           algo = 'md5'
  #         )
  #       )
#         
#        
#         
#         
#         
#       }
#     )
#   )
#     
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   message(
#     paste(
#       'Generating cheksums:', paste(
#         chars, collapse = ','
#       )
#     )
#       )
#   
#   
#   model_checksums <- rbindlist(map(
#     1:nrow(assignment_dt),
#     function(i) {
# 
# 
#       suppressMessages(
#         model_data <- tryCatch(spread(
#           grind(
#             model_data$data_list,
#             control = assignment_dt$control[i],
#             intervention = assignment_dt$intervention[i],
#             chars = chars,
#             allocators = unlist(model_data$parameters$outcome)
#           )
#       ),
#       warning = function(condition) {
# 
#         data.table(
# 
#         )
# 
#       },
#       error = function(condition) {
# 
#         data.table(
# 
#         )
# 
#       }
#       )
# 
#       )
# 
#       data.table(
#         control      = assignment_dt$control[i],
#         intervention = assignment_dt$intervention[i],
# 
#         checksum = digest::digest(
#           model_data,
#           algo = 'md5'
#         )
#       )
# 
# 
# 
#     }
#   ))
#   
#   
#   
#   
# 
#   
#   
#   return(
#     model_checksums[data_checksums, on = .(control, intervention)]
#   )
#   
#   
# }
# 
# 
# 
