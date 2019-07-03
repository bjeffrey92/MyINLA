mesh_and_spde <- build_mesh_and_spde(Data, 'capital_long', 'capital_lat', 'Time')

covariate_cols <- c("ConsumptionValue", "GDP")

stack_and_formula <- build_stack_and_formula(Data, 'capital_long', 'capital_lat', 
                                            'Time', 'NumValue', 1, covariate_cols, 
                                            mesh_and_spde[['mesh2d']], 
                                            mesh_and_spde[['mesh1d']])

result <- run_model(stack_and_formula[['stack']], 
                    stack_and_formula[['formula_string']], 
                    mesh_spde[['spde']])

# stack <- stack_and_formula[['stack']]
# spde <- mesh_spde[['spde']]
# likelihood = 'gaussian'
# formula <- '(response ~ -1 + Intercept + ConsumptionValue + GDP +
#             f(field, model=spde, group=field.group, 
#             control.group=list(model="ar", order = 1)))'
# result <-
#         INLA::inla(formula,   
#                 data=INLA::inla.stack.data(stack, spde=spde),
#                 family=likelihood,
#                 control.predictor=list(A=INLA::inla.stack.A(stack), compute=TRUE),
#                 control.compute=list(cpo=FALSE),
#                 control.inla = list(reordering = "metis"),
#                 keep=FALSE, verbose=TRUE)

# temp1 <- (response ~ -1 + Intercept + ConsumptionValue + GDP + INLA::f(field, 
#         model = spde, group = field.group, control.group = list(model = "ar", 
#             order = 1)))


# temp <- (resistance ~ -1 + Intercept + ConsumptionValue + GDP +
#             f(field, model=spde, group=field.group, 
#             control.group=list(model="ar", order = 1)))
