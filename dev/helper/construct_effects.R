# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# construct feature effects from learner
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

construct_effects <- function(learner_nonski, learner_ski, data_reference, data_ski, feature, method = "pdp+ice") {
  predictor_nonski <- Predictor$new(learner_nonski, data = data_reference, y = "psi_intervall")
  predictor_ski <- Predictor$new(learner_ski, data = data_ski, y = "psi_intervall")
  effect_reference <- FeatureEffect$new(predictor_nonski,
    feature = feature,
    method = method
  )
  effect_ski <- FeatureEffect$new(predictor_ski,
    feature = feature,
    method = method
  )
  list(reference = effect_reference$results, ski = effect_ski$results)
}
