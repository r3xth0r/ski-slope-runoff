# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# construct feature effects from learner
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

construct_effects <- function(learner_nonski, learner_ski, feature) {
  predictor_nonski <- Predictor$new(learner_nonski, data = dat_nonski, y = "psi_intervall")
  predictor_ski <- Predictor$new(learner_ski, data = dat_ski, y = "psi_intervall")
  effect_nonski <- FeatureEffect$new(predictor_nonski,
    feature = feature,
    method = "pdp+ice"
  )
  effect_ski <- FeatureEffect$new(predictor_ski,
    feature = feature,
    method = "pdp+ice"
  )
  list(nonski = effect_nonski$results, ski = effect_ski$results)
}
