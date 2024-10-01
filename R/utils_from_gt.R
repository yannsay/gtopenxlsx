glue_gt_gtopenxlsx <- utils::getFromNamespace("glue_gt", "gt")
resolve_secondary_pattern_gtopenxlsx <- utils::getFromNamespace("resolve_secondary_pattern", "gt")

# unused function to make sure gt does not make an error in
# Namespace in Imports field not imported from: 'gt'
# All declared Imports should be used.

not_used <- function() {
  gt::gt()
}
