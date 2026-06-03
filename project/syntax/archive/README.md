# Archive — Deprecated analyses

This folder contains code that was explored during the analysis phase
but is NOT part of the final thesis pipeline. It is retained for
methodological transparency and for potential reference in viva
or revisions.

## Contents

- Hofstede_explorations.R: Multilevel models including UAI alone (B2),
  the five additional Hofstede dimensions (PDI, IDV, MAS, LTO, IVR),
  the joint multidimensional model (B2_multi), and cross-level
  interactions (B3). Plus VIF diagnostics and Figure 5 (UAI scatter)
  and Figure 6 (random effects A2 vs B2).

## Reason for exclusion

UAI as a single predictor showed p = 0.304 in the full four-wave model
on rob2item, with 0.0% reduction in L2 variance vs. Model A2. The
six-dimension joint model on N = 27 L2 units is at the limit of
recommended sample size for multilevel modeling (Maas & Hox). The
thesis retains Hofstede only as an interpretive framework in the
written text, not as inferential predictors in the model.
