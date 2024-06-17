library(testthat)
library(tsoutliers)  # Pour la fonction tso

# Fonction de mock pour tso
mock_tso <- function(x) {
  structure(
    list(
      yadj = x,
      outliers = data.frame(type = "AO", ind = 1, coefhat = 1),
      others = list()
    ),
    class = "tso"
  )
}

# Test unitaire
test_that("atypical_tso fonctionne correctement", {
  # Créer des données de test
  ts_list <<- list(
    col1 = ts(rnorm(100), frequency = 12),
    col2 = ts(rnorm(100), frequency = 12)
  )

  # Assigner les séries temporelles à l'environnement global
  assign("ts_col1", ts_list$col1, envir = .GlobalEnv)
  assign("ts_col2", ts_list$col2, envir = .GlobalEnv)

  # Remplacer la fonction tso par notre mock
  with_mock(
    tso = mock_tso,
    {
      # Exécuter la fonction
      atypical_tso()

      # Vérifier que ts_list_adj a été créé et contient les bonnes colonnes
      expect_true(exists("ts_list_adj", envir = .GlobalEnv))
      expect_equal(names(ts_list_adj), c("col1", "col2"))

      # Vérifier que les séries ajustées ont été créées
      expect_true(exists("ts_col1_adj", envir = .GlobalEnv))
      expect_true(exists("ts_col2_adj", envir = .GlobalEnv))

      # Vérifier que les objets tso ont été créés
      expect_true(exists("tso_col1", envir = .GlobalEnv))
      expect_true(exists("tso_col2", envir = .GlobalEnv))

      # Vérifier que les séries ajustées sont identiques aux originales (dans notre mock)
      expect_equal(ts_col1_adj, ts_list$col1)
      expect_equal(ts_col2_adj, ts_list$col2)
    }
  )
})
