library(testthat)
library(FastForecast)

# Créer des données de test
test_df <- data.frame(
  A = 1:24,
  B = rnorm(24),
  C = rep(c("X", "Y"), 12)
)

test_that("ts_transfo fonctionne correctement", {
  # Test de base
  result <- ts_transfo(test_df, 2020, 01, 12)

  # Vérifier que le résultat est une liste
  expect_type(result, "list")

  # Vérifier que la liste contient le bon nombre d'éléments
  expect_equal(length(result), ncol(test_df))

  # Vérifier que chaque élément de la liste est une série temporelle
  expect_true(all(sapply(result, is.ts)))

  # Vérifier que les noms des éléments de la liste correspondent aux noms des colonnes
  expect_equal(names(result), names(test_df))

  # Vérifier les propriétés d'une série temporelle spécifique
  expect_equal(start(result$A), c(2020, 1))
  expect_equal(frequency(result$A), 12)
  expect_equal(length(result$A), nrow(test_df))

  # Vérifier que les objets sont créés dans l'environnement global
  expect_true(exists("ts_A", envir = .GlobalEnv))
  expect_true(exists("ts_B", envir = .GlobalEnv))
  expect_true(exists("ts_C", envir = .GlobalEnv))
  expect_true(exists("ts_list", envir = .GlobalEnv))

  # Nettoyer l'environnement global après le test
  rm(list = c("ts_A", "ts_B", "ts_C", "ts_list"), envir = .GlobalEnv)
})

test_that("ts_transfo gère correctement différentes fréquences", {
  result_quarterly <- ts_transfo(test_df[1:12,], 2020, 1, 4)

  expect_equal(frequency(result_quarterly$A), 4)
  expect_equal(start(result_quarterly$A), c(2020, 1))

  # Nettoyer l'environnement global après le test
  rm(list = c("ts_A", "ts_B", "ts_C", "ts_list"), envir = .GlobalEnv)
})

test_that("ts_transfo gère les erreurs", {
  expect_error(ts_transfo(test_df, "2020", 1, 12), "YEAR, MONTH et FREQUENCY doivent être des valeurs numériques")
  expect_error(ts_transfo(test_df, 2020, "1", 12), "YEAR, MONTH et FREQUENCY doivent être des valeurs numériques")
  expect_error(ts_transfo(test_df, 2020, 1, "12"), "YEAR, MONTH et FREQUENCY doivent être des valeurs numériques")
  expect_error(ts_transfo("not a dataframe", 2020, 1, 12), "DATAFRAME doit être un data.frame")
})
