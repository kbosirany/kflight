test_that("les combinaisons couvrent toute la grille", {
  cfg <- lire_config(depot_test())$recherches
  combos <- combinaisons(cfg)
  expect_equal(nrow(combos), 2 * 2 * 2 * 2)
  expect_equal(combos$retour - combos$depart, as.difftime(combos$duree, units = "days"))
  expect_false(anyDuplicated(combos$id_recherche) > 0)
})

test_that("le suivi quotidien passe en premier, puis la rotation", {
  cfg <- lire_config(depot_test())$recherches
  combos <- combinaisons(cfg)
  choix <- choisir_recherches(combos, data.frame(), cfg)
  expect_equal(nrow(choix), 3)
  expect_equal(choix$id_recherche[1], "Paris | Nosy Be | 2027-08-17 | 28j")

  # Les recherches faites hier passent derrière celles jamais faites.
  journal <- data.frame(date_collecte = "2026-09-30", id_recherche = choix$id_recherche,
                        statut = "ok")
  choix2 <- choisir_recherches(combos, journal, cfg)
  expect_equal(choix2$id_recherche[1], choix$id_recherche[1])
  expect_length(intersect(choix2$id_recherche[-1], choix$id_recherche[-1]), 0)
})

test_that("une recherche en erreur reste prioritaire", {
  cfg <- lire_config(depot_test())$recherches
  combos <- combinaisons(cfg)
  choix <- choisir_recherches(combos, data.frame(), cfg)
  journal <- data.frame(date_collecte = "2026-09-30", id_recherche = choix$id_recherche[2],
                        statut = "erreur")
  expect_equal(choisir_recherches(combos, journal, cfg)$id_recherche[2], choix$id_recherche[2])
})

test_that("une recherche vide est retentée moins vite qu'une recherche réussie", {
  cfg <- lire_config(depot_test())$recherches
  cfg$recherches_par_jour <- 100
  combos <- combinaisons(cfg)
  autres <- setdiff(combos$id_recherche, "Paris | Nosy Be | 2027-08-17 | 28j")
  journal <- data.frame(
    date_collecte = c("2026-09-30", "2026-09-30"),
    id_recherche = autres[1:2], statut = c("vide", "ok")
  )
  ordre <- choisir_recherches(combos, journal, cfg)$id_recherche
  expect_gt(match(autres[1], ordre), match(autres[2], ordre))
})
