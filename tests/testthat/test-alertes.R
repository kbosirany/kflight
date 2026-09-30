faux_prix <- function(prix) {
  function(...) {
    r <- fixture()
    r$best_flights[[1]]$price <- prix
    r
  }
}

test_that("un aller-retour sous le budget déclenche une alerte, sans doublon", {
  dossier <- depot_test()
  jour <- as.Date("2027-06-01")
  collecter(dossier, cle = "k", date_collecte = jour, requeteur = faux_prix(1100))
  a <- preparer_alertes(dossier, jour)
  expect_gt(nrow(a), 0)
  expect_true(all(a$niveau == "bon"))
  titre <- readLines(file.path(dossier, "alerte_titre.txt"), encoding = "UTF-8")
  expect_match(titre, "Bon prix")
  expect_match(paste(readLines(file.path(dossier, "alerte_corps.md"), encoding = "UTF-8"), collapse = "\n"),
               "@kbosirany")

  # Même prix le lendemain : pas de nouvelle alerte, pas de fichier.
  jour2 <- jour + 1
  collecter(dossier, cle = "k", date_collecte = jour2, requeteur = faux_prix(1090))
  expect_equal(nrow(preparer_alertes(dossier, jour2)), 0)
  expect_false(file.exists(file.path(dossier, "alerte_titre.txt")))

  # Une vraie baisse (au moins 50 EUR) et le très bon prix repartent en alerte.
  jour3 <- jour + 2
  collecter(dossier, cle = "k", date_collecte = jour3, requeteur = faux_prix(950))
  a3 <- preparer_alertes(dossier, jour3)
  expect_gt(nrow(a3), 0)
  expect_true(all(a3$niveau == "tres_bon"))
  expect_match(readLines(file.path(dossier, "alerte_titre.txt"), encoding = "UTF-8"), "Tr\u00e8s bon prix|options")
})

test_that("un prix au-dessus du budget ou en aller simple ne déclenche rien", {
  dossier <- depot_test()
  collecter(dossier, cle = "k", date_collecte = as.Date("2027-06-01"), requeteur = faux_prix(1300))
  expect_equal(nrow(preparer_alertes(dossier, as.Date("2027-06-01"))), 0)

  # Aller simple à 500 EUR : non comparable au budget aller-retour.
  collecter(dossier, cle = "k", date_collecte = as.Date("2026-09-30"), requeteur = faux_prix(500))
  expect_equal(nrow(preparer_alertes(dossier, as.Date("2026-09-30"))), 0)
})
