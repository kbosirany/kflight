test_that("la réponse SerpApi est convertie en tables", {
  cfg <- lire_config(depot_test())$recherches
  combo <- combinaisons(cfg)[1, ]
  res <- lire_reponse(fixture(), combo, as.Date("2026-09-30"),
                      c(cfg$arrivee_min, cfg$arrivee_max))
  expect_equal(nrow(res$offres), 3)
  expect_equal(res$offres$compagnies[1], "Air France + Madagascar Airlines")
  expect_equal(res$offres$escales, c(1L, 1L, 0L))
  expect_equal(res$offres$arrivee_dans_fenetre, c(TRUE, FALSE, TRUE))
  expect_true(is.na(res$offres$prix[3]))
  expect_equal(res$insights$niveau_prix, "typical")
  expect_equal(nrow(res$historique), 2)
})

test_that("la collecte ajoute sans écraser et journalise les erreurs", {
  dossier <- depot_test()
  n <- 0
  faux <- function(combo, cle, devise) {
    n <<- n + 1
    if (n == 2) stop("quota dépassé pour la cle SECRET")
    fixture()
  }
  j1 <- collecter(dossier, cle = "SECRET", date_collecte = as.Date("2026-09-30"), requeteur = faux)
  expect_equal(j1$statut, c("ok", "erreur", "ok"))
  expect_false(any(grepl("SECRET", j1$message)))
  expect_equal(nrow(lire_donnees("offres", dossier)), 6)

  j2 <- collecter(dossier, cle = "SECRET", date_collecte = as.Date("2026-10-01"), requeteur = faux)
  expect_equal(nrow(lire_donnees("offres", dossier)), 15)
  expect_equal(nrow(lire_donnees("journal", dossier)), 6)
  # L'historique Google n'est pas dupliqué d'un jour à l'autre.
  expect_equal(nrow(lire_donnees("historique", dossier)), 2 * length(unique(c(j1$id_recherche[j1$statut == "ok"], j2$id_recherche))))
})

test_that("sans clé, rien n'est lancé", {
  dossier <- depot_test()
  expect_message(collecter(dossier, cle = ""), "SERPAPI_KEY")
  expect_equal(nrow(lire_donnees("offres", dossier)), 0)
})

test_that("les meilleures options tiennent compte des transferts", {
  dossier <- depot_test()
  cfg <- lire_config(dossier)
  collecter(dossier, cle = "k", date_collecte = as.Date("2026-09-30"),
            requeteur = function(...) fixture())
  opts <- meilleures_options(lire_donnees("offres", dossier), 6, cfg$transferts)
  expect_true(all(opts$prix == 1450))
  bordeaux_diego <- opts[opts$origine == "Bordeaux" & opts$destination == "Diego-Suarez", ]
  expect_equal(bordeaux_diego$total_complet_groupe, (1450 + 0 + 50) * 6)
  # Transfert Nosy Be inconnu : pas de coût complet inventé.
  expect_true(is.na(opts$total_complet_groupe[opts$destination == "Nosy Be"]))
  expect_gt(nrow(evolution_prix(lire_donnees("offres", dossier))), 0)
})

test_that("le lien Google Flights est bien formé", {
  l <- lien_google_flights("CDG,ORY", "DIE", "2027-08-17", "2027-09-14")
  expect_match(l, "^https://www.google.com/travel/flights\\?")
  expect_match(l, "CDG%20to%20DIE")
})

test_that("l'aller simple remplace l'aller-retour hors de l'horizon Google", {
  dossier <- depot_test()
  vus <- character()
  faux <- function(combo, cle, devise) { vus <<- c(vus, combo$trajet); fixture() }
  collecter(dossier, cle = "k", date_collecte = as.Date("2026-09-30"), requeteur = faux)
  expect_true(all(vus == "aller simple"))
  expect_true(all(lire_donnees("offres", dossier)$trajet == "aller simple"))
  vus <- character()
  j <- collecter(dossier, cle = "k", date_collecte = as.Date("2027-01-15"), requeteur = faux)
  expect_true(all(vus == "aller-retour"))
  # Une recherche refaite en aller-retour remplace son ancien aller simple.
  opts <- meilleures_options(lire_donnees("offres", dossier), 6, lire_config(dossier)$transferts)
  paris_nosy <- opts[opts$origine == "Paris" & opts$destination == "Nosy Be", ]
  expect_equal(paris_nosy$trajet, "aller-retour")
})

test_that("une recherche sans résultat n'est pas réessayée en priorité", {
  dossier <- depot_test()
  vide <- function(...) list(error = "Google Flights hasn't returned any results for this query.")
  j1 <- collecter(dossier, cle = "k", date_collecte = as.Date("2026-09-30"), requeteur = vide)
  expect_true(all(j1$statut == "vide"))
  j2 <- collecter(dossier, cle = "k", date_collecte = as.Date("2026-10-01"), requeteur = vide)
  expect_length(intersect(j2$id_recherche[-1], j1$id_recherche[-1]), 0)
})

test_that("le lien aller simple est bien formé", {
  expect_match(lien_google_flights("BOD", "NOS", "2027-08-17"), "One%20way")
})
