# Crée un faux dépôt kflight dans un dossier temporaire.
depot_test <- function(env = parent.frame()) {
  dossier <- tempfile("kflight")
  dir.create(file.path(dossier, "config"), recursive = TRUE)
  do.call(on.exit, list(bquote(unlink(.(dossier), recursive = TRUE)), add = TRUE), envir = env)
  writeLines(c(
    "voyageurs: 6",
    "devise: EUR",
    "arrivee_min: 2027-08-16",
    "arrivee_max: 2027-08-20",
    "dates_depart: [2027-08-16, 2027-08-17]",
    "durees_sejour: [21, 28]",
    "origines:",
    "  - {nom: Paris, codes: 'CDG,ORY'}",
    "  - {nom: Bordeaux, codes: BOD}",
    "destinations:",
    "  - {nom: Diego-Suarez, codes: DIE}",
    "  - {nom: Nosy Be, codes: NOS}",
    "budget: {vol_cible_min: 1000, vol_cible_max: 1200, total_max: 1500, baisse_min: 50}",
    "recherches_par_jour: 3",
    "suivi_quotidien:",
    "  - {origine: Paris, destination: Nosy Be, depart: 2027-08-17, duree: 28}"
  ), file.path(dossier, "config", "recherches.yml"))
  writeLines(c(
    "arrivee:",
    "  Diego-Suarez: {cout_par_personne: 0}",
    "  Nosy Be: {cout_par_personne: null}",
    "approche:",
    "  Paris: {cout_par_personne: 100}",
    "  Bordeaux: {cout_par_personne: 50}"
  ), file.path(dossier, "config", "transferts.yml"))
  writeLines("- {nom: Air France, site: 'https://www.airfrance.fr'}",
             file.path(dossier, "config", "compagnies.yml"))
  dossier
}

fixture <- function() {
  jsonlite::read_json(test_path("fixtures", "reponse_serpapi.json"), simplifyVector = FALSE)
}
