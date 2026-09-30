#' Lancer la collecte du jour
#'
#' Choisit les recherches du jour, interroge SerpApi et ajoute les résultats
#' aux fichiers de `data/`. Une recherche en échec est notée dans le journal
#' sans interrompre les autres.
#'
#' @param dossier Racine du dépôt.
#' @param cle Clé API SerpApi (variable d'environnement `SERPAPI_KEY` par
#'   défaut).
#' @param date_collecte Date à enregistrer.
#' @param requeteur Fonction `(combo, cle, devise)` qui renvoie la réponse
#'   décodée ; remplaçable pour les tests.
#' @return Le journal de la collecte, invisible.
#' @export
collecter <- function(dossier = ".", cle = Sys.getenv("SERPAPI_KEY"),
                      date_collecte = Sys.Date(), requeteur = requete_serpapi) {
  if (!nzchar(cle)) {
    message("Pas de cl\u00e9 SERPAPI_KEY : aucune recherche lanc\u00e9e.")
    return(invisible(data.frame()))
  }
  cfg <- lire_config(dossier)$recherches
  devise <- cfg$devise %||% "EUR"
  fenetre <- c(cfg$arrivee_min, cfg$arrivee_max)
  a_faire <- choisir_recherches(combinaisons(cfg), lire_donnees("journal", dossier), cfg)

  journal <- lapply(seq_len(nrow(a_faire)), function(i) {
    combo <- a_faire[i, , drop = FALSE]
    message("Recherche : ", combo$id_recherche)
    res <- tryCatch({
      rep <- requeteur(combo, cle, devise)
      if (!is.null(rep$error)) stop(rep$error)
      lire_reponse(rep, combo, date_collecte, fenetre, devise)
    }, error = function(e) e)

    if (inherits(res, "error")) {
      statut <- "erreur"
      n <- 0L
      msg <- gsub(cle, "***", conditionMessage(res), fixed = TRUE)
      message("  erreur : ", msg)
    } else {
      ajouter_donnees(res$offres, "offres", dossier)
      ajouter_donnees(res$insights, "insights", dossier)
      ajouter_historique(res$historique, dossier)
      statut <- "ok"
      n <- nrow(res$offres)
      msg <- ""
      message("  ", n, " offres")
    }
    data.frame(
      date_collecte = format(date_collecte),
      heure = format(Sys.time(), "%H:%M:%S", tz = "UTC"),
      id_recherche = combo$id_recherche,
      statut = statut,
      n_offres = n,
      message = gsub("[\r\n]+", " ", msg),
      stringsAsFactors = FALSE
    )
  })
  journal <- do.call(rbind, journal)
  ajouter_donnees(journal, "journal", dossier)
  invisible(journal)
}

`%||%` <- function(a, b) if (is.null(a)) b else a
