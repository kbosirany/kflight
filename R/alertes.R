#' Préparer l'alerte de prix du jour
#'
#' Parmi les allers-retours de la collecte du jour, repère les options dont le
#' meilleur prix passe sous `budget$vol_cible_max`. Une option n'est signalée
#' de nouveau que si son prix a baissé d'au moins `budget$baisse_min` depuis
#' sa dernière alerte. Écrit `alerte_titre.txt` et `alerte_corps.md` dans
#' `dossier` (le workflow en fait une issue GitHub, donc un e-mail) et note
#' l'alerte dans `data/alertes.csv`. Les allers simples ne sont pas comparables
#' au budget et ne déclenchent rien.
#'
#' @param dossier Racine du dépôt.
#' @param date_collecte Jour de la collecte à examiner.
#' @return Les nouvelles alertes, invisible (data.frame vide s'il n'y en a pas).
#' @export
preparer_alertes <- function(dossier = ".", date_collecte = Sys.Date()) {
  unlink(file.path(dossier, c("alerte_titre.txt", "alerte_corps.md")))
  cfg <- lire_config(dossier)
  budget <- cfg$recherches$budget
  offres <- lire_donnees("offres", dossier)
  if (is.null(budget) || !nrow(offres)) return(invisible(data.frame()))

  jour <- offres[as.Date(offres$date_collecte) == as.Date(date_collecte) &
                   offres$trajet %in% "aller-retour" &
                   offres$arrivee_dans_fenetre %in% TRUE & !is.na(offres$prix), , drop = FALSE]
  if (!nrow(jour)) return(invisible(data.frame()))
  jour <- jour[order(jour$prix, jour$duree_min), , drop = FALSE]
  jour <- jour[!duplicated(paste(jour$origine, jour$destination)), , drop = FALSE]
  jour <- jour[jour$prix <= budget$vol_cible_max, , drop = FALSE]
  if (!nrow(jour)) return(invisible(data.frame()))

  jour$option <- libelle_option(jour$origine, jour$destination)
  passees <- lire_donnees("alertes", dossier)
  derniere <- function(option) {
    p <- passees$prix[passees$option == option]
    if (length(p)) p[length(p)] else Inf
  }
  neuves <- vapply(seq_len(nrow(jour)), function(i) {
    jour$prix[i] <= derniere(jour$option[i]) - budget$baisse_min
  }, TRUE)
  jour <- jour[neuves, , drop = FALSE]
  if (!nrow(jour)) return(invisible(data.frame()))
  jour$niveau <- ifelse(jour$prix <= budget$vol_cible_min, "tres_bon", "bon")

  ecrire_alerte(jour, cfg, budget, dossier)
  alertes <- data.frame(date_alerte = format(date_collecte), option = jour$option,
                        prix = jour$prix, niveau = jour$niveau, stringsAsFactors = FALSE)
  ajouter_donnees(alertes, "alertes", dossier)
  invisible(alertes)
}

ecrire_alerte <- function(jour, cfg, budget, dossier) {
  n <- as.integer(cfg$recherches$voyageurs)
  codes <- function(liste, nom) Find(function(x) x$nom == nom, liste)$codes
  titre <- if (nrow(jour) == 1) {
    sprintf("%s : %s \u00e0 %s par personne (aller-retour)",
            if (jour$niveau == "tres_bon") "Tr\u00e8s bon prix" else "Bon prix",
            jour$option, format_euros(jour$prix))
  } else {
    sprintf("Bon prix : %d options sous le budget", nrow(jour))
  }
  lignes <- vapply(seq_len(nrow(jour)), function(i) {
    o <- jour[i, ]
    transfert <- cfg$transferts$arrivee[[o$destination]]$cout_par_personne
    approche <- cfg$transferts$approche[[o$origine]]$cout_par_personne
    total <- if (is.null(transfert) || is.null(approche)) NA_real_ else o$prix + transfert + approche
    lien <- lien_google_flights(codes(cfg$recherches$origines, o$origine),
                                codes(cfg$recherches$destinations, o$destination),
                                o$date_depart, o$date_retour)
    paste0(
      "### ", o$option, " : ", format_euros(o$prix), " par personne\n\n",
      "- Dates : ", o$date_depart, " \u2192 ", o$date_retour, "\n",
      "- Compagnies : ", o$compagnies, " (", if (o$escales == 0) "direct" else paste0(o$escales, " escale(s) : ", o$lieux_escale), ")\n",
      "- Dur\u00e9e : ", format_duree(o$duree_min), "\n",
      "- Pour ", n, " voyageurs : ", format_euros(o$prix * n), " de vols\n",
      if (is.na(total)) "- Co\u00fbt complet (vol + route + approche) : \u00e0 renseigner dans `config/transferts.yml`\n"
      else sprintf("- Co\u00fbt complet par personne : %s (plafond %s)\n", format_euros(total), format_euros(budget$total_max)),
      "- [V\u00e9rifier sur Google Flights](", lien, ")\n"
    )
  }, "")
  corps <- paste0(
    "@kbosirany un aller-retour est pass\u00e9 sous ton budget de vol (",
    format_euros(budget$vol_cible_min), " \u00e0 ", format_euros(budget$vol_cible_max), ").\n\n",
    paste(lignes, collapse = "\n"),
    "\nV\u00e9rifie le prix et les places disponibles pour tout le groupe sur le site de la compagnie avant d'acheter. ",
    "Tableau de bord : https://kbosirany.github.io/kflight/\n"
  )
  writeLines(titre, file.path(dossier, "alerte_titre.txt"), useBytes = TRUE)
  writeLines(corps, file.path(dossier, "alerte_corps.md"), useBytes = TRUE)
}
