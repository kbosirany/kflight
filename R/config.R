#' Lire la configuration kflight
#'
#' @param dossier Racine du dépôt (contient `config/`).
#' @return Une liste avec `recherches`, `transferts` et `compagnies`.
#' @export
lire_config <- function(dossier = ".") {
  lire <- function(fichier) yaml::read_yaml(file.path(dossier, "config", fichier))
  recherches <- lire("recherches.yml")
  recherches$dates_depart <- as.Date(unlist(recherches$dates_depart))
  recherches$arrivee_min <- as.Date(recherches$arrivee_min)
  recherches$arrivee_max <- as.Date(recherches$arrivee_max)
  recherches$durees_sejour <- as.integer(unlist(recherches$durees_sejour))
  list(
    recherches = recherches,
    transferts = lire("transferts.yml"),
    compagnies = lire("compagnies.yml")
  )
}

#' Toutes les combinaisons de recherche décrites par la configuration
#'
#' @param recherches L'élément `recherches` de [lire_config()].
#' @return Un data.frame, une ligne par recherche possible.
#' @export
combinaisons <- function(recherches) {
  o <- do.call(rbind, lapply(recherches$origines, as.data.frame))
  d <- do.call(rbind, lapply(recherches$destinations, as.data.frame))
  grille <- expand.grid(
    io = seq_len(nrow(o)), id = seq_len(nrow(d)),
    depart = recherches$dates_depart, duree = recherches$durees_sejour,
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  res <- data.frame(
    origine = o$nom[grille$io],
    codes_origine = o$codes[grille$io],
    destination = d$nom[grille$id],
    codes_destination = d$codes[grille$id],
    depart = grille$depart,
    retour = grille$depart + grille$duree,
    duree = grille$duree,
    stringsAsFactors = FALSE
  )
  res$id_recherche <- id_recherche(res$origine, res$destination, res$depart, res$duree)
  res[order(res$origine, res$destination, res$depart, res$duree), , drop = FALSE]
}

id_recherche <- function(origine, destination, depart, duree) {
  paste(origine, destination, format(as.Date(depart)), paste0(duree, "j"), sep = " | ")
}

#' Choisir les recherches de la collecte du jour
#'
#' Les combinaisons de `suivi_quotidien` passent en premier, puis les autres
#' par ordre d'ancienneté de leur dernière recherche (jamais cherchées d'abord),
#' dans la limite de `recherches_par_jour`.
#'
#' @param combos Résultat de [combinaisons()].
#' @param journal Journal des recherches déjà faites (colonnes `id_recherche`,
#'   `date_collecte`, `statut`), éventuellement vide. Les statuts `"ok"` et
#'   `"vide"` (aucun résultat) comptent comme faits.
#' @param recherches L'élément `recherches` de [lire_config()].
#' @return Les lignes de `combos` à chercher, dans l'ordre.
#' @export
choisir_recherches <- function(combos, journal, recherches) {
  budget <- as.integer(recherches$recherches_par_jour)
  suivi <- recherches$suivi_quotidien
  ids_suivi <- if (length(suivi)) {
    vapply(suivi, function(s) id_recherche(s$origine, s$destination, s$depart, s$duree), "")
  } else character()
  ids_suivi <- intersect(ids_suivi, combos$id_recherche)

  derniere <- rep(as.Date("1900-01-01"), nrow(combos))
  if (!is.null(journal) && nrow(journal)) {
    ok <- journal[journal$statut %in% c("ok", "vide"), , drop = FALSE]
    if (nrow(ok)) {
      d <- tapply(as.Date(ok$date_collecte), ok$id_recherche, max)
      trouve <- combos$id_recherche %in% names(d)
      derniere[trouve] <- as.Date(d[combos$id_recherche[trouve]], origin = "1970-01-01")
    }
  }
  autres <- which(!combos$id_recherche %in% ids_suivi)
  autres <- autres[order(derniere[autres], seq_along(autres))]
  ordre <- c(match(ids_suivi, combos$id_recherche), autres)
  combos[utils::head(ordre, budget), , drop = FALSE]
}
