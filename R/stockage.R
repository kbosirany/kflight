# Fichiers de données, relatifs à la racine du dépôt.
fichiers_donnees <- c(
  offres = "data/offres.csv",
  insights = "data/insights.csv",
  historique = "data/historique_google.csv",
  journal = "data/journal.csv",
  manuels = "data/prix_manuels.csv",
  alertes = "data/alertes.csv"
)

#' Lire une table de données kflight
#'
#' @param nom Un de `"offres"`, `"insights"`, `"historique"`, `"journal"`,
#'   `"manuels"`.
#' @param dossier Racine du dépôt.
#' @return Un data.frame (vide si le fichier n'existe pas encore).
#' @export
lire_donnees <- function(nom, dossier = ".") {
  chemin <- file.path(dossier, fichiers_donnees[[nom]])
  if (!file.exists(chemin)) return(data.frame())
  as.data.frame(readr::read_csv(chemin, show_col_types = FALSE, progress = FALSE))
}

# Ajoute des lignes à une table sans jamais réécrire les lignes existantes.
ajouter_donnees <- function(df, nom, dossier = ".") {
  if (is.null(df) || !nrow(df)) return(invisible(0L))
  chemin <- file.path(dossier, fichiers_donnees[[nom]])
  dir.create(dirname(chemin), showWarnings = FALSE, recursive = TRUE)
  readr::write_csv(df, chemin, append = file.exists(chemin), na = "")
  invisible(nrow(df))
}

# L'historique Google redonne à chaque recherche les mêmes jours passés : on
# n'ajoute que les jours pas encore connus pour cette recherche.
ajouter_historique <- function(df, dossier = ".") {
  if (is.null(df) || !nrow(df)) return(invisible(0L))
  existant <- lire_donnees("historique", dossier)
  if (nrow(existant)) {
    cle <- paste(df$id_recherche, df$trajet, df$date)
    df <- df[!cle %in% paste(existant$id_recherche, existant$trajet, format(as.Date(existant$date))), , drop = FALSE]
  }
  ajouter_donnees(df, "historique", dossier)
}
