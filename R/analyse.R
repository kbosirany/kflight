#' Lien de recherche Google Flights prêt à cliquer
#'
#' @param codes_origine,codes_destination Codes IATA (plusieurs séparés par
#'   une virgule acceptés : seul le premier est utilisé dans le lien).
#' @param depart,retour Dates de l'aller-retour ; `retour` à `NA` pour un
#'   aller simple.
#' @return Une URL.
#' @export
lien_google_flights <- function(codes_origine, codes_destination, depart, retour = NA) {
  premier <- function(x) sub(",.*$", "", x)
  q <- ifelse(
    is.na(retour),
    sprintf("One way flights from %s to %s on %s",
            premier(codes_origine), premier(codes_destination), format(as.Date(depart))),
    sprintf("Flights from %s to %s on %s through %s",
            premier(codes_origine), premier(codes_destination),
            format(as.Date(depart)), format(as.Date(retour)))
  )
  paste0("https://www.google.com/travel/flights?hl=fr&curr=EUR&q=",
         vapply(q, utils::URLencode, "", reserved = TRUE, USE.NAMES = FALSE))
}

# Offres de la dernière collecte de chaque recherche, arrivant dans la fenêtre.
offres_recentes <- function(offres) {
  if (!nrow(offres)) return(offres)
  offres <- offres[offres$arrivee_dans_fenetre %in% TRUE & !is.na(offres$prix), , drop = FALSE]
  if (!nrow(offres)) return(offres)
  derniere <- tapply(as.Date(offres$date_collecte), offres$id_recherche, max)
  garde <- as.Date(offres$date_collecte) == as.Date(derniere[offres$id_recherche], origin = "1970-01-01")
  offres[garde, , drop = FALSE]
}

#' Meilleure offre actuelle par option (origine x destination x trajet)
#'
#' @param offres Table `offres` ([lire_donnees()]).
#' @param voyageurs Nombre de voyageurs du groupe.
#' @param transferts L'élément `transferts` de [lire_config()].
#' @return Un data.frame trié du moins cher au plus cher (coût complet
#'   quand il est connu, sinon prix des vols).
#' @export
meilleures_options <- function(offres, voyageurs, transferts) {
  r <- offres_recentes(offres)
  if (!nrow(r)) return(data.frame())
  r <- r[order(r$prix, r$duree_min), , drop = FALSE]
  r <- r[!duplicated(paste(r$origine, r$destination, r$trajet)), , drop = FALSE]
  # Dès qu'un aller-retour existe pour une option, l'aller simple n'est plus
  # montré : les deux prix ne sont pas comparables.
  a_retour <- paste(r$origine, r$destination)[r$trajet == "aller-retour"]
  r <- r[!(r$trajet == "aller simple" & paste(r$origine, r$destination) %in% a_retour), , drop = FALSE]
  cout <- function(liste, nom) {
    v <- liste[[nom]]$cout_par_personne
    if (is.null(v)) NA_real_ else as.numeric(v)
  }
  r$transfert_pp <- vapply(r$destination, function(d) cout(transferts$arrivee, d), 0)
  r$approche_pp <- vapply(r$origine, function(o) cout(transferts$approche, o), 0)
  r$total_vols_groupe <- r$prix * voyageurs
  r$total_complet_groupe <- (r$prix + r$transfert_pp + r$approche_pp) * voyageurs
  tri <- ifelse(is.na(r$total_complet_groupe), r$total_vols_groupe, r$total_complet_groupe)
  r[order(tri), , drop = FALSE]
}

#' Évolution du meilleur prix par option et par jour de collecte
#'
#' @param offres Table `offres`.
#' @return Un data.frame `date_collecte`, `option`, `prix_min`.
#' @export
evolution_prix <- function(offres) {
  o <- offres[offres$arrivee_dans_fenetre %in% TRUE & !is.na(offres$prix), , drop = FALSE]
  if (!nrow(o)) return(data.frame())
  o$option <- libelle_option(o$origine, o$destination, o$trajet)
  a <- stats::aggregate(prix ~ date_collecte + option, data = o, FUN = min)
  names(a)[3] <- "prix_min"
  a$date_collecte <- as.Date(a$date_collecte)
  a[order(a$option, a$date_collecte), , drop = FALSE]
}

#' Libellé d'une option : « Paris → Nosy Be », suffixé pour un aller simple
#' @param origine,destination Noms des aéroports.
#' @param trajet `"aller-retour"` ou `"aller simple"`.
#' @export
libelle_option <- function(origine, destination, trajet = "aller-retour") {
  paste0(origine, " \u2192 ", destination,
         ifelse(trajet %in% "aller simple", " (aller simple)", ""))
}

#' Formater une durée en minutes en « 14 h 05 »
#' @param minutes Durée en minutes.
#' @export
format_duree <- function(minutes) {
  ifelse(is.na(minutes), "", sprintf("%d h %02d", minutes %/% 60, as.integer(minutes %% 60)))
}

#' Formater un montant en euros
#' @param x Montant.
#' @export
format_euros <- function(x) {
  ifelse(is.na(x), "\u00e0 renseigner",
         paste(formatC(round(x), format = "d", big.mark = "\u202f"), "\u20ac"))
}
