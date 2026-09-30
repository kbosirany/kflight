#' Interroger Google Flights via SerpApi
#'
#' Cherche un aller-retour pour 1 adulte : le prix renvoyé est donc un prix
#' par personne, aller et retour compris.
#'
#' @param combo Une ligne de [combinaisons()].
#' @param cle Clé API SerpApi.
#' @param devise Code devise, par exemple `"EUR"`.
#' @return La réponse JSON décodée (liste).
#' @export
requete_serpapi <- function(combo, cle, devise = "EUR") {
  req <- httr2::request("https://serpapi.com/search.json")
  req <- httr2::req_url_query(
    req,
    engine = "google_flights",
    departure_id = combo$codes_origine,
    arrival_id = combo$codes_destination,
    outbound_date = format(combo$depart),
    return_date = format(combo$retour),
    type = 1,
    adults = 1,
    travel_class = 1,
    currency = devise,
    hl = "fr",
    gl = "fr",
    api_key = cle
  )
  req <- httr2::req_timeout(req, 90)
  req <- httr2::req_retry(req, max_tries = 3)
  httr2::resp_body_json(httr2::req_perform(req), simplifyVector = FALSE)
}

#' Transformer une réponse SerpApi en tables
#'
#' @param rep Réponse décodée par [requete_serpapi()].
#' @param combo La ligne de [combinaisons()] cherchée.
#' @param date_collecte Date de la collecte.
#' @param fenetre Vecteur de deux dates : arrivée minimale et maximale.
#' @param devise Devise des prix.
#' @return Une liste de data.frames `offres`, `insights` et `historique`.
#' @export
lire_reponse <- function(rep, combo, date_collecte, fenetre, devise = "EUR") {
  offres <- rbind(
    lire_offres(rep$best_flights, "meilleure"),
    lire_offres(rep$other_flights, "autre")
  )
  if (nrow(offres)) {
    arrivee <- as.Date(substr(offres$heure_arrivee, 1, 10))
    offres <- cbind(
      data.frame(
        date_collecte = format(date_collecte),
        id_recherche = combo$id_recherche,
        origine = combo$origine,
        destination = combo$destination,
        date_depart = format(combo$depart),
        date_retour = format(combo$retour),
        stringsAsFactors = FALSE
      ),
      offres,
      data.frame(
        devise = devise,
        arrivee_dans_fenetre = !is.na(arrivee) & arrivee >= fenetre[1] & arrivee <= fenetre[2]
      )
    )
  }

  pi <- rep$price_insights
  insights <- data.frame()
  historique <- data.frame()
  if (!is.null(pi)) {
    fourchette <- unlist(pi$typical_price_range)
    insights <- data.frame(
      date_collecte = format(date_collecte),
      id_recherche = combo$id_recherche,
      prix_min = num_ou_na(pi$lowest_price),
      niveau_prix = chr_ou_na(pi$price_level),
      fourchette_min = num_ou_na(fourchette[1]),
      fourchette_max = num_ou_na(fourchette[2]),
      stringsAsFactors = FALSE
    )
    h <- pi$price_history
    if (length(h)) {
      historique <- data.frame(
        id_recherche = combo$id_recherche,
        date = format(as.Date(as.POSIXct(vapply(h, function(x) as.numeric(x[[1]]), 0),
                                         origin = "1970-01-01", tz = "UTC"))),
        prix = vapply(h, function(x) as.numeric(x[[2]]), 0),
        date_collecte = format(date_collecte),
        stringsAsFactors = FALSE
      )
    }
  }
  list(offres = offres, insights = insights, historique = historique)
}

lire_offres <- function(liste, categorie) {
  if (!length(liste)) return(data.frame())
  lignes <- lapply(liste, function(o) {
    segs <- o$flights
    premier <- segs[[1]]
    dernier <- segs[[length(segs)]]
    escales <- o$layovers
    data.frame(
      categorie = categorie,
      compagnies = paste(unique(vapply(segs, function(s) chr_ou_na(s$airline), "")), collapse = " + "),
      vols = paste(vapply(segs, function(s) chr_ou_na(s$flight_number), ""), collapse = ", "),
      aeroport_depart = chr_ou_na(premier$departure_airport$id),
      aeroport_arrivee = chr_ou_na(dernier$arrival_airport$id),
      heure_depart = chr_ou_na(premier$departure_airport$time),
      heure_arrivee = chr_ou_na(dernier$arrival_airport$time),
      escales = length(escales),
      lieux_escale = paste(vapply(escales, function(e) chr_ou_na(e$id), ""), collapse = ", "),
      duree_min = num_ou_na(o$total_duration),
      prix = num_ou_na(o$price),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, lignes)
}

num_ou_na <- function(x) if (is.null(x) || !length(x)) NA_real_ else as.numeric(x[[1]])
chr_ou_na <- function(x) if (is.null(x) || !length(x)) NA_character_ else as.character(x[[1]])
