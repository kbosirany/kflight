# kflight

Suivi des prix des vols France → Antsiranana (Diego-Suarez) pour le mariage du 26 août 2027.

Chaque jour, GitHub Actions lance une petite série de recherches Google Flights (via [SerpApi](https://serpapi.com)), ajoute les résultats datés aux fichiers de `data/` et publie un tableau de bord sur GitHub Pages. Aucun serveur, aucun coût.

**Tableau de bord :** https://kbosirany.github.io/kflight/

## Mise en route (une seule fois)

1. **Rendre le dépôt public** : *Settings → General → Danger Zone → Change visibility*. GitHub Pages est gratuit uniquement sur les dépôts publics. Ne mettez jamais de noms, numéros de passeport ou clés dans les fichiers.
2. **Créer un compte SerpApi gratuit** sur https://serpapi.com (250 recherches par mois) et copier la clé API depuis le tableau de bord SerpApi.
3. **Ajouter la clé au dépôt** : *Settings → Secrets and variables → Actions → New repository secret*, nom `SERPAPI_KEY`, valeur la clé. Elle reste secrète, même dans un dépôt public.
4. **Activer GitHub Pages** : *Settings → Pages → Build and deployment → Source : GitHub Actions*.
5. **Lancer une première collecte** : onglet *Actions → Collecte et tableau de bord → Run workflow*.

Ensuite, tout tourne seul chaque matin.

## Régler les recherches

Tout se passe dans `config/`, modifiable directement depuis GitHub (bouton crayon) :

- `recherches.yml` : nombre de voyageurs, fenêtre d'arrivée, dates de départ, durées de séjour, aéroports, nombre de recherches par jour, combinaisons suivies chaque jour.
- `transferts.yml` : coût et durée du trajet Nosy Be → Diego, coût pour rejoindre chaque aéroport de départ. Laissez `null` tant que ce n'est pas connu.
- `compagnies.yml` : liens vers les sites des compagnies affichés dans le tableau de bord.

Les prix sont collectés **par personne** (recherche pour 1 adulte) : changer le nombre de voyageurs met à jour les totaux sans casser l'historique. Pour un groupe, il peut rester moins de places au tarif le plus bas : vérifiez pour tout le groupe avant de réserver.

## Alertes de prix

Quand un **aller-retour** passe sous `budget.vol_cible_max` (dans `config/recherches.yml`), la collecte du jour ouvre une issue GitHub qui te mentionne : tu la reçois par e-mail avec les détails du vol et un lien Google Flights. Une nouvelle alerte n'est envoyée pour la même option que si le prix baisse d'au moins `baisse_min` euros. Les allers simples ne déclenchent rien.

Pour vérifier que tu reçois bien les e-mails : Actions → *Collecte et tableau de bord* → *Run workflow*, coche « Envoyer une alerte de test » et décoche « Lancer aussi une collecte ».

## Ajouter un prix vu ailleurs

Ajoutez une ligne à `data/prix_manuels.csv` (par exemple un tarif trouvé sur le site d'Air France ou de Madagascar Airlines). Il apparaît dans le tableau de bord à la publication suivante.

## Données

| Fichier | Contenu |
|---|---|
| `data/offres.csv` | Chaque offre trouvée : date de collecte, recherche, compagnies, vols, escales, durée, prix par personne, arrivée dans la fenêtre ou non |
| `data/insights.csv` | Avis de Google sur le prix (bas, normal, élevé) et fourchette habituelle |
| `data/historique_google.csv` | Historique du prix le plus bas fourni par Google, y compris avant le début de la collecte |
| `data/journal.csv` | Chaque recherche lancée, réussie ou en erreur |
| `data/prix_manuels.csv` | Prix relevés à la main |

Les fichiers ne sont jamais réécrits : chaque collecte ajoute des lignes, et Git garde la trace de chaque version.

## Développement local

Le code est un petit package R.

```r
# Installer et tester
devtools::install()
devtools::test()

# Lancer une collecte depuis la racine du dépôt (consomme le quota SerpApi)
Sys.setenv(SERPAPI_KEY = "...")
kflight::collecter()
```

```sh
# Construire le tableau de bord
quarto render dashboard/index.qmd --output-dir ../_site
```

## Limites

- Les prix viennent de Google Flights, qui couvre la plupart des compagnies de la liste mais pas forcément tous les tarifs (vols intérieurs malgaches notamment). D'où les liens et le fichier de prix manuels.
- Avec 250 recherches par mois, toutes les combinaisons ne sont pas cherchées chaque jour : les combinaisons de `suivi_quotidien` le sont, les autres tournent.
- Pour un aller-retour, Google renvoie le prix total aller et retour mais le détail du vol aller seulement.
