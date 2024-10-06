
library(tidyverse)
library(glue)
library(readxl)

# Charger le fichier Excel contenant les informations des triathlons
data <- read_excel("tri/triathlon.xlsx", sheet = "Feuil2")

# Dossier contenant les images des athlètes
image_folder <- "img/profil_photo/"

# Pour chaque ligne du fichier Excel, générer un fichier .qmd
pwalk(data, function(nom, surnom, type, citation, photo_link, strava_link) {
  
  # Définir le nom de fichier avec le nom de la personne
  file_name <- paste0(nom, ".qmd")
  
  if (type == "ATH") {
    
    if (strava_link != "NA") {
      links_section <- glue("
  links:
    - icon: 'bi bi-strava'
      text: Strava
      href: '{strava_link}'
")
    } else {
      links_section <- ""
    }
    
  # Créer le contenu du fichier Quarto
  content <- glue("
  
---
title: {nom} ({surnom})
subtitle: {citation} 
image: '{image_folder}/{photo_link}'
toc: false
about:
  template: trestles
  {links_section}
---

## Performance de {nom}

```{{r, include = FALSE}}
# Charger le script externe contenant l'analyse
source('C:/Users/bbell/Desktop/mysport/function_profil_stat.R')

# Exécuter l'analyse pour {nom}
name <- '{nom}'
result <- triathlon_analysis(name)
```

-   **Nombre de triathlons** : `r name_print_df$numb_tri`

-   **Premier triathlon** : `r name_print_df$first_tri`

-   **Dernier triathlon** : `r name_print_df$last_tri`

-   **Plus long triathlon** : `r name_print_df$max_cat_tri`

-   **Meilleur performance** : `r name_print_df$best_class`

-   **BD en natation** : `r name_print_df$max_nat`

-   **MR en natation** : `r name_print_df$mean_nat` 

-   **PR en natation** : `r name_print_df$best_nat` 

-   **BD parcourue en cyclisme** : `r name_print_df$max_cyc`

-   **MR en cyclisme** : `r name_print_df$mean_cyc`

-   **PR en cyclisme** : `r name_print_df$best_cyc`

-   **BD en course à pied** : `r name_print_df$max_cap` 

-   **MR en course à pied** : `r name_print_df$mean_cap` 

-   **PR en course à pied** : `r name_print_df$best_cap` 

")
  
  } else {
    
    # Créer le contenu du fichier Quarto
    content <- glue("
  
---
title: {nom} ({surnom})
image: '{image_folder}/{photo_link}'
toc: false
about:
  template: jolla
---
")
    
  }
  
  # Écrire le contenu dans le fichier .qmd
  writeLines(content, file_name)
  
  # Retourner un message d'info
  message(glue("Fichier {file_name} généré avec succès."))
  
})
