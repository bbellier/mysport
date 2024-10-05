library(readxl)
library(glue)
library(tidyverse)

# Charger les données Excel
triathlon <- read_excel("tri/triathlon.xlsx") %>% 
  arrange(desc(Date)) %>%
  mutate(Course = paste0(Course, " ", year(Date))) 

# Extraire les noms uniques des courses
tri <- unique(triathlon$Course)

# Fonction pour générer une galerie d'images pour une course spécifique
generate_gallery <- function(course_name, folder, count, group_name) {
  gallery <- paste0("## ", course_name, "\n\n")
  
  # Ajouter la première image avec un width de 300px et un underscore dans le chemin
  gallery <- paste0(gallery, glue("![]({folder}/1.jpg){{group=\"{group_name}\" width=\"300px\"}}
                                  
                                  \n\n"))
  
  # Ajouter les images suivantes avec display:none et un underscore dans le chemin
  for (i in 2:count) {
    gallery <- paste0(gallery, glue("![]({folder}/{i}.jpg){{group=\"{group_name}\" style=\"display:none\"}}
                                    
                                    \n\n"))
  }
  
  return(gallery)
}

# Fonction pour générer un chemin de dossier en fonction du nom de la course
generate_folder_name <- function(course_name) {
  # Convertir en minuscules, remplacer les espaces par des underscores
  folder <- paste0("photo/", gsub(" ", "_", tolower(course_name)))
  return(folder)
}

# Fonction pour compter le nombre de fichiers d'images dans un dossier
count_images_in_folder <- function(folder) {
  # Lister les fichiers dans le dossier et filtrer uniquement les fichiers .jpg
  files <- list.files(folder, pattern = "\\.jpg$", full.names = TRUE)
  return(length(files))
}

# Générer dynamiquement la liste courses_list en fonction des noms de courses
courses_list <- list()

for (course_name in tri) {
  folder_name <- generate_folder_name(course_name)
  
  # Compter dynamiquement le nombre d'images dans le dossier
  if (dir.exists(folder_name)) {
    image_count <- count_images_in_folder(folder_name)
  } else {
    # Si le dossier n'existe pas, on peut garder le nombre d'images par défaut (ou 0)
    image_count <- default_image_count
  }
  
  # Ajouter l'entrée dans la liste courses_list
  courses_list[[course_name]] <- list(folder = folder_name, count = image_count)
}

# Afficher courses_list pour vérification
print(courses_list)


# Initialiser le contenu du fichier .qmd
content <- glue("---\ntitle: \"Photos des triathlons\"\nlightbox: true\noutput: html_document\n---\n\n")

# Parcourir chaque course et générer la galerie correspondante
for (course_name in tri) {
  if (course_name %in% names(courses_list)) {
    params <- courses_list[[course_name]]
    gallery_content <- generate_gallery(course_name, params$folder, params$count, gsub(" ", "_", tolower(course_name)))
    content <- paste0(content, gallery_content, "\n")
  }
}

# Nom du fichier de sortie
file_name <- "photo_gallery.qmd"

# Écrire le contenu dans le fichier .qmd
writeLines(content, file_name)

# Message de confirmation
cat("Le fichier", file_name, "a été généré avec succès.\n")
