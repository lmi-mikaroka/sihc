# sihc
application shiny pour Suivi des informations Halieutiques sur les Crabes

l'application shiny peut etre accesible via:http://vps-36ad3311.vps.ovh.net/sihc/

1- Avant d'éxécuter l'application Shiny il faut d'abord il faut configurer le fichier .env

2- charger les données en éxécutant le script cron.R (Rscript cron.R) 

2- N'oubliez pas de changer le dossier de travail (working directory) en modifiant la première ligne de cron.R (setwd("lien_de_working_directory")) avant de l'éxécuter 

3- Vous pouvez maintenant lancer votre application Shiny en faisant un RunApp() 

4- Pour mettre à jour les données automatiquement à une période donnée, il suffit d'ajouter dans les tâches planifiées la commande Rscript cron.R (vous pouvez le faire manuellement aussi comme dans la première étape) 

5- voilà!
