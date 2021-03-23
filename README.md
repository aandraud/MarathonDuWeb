# MarathonDuWeb GROUPE 3

# Projet : 
Accompagner la clientèle pro du Crédit Agricole en sortie de confinement

# Intervenants : 
Soraya Tatem
Pascal Ruiz

# Contexte du projet : 
Le crédit agricole a été très impacté par la crise de la Covid-19 malgré les nombreuses aides proposées par le gouvernement. Les professionnels sont les plus touchés même si tous les domaines ne sont pas tous affectées de la même manière. L'année 2020 a cependeant été l'année record en création d'entreprise en augmentant de 4% par rapport à l'année précédente (livraison,t ransport etc...).
De nombreux profils sont présents dans la clientèle du Crédit Agricole avec plus d'un million de client. L'enjeu est d'avoir une très bonne connaissance de leurs clients. Cette connaissance passe par les données, notamment le chiffre d'affaire qui est directement lié au résultat net et à la santé de l'entreprise. 

# Objectifs : 
D'un point de vue data science : prédire la reprise d'activité de la clientèle professionnelle dans un contexte de sortie de confinement en mars 2021 afin de mettre en place un accompagnement bancaire personnalisé et adapté. Cela passe par des traitements de Data Science et des visualistations associées pour :
- Estimer le chiffre d'affaires 2020
- Prédire le chiffre d'affaires 2021
- Catégoriser les différentes situations et définir des pistes d'accompagnement bancaire adapté à chaque situation. 

D'un point de vue visualisation, l'objectif est d'élaborer une plateforme web à destination des professionnels dont l'intéret est de mettre à disposition une vision de notre positionnement sur le marché et de faire de l'acquisition digitale, mais d'également d'exposer nos résultats de traitement de données avec de la visualisation : degrés de difficultés des entreprises, faire un zoom par secteur d'activité etc... 

# Jeu de données : 
4 fichiers :
- MRTHN_CA_20210317 : fichier CSV qui contient les chiffres d'affaires des entreprises entre 2017 et 2021. Taille du fichier : 895 ko. Variables : YRMM_ARRT_CMPTBL, MT_CA, NB_J_DUR_EXRCC, SOURCE, IDENTR.
- MRTHN_DICTIONNAIRE_20210317 : fichier excel qui contient la défintion des noms des variables. Taille du fichier : 47 ko. Variables : TABLE, Nom de la variable, Libellé de la variable, Commentaire.
- MRTHN_ETABLISSEMENTS_20210317 : fichier CSV qui rescence l'ensemble des entreprises présentes dans les données. Taille du fichier : 5532 ko. Variables : TYPE_CC, MARCHE_CC, NB_ETS_CC, CD_FORM_JUR, CD_RGM_FISC, CD_ANC_CREA_ENTR, CD_EFFECTIF, MARCHE_PART, CD_ANC_EER, DEPT, TP_PTF, TP_SOCIETR, TP_DBTR, TP_INCDNT_BQR, TP_JEUNE_AGRI, CD_NAF, TYPE_ACTIVITE, SECTEUR_ACTIVITE, CD_SEGMENT, NOTE_BALE2, YRMM_1ER_PGE, YRMM_DER_PGE, ANC_DER_ENTRTN, LIB_METIER, ID_EDS_AGP, ID_EDS_SECTEUR, ID_EDS_DIRCO, TYPE_RL, NATU_LIEN_RL, CD_AGE_RL, IDGRPRISQ ,IDENTR ,IDPART.
- MRTHN_FLX_SLD_DAV_20210317 : fichier CSV qui contient tous les montants des flux (créditeurs, débiteurs...), montant du solde du compte courant, toutes les montants des transactions. Taille du fichier : 62152 ko. Variables : MOIS, NB_FLUX_CRED_CONF, MT_FLUX_CRED_CONF, NB_FLUX_CRED, MT_FLUX_CRED, NB_FLUX_DEB, MT_FLUX_DEB, MT_SLD_DAV, IDENTR, IDPART.

# Questions : 

Plateforme ->  de faire de l’acquisition digitale??
définir des pistes d'accompagnement bancaire adapté à chaque situation??
Site dédié uniquement aux clients?
Pourquoi NB_J_DUR_EXRCC >365 (va de 30 à 930)
Quelles colonnes nécessaire?
Présentation du site web? Dans la vidéo ? faire un lien?
Chiffre d'affaire -> Est_ce que c'est le début ou la fin des données?
Pour le chiffre d'affaire de 2019, on l'a déjà? Il faut faire des catégories? Chiffre d'affaire pour toute l'année de toutes les entreprises?
Chiffre d'affaire 2020, est-ce que c'est décalé? Du 1Er janvier 2019 au 31 décembre 2019


Lien Trello : https://trello.com/b/BzjhWhf6/sprint-1

