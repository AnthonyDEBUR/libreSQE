library(LibreSQE)
library(openxlsx)
library(stringi) # pour comparer les noms de stations sans prendre en compte les accents

#ajouter tests cohérence NGL / NH4 / NO2 / NO3 / NTK
# voir ex Careil 2 2023 qd 1 pesticide commandé sur station prélevée à plusieurs dates et plusieurs pest rendus, on ne détecte pas les pest en trop

# ajouter tests sur valeurs numériques des paramètres environnementaux (pression atmo)


#####Fichier a tester #####
# file.choose()

##### SCDI Calendaire (04000003033) #####
# SCDI Calendaire 1
# fichier <-
#   "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-01\\QUESU3_SMBV_CHERE_DON-CAB_2024-01-26_0400003033.xml"
# bon_de_commande_id <- 2229

# SCDI Calendaire 2
# fichier <-
#   "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-02\\QUESU3_SMBV_CHERE_DON-CAB_2024-02-27_0400003033.xml"
# bon_de_commande_id <- 2296

# SCDI Calendaire 3
# fichier <-
#   "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-03\\QUESU3_SMBV_CHERE_DON-CAB_2024-03-28_0400003033.xml"
# bon_de_commande_id <- 2297

# SCDI Calendaire 3 bis
# fichier <-
#   "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-03\\QUESU3_SMBV_CHERE_DON-CAB_2024-03-29_0400003033.xml"
# bon_de_commande_id <- 2297

# SCDI Calendaire 4
# fichier <-
#   "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-04\\QUESU3_SMBV_CHERE_DON-CAB_2024-04-26_0400003033.xml"
# bon_de_commande_id <- 2298

# SCDI Calendaire 4 (part 2)
# fichier <-
#   "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-04\\QUESU3_SMBV_CHERE_DON-CAB_2024-04-25_0400003033.xml"
# bon_de_commande_id <- 2298

# SCDI Calendaire 5
# fichier <-
#   "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-05\\QUESU3_SMBV_CHERE_DON-CAB_2024-05-27_0400003033.xml"
# bon_de_commande_id <- 2299

# SCDI Calendaire 5 part 2
# fichier <-
#   "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-05\\QUESU3_SMBV_CHERE_DON-CAB_2024-05-29_0400003033.xml"
# bon_de_commande_id <- 2299

# SCDI Calendaire 6
# fichier <-
#   "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-06\\QUESU3_SMBV_CHERE_DON-CAB_2024-06-25_0400003033.xml"
# bon_de_commande_id <- 2300

# SCDI Calendaire 6 - part 2
# fichier <-
#   "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-06\\QUESU3_SMBV_CHERE_DON-CAB_2024-06-26_0400003033.xml"
# bon_de_commande_id <- 2300

# SCDI Calendaire 7
# fichier <-
#   "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-09\\QUESU3_SMBV_CHERE_DON-CAB_2024-09-25_0400003033.xml"
# bon_de_commande_id <- 2301

# SCDI Calendaire 8
# fichier <-
#   "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-10\\QUESU3_SMBV_CHERE_DON-CAB_2024-10-29_0400003033.xml"
# bon_de_commande_id <- 2302

# SCDI Calendaire 9 - part 1
# fichier <-
#   "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-11\\QUESU3_SMBV_CHERE_DON-CAB_2024-11-21_0400003033.xml"
# bon_de_commande_id <- 2303

# SCDI Calendaire 9 - part 2
# fichier <-
#   "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-11\\QUESU3_SMBV_CHERE_DON-CAB_2024-11-25_0400003033.xml"
# bon_de_commande_id <- 2303

# SCDI Calendaire 10 (décembre)
# fichier <-
#   "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-12\\QUESU3_SMBV_CHERE_DON-CAB_2024-12-27_0400003033.xml"
# bon_de_commande_id <- 2304

##### UGVE CALENDAIRE ( 0400003223 )#####
##### UGVE Calendaire 1 #####
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-01\\QUESU3_EPTB_VILAINE-CAB_2024-01-25_0400003223.xml"
# bon_de_commande_id <- 2228

##### UGVE Calendaire 1 - fichier 2 #####
# fichier <-
#   "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-01\\QUESU3_EPTB_VILAINE-CAB_2024-01-26_0400003223.xml"
# bon_de_commande_id <- 2228

##### UGVE Calendaire 1 - fichier 3 #####
# fichier <-
#   "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-01\\QUESU3_EPTB_VILAINE-CAB_2024-01-30_0400003223.xml"
# bon_de_commande_id <- 2228


##### UGVE Calendaire 2 #####
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-02\\QUESU3_EPTB_VILAINE-CAB_2024-02-21_0400003223.xml"
# bon_de_commande_id <- 2274

# UGVE Calendaire 2 bis
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-02\\QUESU3_EPTB_VILAINE-CAB_2024-02-22_0400003223.xml"
# bon_de_commande_id <- 2274

##### UGVE Calendaire 3 #####
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-03\\QUESU3_EPTB_VILAINE-CAB_2024-03-25_0400003223.xml"
# bon_de_commande_id <- 2269

# UGVE Calendaire 3 bis
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-03\\QUESU3_EPTB_VILAINE-CAB_2024-03-26_0400003223.xml"
# bon_de_commande_id <- 2269

# UGVE Calendaire 3 ter
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-03\\QUESU3_EPTB_VILAINE-CAB_2024-03-27_0400003223.xml"
# bon_de_commande_id <- 2269

##### UGVE Calendaire 4 #####
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-04\\QUESU3_EPTB_VILAINE-CAB_2024-04-22_0400003223.xml"
# bon_de_commande_id <- 2275

# UGVE Calendaire 4 part 2
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-04\\QUESU3_EPTB_VILAINE-CAB_2024-04-23_0400003223.xml"
# bon_de_commande_id <- 2275

# UGVE Calendaire 4 part 3
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-04\\QUESU3_EPTB_VILAINE-CAB_2024-04-24_0400003223.xml"
# bon_de_commande_id <- 2275

# UGVE Calendaire 4 part 4
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-04\\QUESU3_EPTB_VILAINE-CAB_2024-04-30_0400003223.xml"
# bon_de_commande_id <- 2275


# UGVE Calendaire 5
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-05\\QUESU3_EPTB_VILAINE-CAB_2024-05-27_0400003223.xml"
# bon_de_commande_id <- 2270

# UGVE Calendaire 5 (part. 2)
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-05\\QUESU3_EPTB_VILAINE-CAB_2024-05-28_0400003223.xml"
# bon_de_commande_id <- 2270

# UGVE Calendaire 5 (part. 3)
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-05\\QUESU3_EPTB_VILAINE-CAB_2024-05-29_0400003223.xml"
# bon_de_commande_id <- 2270

# UGVE Calendaire 6 - part 1
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-06\\QUESU3_EPTB_VILAINE-CAB_2024-06-19_0400003223.xml"
# bon_de_commande_id <- 2276

# UGVE Calendaire 6 - part 2
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-06\\QUESU3_EPTB_VILAINE-CAB_2024-06-27_0400003223.xml"
# bon_de_commande_id <- 2276

# UGVE Calendaire 6 - part 3
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-06\\QUESU3_EPTB_VILAINE-CAB_2024-06-28_0400003223.xml"
# bon_de_commande_id <- 2276

# UGVE Calendaire 6 - part 4
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-06\\QUESU3_EPTB_VILAINE-CAB_2024-06-24_0400003223.xml"
# bon_de_commande_id <- 2276

# UGVE Calendaire 6 - part 5
# fichier <-
#   "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-06\\QUESU3_EPTB_VILAINE-CAB_2024-06-25_0400003223.xml"
# bon_de_commande_id <- 2276

# UGVE Calendaire 6 - part 6
# fichier <-
#   "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-06\\QUESU3_EPTB_VILAINE-CAB_2024-06-26_0400003223.xml"
# bon_de_commande_id <- 2276

# UGVE Calendaire 7
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-07\\QUESU3_EPTB_VILAINE-CAB_2024-07-25_0400003223.xml"
# bon_de_commande_id <- 2271

# UGVE Calendaire 8
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-08\\QUESU3_EPTB_VILAINE-CAB_2024-08-27_0400003223.xml"
# bon_de_commande_id <- 2277

# UGVE Calendaire 9
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-09\\QUESU3_EPTB_VILAINE-CAB_2024-09-26_0400003223.xml"
# bon_de_commande_id <- 2272

# UGVE Calendaire 10
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-10\\QUESU3_EPTB_VILAINE-CAB_2024-10-28_0400003223.xml"
# bon_de_commande_id <- 2278

# UGVE Calendaire 11
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-11\\QUESU3_EPTB_VILAINE-CAB_2024-11-28_0400003223.xml"
# bon_de_commande_id <- 2273

# UGVE Calendaire 12 - part 1
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-12\\QUESU3_EPTB_VILAINE-CAB_2024-12-19_0400003223.xml"
# bon_de_commande_id <- 2279

# UGVE Calendaire 12 - part 2
# fichier <-
#   "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-12\\QUESU3_EPTB_VILAINE-CAB_2024-12-16_0400003223.xml"
# bon_de_commande_id <- 2279


##### UGVE CAPTAGES #####
# _UGVE-CAPTAGES_calend_1 (mars)
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-03\\QUESU3_EPTB_VILAINE-CAB_2024-03-26_0400003223.xml"
# bon_de_commande_id <- 2305

# _UGVE-CAPTAGES_calend_1 (mars) partie 2
# fichier <-
#   "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-03\\QUESU3_EPTB_VILAINE-CAB_2024-03-27_0400003223.xml"
# bon_de_commande_id <- 2305

# _UGVE-CAPTAGES_calend_2 (avril)
# fichier <-
#   "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-04\\QUESU3_EPTB_VILAINE-CAB_2024-04-23_0400003223.xml"
# bon_de_commande_id <- 2306

# _UGVE-CAPTAGES_calend_2 (avril - part 2)
# fichier <-
#   "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-04\\QUESU3_EPTB_VILAINE-CAB_2024-04-24_0400003223.xml"
# bon_de_commande_id <- 2306


# _UGVE-CAPTAGES_calend_3 (mai)
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-05\\QUESU3_EPTB_VILAINE-CAB_2024-05-27_0400003223.xml"
# bon_de_commande_id <- 2307

# _UGVE-CAPTAGES_calend_3 (mai) partie 2
# fichier <-
#   "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-05\\QUESU3_EPTB_VILAINE-CAB_2024-05-28_0400003223.xml"
# bon_de_commande_id <- 2307

# _UGVE-CAPTAGES_calend_4 (juin)
# fichier <-
#   "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-06\\QUESU3_EPTB_VILAINE-CAB_2024-06-24_0400003223.xml"
# bon_de_commande_id <- 2308

# _UGVE-CAPTAGES_calend_5 (juillet)
# fichier <-
#   "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-07\\QUESU3_EPTB_VILAINE-CAB_2024-07-24_0400003223.xml"
# bon_de_commande_id <- 2309

# _UGVE-CAPTAGES_calend_6 (aout)
# fichier <-
#   "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-08\\QUESU3_EPTB_VILAINE-CAB_2024-08-29_0400003223.xml"
# bon_de_commande_id <- 2310

# _UGVE-CAPTAGES_calend_7 (septembre)
# fichier <-
#   "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-09\\QUESU3_EPTB_VILAINE-CAB_2024-09-24_0400003223.xml"
# bon_de_commande_id <- 2311

# _UGVE-CAPTAGES_calend_8 (octobre)
# fichier <-
#   "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-10\\QUESU3_EPTB_VILAINE-CAB_2024-10-29_0400003223.xml"
# bon_de_commande_id <- 2312

# _UGVE-CAPTAGES_calend_9 (novembre)
# fichier <-
#   "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-11\\QUESU3_EPTB_VILAINE-CAB_2024-11-26_0400003223.xml"
# bon_de_commande_id <- 2313

# _UGVE-CAPTAGES_calend_10 (décembre)
# fichier <-
#   "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-12\\QUESU3_EPTB_VILAINE-CAB_2024-12-16_0400003223.xml"
# bon_de_commande_id <- 2314

##### UGVE Pluie (régie) #####
##### UGVE Pluie 1 régie #####
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-01\\QUESU3_EPTB_VILAINE-CAB_2024-01-18_0400003223.xml"
# bon_de_commande_id <- 2288

##### UGVE Pluie 1 régie #####
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-01\\QUESU3_EPTB_VILAINE-CAB_2024-01-18_0400003223.xml"
# bon_de_commande_id <- 2288

##### UGVE Pluie 2 régie (mars) #####
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-03\\QUESU3_EPTB_VILAINE-CAB_2024-03-29_0400003223.xml"
# bon_de_commande_id <- 2289

##### UGVE Pluie 3 régie (juin) #####
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-06\\QUESU3_EPTB_VILAINE-CAB_2024-06-18_0400003223.xml"
# bon_de_commande_id <- 2290

##### UGVE Pluie 4 régie (septembre) #####
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-09\\QUESU3_EPTB_VILAINE-CAB_2024-09-26_0400003223.xml"
# bon_de_commande_id <- 2291

##### UGVE Pluie 5 régie (octobre) #####
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-10\\QUESU3_EPTB_VILAINE-CAB_2024-10-10_0400003223.xml"
# bon_de_commande_id <- 2292

##### UGVE Pluie 6 régie (novembre) #####
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-11\\QUESU3_EPTB_VILAINE-CAB_2024-11-28_0400003223.xml"
# bon_de_commande_id <- 2293


##### UGVE Pluie (externalisé) #####
##### UGVE Pluie 1 externalisé #####
# fichier <-
#   "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-01\\QUESU3_EPTB_VILAINE-CAB_2024-01-18_0400003223.xml"
# bon_de_commande_id <- 2280

##### UGVE Pluie 2 externalisé #####
# fichier <-
#   "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-06\\QUESU3_EPTB_VILAINE-CAB_2024-06-20_0400003223.xml"
# bon_de_commande_id <- 2281

##### UGVE Pluie 3 externalisé (septembre) #####
# fichier <-
#   "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-09\\QUESU3_EPTB_VILAINE-CAB_2024-09-27_0400003223.xml"
# bon_de_commande_id <- 2282

##### UGVE Pluie 4 externalisé (octobre) #####
# fichier <-
#   "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-10\\QUESU3_EPTB_VILAINE-CAB_2024-10-10_0400003223.xml"
# bon_de_commande_id <- 2283

##### UGVE Pluie 5 externalisé (novembre) #####
# fichier <-
#   "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-11\\QUESU3_EPTB_VILAINE-CAB_2024-11-26_0400003223.xml"
# bon_de_commande_id <- 2284


##### UGVO CALENDAIRE ( 0400003224 )#####
# UGVO Calendaire 1
# fichier <-
#   "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-01\\QUESU3_EPTB_VILAINE-CAB_2024-01-29_0400003224.xml"
# bon_de_commande_id <- 2226

# UGVO Calendaire 1 bis
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-01\\QUESU3_EPTB_VILAINE-CAB_2024-01-30_0400003224.xml"
# bon_de_commande_id <- 2226

# UGVO Calendaire 2
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-02\\QUESU3_EPTB_VILAINE-CAB_2024-02-20_0400003224.xml"
# bon_de_commande_id <- 2243

# UGVO Calendaire 2 bis
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-02\\QUESU3_EPTB_VILAINE-CAB_2024-02-21_0400003224.xml"
# bon_de_commande_id <- 2243

# UGVO Calendaire 3
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-03\\QUESU3_EPTB_VILAINE-CAB_2024-03-26_0400003224.xml"
# bon_de_commande_id <- 2230

# UGVO Calendaire 4
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-04\\QUESU3_EPTB_VILAINE-CAB_2024-04-22_0400003224.xml"
# bon_de_commande_id <- 2244

# UGVO Calendaire 5 (part. 1)
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-05\\QUESU3_EPTB_VILAINE-CAB_2024-05-30_0400003224.xml"
# bon_de_commande_id <- 2231

# UGVO Calendaire 5 (part. 2)
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-05\\QUESU3_EPTB_VILAINE-CAB_2024-05-31_0400003224.xml"
# bon_de_commande_id <- 2231

# UGVO Calendaire 6
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-06\\QUESU3_EPTB_VILAINE-CAB_2024-06-24_0400003224.xml"
# bon_de_commande_id <- 2245

# UGVO Calendaire 7
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-07\\QUESU3_EPTB_VILAINE-CAB_2024-07-23_0400003224.xml"
# bon_de_commande_id <- 2232

# UGVO Calendaire 8
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-08\\QUESU3_EPTB_VILAINE-CAB_2024-08-26_0400003224.xml"
# bon_de_commande_id <- 2246

# UGVO Calendaire 9 - part 1
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-09\\QUESU3_EPTB_VILAINE-CAB_2024-09-24_0400003224.xml"
# bon_de_commande_id <- 2233

# UGVO Calendaire 9 - part 2
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-09\\QUESU3_EPTB_VILAINE-CAB_2024-09-25_0400003224.xml"
# bon_de_commande_id <- 2233

# UGVO Calendaire 10
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-10\\QUESU3_EPTB_VILAINE-CAB_2024-10-30_0400003224.xml"
# bon_de_commande_id <- 2247

# UGVO Calendaire 11
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-11\\QUESU3_EPTB_VILAINE-CAB_2024-11-25_0400003224.xml"
# bon_de_commande_id <- 2234

# UGVO Calendaire 12
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-12\\QUESU3_EPTB_VILAINE-CAB_2024-12-23_0400003224.xml"
# bon_de_commande_id <- 2248

##### UGVO Careil calendaire #####
##### UGVO Careil calendaire 1 #####
# fichier <-
#   "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-01\\QUESU3_EPTB_VILAINE-CAB_2024-01-29_0400003224.xml"
# bon_de_commande_id <- 2257

##### UGVO Careil calendaire 2 #####
# fichier <-
#   "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-02\\QUESU3_EPTB_VILAINE-CAB_2024-02-20_0400003224 - Careil.xml"
# bon_de_commande_id <- 2258

##### UGVO Careil calendaire 3 #####
# fichier <-
#   "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-03\\QUESU3_EPTB_VILAINE-CAB_2024-03-26_0400003224_CAREIL.xml"
# bon_de_commande_id <- 2259

##### UGVO Careil calendaire 4 #####
# fichier <-
#   "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-04\\QUESU3_EPTB_VILAINE-CAB_2024-04-22_0400003224_CAREIL.xml"
# bon_de_commande_id <- 2260

##### UGVO Careil calendaire 5 #####
# fichier <-
#   "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-05\\QUESU3_EPTB_VILAINE-CAB_2024-05-30_0400003224_CAREIL.xml"
# bon_de_commande_id <- 2261

##### UGVO Careil calendaire 7 #####
# fichier <-
#   "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-07\\QUESU3_EPTB_VILAINE-CAB_2024-07-23_0400003224_CAREIL.xml"
# bon_de_commande_id <- 2263


##### UGVO Careil calendaire 10 #####
# fichier <-
#   "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-10\\QUESU3_EPTB_VILAINE-CAB_2024-10-30_0400003224_CAREIL.xml"
# bon_de_commande_id <- 2266

##### UGVO Careil calendaire 11
# fichier <-
#   "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-11\\QUESU3_EPTB_VILAINE-CAB_2024-11-25_0400003224_CAREIL.xml"
# bon_de_commande_id <- 2267

##### UGVO Careil calendaire 12
# fichier <-
#   "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-12\\QUESU3_EPTB_VILAINE-CAB_2024-12-23_0400003224_CAREIL.xml"
# bon_de_commande_id <- 2268

##### UGVO CAREIL PLUIE #####
# UGVO CAREIL PLUIE 1
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-02\\QUESU3_EPTB_VILAINE-CAB_2024-02-08_0400003224.xml"
# bon_de_commande_id <- 2249

# UGVO CAREIL PLUIE 2
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-03\\QUESU3_EPTB_VILAINE-CAB_2024-03-28_0400003224_CAREIL.xml"
# bon_de_commande_id <- 2250

# UGVO CAREIL PLUIE 3
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-05\\QUESU3_EPTB_VILAINE-CAB_2024-05-22_0400003224_CAREIL.xml"
# bon_de_commande_id <- 2251


# UGVO CAREIL PLUIE 4
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-00\\QUESU3_EPTB_VILAINE-CAB_2024-03-28_0400003224.xml"
# bon_de_commande_id <- 2252

# UGVO CAREIL PLUIE 5 (octobre)
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-10\\QUESU3_EPTB_VILAINE-CAB_2024-10-09_0400003224_CAREIL.xml"
# bon_de_commande_id <- 2253

##### UGVO PLUIE #####
# UGVO PLUIE 1
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-02\\QUESU3_EPTB_VILAINE-CAB_2024-02-22_0400003224.xml"
# bon_de_commande_id <- 2235

# UGVO PLUIE 2 (mars)
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-03\\QUESU3_EPTB_VILAINE-CAB_2024-03-28_0400003224.xml"
# bon_de_commande_id <- 2236

# UGVO PLUIE 3 (juin)
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-06\\QUESU3_EPTB_VILAINE-CAB_2024-06-20_0400003224.xml"
# bon_de_commande_id <- 2237

# UGVO PLUIE 4 (juillet)
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-07\\QUESU3_EPTB_VILAINE-CAB_2024-07-12_0400003224.xml"
# bon_de_commande_id <- 2238

# UGVO PLUIE 5 (septembre)
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-09\\QUESU3_EPTB_VILAINE-CAB_2024-09-26_0400003224.xml"
# bon_de_commande_id <- 2239

# UGVO PLUIE 6 (octobre) - part 1
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-10\\QUESU3_EPTB_VILAINE-CAB_2024-10-09_0400003224.xml"
# bon_de_commande_id <- 2240

# UGVO PLUIE 6 (octobre) - part 2
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-10\\QUESU3_EPTB_VILAINE-CAB_2024-10-10_0400003224.xml"
# bon_de_commande_id <- 2240

# UGVO PLUIE 7 (décembre)
# fichier <-
#  "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-12\\QUESU3_EPTB_VILAINE-CAB_2024-12-09_0400003224.xml"
# bon_de_commande_id <- 2241


##### UGVA CALENDAIRE 04000003185 #####
# UGVA Calendaire 1
# fichier <-
#   "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-01\\QUESU3_EPTB_VILAINE-CAB_2024-01-31_0400003185.xml"
# bon_de_commande_id <- 2227

# UGVA Calendaire 2
# fichier <-
#   "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-03\\QUESU3_EPTB_VILAINE-CAB_2024-03-11_0400003185.xml"
# bon_de_commande_id <- 2315

# UGVA Calendaire 3
# fichier <-
#   "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-04\\QUESU3_EPTB_VILAINE-CAB_2024-04-17_0400003185.xml"
# bon_de_commande_id <- 2316

# UGVA Calendaire 4
# fichier <-
#   "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-05\\QUESU3_EPTB_VILAINE-CAB_2024-05-22_0400003185.xml"
# bon_de_commande_id <- 2317

# UGVA Calendaire 5
# fichier <-
#   "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-06\\QUESU3_EPTB_VILAINE-CAB_2024-06-20_0400003185.xml"
# bon_de_commande_id <- 2318

# UGVA Calendaire 6
# fichier <-
#   "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-07\\QUESU3_EPTB_VILAINE-CAB_2024-07-17_0400003185.xml"
# bon_de_commande_id <- 2319

# UGVA Calendaire 7(septembre)
# fichier <-
#   "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-09\\QUESU3_EPTB_VILAINE-CAB_2024-09-11_0400003185.xml"
# bon_de_commande_id <- 2320

# UGVA Calendaire 8
# fichier <-
#   "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-10\\QUESU3_EPTB_VILAINE-CAB_2024-10-16_0400003185.xml"
# bon_de_commande_id <- 2321

# UGVA Calendaire 9
# fichier <-
#   "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-11\\QUESU3_EPTB_VILAINE-CAB_2024-11-13_0400003185.xml"
# bon_de_commande_id <- 2322

# UGVA Calendaire 10
# fichier <-
#   "C:\\Users\\anthony.deburghrave\\OneDrive - EPTB Vilaine\\Documents\\suivis EPTB\\2024\\7_livrables\\2024-12\\QUESU3_EPTB_VILAINE-CAB_2024-12-17_0400003185.xml"
# bon_de_commande_id <- 2323

##### Connexion bdd #####
connexion <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = "libresqe",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "postgres"
)

##### Chargement données de référence #####
table_stat_analyses <-
  readRDS(
    "C:/workspace/LibreSQE/dev/prototype_interface_libreSQE/data/table_stat_analyses.rds"
  )
table_stat_analyses_toutes_staq <-
  readRDS(
    "C:/workspace/LibreSQE/dev/prototype_interface_libreSQE/data/table_stat_analyses_toutes_staq.rds"
  )

##### chargement des analyses attendues pour le bon de commande #####
# Écrire une requête SQL qui utilise la clause IN pour filtrer les résultats
requete <-
  paste(
    "SELECT * FROM sqe.t_resultatanalyse_rea WHERE res_bco_id IN (",
    bon_de_commande_id,
    ")",
    sep = ""
  )

# Exécuter la requête en utilisant la connexion à la base de données
analyses_attendues <- DBI::dbGetQuery(connexion, requete)

analyses_attendues <- analyses_attendues %>%
  subset(res_stm_cdstationmesureinterne != "sans_objet")

analyses_attendues$nomparametre <- func_ajoute_nom_sandre(connexion,
                                                          code = analyses_attendues$rea_par_cdparametre,
                                                          out = "nom_long_parametre")

analyses_attendues$nomfraction <- func_ajoute_nom_sandre(connexion,
                                                         code = analyses_attendues$rea_cdfractionanalysee,
                                                         out = "nom_fraction")

analyses_attendues$nompreleveur <- func_ajoute_nom_sandre(connexion,
                                                          code = analyses_attendues$rea_cdpreleveur,
                                                          out = "nom_intervenant")

analyses_attendues$nomlabo <- func_ajoute_nom_sandre(connexion,
                                                     code = analyses_attendues$rea_cdlaboratoire,
                                                     out = "nom_intervenant")

analyses_attendues$insitu <- func_ajoute_nom_sandre(connexion,
                                                     code = analyses_attendues$rea_cdlaboratoire,
                                                     out = "insitu")

##### référence bon de commande #####
# Écrire une requête SQL qui utilise la clause IN pour filtrer les résultats
requete <-
  paste(
    "SELECT bco_refcommande FROM sqe.t_boncommande_bco WHERE bco_id IN (",
    bon_de_commande_id,
    ")",
    sep = ""
  )

# Exécuter la requête en utilisant la connexion à la base de données
ref_bon_de_commande <- DBI::dbGetQuery(connexion, requete)


##### initialisation du rapport d'import #####
Rapport <- list()

##### Import et mise en forme du fichier à tester #####
test <- func_importe_quesu(fichier)
Analyses <- test$Analyses
Stations <- Analyses %>%
  dplyr::select(CdStationMesureEauxSurface, LbStationMesureEauxSurface) %>%
  unique()
Res_env <- test$Res_env
Echantillon <- test$Echantillon
Operation <- test$Operation
rm(test)


##### Synthèse rapport #####
Rapport$fichier <- fichier
Rapport$bon_de_commande <- ref_bon_de_commande
Rapport$nb_lignes <- data.frame(c("Analyses", "Stations", "ResEnv"),
                                c(nrow(Analyses), nrow(Stations), nrow(Res_env)))


###### conversion des données ######
# Analyses
Analyses <- dplyr::rename(
  Analyses,
  cdstationmesureinterne = CdStationMesureEauxSurface,
  codeprel = CdPrelevement,
  dateprel = DatePrel,
  cdsupport = CdSupport,
  cdfractionanalysee = CdFractionAnalysee,
  heureprel = HeurePrel,
  dateana = DateAna,
  heureana = HeureAna,
  cdparametre = CdParametre,
  rsana = RsAna,
  cdunitemesure = CdUniteMesure,
  cdrqana = CdRqAna,
  cdinsituana = CdInsituAna,
  profondeurpre = ProfondeurPrel,
  ldana = LDAna,
  lqana = LQAna,
  lsana = LSAna,
  cdmetfractionnement = CdMetFractionnement,
  cdmethode = CdMethode,
  rdtextraction = RdtExtraction,
  cdmethodeextraction = CdMethodeExtraction,
  cdaccreana = CdAccreAna,
  agreana = AgreAna,
  incertitude = IncertAna,
  commentairesana = CommentairesAna,
  cdrdd = CdRdd,
  cdpreleveur = CdPreleveur,
  cdlaboratoire = CdLaboratoire,
  LbParametre = LbParametre,
  LbFraction = LbFractionAnalysee,
  LbSupport = LbSupport,
  LbMethode = LbMethode,
  SymUnite = LbUniteMesure,
  cdproducteur = CdProducteur
)


Analyses$rsana <- as.numeric(Analyses$rsana)
Analyses$lqana <- as.numeric(Analyses$lqana)
Analyses$lsana <- as.numeric(Analyses$lsana)
Analyses$ldana <- as.numeric(Analyses$ldana)
Analyses$incertitude <- as.numeric(Analyses$incertitude)



# Ajout des noms paramètres, fraction, symboles unités, nom labo, noms Rdd
Analyses$nomparametre <-
  func_ajoute_nom_sandre(connexion = connexion,
                         code = Analyses$cdparametre,
                         out = "nom_parametre")

Analyses$nomlongparametre <-
  func_ajoute_nom_sandre(connexion = connexion,
                         code = Analyses$cdparametre,
                         out = "nom_long_parametre")

Analyses$unite <- func_ajoute_nom_sandre(connexion = connexion,
                                         code = Analyses$cdunitemesure,
                                         out = "nom_unite")

Analyses$nomfraction <- func_ajoute_nom_sandre(
  connexion = connexion,
  code = Analyses$cdfractionanalysee,
  out = "nom_fraction"
)

Analyses$nomlabo <- func_ajoute_nom_sandre(connexion = connexion,
                                           code = Analyses$cdlaboratoire,
                                           out = "nom_long_intervenant")

Analyses$nompreleveur <-
  func_ajoute_nom_sandre(connexion = connexion,
                         code = Analyses$cdpreleveur,
                         out = "nom_long_intervenant")

Analyses$insitu <- func_ajoute_nom_sandre(connexion = connexion,
                                          code = Analyses$cdinsituana,
                                          out = "insitu")

Analyses$station <- func_ajoute_nom_sandre(
  connexion = connexion,
  code = Analyses$cdstationmesureinterne,
  out = "nom_station"
)


func_noms_codes <- function(codesrdd_vecteur) {
  # Divise chaque élément du vecteur en entrée en un sous-vecteur de codes
  codes_separe <- strsplit(codesrdd_vecteur, "/")

  # Applique la fonction func_ajoute_nom_sandre à chaque sous-vecteur de codes
  noms_codes <-
    lapply(codes_separe, function(x)
      func_ajoute_nom_sandre(
        connexion = connexion,
        code =
          x,
        out =
          "nom_rdd"
      ))

  # Concatène les noms de code séparés par des slashs à nouveau
  noms_codes_concatenes <- sapply(noms_codes, paste, collapse = "/")

  return(noms_codes_concatenes)
}

Analyses$nomrdd <- func_noms_codes(Analyses$cdrdd)


# Res_env
Res_env <- dplyr::rename(
  Res_env,
  cdstationmesureinterne = CdStationMesureEauxSurface,
  codeprel = CdPrelevement,
  dateprel = DateParEnv,
  cdparametre = CdParametreEnv,
  rsparenv = RsParEnv,
  cdunitemesure = CdUniteMesure,
  cdrqparenv = CdRqParEn,
  dateparenv = DateParEnv,
  met_code = CdMethodeParEnv,
  cdpreleveur = CdPreleveur,
  cdproducteur = CdProducteur,
  LbParametre = LbParametreEnv
)

##### création tableau intervenants #####
Intervenants_anal<-Analyses%>%
  dplyr::select(cdproducteur,
                LbProducteur,
                cdpreleveur,
                LbPreleveur,
                cdlaboratoire,
                LbLaboratoire)%>%
  unique()

# Création des trois data.frames en regroupant les colonnes
df_producteur <- Intervenants_anal %>%
  dplyr::select(cdproducteur, LbProducteur) %>%
  dplyr::rename(CdIntervenant = cdproducteur, NomIntervenant = LbProducteur) %>%
  dplyr::mutate(role = "producteur")

df_preleveur <- Intervenants_anal %>%
  dplyr::select(cdpreleveur, LbPreleveur) %>%
  dplyr::rename(CdIntervenant = cdpreleveur, NomIntervenant = LbPreleveur) %>%
  dplyr::mutate(role = "preleveur")

df_laboratoire <- Intervenants_anal %>%
  dplyr::select(cdlaboratoire, LbLaboratoire) %>%
  dplyr::rename(CdIntervenant = cdlaboratoire, NomIntervenant = LbLaboratoire) %>%
  dplyr::mutate(role = "laboratoire")

# Empilement des trois data.frames
Intervenants <- dplyr::bind_rows(df_producteur, df_preleveur, df_laboratoire)

rm(df_producteur, df_preleveur, df_laboratoire, Intervenants_anal)

Intervenants_Operation<-Operation%>%
  dplyr::select(CdProducteur,
                LbProducteur,
                CdPreleveur,
                LbPreleveur)%>%
  unique()

df_producteur <- Intervenants_Operation %>%
  dplyr::select(CdProducteur, LbProducteur) %>%
  dplyr::rename(CdIntervenant = CdProducteur, NomIntervenant = LbProducteur) %>%
  dplyr::mutate(role = "producteur")

df_preleveur <- Intervenants_Operation %>%
  dplyr::select(CdPreleveur, LbPreleveur) %>%
  dplyr::rename(CdIntervenant = CdPreleveur, NomIntervenant = LbPreleveur) %>%
  dplyr::mutate(role = "preleveur")

# Empilement des data.frames
Intervenants <- dplyr::bind_rows(df_producteur, df_preleveur, Intervenants)%>%unique()
rm(df_producteur, df_preleveur, Intervenants_Operation)




##### test cohérence code / nom station #####
# on ne tient pas compte de la casse ni des accents

normalize_string <- function(string) {
  string %>%
    tolower() %>%  # Convertir en minuscules
    stringi::stri_trans_general("Latin-ASCII") %>%  # Supprimer les accents
    gsub("-", "", .)  # Supprimer les tirets
}

Stations$LbStationMesureEauxSurface_attendu <-
  func_ajoute_nom_sandre(connexion ,
                         code = Stations$CdStationMesureEauxSurface,
                         out = "nom_station")
Stations$LbStationConforme <-
  ifelse(
    normalize_string(Stations$LbStationMesureEauxSurface) == normalize_string(Stations$LbStationMesureEauxSurface_attendu),
    TRUE,
    FALSE
  )
Stations <- dplyr::rename(
  Stations,
  cdstationmesureinterne = CdStationMesureEauxSurface,
  rendu_dans_xml = LbStationMesureEauxSurface,
  attendu = LbStationMesureEauxSurface_attendu
)


Rapport$nom_cd_station <- unique(
  Stations %>%
    subset(
      LbStationConforme == FALSE &
        !is.na(rendu_dans_xml) & rendu_dans_xml != ""
    ) %>%
    dplyr::select(cdstationmesureinterne,
                  rendu_dans_xml,
                  attendu)
)

##### test de renseignement des codes préleveurs, code laboratoire et code producteur #####
CdProducteurManquant <- Analyses %>%
  subset(is.na(cdproducteur) | cdproducteur=="")

if (nrow(CdProducteurManquant) > 0) {
  Rapport$CdProducteurManquant <- CdProducteurManquant
}

CdPreleveurManquant <- Analyses %>%
  subset(is.na(cdpreleveur) | cdpreleveur=="")

if (nrow(CdPreleveurManquant) > 0) {
  Rapport$CdPreleveurManquant <- CdPreleveurManquant
}

CdLaboManquant <- Analyses %>%
  subset(is.na(cdlaboratoire) | cdlaboratoire=="")

if (nrow(CdLaboManquant) > 0) {
  Rapport$CdLaboManquant <- CdLaboManquant
}



##### test du bon renseignement de la LqAna (sauf T° réception échantillon) #####
LqManquante <- Analyses %>%
  subset(cdrqana %in% c("1", "10", "7") &
           !cdparametre == "8727" &
           is.na(lqana) & !cdinsituana == "1")

if (nrow(LqManquante) > 0) {
  Rapport$LqManquante <- LqManquante
}

##### test du bon renseignement de la RsAna #####
RsAnaManquants <- Analyses %>%
  subset(cdrqana %in% c("1", "10", "7") & is.na(rsana))

if (nrow(RsAnaManquants) > 0) {
  Rapport$RsAnaManquants <- RsAnaManquants
}


##### test cohérence code / nom paramètre (analyses et Res_enc) #####
#####Analyses #####
Parametres <- Analyses %>%
  dplyr::select(cdparametre, LbParametre) %>%
  dplyr::distinct()
cdparametre <-
  paste0("'", unique(Parametres$cdparametre), "'", collapse = ", ")
requete <-
  paste(
    "SELECT * FROM refer.tr_parametre_par WHERE par_cdparametre IN (",
    cdparametre,
    ")",
    sep = ""
  )

refer_param <- DBI::dbGetQuery(connexion, requete)
Parametres <-
  dplyr::left_join(Parametres,
                   refer_param,
                   by = c("cdparametre" = "par_cdparametre"))
Parametres$LbParametreConforme <-
  ifelse(
    Parametres$LbParametre == Parametres$par_nomparametre |
      Parametres$LbParametre == Parametres$par_nomcourt,
    TRUE,
    FALSE
  )

Parametres <- dplyr::rename(
  Parametres,
  rendu_dans_xml = LbParametre,
  attendu = par_nomparametre,
  nom_court_attendu = par_nomcourt,
  statut_parametre_dans_SANDRE = par_statutparametre
)


Rapport$nom_cd_parametre_analyses <- Parametres %>%
  subset((
    LbParametreConforme == FALSE &
      !is.na(rendu_dans_xml) &
      rendu_dans_xml != ""
  ) |
    statut_parametre_dans_SANDRE != "Validé"
  ) %>%
  dplyr::select(
    cdparametre,
    rendu_dans_xml,
    attendu,
    nom_court_attendu,
    statut_parametre_dans_SANDRE
  )

#### Res_env #####
Parametres <- Res_env %>%
  dplyr::select(cdparametre, LbParametre) %>%
  dplyr::distinct()
cdparametre <-
  paste0("'", unique(Parametres$cdparametre), "'", collapse = ", ")
requete <-
  paste(
    "SELECT * FROM refer.tr_parametre_par WHERE par_cdparametre IN (",
    cdparametre,
    ")",
    sep = ""
  )

refer_param <- DBI::dbGetQuery(connexion, requete)
Parametres <-
  dplyr::left_join(Parametres,
                   refer_param,
                   by = c("cdparametre" = "par_cdparametre"))
Parametres$LbParametreConforme <-
  ifelse(
    Parametres$LbParametre == Parametres$par_nomparametre |
      Parametres$LbParametre == Parametres$par_nomcourt,
    TRUE,
    FALSE
  )

Parametres <- dplyr::rename(
  Parametres,
  rendu_dans_xml = LbParametre,
  attendu = par_nomparametre,
  statut_parametre_dans_SANDRE = par_statutparametre
)


Rapport$nom_cd_parametre_res_env <- Parametres %>%
  subset((
    LbParametreConforme == FALSE &
      !is.na(rendu_dans_xml) &
      rendu_dans_xml != ""
  ) |
    statut_parametre_dans_SANDRE != "Validé"
  ) %>%
  dplyr::select(cdparametre,
                rendu_dans_xml,
                attendu,
                statut_parametre_dans_SANDRE)



##### test cohérence code fraction / nom fraction #####
#####Analyses #####
Fraction <- Analyses %>%
  dplyr::select(cdfractionanalysee, LbFraction) %>%
  dplyr::distinct()
cdfraction <-
  paste0("'", unique(Fraction$cdfractionanalysee), "'", collapse = ", ")
requete <-
  paste("SELECT * FROM refer.tr_fraction_fra WHERE fra_codefraction IN (",
        cdfraction,
        ")",
        sep = "")

refer_frac <- DBI::dbGetQuery(connexion, requete)
Fraction <-
  dplyr::left_join(Fraction,
                   refer_frac,
                   by = c("cdfractionanalysee" = "fra_codefraction"))
Fraction$LbFractionConforme <-
  ifelse(Fraction$LbFraction == Fraction$fra_nomfraction,
         TRUE,
         FALSE)

Fraction <- dplyr::rename(Fraction,
                          rendu_dans_xml = LbFraction,
                          attendu = fra_nomfraction)


Rapport$nom_cd_fraction_analyses <- Fraction %>%
  subset(LbFractionConforme == FALSE &
           !is.na(rendu_dans_xml) &
           rendu_dans_xml != "")

##### test cohérence code unité / nom unité #####
Unites <- Analyses %>%
  dplyr::select(cdunitemesure, SymUnite) %>%
  dplyr::distinct()
cdunitemesure <-
  paste0("'", unique(Unites$cdunitemesure), "'", collapse = ", ")
requete <-
  paste(
    "SELECT * FROM refer.tr_uniteparametre_uni WHERE uni_codesandreunite IN (",
    cdunitemesure,
    ")",
    sep = ""
  )

refer_unit <- DBI::dbGetQuery(connexion, requete)
Unites <-
  dplyr::left_join(Unites,
                   refer_unit,
                   by = c("cdunitemesure" = "uni_codesandreunite"))
Unites$LbUniteConforme <-
  ifelse(Unites$SymUnite == Unites$uni_symbole,
         TRUE,
         FALSE)

Unites <- dplyr::rename(
  Unites,
  Lb_rendu_dans_xml = SymUnite,
  Lb_attendu = uni_symbole
)


Rapport$nom_cd_unites_analyses <- Unites %>%
  subset((
    LbUniteConforme == FALSE &
      !is.na(Lb_rendu_dans_xml) &
      Lb_rendu_dans_xml != ""
  )
  ) %>%
  dplyr::select(cdunitemesure,
                Lb_rendu_dans_xml,
                Lb_attendu)


##### test cohérence code / nom intervenants #####
cdintervenants <-
  paste0("'", unique(Intervenants$CdIntervenant), "'", collapse = ", ")
requete <-
  paste(
    "SELECT * FROM refer.tr_intervenantsandre_isa WHERE isa_codesandre IN (",
    cdintervenants,
    ")",
    sep = ""
  )

refer_interv <- DBI::dbGetQuery(connexion, requete)
Intervenants <-
  dplyr::left_join(Intervenants,
                   refer_interv,
                   by = c("CdIntervenant" = "isa_codesandre"))
Intervenants$NomIntervenantConforme <-
  ifelse(Intervenants$NomIntervenant == Intervenants$isa_nom,
         TRUE,
         FALSE)

Intervenants <- dplyr::rename(
  Intervenants,
  Nom_rendu_dans_xml = NomIntervenant,
  Nom_attendu = isa_nom
)


Rapport$nom_mnemo_intervenants <- Intervenants %>%
  subset((
    NomIntervenantConforme == FALSE &
      !is.na(Nom_rendu_dans_xml) &
      Nom_rendu_dans_xml != ""
  )
  ) %>%
  dplyr::select(CdIntervenant,
                Nom_rendu_dans_xml,
                Nom_attendu)

##### test cohérence format dates + date analyse >= date prélèvement (idem date heure) #####
Analyses$dateprel_date<-as.Date(Analyses$dateprel, format="%Y-%m-%d")
Analyses$dateana_date<-as.Date(Analyses$dateana, format="%Y-%m-%d")
Analyses$heureprel_date<-as.POSIXct(Analyses$heureprel, format="%Y-%m-%dT%H:%M:%S")
Analyses$heureana_date<-as.POSIXct(Analyses$heureana, format="%Y-%m-%dT%H:%M:%S")

Rapport$dateprel_format <- Analyses[is.na(Analyses$dateprel_date),]%>%dplyr::select("cdstationmesureinterne",
                                                                             "LbStationMesureEauxSurface",
                                                                             "dateprel")%>%unique()
Rapport$dateana_format <- Analyses[is.na(Analyses$dateana_date),]
Rapport$heureprel_format <- Analyses[is.na(Analyses$heureprel_date),]%>%dplyr::select("cdstationmesureinterne",
                                                                                      "LbStationMesureEauxSurface",
                                                                                      "heureprel")%>%unique()
Rapport$heureana_format <- Analyses[is.na(Analyses$heureana_date),]
Rapport$incoherence_dates <- Analyses[Analyses$dateprel_date>Analyses$dateana_date,]
Rapport$incoherence_heures <- Analyses[Analyses$heureprel_date>Analyses$heureana_date,]

Res_env$dateparenv_date<-as.Date(Res_env$dateparenv, format="%Y-%m-%d")
Res_env$heureparenv_date<-as.POSIXct(Res_env$HeureParEnv, format="%Y-%m-%dT%H:%M:%S")

Rapport$dateparaenv_format <- Res_env[is.na(Res_env$dateparenv_date),]
Rapport$heureparaenv_format <- Res_env[is.na(Res_env$heureparenv_date),]

Analyses<-Analyses%>%
  dplyr::select(-dateprel_date, -dateana_date, -heureprel_date, -heureana_date)

Res_env<-Res_env%>%
  dplyr::select(-dateparenv_date, -heureparenv_date)

##### test date / heure res_env dans dates de prélèvement #####

# Filtrer les lignes uniques de Res_env et Analyses pour la jointure anti
Res_env_unique <- Res_env %>% dplyr::distinct(cdstationmesureinterne, dateparenv, codeprel)
Analyses_unique <- Analyses %>% dplyr::distinct(cdstationmesureinterne, dateprel, codeprel)

# Effectuer la jointure anti
Rapport$dateResEnv_diff_datePrel <- dplyr::anti_join(Res_env_unique, Analyses_unique,
                                      by = c("cdstationmesureinterne" = "cdstationmesureinterne",
                                             "dateparenv" = "dateprel",
                                             "codeprel"="codeprel"))

rm(Res_env_unique, Analyses_unique)

# Filtrer les lignes uniques de Res_env et Analyses pour la jointure anti
Res_env_unique <- Res_env %>% dplyr::distinct(cdstationmesureinterne, dateparenv, HeureParEnv, codeprel)
Analyses_unique <- Analyses %>% dplyr::distinct(cdstationmesureinterne, dateprel, heureprel, codeprel)

# Effectuer la jointure anti
Rapport$heureResEnv_diff_heurePrel <- dplyr::anti_join(Res_env_unique, Analyses_unique,
                                                     by = c("cdstationmesureinterne" = "cdstationmesureinterne",
                                                            "dateparenv" = "dateprel",
                                                            "HeureParEnv"="heureprel",
                                                            "codeprel"="codeprel"
                                                            ))

rm(Res_env_unique, Analyses_unique)


##### test cohérence valeur résultat, LQ, code remarque #####
incoherence_cd_rq <-
  (
    ifelse(
      Analyses$cdrqana == "10" &
        Analyses$rsana > Analyses$lqana,
      TRUE,
      FALSE
    ) |
      ifelse(
        Analyses$cdrqana == "1" &
          Analyses$rsana < Analyses$lqana,
        TRUE,
        FALSE
      )
  ) &
  ifelse(is.na(Analyses$lqana) | is.na(Analyses$rsana), FALSE, TRUE)

Rapport$cd_rq_ou_lq_incoherent <- Analyses[incoherence_cd_rq,]



##### test cohérence O2 satO2 temp #####

analyses_a_tester <-
  Analyses %>% subset(cdparametre %in% c("1301", "1311", "1312") &
                        cdsupport == "3" &
                        cdinsituana == "1") %>% unique()

if (nrow(analyses_a_tester) > 0) {
  analyses_a_tester <-
    analyses_a_tester %>% tidyr::pivot_wider(
      id_cols = c(
        cdstationmesureinterne,
        dateprel,
        heureprel,
        codeprel,
        cdrdd
      ),
      names_from = cdparametre,
      names_prefix = "p",
      values_from = rsana,
      values_fn = max
    )


  #on teste seulement si on a des données à la fois sur O2 dissous, sat O2 et température
  if (all(c("p1311", "p1312", "p1301") %in% names(analyses_a_tester)))
  {
    analyses_a_tester <-
      analyses_a_tester %>% subset(!is.na(p1301) &
                                     !is.na(p1311) & !is.na(p1312))

    fct_test_O2 <-
      function(x) {
        func_test_metier_coherenceO2(O2 = analyses_a_tester[["p1311"]][x],
                                     satO2 = analyses_a_tester[["p1312"]][x],
                                     temp = analyses_a_tester[["p1301"]][x])
      }

    fct_test_O2_val <-
      function(x) {
        func_test_metier_coherenceO2(
          O2 = analyses_a_tester[["p1311"]][x],
          satO2 = analyses_a_tester[["p1312"]][x],
          temp = analyses_a_tester[["p1301"]][x],
          export = "value"
        )
      }

    analyses_a_tester$testO2 <-
      sapply(seq_along(analyses_a_tester$cdstationmesureinterne),
             FUN = fct_test_O2)
    analyses_a_tester$test_satO2_attendu <-
      sapply(seq_along(analyses_a_tester$cdstationmesureinterne),
             FUN = fct_test_O2_val)

    analyses_a_tester <-
      analyses_a_tester %>% dplyr::rename(O2 = p1311,
                                          satO2 = p1312,
                                          temperature = p1301)
    analyses_a_tester$testO2 <- analyses_a_tester$testO2 %>%
      dplyr::case_match("1" ~ "correcte", "2" ~ "incertain", "3" ~ "incorrect")

    analyses_a_tester$test_satO2_attendu <-
      round(analyses_a_tester$test_satO2_attendu, 1)


    Rapport$coherence_O2_temp <-
      analyses_a_tester %>% subset(testO2 != "correcte")
  }
}

##### test cohérence Ptot PO4 #####

# Cd Sandre PO4 = 1433
# Cd Sandre Ptot = 1350

analyses_a_tester <-
  Analyses %>% subset((cdparametre == "1350" &
                         cdfractionanalysee == "23") |
                        (cdparametre == "1433" &
                           cdsupport == "3")
  ) %>% unique()

if (nrow(analyses_a_tester) > 0)
{
  incertitudes_a_tester <-
    analyses_a_tester %>% tidyr::pivot_wider(
      id_cols = c(
        cdstationmesureinterne,
        dateprel,
        heureprel,
        codeprel,
        cdrdd
      ),
      names_from = cdparametre,
      names_prefix =
        "incer",
      values_from = incertitude,
      values_fn = max
    )

  analyses_a_tester <-
    analyses_a_tester %>% tidyr::pivot_wider(
      id_cols = c(
        cdstationmesureinterne,
        dateprel,
        heureprel,
        codeprel,
        cdrdd
      ),
      names_from =
        cdparametre,
      names_prefix =
        "p",
      values_from = rsana,
      values_fn = max
    )

  analyses_a_tester <- dplyr::left_join(
    analyses_a_tester,
    incertitudes_a_tester,
    by = c(
      "cdstationmesureinterne",
      "dateprel",
      "heureprel",
      "codeprel",
      "cdrdd"
    )
  )


  #on teste seulement si on a des données à la fois sur PO4 et Ptot
  if (all(c("p1350", "p1433") %in% names(analyses_a_tester)))
  {
    analyses_a_tester <-
      analyses_a_tester %>% subset(!is.na(p1350) & !is.na(p1433))

    fct_test_P <-
      function(x) {
        print(analyses_a_tester[["cdstationmesureinterne"]][x])
        func_test_metier_coherenceP(
          Ptot = analyses_a_tester[["p1350"]][x],
          PO4 = analyses_a_tester[["p1433"]][x],
          incertPtot = analyses_a_tester[["incer1350"]][x],
          incertPO4 = analyses_a_tester[["incer1350"]][x],
          forceincert = TRUE
        )
      }

    fct_test_P_val <-
      function(x) {
        func_test_metier_coherenceP(
          Ptot = analyses_a_tester[["p1350"]][x],
          PO4 = analyses_a_tester[["p1433"]][x],
          incertPtot = analyses_a_tester[["incer1350"]][x],
          incertPO4 = analyses_a_tester[["incer1350"]][x],
          export = "value",
          forceincert = TRUE
        )
      }


    analyses_a_tester$testPtot_PO4 <-
      sapply(seq_along(analyses_a_tester$cdstationmesureinterne),
             FUN = fct_test_P)
    analyses_a_tester$PO4_en_PPO4 <-
      sapply(seq_along(analyses_a_tester$cdstationmesureinterne),
             FUN = fct_test_P_val)

    analyses_a_tester$pourcentage_PO4_sur_Ptot <-
      round(analyses_a_tester$PO4_en_PPO4 / analyses_a_tester$p1350 * 100,
            1)

    analyses_a_tester <-
      analyses_a_tester %>% dplyr::rename(
        Ptot = p1350,
        PO4 = p1433,
        incertitude_Ptot = incer1350,
        incertitude_PO4 = incer1433
      )
    analyses_a_tester$testPtot_PO4 <-
      analyses_a_tester$testPtot_PO4 %>%
      dplyr::case_match("1" ~ "correcte",
                        "2" ~ "dans marge incertitude",
                        "3" ~ "incorrect")

    Rapport$coherence_Ptot_PO4 <-
      analyses_a_tester %>% subset(testPtot_PO4 != "correcte")
  }
}

##### Test vraissemblance des résultats d'analyses #####
Analyses$cle_frac_unit <- paste0(
  Analyses$cdparametre,
  "_",
  Analyses$cdfractionanalysee,
  "_",
  Analyses$cdunitemesure
)

Analyses$cle_staq_frac_unit <-
  paste0(
    Analyses$cdstationmesureinterne,
    "_",
    Analyses$cdparametre,
    "_",
    Analyses$cdfractionanalysee,
    "_",
    Analyses$cdunitemesure
  )
verif <- Analyses %>%
  dplyr::left_join(table_stat_analyses, by = "cle_staq_frac_unit") %>%
  dplyr::left_join(table_stat_analyses_toutes_staq, by = "cle_frac_unit") %>%
  subset(cdrqana == "1")

##### qualif par station #####
verif$classement_par_station <- 99

# classe 1
verif$classement_par_station <-
  ifelse(
    verif$rsana >= verif$Q10ST &
      verif$rsana <= verif$Q90ST,
    1,
    verif$classement_par_station
  )

# classe 2
verif$classement_par_station <- ifelse(
  (verif$rsana >= verif$Q5ST & verif$rsana < verif$Q10ST) |
    (verif$rsana <= verif$Q95ST &
       verif$rsana > verif$Q90ST),
  2,
  verif$classement_par_station
)

# classe 3
verif$classement_par_station <- ifelse(
  (verif$rsana >= verif$minST & verif$rsana < verif$Q5ST) |
    (verif$rsana <= verif$maxST &
       verif$rsana > verif$Q95ST),
  3,
  verif$classement_par_station
)

# classe 7
verif$classement_par_station <- ifelse(
  (
    verif$rsana >= (verif$minST - verif$sdST) &
      verif$rsana < verif$minST
  ) |
    (
      verif$rsana <= (verif$maxST + verif$sdST) &
        verif$rsana > verif$maxST
    ),
  7,
  verif$classement_par_station
)

# classe 9
verif$classement_par_station <- ifelse(
  (
    verif$rsana >= (verif$minST - 2 * verif$sdST) &
      verif$rsana < (verif$minST - verif$sdST)
  ) |
    (
      verif$rsana <= (verif$maxST + 2 * verif$sdST) &
        verif$rsana > (verif$maxST + verif$sdST)
    ),
  9,
  verif$classement_par_station
)

# classe 10
verif$classement_par_station <-
  ifelse((verif$rsana <= (verif$minST - 2 *
                            verif$sdST)) |
           (verif$rsana >= (verif$maxST + 2 * verif$sdST)),
         10,
         verif$classement_par_station)

# classe 0
verif$classement_par_station <-
  ifelse(is.na(verif$classement_par_station),
         0,
         verif$classement_par_station)


##### qualif toutes stations

verif$classement_toutes_station <- 99

# classe 2
verif$classement_toutes_station <-
  ifelse(
    verif$rsana >= verif$Q10 &
      verif$rsana <= verif$Q90,
    2,
    verif$classement_toutes_station
  )

# classe 3
verif$classement_toutes_station <- ifelse(
  (verif$rsana >= verif$Q5 & verif$rsana < verif$Q10) |
    (verif$rsana <= verif$Q95 &
       verif$rsana > verif$Q90),
  3,
  verif$classement_toutes_station
)

# classe 4
verif$classement_toutes_station <- ifelse(
  (verif$rsana >= verif$Q1 & verif$rsana < verif$Q5) |
    (verif$rsana <= verif$Q99 &
       verif$rsana > verif$Q95),
  4,
  verif$classement_toutes_station
)

# classe 6
verif$classement_toutes_station <- ifelse(
  (verif$rsana >= verif$min & verif$rsana < verif$Q1) |
    (verif$rsana <= verif$max &
       verif$rsana > verif$Q99),
  6,
  verif$classement_toutes_station
)

# classe 8
verif$classement_toutes_station <- ifelse(
  (verif$rsana >= (verif$min - verif$sd) &
     verif$rsana < verif$min) |
    (verif$rsana <= (verif$max + verif$sd) &
       verif$rsana > verif$max),
  8,
  verif$classement_toutes_station
)


# classe 10
verif$classement_toutes_station <-
  ifelse((verif$rsana <= (verif$min -
                            verif$sd)) |
           (verif$rsana >= (verif$max + verif$sd)),
         10,
         verif$classement_toutes_station)

# classe 0
verif$classement_toutes_station <-
  ifelse(is.na(verif$classement_toutes_station),
         0,
         verif$classement_toutes_station)

##### agrégation des vérifications
verif$cdqualana <-
  ifelse(verif$classement_toutes_station == 0 &
           verif$classement_par_station == 0,
         3,
         4)

verif$cdqualana <-
  ifelse(
    verif$classement_toutes_station %in% c(2, 3, 4) &
      verif$classement_par_station %in% c(0, 1, 2),
    1,
    verif$cdqualana
  )

verif$cdqualana <-
  ifelse(
    verif$classement_toutes_station %in% c(2, 3) &
      verif$classement_par_station %in% c(3),
    1,
    verif$cdqualana
  )

verif$cdqualana <-
  ifelse(
    verif$classement_toutes_station %in% c(6) &
      verif$classement_par_station %in% c(3),
    1,
    verif$cdqualana
  )

verif$cdqualana <-
  ifelse(
    verif$classement_toutes_station %in% c(6) &
      verif$classement_par_station %in% c(0, 1, 2, 3, 7),
    3,
    verif$cdqualana
  )

verif$cdqualana <-
  ifelse(
    verif$classement_toutes_station %in% c(4) &
      verif$classement_par_station %in% c(3, 7),
    3,
    verif$cdqualana
  )

verif$cdqualana <-
  ifelse(
    verif$classement_toutes_station %in% c(3) &
      verif$classement_par_station %in% c(7),
    3,
    verif$cdqualana
  )

verif$cdqualana <-
  ifelse(
    verif$classement_toutes_station %in% c(2) &
      verif$classement_par_station %in% c(7, 9),
    3,
    verif$cdqualana
  )

verif$cdqualana <-
  ifelse(verif$classement_toutes_station %in% c(8, 10),
         2,
         verif$cdqualana)

verif$cdqualana <-
  ifelse(
    verif$classement_toutes_station %in% c(3, 4, 6) &
      verif$classement_par_station %in% c(9, 10),
    2,
    verif$cdqualana
  )

verif$cdqualana <-
  ifelse(
    verif$classement_toutes_station %in% c(2) &
      verif$classement_par_station %in% c(10),
    2,
    verif$cdqualana
  )


verif$cdqualana <- as.character(verif$cdqualana)
verif$nomclassement <- verif$cdqualana %>%
  dplyr::case_match(
    "0" ~ "non definissable",
    "1" ~ "correct",
    "2" ~ "incorrect",
    "3" ~ "incertain",
    "4" ~ "non qualifié"
  )

# export des resultats

Rapport$resultats_a_confirmer <- verif %>%
  dplyr::select(
    "nomclassement",
    "cdstationmesureinterne",
    "station",
    "dateprel",
    "heureprel",
    "profondeurpre",
    "ZoneVerticaleProspectee",
    "cdparametre",
    "nomparametre",
    "nomlongparametre",
    "insitu",
    "nomfraction",
    "cdrqana",
    "rsana",
    "unite",
    "lqana",
    "commentairesana",
    "cdpreleveur",
    "nompreleveur",
    "cdlaboratoire",
    "nomlabo",
    "cdrdd",
    "nomrdd"
  ) %>%
  subset(nomclassement %in% c("incorrect",
                              "incertain"))

##### ajout de la qualification à la table Analyses #####
verif <-
  verif %>% dplyr::select(
    "cdqualana",
    "cdstationmesureinterne",
    "dateprel",
    "heureprel",
    "profondeurpre",
    "ZoneVerticaleProspectee",
    "cdparametre",
    "cdinsituana",
    "cdfractionanalysee",
    "cdrqana",
    "rsana",
    "cdunitemesure",
    "cdpreleveur",
    "cdlaboratoire"
  )

Analyses <-
  Analyses %>% dplyr::left_join(
    verif,
    by = c(
      "cdstationmesureinterne",
      "dateprel",
      "heureprel",
      "profondeurpre",
      "ZoneVerticaleProspectee",
      "cdparametre",
      "cdinsituana",
      "cdfractionanalysee",
      "cdrqana",
      "rsana",
      "cdunitemesure",
      "cdpreleveur",
      "cdlaboratoire"
    ),
    relationship = "many-to-many"
  )


##### Comparaison bon de commande - réalisé #####

analyses_attendues$cle <-
  paste0(
    analyses_attendues$res_stm_cdstationmesureinterne,
    "_",
    analyses_attendues$rea_cdfractionanalysee,
    "_",
    analyses_attendues$rea_par_cdparametre,
    "_",
    analyses_attendues$rea_cdunitemesure,
    "_",
    analyses_attendues$rea_cdinsituana
  )

Analyses$cle <-
  paste0(
    Analyses$cdstationmesureinterne,
    "_",
    Analyses$cdfractionanalysee,
    "_",
    Analyses$cdparametre,
    "_",
    Analyses$cdunitemesure,
    "_",
    Analyses$cdinsituana
  )



##### Stations commandées et réalisées #####

Rapport$stations_commandees_analysees <- Analyses %>%
  subset(
    cdstationmesureinterne %in% analyses_attendues$res_stm_cdstationmesureinterne &
      cdrqana != "0"
  ) %>%
  dplyr::group_by(cdstationmesureinterne, station, dateprel) %>%
  dplyr::summarise(nb_donnees = dplyr::n())

##### Stations non commandées et réalisées #####                                                                                                                  )
Rapport$stations_non_commandees_analysees <- Analyses %>%
  subset(
    !(
      cdstationmesureinterne %in% analyses_attendues$res_stm_cdstationmesureinterne
    )
  ) %>%
  dplyr::group_by(cdstationmesureinterne, station, dateprel) %>%
  dplyr::summarise(nb_donnees = dplyr::n())

##### Stations commandées et non réalisées #####                                                                                                                  )
analyses_attendues$station <- func_ajoute_nom_sandre(connexion,
                                                     code = analyses_attendues$res_stm_cdstationmesureinterne,
                                                     out = "nom_station")

Rapport$stations_manquantes <- analyses_attendues %>%
  subset(!(
    res_stm_cdstationmesureinterne %in% Analyses$cdstationmesureinterne
  )) %>%
  dplyr::group_by(res_stm_cdstationmesureinterne, station) %>%
  dplyr::summarise(nb_donnees = dplyr::n())


##### Analyses hors bon de commande (analyses en +) #####
Rapport$analyses_hors_bon_de_commande <- Analyses %>%
  subset((!(cle %in% analyses_attendues$cle)) &
           (
             !cdstationmesureinterne %in% Rapport$stations_manquantes$res_stm_cdstationmesureinterne
           )
  ) %>%
  dplyr::select(
    "cdstationmesureinterne",
    "station",
    "dateprel",
    "heureprel",
    "profondeurpre",
    "ZoneVerticaleProspectee",
    "cdparametre",
    "nomparametre",
    "insitu",
    "nomfraction",
    "cdrqana",
    "rsana",
    "unite",
    "lqana",
    "commentairesana",
    "cdpreleveur",
    "nompreleveur",
    "cdlaboratoire",
    "nomlabo",
    "cdrdd",
    "nomrdd"
  ) %>% unique()

##### Analyses en doublon #####
doublons <- Analyses %>%
  dplyr::group_by(cle, dateprel) %>%
  dplyr::summarise(nb = dplyr::n()) %>%
  subset(nb > 1)

Rapport$analyses_en_doublon <- Analyses %>%
  dplyr::inner_join(doublons, by = c("cle", "dateprel")) %>%
  dplyr::select(
    "cdstationmesureinterne",
    "station",
    "dateprel",
    "heureprel",
    "dateana",
    "heureana",
    "profondeurpre",
    "ZoneVerticaleProspectee",
    "cdparametre",
    "nomparametre",
    "insitu",
    "nomfraction",
    "cdrqana",
    "rsana",
    "unite",
    "lqana",
    "commentairesana",
    "cdpreleveur",
    "nompreleveur",
    "cdlaboratoire",
    "nomlabo",
    "cdrdd",
    "nomrdd"
  ) %>% unique()

##### Analyses manquantes #####
nb_analyses_attendues <- analyses_attendues %>%
  subset(
    !res_stm_cdstationmesureinterne %in%
      Rapport$stations_manquantes$res_stm_cdstationmesureinterne
  ) %>%
  unique %>%
  dplyr::group_by(cle) %>%
  dplyr::summarise(nb_attendu = dplyr::n()) %>%
  dplyr::ungroup()

nb_analyses_rendues <- Analyses %>%
  subset(cdrqana != "0") %>%
  dplyr::select(cle, dateprel) %>%
  unique %>%
  dplyr::group_by(cle) %>%
  dplyr::summarise(nb_rendu = dplyr::n()) %>%
  dplyr::ungroup()
nb_analyses_rendues$nb_rendu <-
  ifelse(is.na(nb_analyses_rendues$nb_rendu),
         0,
         nb_analyses_rendues$nb_rendu)


delta_analyses <- dplyr::left_join(nb_analyses_attendues,
                                   nb_analyses_rendues,
                                   by = "cle") %>%
  dplyr::mutate(delta = nb_attendu - ifelse(is.na(nb_rendu), 0, nb_rendu)) %>%
  subset(delta > 0)

extraire_lignes <- function(df, delta_analyses) {
  # Calculer le nombre d'occurrences de chaque clé dans le data.frame d'analyses
  df$occurrences <- ave(df$cle, df$cle, FUN = length)

  # Fusionner avec le data.frame delta pour récupérer les nombres d'occurrences souhaités
  df <- merge(df, delta_analyses, by = "cle", all = TRUE)

  # Extraire les lignes avec les clés et les nombres d'occurrences souhaités
  df <- subset(df, occurrences <= delta)

  # Retirer la colonne "occurrences" ajoutée précédemment
  df$occurrences <- NULL

  # Retourner le data.frame résultant
  return(df)
}




Rapport$analyses_manquantes <-
  extraire_lignes(analyses_attendues, delta_analyses) %>%
  dplyr::select(
    "res_stm_cdstationmesureinterne",
    "station",
    "rea_dateprel_prev",
    "rea_profondeurpre",
    "rea_par_cdparametre",
    "nomparametre",
    "rea_cdinsituana",
    "insitu",
    "nomfraction",
    "rea_cdpreleveur",
    "nompreleveur",
    "rea_cdlaboratoire",
    "nomlabo",
    "rea_rdd_cdrdd"
  ) %>% unique() %>%
  subset(
    !res_stm_cdstationmesureinterne %in%
      Rapport$stations_manquantes$res_stm_cdstationmesureinterne
  )

##### Vérification dispositif de collecte #####
Rapport$reseaux_de_mesures <- Analyses %>%
  dplyr::group_by(cdrdd, nomrdd) %>%
  dplyr::summarise(nb = dplyr::n(),
                   pourcent = 100 * dplyr::n() / nrow(Analyses))

##### Vérification respect des LQ contractuelles #####

compar_perf_anal <- dplyr::inner_join(
  Analyses,
  analyses_attendues %>% subset(!is.na(rea_par_cdparametre)),
  by = "cle",
  suffix = c("", ".attendu"),
  multiple = "all",
  relationship = "many-to-many"
)


Rapport$lq_non_conforme <- compar_perf_anal %>%
  subset((lqana > rea_lqprev) & cdrqana == "10") %>%
  dplyr::select(
    "cdstationmesureinterne",
    "station",
    "dateprel",
    "heureprel",
    "profondeurpre",
    "ZoneVerticaleProspectee",
    "cdparametre",
    "nomparametre",
    "insitu",
    "nomfraction",
    "cdrqana",
    "rsana",
    "unite",
    "lqana",
    "rea_lqprev",
    "commentairesana",
    "cdpreleveur",
    "nompreleveur",
    "cdlaboratoire",
    "nomlabo",
    "cdrdd",
    "nomrdd"
  ) %>%
  dplyr::rename("lq_au_bpu" = "rea_lqprev") %>%
  unique()

##### Vérification respect des accréditations #####

Rapport$accreditation_non_conforme <- compar_perf_anal %>%
  subset(rea_cdaccreanaprev == "true" & cdaccreana != "1") %>%
  dplyr::select(
    "cdstationmesureinterne",
    "station",
    "dateprel",
    "heureprel",
    "profondeurpre",
    "ZoneVerticaleProspectee",
    "cdparametre",
    "nomparametre",
    "insitu",
    "nomfraction",
    "cdrqana",
    "rsana",
    "unite",
    "cdaccreana",
    "rea_cdaccreanaprev",
    "commentairesana",
    "cdpreleveur",
    "nompreleveur",
    "cdlaboratoire",
    "nomlabo",
    "cdrdd",
    "nomrdd"
  ) %>%
  dplyr::rename("accreditation_attendue" = "rea_cdaccreanaprev")


# calcul taux de résultats rendus sous accréditation par rapport à celui attendu

if (nrow(compar_perf_anal %>%
         subset(rea_cdaccreanaprev == "true")) > 0)
{
  Rapport$taux_result_accredites_sur_attendus <-
    nrow(compar_perf_anal %>%
           subset(rea_cdaccreanaprev ==
                    "true" &
                    cdaccreana == "1")) / nrow(compar_perf_anal %>%
                                                 subset(rea_cdaccreanaprev ==
                                                          "true")) * 100
}

##### Vérification des incertitudes contractuelles #####
Rapport$incertitude_non_conforme <- compar_perf_anal %>%
  subset(incertitude > rea_incertitudeprev) %>%
  dplyr::select(
    "cdstationmesureinterne",
    "station",
    "dateprel",
    "heureprel",
    "profondeurpre",
    "ZoneVerticaleProspectee",
    "cdparametre",
    "nomparametre",
    "insitu",
    "nomfraction",
    "cdrqana",
    "rsana",
    "unite",
    "lqana",
    "incertitude",
    "rea_incertitudeprev",
    "commentairesana",
    "cdpreleveur",
    "nompreleveur",
    "cdlaboratoire",
    "nomlabo",
    "cdrdd",
    "nomrdd"
  ) %>%
  dplyr::rename("incertitude_au_bpu" = "rea_incertitudeprev") %>%
  unique()


##### Vérification des méthodes analytiques contractuelles #####
compar_perf_anal$methode_au_bpu <- func_ajoute_nom_sandre(connexion,
                                                          code = compar_perf_anal$rea_cdmethode,
                                                          out = "nom_methode")

compar_perf_anal$methode_analyse <-
  func_ajoute_nom_sandre(connexion,
                         code = compar_perf_anal$cdmethode,
                         out = "nom_methode")

Rapport$methode_non_conforme <- compar_perf_anal %>%
  subset(cdmethode != rea_cdmethode) %>%
  dplyr::select(
    "cdstationmesureinterne",
    "station",
    "dateprel",
    "heureprel",
    "profondeurpre",
    "ZoneVerticaleProspectee",
    "cdparametre",
    "nomparametre",
    "insitu",
    "nomfraction",
    "cdrqana",
    "rsana",
    "unite",
    "cdmethode",
    "methode_analyse",
    "rea_cdmethode",
    "methode_au_bpu",
    "commentairesana",
    "cdpreleveur",
    "nompreleveur",
    "cdlaboratoire",
    "nomlabo",
    "cdrdd",
    "nomrdd"
  ) %>%
  dplyr::rename("cd_methode_au_bpu" = "rea_cdmethode") %>%
  unique()


##### Test durée transport / réception échantillon labo #####

Rapport$duree_transport <- "Fonction à implémenter"



##### Test température réception échantillon #####

Rapport$temperature_reception_echantillon <-
  "Fonction à implémenter"


##### Génération d'un rapport xlsx #####
wb <- createWorkbook()
for (i in 1:length(Rapport)) {
  if (is.data.frame(Rapport[[i]])) {
    if (nrow(Rapport[[i]]) > 0) {
      if (nchar((names(Rapport))[i]) > 30) {
        nom_sheet <-
          paste0(substr((names(Rapport))[i], 1, 15), "_", substr((names(Rapport))[i], nchar((
            names(Rapport)
          )[i]) - 14, nchar((
            names(Rapport)
          )[i])))
      } else {
        nom_sheet <- (names(Rapport))[i]
      }
      addWorksheet(wb, sheetName = nom_sheet)
      writeData(wb, sheet = nom_sheet, x = Rapport[[i]])
    }
  }
}
saveWorkbook(wb, paste0(basename(Rapport$fichier), "_", ref_bon_de_commande, ".xlsx"), overwrite =
               TRUE)

##### Sauvegarde au format xls des données d'analyses #####
export <- Analyses %>%
  dplyr::select(
    "cdstationmesureinterne",
    "station",
    "dateprel",
    "heureprel",
    "profondeurpre",
    "ZoneVerticaleProspectee",
    "cdparametre",
    "nomparametre",
    "insitu",
    "nomfraction",
    "cdrqana",
    "rsana",
    "unite",
    "lqana",
    "commentairesana",
    "cdpreleveur",
    "nompreleveur",
    "cdlaboratoire",
    "nomlabo",
    "cdrdd",
    "nomrdd"
  )


write.xlsx(export,
           paste0(basename(Rapport$fichier), "_", ref_bon_de_commande,"_data.xlsx"),
           overwrite = TRUE)



##### Sauvegarde des données correctes en QUESU v3.1 #####
#
# names(Analyses)
# cd_emetteur <- "25440124300012"
# nom_emetteur <-
#   "ETABLISSEMENT PUBLIC TERRITORIAL DU BASSIN DE LA VILAINE (EPTB)"
# cd_destinataire <- "22350001800013" #CD35
# nom_destinataire <- "DEPARTEMENT D ILLE ET VILAINE"
# # On passe les analyses en contrôlées niveau 1
# Analyses$statutana <- 2
#
# # ces codes n'étant pas renseignés on passe tous les codes points de prélèvement à 031
# code_pt_prelevement <- "031"
#
# #méthode de prélèvement : par défaut Prélèvement eau brute (code 720)
# code_methode_prelevement <- "720"
#
#
# Charger la bibliothèque xml2
# library(xml2)
#
# # # Créer le document XML
# # doc <- xml_new_document(version = "1.0", encoding = "UTF-8")
# # quesu <- xml_new_root("QUESU", doc = doc)
#
# quesu <- xml_new_root("QUESU")
#
# # Ajouter les attributs à la balise QUESU
# xml_set_attr(quesu,
#              "xmlns",
#              "http://xml.sandre.eaufrance.fr/scenario/quesu/3.1")
# xml_set_attr(quesu,
#              "xmlns:xsi",
#              "http://www.w3.org/2001/XMLSchema-instance")
# # xml_set_attr(
# #   quesu,
# #   "xsi:schemalocation",
# #   "http://xml.sandre.eaufrance.fr/scenario/quesu/3.1 http://xml.sandre.eaufrance.fr/scenario/quesu/3.1/sandre_sc_quesu.xsd"
# # )
#
# # Créer l'élément Scenario
# scenario <- xml_add_child(quesu, "Scenario")
#
# # Ajouter les balises enfant de Scenario
# xml_add_child(scenario, "CodeScenario", "QUESU_PHY")
# xml_add_child(scenario, "VersionScenario", "3.1")
# xml_add_child(
#   scenario,
#   "NomScenario",
#   "Qualité des eaux superficielles continentales – Données physico-chimiques et microbiologiques"
# )
# xml_add_child(scenario,
#               "DateCreationFichier",
#               format(Sys.Date(), "%Y-%m-%d"))
#
# # Ajouter les balises Emetteur et Destinataire sous l'élément Scenario
# emetteur <- xml_add_child(scenario, "Emetteur")
# xml_add_child(emetteur, "CdIntervenant", cd_emetteur) %>%
#   xml_set_attr("schemeAgencyID", "SIRET")
# xml_add_child(emetteur, "NomIntervenant", nom_emetteur)
#
# destinataire <- xml_add_child(scenario, "Destinataire")
# xml_add_child(destinataire, "CdIntervenant", cd_destinataire) %>%
#   xml_set_attr("schemeAgencyID", "SIRET")
# xml_add_child(destinataire, "NomIntervenant", nom_destinataire)
#
#
# # Créer une série de balises StationMesureEauxSurface à partir des données dans le data.frame Analyses
# stations <- unique(Analyses$cdstationmesureinterne)
# for (i in seq_along(stations)) {
#   # <ResPC>
#   respc <- xml_add_child(quesu, "ResPC")
#   # <StationMesureEauxSurface>
#   station <- xml_add_child(respc, "StationMesureEauxSurface")
#   cd_station <-
#     xml_add_child(station, "CdStationMesureEauxSurface", stations[i])
#   xml_set_attr(cd_station, "schemeID", "STQ")
#   xml_set_attr(cd_station, "schemeAgencyID", "AE")
#   # <OperationPrel>
#   oper <-
#     Operation %>% subset(StationPrelevement_CdStationPrelevement == stations[i])
#   oper$dateheure <- paste0(oper$DatePrel, oper$HeurePrel)
#   for (j in seq_along(unique(oper$dateheure))) {
#     operationprel <- xml_add_child(respc, "OperationPrel")
#     xml_set_attr(operationprel, "Action", "A")
#     xml_add_child(operationprel, "DateDebutOperationPrel", oper$DatePrel[j])
#     xml_add_child(
#       operationprel,
#       "HeureDebutOperationPrel",
#       paste0(oper$DatePrel[j],
#              "T",
#              oper$HeurePrel[j])
#     )
#
#
#
#     prelevement <- xml_add_child(operationprel, "Prelevement")
#
#
#     anal <- Analyses %>% subset(
#       cdstationmesureinterne == stations[i] &
#         dateprel == oper$DatePrel[j] &
#         heureprel == oper$HeurePrel[j]
#     )
#
#     #<Prelevement>
#     xml_add_child(
#       prelevement,
#       "CdPrelevement",
#       ifelse(anal$codeprel[1] != "",
#              anal$codeprel[1],
#              "000000")
#     )
#     xml_add_child(prelevement, "DatePrel", oper$DatePrel[j])
#     xml_add_child(prelevement,
#                   "HeurePrel",
#                   paste0(oper$DatePrel[j], "T", oper$HeurePrel[j]))
#     xml_add_child(prelevement, "DifficultePrel", 0) %>%
#       xml_set_attr("listID", "67")
#     xml_add_child(prelevement, "AccredPrel", oper$AccredPrel[j]) %>%
#       xml_set_attr("listID", "333")
#     xml_add_child(prelevement, "FinalitePrel", "0") %>%
#       xml_set_attr("listID", "645")
#
#     # methode_plt<-xml_add_child(prelevement, "MethodePrlvt")
#     # xml_add_child(methode_plt, "CdMethode", code_methode_prelevement)%>%
#     #   xml_set_attr("schemeID", "MET")
#
#
#     pointprel <- xml_add_child(prelevement, "PointPrel")
#
#     CdPointEauxSurf <-
#       xml_add_child(pointprel,
#                     "CdPointEauxSurf",
#                     code_pt_prelevement)
#     xml_set_attr(CdPointEauxSurf, "schemeID", "STM")
#     xml_set_attr(CdPointEauxSurf, "schemeAgencyID", "AE")
#
#
#     support <- xml_add_child(prelevement, "Support")
#     xml_add_child(support, "CdSupport", oper$Support_CdSupport[j]) %>%
#       xml_set_attr("schemeID", "SUP")
#
#     producprel <-
#       xml_add_child(prelevement, "ProducteurPrelevement")
#     product <-
#       xml_add_child(producprel,
#                     "CdIntervenant",
#                     Demande$Commanditaire_CdIntervenant)
#     xml_set_attr(product, "schemeAgencyID", "SIRET")
#     xml_set_attr(product, "schemeID", "INT")
#
#
#     preleveur <- xml_add_child(prelevement, "Preleveur")
#     prel <-
#       xml_add_child(preleveur,
#                     "CdIntervenant",
#                     oper$Preleveur_CdIntervenant[j])
#     xml_set_attr(prel, "schemeAgencyID", "SIRET")
#     xml_set_attr(prel, "schemeID", "INT")
#
#
#     # # Diviser la valeur en parties individuelles
#     #  cdrdd_parts <- strsplit(anal$cdrdd[1], "/") %>% unlist()
#     #
#     #
#     # # cdrdd <- xml_add_child(rdd, "CodeSandreRdd", anal$cdrdd[1])
#     #
#     # # Parcourir les parties et ajouter des éléments CodeSandreRdd
#     # for (i in seq_along(cdrdd_parts)) {
#     #   rdd <- xml_add_child(prelevement, "Rsx")
#     #         xml_add_child(rdd, "CodeSandreRdd", cdrdd_parts[i])%>%xml_set_attr("schemeID", "RSX")
#     # }
#
#
#
#
#
#     if (oper$CommentairesPrel[j] != "") {
#       xml_add_child(prelevement,
#                     "CommentairesPrel",
#                     oper$CommentairesPrel[j])
#     }
#
#     #<Analyse>
#
#     for (k in seq_along(anal$cdparametre))
#     {
#       analyse <- xml_add_child(prelevement, "Analyse")
#       xml_add_child(analyse, "RefAnaProd", anal$RefAna[k])
#       xml_add_child(analyse, "DateAna", anal$dateana[k])
#       if (anal$heureana[k] != "") {
#         xml_add_child(analyse,
#                       "HeureAna",
#                       paste0(anal$dateana[k],
#                              "T",
#                              anal$heureana[k]))
#       }
#       parametre <- xml_add_child(analyse, "Parametre")
#       cdparametre <-
#         xml_add_child(parametre, "CdParametre", anal$cdparametre[k])
#       xml_set_attr(cdparametre, "schemeID", "PAR")
#       xml_set_attr(cdparametre, "schemeAgencyID", "SANDRE")
#       fractionanalysee <- xml_add_child(analyse, "FractionAnalysee")
#       cdfraction <-
#         xml_add_child(fractionanalysee,
#                       "CdFractionAnalysee",
#                       anal$cdfractionanalysee[k])
#       xml_set_attr(cdfraction, "schemeID", "FAN")
#       xml_add_child(analyse, "RsAna", anal$rsana[k])
#       unitemesure <- xml_add_child(analyse, "UniteMesure")
#       CdUniteMesure <-
#         xml_add_child(unitemesure, "CdUniteMesure", anal$cdunitemesure[k])
#       xml_set_attr(CdUniteMesure, "schemeID", "URF")
#       xml_add_child(analyse, "RqAna", anal$cdrqana[k]) %>%
#         xml_set_attr("listID", "155")
#       xml_add_child(analyse,
#                     "InsituAna",
#                     ifelse(
#                       anal$insitu[k] == "In situ",
#                       1,
#                       ifelse(
#                         anal$insitu[k] == "Laboratoire",
#                         2,
#                         ifelse(anal$insitu[k] == "Sans objet", 3, 0)
#                       )
#                     )) %>%
#         xml_set_attr("listID", "156")
#       xml_add_child(analyse, "DifficulteAna", 0) %>%
#         xml_set_attr("listID", "43")
#       xml_add_child(analyse, "QualAna", ifelse(!is.na(anal$cdqualana[k]),
#                                                anal$cdqualana[k],
#                                                0)) %>%
#         xml_set_attr("listID", "414")
#       if (length(anal$commentairesana[k]) > 1) {
#         xml_add_child(analyse, "CommentairesAna", anal$commentairesana[k])
#       }
#       xml_add_child(analyse,
#                     "ComResultatAna",
#                     "commentaire resultat ana à implémenter")
#       xml_add_child(analyse, "StatutAna", anal$statutana[k]) %>%
#         xml_set_attr("listID", "446")
#       xml_add_child(analyse,
#                     "AccreAna",
#                     ifelse(anal$cdaccreana[k] != "", anal$cdaccreana[k], 0)) %>%
#         xml_set_attr("listID", "299")
#       if (!is.na(anal$ldana[k])) {
#         xml_add_child(analyse, "LDAna", anal$ldana[k])
#       }
#       if (!is.na(anal$lqana[k])) {
#         xml_add_child(analyse, "LQAna", anal$lqana[k])
#       }
#       if (!is.na(anal$lsana[k])) {
#         xml_add_child(analyse, "LSAna", anal$lsana[k])
#       }
#       if (!is.na(anal$ldana[k])) {
#         xml_add_child(analyse, "IncertAna", anal$incertitude[k])
#       }
#       xml_add_child(analyse, "AgreAna", ifelse(anal[k,]$agreana == "1", 1, 0))
#       if (anal$cdmetfractionnement[k] != "") {
#         metfra <- xml_add_child(analyse, "MetFractionnement")
#         xml_add_child(metfra, "CdMethode", anal$cdmetfractionnement[k]) %>%
#           xml_set_attr("schemeID", "MET")
#       }
#       if (anal$cdmethode[k] != "") {
#         met <- xml_add_child(analyse, "Methode")
#         xml_add_child(met, "CdMethode", anal$cdmethode[k]) %>% xml_set_attr("schemeID", "MET")
#       } else
#       {
#         met <- xml_add_child(analyse, "Methode")
#         xml_add_child(met, "CdMethode", "0") %>% xml_set_attr("schemeID", "MET")
#       }
#
#       produc <-
#         xml_add_child(analyse, "Producteur")
#       product <-
#         xml_add_child(produc,
#                       "CdIntervenant",
#                       anal$cdproducteur[k])
#       xml_set_attr(product, "schemeAgencyID", "SIRET")
#       xml_set_attr(product, "schemeID", "INT")
#
#       if (anal$cdlaboratoire[k] != "")
#       {
#         labo <-
#           xml_add_child(analyse, "Laboratoire")
#         product <-
#           xml_add_child(labo,
#                         "CdIntervenant",
#                         anal$cdlaboratoire[k])
#         xml_set_attr(product, "schemeAgencyID", "SIRET")
#         xml_set_attr(product, "schemeID", "INT")
#       }
#
#
#       # Diviser la valeur en parties individuelles
#       cdrdd_parts <- strsplit(anal$cdrdd[1], "/") %>% unlist()
#
#
#       # Parcourir les parties et ajouter des éléments CodeSandreRdd
#       for (i in seq_along(cdrdd_parts)) {
#         rdd <- xml_add_child(analyse, "Rsx")
#         xml_add_child(rdd, "CodeSandreRdd", cdrdd_parts[i]) %>% xml_set_attr("schemeID", "RSX")
#       }
#
#
#     }
#   }
# }
#
# # Enregistrer le document XML dans un fichier
# write_xml(quesu, paste0(basename(Rapport$fichier), "_quesu3.1.xml"))

##### Temporaire enregistrement format RDS #####
# analyses_Eaux_Vilaine <-
#   readRDS("~/R_Anthony/Naiades/bdd_locale/analyses_Eaux_Vilaine.rds")
#
# analyses_rds <- Analyses %>%
#   dplyr::rename(
#     CdStationMesureEauxSurface = cdstationmesureinterne,
#     CdSupport = cdsupport,
#     CdFractionAnalysee = cdfractionanalysee,
#     CdPrelevement = codeprel,
#     DatePrel = dateprel,
#     HeurePrel = heureprel,
#     DateAna = dateana,
#     HeureAna = heureana,
#     CdParametre = cdparametre,
#     RsAna = rsana,
#     CdUniteMesure = cdunitemesure,
#     CdRqAna = cdrqana,
#     CdInsituAna = cdinsituana,
#     ProfondeurPrel = profondeurpre,
#     LdAna = ldana,
#     LqAna = lqana,
#     LsAna = lsana,
#     IncertAna = incertitude,
#     CdMetFractionnement = cdmetfractionnement,
#     CdMethode = cdmethode,
#     RdtExtraction = rdtextraction,
#     CdMethodeExtraction = cdmethodeextraction,
#     CdAccreAna = cdaccreana,
#     AgreAna = agreana,
#     CommentairesAna = commentairesana,
#     ComResultatAna = CommentairesEchant,
#     CdRdd = cdrdd,
#     CdProducteur = cdproducteur,
#     CdPreleveur = cdpreleveur,
#     CdLaboratoire = cdlaboratoire
#   )
#
# analyses_rds$CdDifficulteAna <- "0"
# analyses_rds$CdStatutAna <- "1"
# analyses_rds$CdQualAna <- "1"
# analyses_rds$DatePrel <- as.Date(analyses_rds$DatePrel)
# analyses_rds$DateAna <- as.Date(analyses_rds$DateAna)
# analyses_rds$ProfondeurPrel <-
#   as.numeric(analyses_rds$ProfondeurPrel)
# analyses_rds$IncertAna <- as.character(analyses_rds$IncertAna)
# analyses_rds$AgreAna <-
#   ifelse(analyses_rds$AgreAna == "1", TRUE, FALSE)
#
# analyses_Eaux_Vilaine <-
#   dplyr::bind_rows(analyses_Eaux_Vilaine, analyses_rds)
# analyses_Eaux_Vilaine <- unique(analyses_Eaux_Vilaine)
#
# analyses_CD35_2020_2021 <-
#   readRDS("~/R_Anthony/Naiades/bdd_locale/analyses_CD35_2020_2021.rds")
# analyses_Eaux_Vilaine$source <- "E&V"
# analyses_Eaux_Vilaine <-
#   analyses_Eaux_Vilaine %>%
#   dplyr::select(names(analyses_CD35_2020_2021)) %>%
#   unique()
#
#
# saveRDS(
#   analyses_Eaux_Vilaine,
#   "~/R_Anthony/Naiades/bdd_locale/analyses_Eaux_Vilaine.rds"
# )
