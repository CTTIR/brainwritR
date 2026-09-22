#' Interface translations without changing contributed text
#' @return A data frame with German, English and French interface strings.
#' @keywords internal
#' @noRd
bw_language_dictionary <- function() {
  dictionary <- data.frame(
    de = c(
      "Gemeinsam Ideen weiterdenken",
      "Moderator",
      "Anmelden",
      "Falsche PIN.",
      "Beitritt derzeit nicht moeglich.",
      "Als Gruppe beitreten:",
      "Abbrechen",
      "\u00dcbernehmen",
      "Session beenden?",
      "Die bisherigen Beitr\u00e4ge bleiben erhalten.",
      "Zur Ergebnisansicht",
      "Session beendet",
      "Ein Beitritt ist nicht mehr moeglich.",
      "Die Session wird gerade vorbereitet \u2014 Seite einfach offen lassen.",
      "Mit Name oder Pseudonym beitreten:",
      "Name / Pseudonym",
      "Teilnehmen",
      "Du bist dabei. Warten auf den Start \u2026",
      "Geschafft \u2014 danke!",
      "Alle Beitraege sind gesichert. Die Ergebnisse besprechen wir jetzt gemeinsam.",
      "Los geht's",
      "\u00dcberspringen",
      "Einen Moment \u2014 du wirst einer Gruppe zugeteilt \u2026",
      "Aufgreifen, ergaenzen, weiterentwickeln \u2026",
      "Speichert automatisch beim Tippen.",
      "Noch keine Vorbeitraege \u2014 dieser Bogen beginnt bei dir.",
      "Abgegeben \u2713",
      "Fertig \u2014 weitergeben",
      "\u2713 Abgegeben \u2014 du kannst bis Rundenende weiter aendern.",
      "Abgeben",
      "Session einrichten",
      "Einstellungen laden (YAML)",
      "Spielmodus",
      "Alle am eigenen Ger\u00e4t",
      "Ein Ger\u00e4t pro Gruppe",
      "Reihum an einem Ger\u00e4t",
      "Themen = Gruppen",
      "Runden / Durchg\u00e4nge",
      "Sek./Runde",
      "Sek./Person",
      "Namen (ein Name pro Zeile)",
      "Gruppennamen (ein Name pro Zeile, optional)",
      "Einstellungen exportieren (YAML)",
      "Speichern und Lobby \u00f6ffnen",
      "Speichern und Lobby oeffnen",
      "Speichern und starten",
      "Titel des Themas",
      "Frage 1",
      "Frage 2",
      "Einstellungen pr\u00fcfen",
      "Einstellungen geladen. Bitte pr\u00fcfen und best\u00e4tigen.",
      "Lobby",
      "Teilnehmer scannen den QR-Code oder oeffnen:",
      "Session starten",
      "Ohne alle Gruppen starten",
      "Noch niemand da.",
      "Runde beenden",
      "Abbrechen zur Ergebnisansicht",
      "Nachz\u00fcgler hinzuf\u00fcgen",
      "Ergebnisse",
      "Bericht (PDF)",
      "Neue Session \u2026",
      "Namen pseudonymisieren (TN-01, TN-02, \u2026)",
      "Beitr\u00e4ge",
      "Auswertung",
      "Begriffe aus Themen und Fragen ausblenden",
      "Deskriptive Textauswertung; keine Bewertung der Ideenqualit\u00e4t.",
      "Thema",
      "Alle Themen",
      "Min. gemeinsames Auftreten",
      paste0(
        "Ankn\u00fcpfungsgrad: mittlere Jaccard-\u00dcberlappung der Begriffe ",
        "aufeinanderfolgender Runden je Bogen und Frage. Wortwiederholung ist kein ",
        "Nachweis inhaltlicher Weiterentwicklung."
      ),
      "\u00d8 W\u00f6rter je Beitrag",
      "Abgabequote",
      "Verschiedene Begriffe",
      "Keine Beitraege.",
      "Neue Session starten?",
      "Alle Teilnehmer und Beitraege werden geloescht. Fortfahren?",
      "Alles loeschen und neu starten",
      "Die Session ist nicht in der Lobby.",
      "Bitte alle Gruppen verbinden oder ohne alle Gruppen starten.",
      "Bitte mindestens einen Namen eingeben.",
      "Die Anzahl der Gruppen muss der Themenanzahl entsprechen.",
      "Die Session wurde bereits eingerichtet.",
      "Einstellungen: m\u00fcssen eine benannte Liste sein.",
      "Einstellungen: Schl\u00fcsselnamen m\u00fcssen eindeutig sein.",
      "format: muss brainwriting635-settings/1 sein.",
      "mode: muss individual, group_device oder hot_seat sein.",
      "topics: muss 2 bis 6 Themen enthalten.",
      "groups: Anzahl muss der Anzahl der Themen entsprechen.",
      "Datei: Einstellungsdatei wurde nicht gefunden.",
      "Datei: muss eine YAML-Datei mit h\u00f6chstens 100 KB sein.",
      "Datei: YAML konnte nicht gelesen werden; bitte Syntax pr\u00fcfen.",
      "Close",
      "Browse...",
      "No file selected",
      "Upload complete",
      "Zu wenig Text f\u00fcr diese Ansicht.",
      "Beitr\u00e4ge je Thema und Runde",
      "Top-Begriffe je Thema",
      "Begriffsnetz",
      "Wordcloud",
      "Ankn\u00fcpfung \u00fcber Runden",
      "Mittlere Begriffsoverlappung",
      "Nennungen",
      "Runde",
      "Alle Teilnehmer, Boegen und Beitraege werden geloescht. Vorher exportieren!",
      "Ja, alles loeschen",
      "Hallo",
      "Weitergeben an:",
      "Aktuell:",
      "Gruppe",
      "Bogen",
      "Frage 1 \u2014",
      "Frage 2 \u2014",
      "PDF-Berichte ben\u00f6tigen Cairo-Unterst\u00fctzung.",
      "Neue Session?",
      "Durchsuchen \u2026",
      "Keine Datei ausgew\u00e4hlt"
    ),
    en = c(
      "Develop ideas together",
      "Moderator",
      "Sign in",
      "Incorrect PIN.",
      "Joining is currently unavailable.",
      "Join as a group:",
      "Cancel",
      "Take over",
      "End this session?",
      "Existing contributions will be kept.",
      "Show results",
      "Session finished",
      "This session no longer accepts participants.",
      "The session is being prepared \u2014 leave this page open.",
      "Join with a name or pseudonym:",
      "Name / pseudonym",
      "Join",
      "You have joined. Waiting for the start \u2026",
      "Finished \u2014 thank you!",
      "All contributions are saved. We will now discuss the results together.",
      "Start my turn",
      "Skip",
      "One moment \u2014 you are being assigned to a group \u2026",
      "Build on, add to and develop ideas \u2026",
      "Saves automatically as you type.",
      "No earlier contributions \u2014 you are starting this sheet.",
      "Submitted \u2713",
      "Done \u2014 pass it on",
      "\u2713 Submitted \u2014 you can still edit until the round ends.",
      "Submit",
      "Prepare session",
      "Load settings (YAML)",
      "Play mode",
      "Everyone on their own device",
      "One device per group",
      "Take turns on one device",
      "Topics = groups",
      "Rounds / passes",
      "Seconds / round",
      "Seconds / person",
      "Names (one name per line)",
      "Group names (one per line, optional)",
      "Export settings (YAML)",
      "Save and open lobby",
      "Save and open lobby",
      "Save and start",
      "Topic title",
      "Question 1",
      "Question 2",
      "Review settings",
      "Settings loaded. Please review and confirm.",
      "Lobby",
      "Participants scan the QR code or open:",
      "Start session",
      "Start without all groups",
      "Nobody has joined yet.",
      "End round",
      "Stop and show results",
      "Add late participant",
      "Results",
      "Report (PDF)",
      "New session \u2026",
      "Pseudonymize names (TN-01, TN-02, \u2026)",
      "Contributions",
      "Analysis",
      "Exclude terms from topics and questions",
      "Descriptive text analysis; not an assessment of idea quality.",
      "Topic",
      "All topics",
      "Minimum co-occurrences",
      paste0(
        "Building-on score: mean Jaccard term overlap between consecutive rounds for ",
        "each sheet and question. Repeated words do not demonstrate idea development."
      ),
      "Mean words per contribution",
      "Submission rate",
      "Distinct terms",
      "No contributions.",
      "Start a new session?",
      "All participants and contributions will be deleted. Continue?",
      "Delete everything and restart",
      "The session is not in the lobby.",
      "Please connect all groups or start without all groups.",
      "Please enter at least one name.",
      "The number of groups must match the number of topics.",
      "The session has already been configured.",
      "Settings: must be a named list.",
      "Settings: key names must be unique.",
      "format: must be brainwriting635-settings/1.",
      "mode: must be individual, group_device or hot_seat.",
      "topics: must contain 2 to 6 topics.",
      "groups: the count must match the number of topics.",
      "File: settings file not found.",
      "File: must be a YAML file no larger than 100 KB.",
      "File: YAML could not be read; please check its syntax.",
      "Close",
      "Browse\u2026",
      "No file selected",
      "Upload complete",
      "Too little text for this view.",
      "Contributions by topic and round",
      "Top terms by topic",
      "Term network",
      "Word cloud",
      "Building on previous rounds",
      "Mean term overlap",
      "Occurrences",
      "Round",
      "All participants, sheets and contributions will be deleted. Export first!",
      "Yes, delete everything",
      "Hello",
      "Pass to:",
      "Current:",
      "Group",
      "Sheet",
      "Question 1 \u2014",
      "Question 2 \u2014",
      "PDF reports require Cairo support.",
      "New session?",
      "Browse\u2026",
      "No file selected"
    ),
    fr = c(
      "D\u00e9velopper les id\u00e9es ensemble",
      "Animation",
      "Se connecter",
      "Code PIN incorrect.",
      "Impossible de rejoindre la session actuellement.",
      "Rejoindre en tant que groupe :",
      "Annuler",
      "Reprendre",
      "Terminer cette session ?",
      "Les contributions existantes seront conserv\u00e9es.",
      "Afficher les r\u00e9sultats",
      "Session termin\u00e9e",
      "Il n\u2019est plus possible de rejoindre cette session.",
      "La session est en pr\u00e9paration \u2014 laissez cette page ouverte.",
      "Rejoindre avec un nom ou un pseudonyme :",
      "Nom / pseudonyme",
      "Participer",
      "Vous avez rejoint la session. En attente du d\u00e9marrage\u2026",
      "Termin\u00e9 \u2014 merci !",
      paste0(
        "Toutes les contributions sont enregistr\u00e9es. Nous allons discuter des ",
        "r\u00e9sultats ensemble."
      ),
      "Commencer mon tour",
      "Passer",
      "Un instant \u2014 attribution d\u2019un groupe en cours\u2026",
      "Reprendre, compl\u00e9ter et d\u00e9velopper les id\u00e9es\u2026",
      "Enregistrement automatique pendant la saisie.",
      "Aucune contribution pr\u00e9c\u00e9dente \u2014 vous commencez cette fiche.",
      "Envoy\u00e9 \u2713",
      "Termin\u00e9 \u2014 passer l\u2019appareil",
      "\u2713 Envoy\u00e9 \u2014 vous pouvez modifier jusqu\u2019\u00e0 la fin de la ronde.",
      "Envoyer",
      "Pr\u00e9parer la session",
      "Charger les param\u00e8tres (YAML)",
      "Mode de participation",
      "Chacun sur son appareil",
      "Un appareil par groupe",
      "\u00c0 tour de r\u00f4le sur un appareil",
      "Th\u00e8mes = groupes",
      "Rondes / passages",
      "Secondes / ronde",
      "Secondes / personne",
      "Noms (un nom par ligne)",
      "Noms des groupes (un par ligne, facultatif)",
      "Exporter les param\u00e8tres (YAML)",
      "Enregistrer et ouvrir la salle d\u2019attente",
      "Enregistrer et ouvrir la salle d\u2019attente",
      "Enregistrer et d\u00e9marrer",
      "Titre du th\u00e8me",
      "Question 1",
      "Question 2",
      "V\u00e9rifier les param\u00e8tres",
      "Param\u00e8tres charg\u00e9s. Veuillez v\u00e9rifier et confirmer.",
      "Salle d\u2019attente",
      "Les participants scannent le code QR ou ouvrent :",
      "D\u00e9marrer la session",
      "D\u00e9marrer sans tous les groupes",
      "Personne n\u2019a encore rejoint la session.",
      "Terminer la ronde",
      "Arr\u00eater et afficher les r\u00e9sultats",
      "Ajouter un retardataire",
      "R\u00e9sultats",
      "Rapport (PDF)",
      "Nouvelle session\u2026",
      "Pseudonymiser les noms (TN-01, TN-02, \u2026)",
      "Contributions",
      "Analyse",
      "Exclure les termes des th\u00e8mes et des questions",
      paste0(
        "Analyse descriptive du texte ; aucune \u00e9valuation de la qualit\u00e9 des ",
        "id\u00e9es."
      ),
      "Th\u00e8me",
      "Tous les th\u00e8mes",
      "Cooccurrences minimales",
      paste0(
        "Indice de continuit\u00e9 : chevauchement moyen de Jaccard entre les termes ",
        "de rondes cons\u00e9cutives par fiche et question. La r\u00e9p\u00e9tition de ",
        "mots ne d\u00e9montre pas un d\u00e9veloppement des id\u00e9es."
      ),
      "Moyenne de mots par contribution",
      "Taux d\u2019envoi",
      "Termes distincts",
      "Aucune contribution.",
      "D\u00e9marrer une nouvelle session ?",
      "Tous les participants et toutes les contributions seront supprim\u00e9s. Continuer ?",
      "Tout supprimer et recommencer",
      "La session n\u2019est pas dans la salle d\u2019attente.",
      "Veuillez connecter tous les groupes ou d\u00e9marrer sans eux.",
      "Veuillez saisir au moins un nom.",
      "Le nombre de groupes doit correspondre au nombre de th\u00e8mes.",
      "La session est d\u00e9j\u00e0 configur\u00e9e.",
      "Param\u00e8tres : une liste nomm\u00e9e est requise.",
      "Param\u00e8tres : les noms des cl\u00e9s doivent \u00eatre uniques.",
      "format : doit \u00eatre brainwriting635-settings/1.",
      "mode : doit \u00eatre individual, group_device ou hot_seat.",
      "topics : doit contenir 2 \u00e0 6 th\u00e8mes.",
      "groups : le nombre doit correspondre au nombre de th\u00e8mes.",
      "Fichier : fichier de param\u00e8tres introuvable.",
      "Fichier : doit \u00eatre un fichier YAML de 100 Ko maximum.",
      "Fichier : lecture YAML impossible ; veuillez v\u00e9rifier la syntaxe.",
      "Fermer",
      "Parcourir\u2026",
      "Aucun fichier s\u00e9lectionn\u00e9",
      "T\u00e9l\u00e9versement termin\u00e9",
      "Texte insuffisant pour cette vue.",
      "Contributions par th\u00e8me et par ronde",
      "Termes principaux par th\u00e8me",
      "R\u00e9seau de termes",
      "Nuage de mots",
      "Continuit\u00e9 entre les rondes",
      "Chevauchement moyen des termes",
      "Occurrences",
      "Ronde",
      paste0(
        "Tous les participants, fiches et contributions seront supprim\u00e9s. ",
        "Exportez-les d\u2019abord !"
      ),
      "Oui, tout supprimer",
      "Bonjour",
      "Passer \u00e0 :",
      "Actuellement :",
      "Groupe",
      "Fiche",
      "Question 1 \u2014",
      "Question 2 \u2014",
      "Les rapports PDF n\u00e9cessitent la prise en charge de Cairo.",
      "Nouvelle session ?",
      "Parcourir\u2026",
      "Aucun fichier s\u00e9lectionn\u00e9"
    )
  )

  rbind(dictionary, data.frame(
    de = c("Beispiel-Fragenset verwenden", paste0(
      "Drei bearbeitbare Beispielthemen nach dem 6-3-5-Prinzip: neue Ideen notieren, ",
      "Ideen von oben weiterentwickeln. ",
      "Abw\u00e4hlen stellt die vorherigen Themen wieder her."
    )),
    en = c("Use example questions", paste0(
      "Three editable example topics following the 6-3-5 principle: note new ideas, ",
      "develop ideas from above. ",
      "Uncheck to restore the previous topics."
    )),
    fr = c("Utiliser les exemples de questions", paste0(
      "Trois exemples de th\u00e8mes modifiables selon le principe 6-3-5 : noter de ",
      "nouvelles id\u00e9es, d\u00e9velopper les id\u00e9es ci-dessus. ",
      "D\u00e9cochez pour restaurer les th\u00e8mes pr\u00e9c\u00e9dents."
    ))
  ), data.frame(de = c("Anzahl der Fragen", "Verwendete Fragen"),
                en = c("Number of questions", "Questions used"),
                fr = c("Nombre de questions", "Questions utilis\u00e9es")),
  session_dictionary(), plenum_dictionary())
}

#' Interface strings of the plenum weighting
#' @return A data frame with German, English and French strings.
#' @keywords internal
#' @noRd
plenum_dictionary <- function() {
  rows <- list(
    c("Gewichtung", "Weighting", "Pond\u00e9ration"),
    c("Gewichtung im Plenum", "Plenum weighting", "Pond\u00e9ration en pl\u00e9ni\u00e8re"),
    c("Analyse & Plenum", "Analysis & plenum", "Analyse & pl\u00e9ni\u00e8re"),
    c("Danke!", "Thank you!", "Merci !"),
    c(paste("Verteile je Thema 100 % auf die Beitr\u00e4ge, die dir am wichtigsten sind.",
            "Alle Beitr\u00e4ge sind anonym; gespeichert wird automatisch."),
      paste("Distribute 100 % per topic across the contributions that matter most to you.",
            "All contributions are anonymous; changes save automatically."),
      paste("R\u00e9partissez 100 % par th\u00e8me entre les contributions qui comptent le plus",
            "pour vous. Toutes sont anonymes ; l\u2019enregistrement est automatique.")),
    c("Die Gewichtung ist abgeschlossen. Die Ergebnisse besprechen wir jetzt im Plenum.",
      "The weighting has ended. We will now discuss the results together.",
      paste("La pond\u00e9ration est termin\u00e9e.",
            "Nous discutons maintenant des r\u00e9sultats ensemble.")),
    c("Alle haben gewichtet.", "Everyone has weighted.", "Tout le monde a pond\u00e9r\u00e9."),
    c("Die Moderation beendet die Gewichtung.", "The facilitator ends the weighting.",
      "L\u2019animation termine la pond\u00e9ration."),
    c("Es wurden keine Gewichte vergeben.", "No weights were given.",
      "Aucune pond\u00e9ration n\u2019a \u00e9t\u00e9 attribu\u00e9e."),
    c("Es gibt noch keine Beitr\u00e4ge zum Gewichten.",
      "There are no contributions to weight yet.",
      "Il n\u2019y a pas encore de contributions \u00e0 pond\u00e9rer."),
    c("Gewichtung starten", "Start weighting", "Lancer la pond\u00e9ration"),
    c("Gewichtung l\u00e4uft.", "Weighting in progress.", "Pond\u00e9ration en cours."),
    c("Gewichtung l\u00e4uft", "Weighting", "Pond\u00e9ration en cours"),
    c("Gewichtung beenden", "End weighting", "Terminer la pond\u00e9ration"),
    c("Ergebnisse erscheinen nach dem Beenden.", "Results appear once the weighting ends.",
      "Les r\u00e9sultats s\u2019affichent \u00e0 la fin de la pond\u00e9ration."),
    c("Gewichtung beendet.", "Weighting ended.", "Pond\u00e9ration termin\u00e9e."),
    c("Wieder \u00f6ffnen", "Reopen", "Rouvrir"),
    c("Alle verteilen je Thema 100 % auf die anonym gezeigten Beitr\u00e4ge, auch auf eigene.",
      "Everyone distributes 100 % per topic across the anonymous contributions, own ones included.",
      paste("Chacun r\u00e9partit 100 % par th\u00e8me entre les contributions anonymes,",
            "y compris les siennes.")),
    c("Gewichtung (CSV)", "Weighting (CSV)", "Pond\u00e9ration (CSV)"),
    c("Die Gewichtung ist auch in XLSX, RDS, Markdown und im PDF-Bericht enthalten.",
      "The weighting is also included in XLSX, RDS, Markdown and the PDF report.",
      "La pond\u00e9ration figure aussi dans les exports XLSX, RDS, Markdown et le rapport PDF."),
    c("Die Gewichtung ist erst nach der Schreibphase m\u00f6glich.",
      "Weighting is only possible after the writing phase.",
      "La pond\u00e9ration n\u2019est possible qu\u2019apr\u00e8s la phase d\u2019\u00e9criture."),
    c("Die Gewichtung l\u00e4uft nicht.", "The weighting is not running.",
      "La pond\u00e9ration n\u2019est pas en cours."),
    c("Bitte zuerst die Gewichtung beenden.", "Please end the weighting first.",
      "Veuillez d\u2019abord terminer la pond\u00e9ration.")
  )
  stats::setNames(as.data.frame(do.call(rbind, rows)), c("de", "en", "fr"))
}

#' Interface strings of the session administration
#' @return A data frame with German, English and French strings.
#' @keywords internal
#' @noRd
session_dictionary <- function() {
  rows <- list(
    c("Dunkles Design", "Dark theme", "Th\u00e8me sombre"),
    c("CSV f\u00fcr Tabellenkalkulation absichern (Formeln entsch\u00e4rfen)",
      "Make CSV safe for spreadsheets (defuse formulas)",
      "S\u00e9curiser le CSV pour les tableurs (neutraliser les formules)"),
    c("Die Anmeldung ist abgelaufen. Bitte PIN erneut eingeben.",
      "The sign-in has expired. Please enter the PIN again.",
      "La connexion a expir\u00e9. Veuillez saisir \u00e0 nouveau le PIN."),
    c("Teilnehmende (geplant)", "Participants (planned)", "Participants (pr\u00e9vus)"),
    c("Namen (optional, ein Name pro Zeile)", "Names (optional, one name per line)",
      "Noms (facultatif, un nom par ligne)"),
    c("Alex\nRobin\n\u2026", "Alex\nRobin\n\u2026", "Alex\nRobin\n\u2026"),
    c(paste("Teilnehmende k\u00f6nnen ihren Namen auch beim Beitritt selbst eingeben",
            "\u2013 gerne ein Pseudonym."),
      "Participants can also enter their own name when joining \u2013 a pseudonym is welcome.",
      paste("Les participants peuvent aussi saisir leur nom en rejoignant",
            "\u2013 un pseudonyme convient.")),
    c("Die Namen legen die Reihenfolge fest \u2013 gerne Pseudonyme.",
      "The names set the order \u2013 pseudonyms are welcome.",
      "Les noms fixent l\u2019ordre \u2013 les pseudonymes sont bienvenus."),
    c("Standard-Session", "Standard session", "Session standard"),
    c("Alle Sessions", "All sessions", "Toutes les sessions"),
    c("Abmelden", "Sign out", "Se d\u00e9connecter"),
    c("Session archiviert", "Session archived", "Session archiv\u00e9e"),
    c("Session nicht gefunden", "Session not found", "Session introuvable"),
    c("Diese Session ist abgeschlossen. Ein Beitritt ist nicht mehr m\u00f6glich.",
      "This session has ended. Joining is no longer possible.",
      "Cette session est termin\u00e9e. Il n\u2019est plus possible de la rejoindre."),
    c("Bitte QR-Code oder Link pr\u00fcfen.", "Please check the QR code or link.",
      "Veuillez v\u00e9rifier le code QR ou le lien."),
    c("Zur Startseite", "To the start page", "Vers la page d\u2019accueil"),
    c("Archiviert \u2014 nur Ansicht und Export.", "Archived \u2014 view and export only.",
      "Archiv\u00e9e \u2014 consultation et export uniquement."),
    c("Zur\u00fccksetzen \u2026", "Reset \u2026", "R\u00e9initialiser \u2026"),
    c("Session zur\u00fccksetzen?", "Reset session?", "R\u00e9initialiser la session ?"),
    c("Einrichtung", "Setup", "Pr\u00e9paration"),
    c("L\u00e4uft", "Running", "En cours"),
    c("Beendet", "Finished", "Termin\u00e9e"),
    c("Datei fehlt", "File missing", "Fichier manquant"),
    c("Archiviert", "Archived", "Archiv\u00e9e"),
    c("Basisadresse", "Base address", "Adresse de base"),
    c("\u00d6ffnen", "Open", "Ouvrir"),
    c("QR-Code", "QR code", "Code QR"),
    c("Neu starten", "Restart", "Relancer"),
    c("Wiederherstellen", "Restore", "Restaurer"),
    c("Archivieren", "Archive", "Archiver"),
    c("L\u00f6schen", "Delete", "Supprimer"),
    c("Zur\u00fccksetzen", "Reset", "R\u00e9initialiser"),
    c("Sessions", "Sessions", "Sessions"),
    c("Jede Session hat eine eigene Adresse und einen eigenen QR-Code.",
      "Every session has its own address and QR code.",
      "Chaque session a sa propre adresse et son propre code QR."),
    c("Neue Session", "New session", "Nouvelle session"),
    c("Aktiv", "Active", "Actives"),
    c("Archiv", "Archive", "Archives"),
    c("Keine Sessions.", "No sessions.", "Aucune session."),
    c("Bezeichnung (optional)", "Label (optional)", "Intitul\u00e9 (facultatif)"),
    c("Erstellen", "Create", "Cr\u00e9er"),
    c("QR-Code herunterladen (PNG)", "Download QR code (PNG)",
      "T\u00e9l\u00e9charger le code QR (PNG)"),
    c("Schlie\u00dfen", "Close", "Fermer"),
    c("Diese Session ist noch nicht eingerichtet.", "This session has not been set up yet.",
      "Cette session n\u2019est pas encore pr\u00e9par\u00e9e."),
    c("Standard-Session archivieren?", "Archive the standard session?",
      "Archiver la session standard ?"),
    c(paste("Die Ergebnisse werden als archivierte Session gespeichert.",
            "Danach ist die Standard-Session f\u00fcr die n\u00e4chste Aktivit\u00e4t leer."),
      paste("The results are saved as an archived session.",
            "The standard session is then empty for the next activity."),
      paste("Les r\u00e9sultats sont enregistr\u00e9s comme session archiv\u00e9e.",
            "La session standard est ensuite vide pour la prochaine activit\u00e9.")),
    c("Standard-Session zur\u00fccksetzen?", "Reset the standard session?",
      "R\u00e9initialiser la session standard ?"),
    c("Session l\u00f6schen?", "Delete session?", "Supprimer la session ?"),
    c(paste("Die Session wird mit allen Teilnehmern und Beitr\u00e4gen",
            "endg\u00fcltig gel\u00f6scht. Vorher exportieren!"),
      paste("The session will be deleted permanently with all participants",
            "and contributions. Export first!"),
      paste("La session sera d\u00e9finitivement supprim\u00e9e avec tous les participants",
            "et toutes les contributions. Exportez d\u2019abord !")),
    c("Endg\u00fcltig l\u00f6schen", "Delete permanently", "Supprimer d\u00e9finitivement"),
    c("Session nicht gefunden.", "Session not found.", "Session introuvable."),
    c("Nur beendete Sessions k\u00f6nnen archiviert werden.",
      "Only finished sessions can be archived.",
      "Seules les sessions termin\u00e9es peuvent \u00eatre archiv\u00e9es."),
    c("Die Session-Datei konnte nicht gel\u00f6scht werden.",
      "The session file could not be deleted.",
      "Le fichier de la session n\u2019a pas pu \u00eatre supprim\u00e9.")
  )
  stats::setNames(as.data.frame(do.call(rbind, rows)), c("de", "en", "fr"))
}

#' Translate exact interface strings for static figures
#' @param text Character vector of interface strings.
#' @param language Language code, de, en or fr.
#' @return Translated strings; unknown strings remain unchanged.
#' @keywords internal
#' @noRd
bw_translate <- function(text, language = "de") {
  if (!language %in% c("en", "fr")) return(text)
  dict <- bw_language_dictionary()
  index <- match(text, dict$de)
  hit <- !is.na(index)
  text[hit] <- dict[[language]][index[hit]]
  text
}

#' Compact accessible three-position language switch
#' @return HTML tags.
#' @keywords internal
#' @noRd
language_switch_ui <- function() {
  shiny::tags$div(
    class = "bw-language", `data-bw-language` = "true", translate = "no",
    style = "width:132px;flex-shrink:0;margin-left:auto;",
    shiny::tags$label(`for` = "bw-language-slider", class = "visually-hidden",
                      "Sprache / Language / Langue"),
    shiny::tags$input(
      id = "bw-language-slider", type = "range", min = 1, max = 3, step = 1, value = 1,
      `aria-label` = "Sprache", `aria-valuetext` = "Deutsch", class = "form-range",
      style = "margin:0;accent-color:#0e6e78;"
    ),
    shiny::tags$div(
      style = "display:flex;justify-content:space-between;font-size:.75rem;",
      shiny::tags$span("DE"), shiny::tags$span("EN"), shiny::tags$span("FR")
    )
  )
}

#' Translate interface nodes in place and preserve editable content
#' @return JavaScript source.
#' @keywords internal
#' @noRd
language_js <- function() {
  dict <- bw_language_dictionary()
  quote_js <- function(x) encodeString(x, quote = '"')
  pairs <- vapply(seq_len(nrow(dict)), function(i) {
    paste0(quote_js(dict$de[i]), ":[", quote_js(dict$en[i]), ",", quote_js(dict$fr[i]), "]")
  }, character(1))
  paste0("(function(){'use strict';const dictionary={", paste(pairs, collapse = ","), "};", r"---(
  const languages=['de','en','fr'];
  const originals=new WeakMap();
  const attributeOriginals=new WeakMap();
  let language='de';
  let scheduled=false;
  let observer;
  const excluded='[translate="no"],[data-bw-user],[data-bw-language],script,style,textarea,'+
    '#analytics_topic option:not([value="all"]),'+
    '.selectize-dropdown [data-value]:not([data-value="all"]),'+
    '.selectize-input [data-value]:not([data-value="all"])';
  const templates=[
    [/^Thema (\d+):$/, 'Topic $1:', 'Th\u00e8me $1 :'],
    [/^Runde (\d+) \u00b7 (.*)$/, 'Round $1 \u00b7 $2', 'Ronde $1 \u00b7 $2'],
    [/^Gruppe (\d+) \u2192 (.*)$/, 'Group $1 \u2192 $2', 'Groupe $1 \u2192 $2'],
    [/^Thema (\d+)$/, 'Topic $1', 'Th\u00e8me $1'],
    [/^Gruppe (\d+)$/, 'Group $1', 'Groupe $1'],
    [/^Bogen (\d+)$/, 'Sheet $1', 'Fiche $1'],
    [/^Frage (\d+)$/, 'Question $1', 'Question $1'],
    [/^Runde (\d+) von (\d+)$/, 'Round $1 of $2', 'Ronde $1 sur $2'],
    [/^Durchgang (\d+) von (\d+)$/, 'Pass $1 of $2', 'Passage $1 sur $2'],
    [/^(\d+) Teilnehmer im Raum$/, '$1 participants in the room', '$1 participants dans la salle'],
    [/^(\d+) Teilnehmer$/, '$1 participants', '$1 participants'],
    [/^(\d+) \/ (\d+) abgegeben$/, '$1 / $2 submitted', '$1 / $2 envoy\u00e9s'],
    [/^Runde (\d+)\/(\d+) \u00b7 Gruppe (\d+) \u00b7 Bogen (\d+)$/,
      'Round $1/$2 \u00b7 Group $3 \u00b7 Sheet $4',
      'Ronde $1/$2 \u00b7 Groupe $3 \u00b7 Fiche $4'],
    [/^Weitergeben an: (.*)$/, 'Pass to: $1', 'Passer \u00e0 : $1'],
    [/^Aktuell: (.*)$/, 'Current: $1', 'Actuellement : $1'],
    [/^Hallo (.*)!$/, 'Hello $1!', 'Bonjour $1 !'],
    [/^Gruppe (.*) ist bereits verbunden \u2014 auf diesem Ger\u00e4t \u00fcbernehmen\?$/,
      'Group $1 is already connected \u2014 take over on this device?',
      'Le groupe $1 est d\u00e9j\u00e0 connect\u00e9 \u2014 reprendre sur cet appareil ?'],
    [/^Gesch\u00e4tzte Arbeitszeit: ([\d.,]+) Minuten \(ohne \u00dcbergaben und Pausen\)\.$/,
      'Estimated working time: $1 minutes (excluding handovers and breaks).',
      'Dur\u00e9e de travail estim\u00e9e : $1 minutes (hors transmissions et pauses).'],
    [/^Mindestens (\d+) Teilnehmer noetig \(aktuell (\d+)\)\.$/,
      'At least $1 participants required (currently $2).',
      'Au moins $1 participants requis (actuellement $2).'],
    [/^(\w+): muss zwischen (\d+) und (\d+) liegen \(ganze Zahl\)\.$/,
      '$1: must be an integer between $2 and $3.',
      '$1 : doit \u00eatre un entier entre $2 et $3.'],
    [/^topics: Thema (\d+) hat keinen Titel\.$/,
      'topics: topic $1 has no title.', 'topics : le th\u00e8me $1 n\u2019a pas de titre.'],
    [/^topics: Thema (\d+) hat keine Frage ([12])\.$/,
      'topics: topic $1 has no question $2.',
      'topics : le th\u00e8me $1 n\u2019a pas de question $2.'],
    [/^(groups|participants): Namen m\u00fcssen nicht-leere Texte sein\.$/,
      '$1: names must be non-empty text.', '$1 : les noms doivent \u00eatre des textes non vides.'],
    [/^(groups|participants): Namen m\u00fcssen nach dem Trimmen eindeutig sein\.$/,
      '$1: names must be unique after trimming whitespace.',
      '$1 : les noms doivent \u00eatre uniques sans les espaces ext\u00e9rieurs.'],
    [/^Unbekannte Einstellungen: (.*)$/, 'Unknown settings: $1', 'Param\u00e8tres inconnus : $1'],
    [/^(\d+) Beitr\u00e4ge$/, '$1 contributions', '$1 contributions'],
    [/^(\d+) Beitrag$/, '$1 contribution', '$1 contribution'],
    [/^Noch (\d+) % zu vergeben$/, '$1 % left to distribute', 'Encore $1 % \u00e0 r\u00e9partir'],
    [/^(\d+) von (\d+) hat 100 % vergeben$/, '$1 of $2 has distributed 100 %',
      '$1 sur $2 a r\u00e9parti 100 %'],
    [/^(\d+) von (\d+) haben 100 % vergeben$/, '$1 of $2 have distributed 100 %',
      '$1 sur $2 ont r\u00e9parti 100 %'],
    [/^1 Person hat gewichtet\.$/, '1 person weighted.', '1 personne a pond\u00e9r\u00e9.'],
    [/^(\d+) Personen haben gewichtet\.$/, '$1 people weighted.',
      '$1 personnes ont pond\u00e9r\u00e9.'],
    [/^R(\d+) \u00b7 F(\d+) \u00b7 1 Unterst\u00fctzende$/, 'R$1 \u00b7 Q$2 \u00b7 1 supporter',
      'R$1 \u00b7 Q$2 \u00b7 1 soutien'],
    [/^R(\d+) \u00b7 F(\d+) \u00b7 (\d+) Unterst\u00fctzende$/,
      'R$1 \u00b7 Q$2 \u00b7 $3 supporters',
      'R$1 \u00b7 Q$2 \u00b7 $3 soutiens'],
    [/^Format (\d+)-2-([\d,]+): (\d+) Themen, (\d+) Runden, (\d+) s pro Runde\.$/,
      function(match, people, minutes, topics, rounds, secs) {
        return 'Format ' + people + '-2-' + minutes.replace(',', '.') + ': ' + topics +
          ' topics, ' + rounds + ' rounds, ' + secs + ' s per round.';
      },
      'Format $1-2-$2 : $3 th\u00e8mes, $4 rondes, $5 s par ronde.'],
    [/^(\d+) Themen, (\d+) Runden, (\d+) s pro Runde\.$/,
      '$1 topics, $2 rounds, $3 s per round.', '$1 th\u00e8mes, $2 rondes, $3 s par ronde.'],
    [/^Je Gruppe 1 Person\.$/, '1 person per group.', '1 personne par groupe.'],
    [/^Je Gruppe (\d+) Personen\.$/, '$1 people per group.', '$1 personnes par groupe.'],
    [/^Je Gruppe (\d+)\u2013(\d+) Personen\.$/, '$1\u2013$2 people per group.',
      '$1\u2013$2 personnes par groupe.'],
    [/^F\u00fcr (\d+) Themen sind mindestens (\d+) Teilnehmende n\u00f6tig\.$/,
      '$1 topics need at least $2 participants.',
      '$1 th\u00e8mes n\u00e9cessitent au moins $2 participants.'],
    [/^Nach (\d+) Runden hat jede Person jedes Thema 1\u00d7 bearbeitet\.$/,
      'After $1 rounds everyone has worked on every topic once.',
      'Apr\u00e8s $1 rondes, chacun a travaill\u00e9 une fois sur chaque th\u00e8me.'],
    [/^Mit (\d+) Runden bearbeitet jede Person (\d+) von (\d+) Themen\.$/,
      'With $1 rounds everyone works on $2 of $3 topics.',
      'Avec $1 rondes, chacun travaille sur $2 des $3 th\u00e8mes.'],
    [/^Nach (\d+) Runden hat jede Person jedes Thema bearbeitet; danach wiederholen .*$/,
      'After $1 rounds everyone has worked on every topic; after that the topics repeat.',
      'Apr\u00e8s $1 rondes, chacun a travaill\u00e9 sur chaque th\u00e8me ; ' +
      'ensuite les th\u00e8mes se r\u00e9p\u00e8tent.'],
    [/^Reihum: (\d+) Personen, (\d+) Durchg\u00e4nge, (\d+) s je Person\.$/,
      'Taking turns: $1 people, $2 passes, $3 s per person.',
      '\u00c0 tour de r\u00f4le : $1 personnes, $2 passages, $3 s par personne.'],
    [/^(\d+) von (\d+) Teilnehmenden$/, '$1 of $2 participants', '$1 participants sur $2'],
    [/^(\d+) von (\d+) Gruppen$/, '$1 of $2 groups', '$1 groupes sur $2'],
    [/^Erstellt: (.*)$/, 'Created: $1', 'Cr\u00e9\u00e9e : $1'],
    [/^Zu viele Fehlversuche\. Bitte in (\d+) s erneut versuchen\.$/,
      'Too many failed attempts. Please try again in $1 s.',
      'Trop de tentatives \u00e9chou\u00e9es. R\u00e9essayez dans $1 s.'],
    [/^Bitte Einstellungen korrigieren:\n([\s\S]*)$/,
      'Please correct the settings:\n$1', 'Veuillez corriger les param\u00e8tres :\n$1']
  ];
  function translate(source){
    if(language==='de') return source;
    const key=source.trim();
    const slot=language==='en'?0:1;
    let result=dictionary[key] ? dictionary[key][slot] : null;
    if(result===null){
      for(const rule of templates){
        if(rule[0].test(key)){result=key.replace(rule[0],rule[slot+1]);break;}
      }
    }
    if(result===null) return source;
    return source.slice(0,source.indexOf(key))+result+source.slice(source.indexOf(key)+key.length);
  }
  function translateNode(node){
    if(!node.parentElement || node.parentElement.closest(excluded)) return;
    const current=node.nodeValue;
    let record=originals.get(node);
    if(!record || current!==record.rendered) record={source:current,rendered:current};
    const result=translate(record.source);
    record.rendered=result;
    originals.set(node,record);
    if(current!==result) node.nodeValue=result;
  }
  function translateAttribute(element,name){
    const current=element.getAttribute(name);
    if(current===null) return;
    let records=attributeOriginals.get(element);
    if(!records){records={};attributeOriginals.set(element,records);}
    let record=records[name];
    if(!record || current!==record.rendered) record={source:current,rendered:current};
    const result=translate(record.source);
    record.rendered=result;
    records[name]=record;
    if(current!==result) element.setAttribute(name,result);
  }
  function scan(){
    scheduled=false;
    if(!document.body) return;
    if(observer) observer.disconnect();
    const walker=document.createTreeWalker(document.body,NodeFilter.SHOW_TEXT);
    let node;
    while((node=walker.nextNode())) translateNode(node);
    const attributes='[placeholder],[title],[aria-label],input[type=button],input[type=submit]';
    document.querySelectorAll(attributes)
      .forEach(function(element){
        const skip='[translate="no"],[data-bw-user],[data-bw-language],script,style';
        if(element.closest(skip)) return;
        ['placeholder','title','aria-label'].forEach(name=>translateAttribute(element,name));
        if(element.matches('input[type=button],input[type=submit]')){
          translateAttribute(element,'value');
        }
      });
    document.documentElement.lang=language;
    const slider=document.getElementById('bw-language-slider');
    if(slider){
      slider.value=String(languages.indexOf(language)+1);
      const position=languages.indexOf(language);
      slider.setAttribute('aria-valuetext',['Deutsch','English','Fran\u00e7ais'][position]);
      slider.setAttribute('aria-label',['Sprache','Language','Langue'][position]);
    }
    if(observer) observer.observe(document.body,{subtree:true,childList:true,characterData:true,
      attributes:true,attributeFilter:['placeholder','title','aria-label','value']});
  }
  function send(){
    if(window.Shiny && Shiny.setInputValue){
      Shiny.setInputValue('ui_language',language,{priority:'event'});
    }
  }
  function select(value){
    language=languages.includes(value)?value:'de';
    try{localStorage.setItem('bw_language',language);}catch(ignore){}
    scan();send();
  }
  function boot(){
    try{
      const saved=localStorage.getItem('bw_language');
      if(languages.includes(saved)) language=saved;
    }
    catch(ignore){}
    observer=new MutationObserver(function(){
      if(!scheduled){scheduled=true;requestAnimationFrame(scan);}
    });
    document.addEventListener('input',function(event){
      if(event.target.id==='bw-language-slider') select(languages[Number(event.target.value)-1]);
    });
    document.addEventListener('change',function(event){
      if(event.target.id==='bw-language-slider') select(languages[Number(event.target.value)-1]);
    });
    if(window.jQuery) jQuery(document).on('shiny:connected',send);
    scan();send();
  }
  if(document.readyState==='loading') document.addEventListener('DOMContentLoaded',boot);
  else boot();
})();
)---")
}
