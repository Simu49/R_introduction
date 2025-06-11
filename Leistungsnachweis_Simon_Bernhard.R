# Leistungsnachweis Übung Einführung in die Datenanalyse mit R
# Simon Bernhard
# Universität Bern, FS23

# 25.1 Kleine Auswertungen mit dem European Social Survey

# 1. Laden Sie die Daten des European Social Survey (Auszug) mit load(url("http://www.farys.org/daten/ESS.RDATA")).

# benötigte libraries
library(tidyverse)
library(quantmod)
library(rvest)
library(huxtable)

# setwd("your working dictionary")

load(url("http://www.farys.org/daten/ESS.RDATA")) # Daten laden

# 2. Verwenden Sie die dplyr mit group_by oder data.table um herauszufinden, aus wievielen unterschiedlichen Ländern die Befragten im Datensatz stammen.

# Übersicht über Datensatz mithife der head/tail Funktion verschaffen
head(ess)
tail(ess) 

# Ermitteln der Anzahl unterschiedlichen Länder
ess %>% 
  group_by(Land) %>% # nach Land gruppieren
  summarise(count = unique(Land)) %>% # Zählen der unterschiedlichen/einzigartigen Werte
  nrow() # Ausgabe der Azahl unterschiedlicher Länder

# Anzahl: 29

# 3. Erstellen Sie ein Ranking, welches Land die meisten Befragten hat.
ranking_n <- ess %>% 
  group_by(Land) %>% # gruppieren nach Land
  summarise(n=n()) %>% # Zählen der Anzahl Befragten
  arrange(desc(n)) # Ordnen nach absteigender Reihenfolge

ranking_n # anzeigen des Rankings

# Land mit den meisten Befragten: Deutschland (n = 20'490)

# 4. Welches ist das durchschnittlich glücklichste Land im Datensatz? Welches das unglücklichste?
# Glücklichstes Land
ess %>% 
  group_by(Land) %>% # gruppieren nach Land
  summarize(mean_happy = mean(happy, na.rm = T)) %>% # Berechnen der durchschnittlichen Glücklichkeits Werte 
  arrange(desc(mean_happy)) %>% # Ordnen nach absteigender Reihenfolge
  head(n = 1) # Ausgabe der ersten Reihe

# glücklichstes Land: Dänemark (Durchschnitt: 8.32)

# Unglücklichstes Land
ess %>% 
  group_by(Land) %>% # gruppieren nach Land
  summarize(mean_happy = mean(happy, na.rm = T)) %>% # Berechnen der durchschnittlichen Glücklichkeits Werte 
  arrange(desc(mean_happy)) %>% # Ordnen nach absteigender Reihenfolge
  tail(n = 1) # Ausgabe der letzten Reihe

# unglücklichstes Land: Bulgarien (Durchschnitt 5.30)

# 5. In welchem Land gibt es am meisten “vollkommen glückliche Menschen” (d.h. angegebener Zufriedenheitswert = 10)?
ess %>% 
  group_by(Land) %>% # gruppieren nach Land
  filter(happy==10) %>% # filtern nach Zufriedenheitswert = 10
  summarise(n=n()) %>% # Zusammenfassen der Anzahl
  arrange(desc(n)) %>% # Ordnen nach absteigender Reihenfolge
  head(n=1) # Ausgabe der ersten Reihe

# In Dänemark gibt es am meisten "vollkommen glückliche Menschen" (n = 2108).

# 6.Welches Land hatte von 2008 bis 2010 den grössten Rückgang im Durchschnittsglück?
ess %>% 
  group_by(Land, year) %>% # gruppieren nach Land und Jahr
  filter(year >= 2008, year <=2010) %>% # nur Jahre zwischen 2008 und 2010 behalten
  summarize(mean = mean(happy, na.rm = T)) %>% # Durchschnitt für jedes Jahr berechnen
  pivot_wider(names_from = year, values_from = mean, names_prefix = "year_") %>% # Tabelle ins wide-Format ändern, damit die Durchschnitte eines Landes für die Jahre 2008 und 2010 nebeneinanderstehen
  mutate(difference = year_2010 - year_2008) %>% # neue Spalte mit Jahresdifferenz erstellen
  arrange(difference) %>% # nach aufsteigender Reihenfolge anordnen
  head(n=1) # Ausgabe des Landes mit negativster Differenz

# Land mit grösstem Rückgang im Durchschnittsglück: Irland (Mittelwertsdifferenz = - 0.700)

# 25.2 ggplot Bastelei
# Replizieren Sie so exakt wie möglich die folgende Grafik mit ggplot
getSymbols("LISN.SW",from ="2007-01-01")
df <- data.frame(time(LISN.SW), LISN.SW[,6])

# Grafik
df %>% 
  mutate(pct = LISN.SW.Adjusted/df$LISN.SW.Adjusted[1],
         sadaa = ifelse(pct > 1, "Wachstum", "Rückgang")) %>% # 2 Spalten hinzufügen mit Prozent respektive Wachstum/Rückgang
  ggplot(aes(x = time.LISN.SW., y = pct, group = sadaa, colour = pct >= 1)) +
  scale_colour_manual(values = c("Above" = "green", "Below" = "red")) + # Farbskala damit Werte über 1 grün bzw. unter 1 rot sind
  scale_y_continuous(labels = scales::percent_format(scale = 100), 
                     breaks = seq(0.5, 4.5, 0.5)) + # Festlegen der y-Achse in Prozent und mit vorgegebenen Intervallen
  scale_x_continuous(breaks = seq(as.Date("2008-01-01"), as.Date("2024-01-01"), by = "2 years"),
                     labels = seq(2008, 2024, by = 2)) + # Festlegen der x-Achse als Zeitachse mit vorgegebenen Intervallen
  theme(legend.position = "none") + # Ausblendung der Legende
  ggtitle("Lindt & Sprüngli Kursentwicklung seit Januar 2007") +  # Hinzufügen eines Titels
  labs(x = "Jahr", y = NULL) + # Achsentitel hinzufügen
  geom_ribbon(aes(ymin = 1, ymax = ifelse(pct >= 1, pct, NA)), # grünes Ribbon für Flächen überhalb der 100 % Linie
              fill = rgb(0, 1, 0), alpha = 0.5, outline.type = "upper") +
  geom_ribbon(aes(ymax = 1, ymin = ifelse(pct >= 1, NA, pct), colour = "red"), # rotes Ribbon für Flächen unterhalb der 100 % Linie
              fill = rgb(1, 0, 0), alpha = 0.5, outline.type = "lower") +
  geom_hline(yintercept = 1) + # Hinzufügen einer Linie bei 100 %
  theme_bw() # Festlegen des Themes

# 25.3 Webscraping / Tidying

# 1. Betrachten Sie den Wikipedia-Eintrag zum Klima in Bern: https://de.wikipedia.org/wiki/Bern#Klima. Lesen Sie die Tabelle “Monatliche Durchschnittstemperaturen und -niederschläge für Bern 1981-2010” ein. Verwenden Sie hierfür die Tools aus dem Kapitel “Datenimport aus HTML/XML”, z.B. das Package rvest.
raw <- read_html("https://de.wikipedia.org/wiki/Bern#Klima")
clim_table <- html_table(raw, fill = TRUE, header = FALSE)[[5]]

clim_table <- as.data.frame(clim_table)

head(clim_table)

# 2. Konzentrieren Sie sich auf die ersten drei Zeilen (Monat, Max. Temperatur, Min. Temperatur) und säubern Sie die Daten (vgl. Kapitel “Tidy vs Messy Data”), bis die Daten so (oder ähnlich) aussehen
clim_table <- clim_table[c(1, 3, 4), -c(14, 15)] # Tabelle auf relevante Zeilen und Spalten begrenzen

clim_table <- as.data.frame(t(clim_table)) # Rotieren der Tabelle

colnames(clim_table) <- c("Monat", "Max", "Min") # Spalten entsprechend der Vorgabe benennen

clim_table <- clim_table[-1,] # Löschen der ersten Zeile

rownames(clim_table) <- c(1:12) # Zeilen sinnvoll nummerieren

clim_table$Monat <- c("Januar", "Februar", "März", "April", "Mai", "Juni",
                      "Juli", "August", "September", "Oktober", "November", "Dezember") # Zeilen entsprechend der Vorgabe benennen

# 3. Exportieren Sie die Tabelle, so dass sie hübsch aussieht.
clim_table <- as_hux(clim_table, add_colnames = FALSE) %>% # Tabelle als huxtable formatieren
  insert_row("Monat", "Min", "Max", after = 0) %>% # Zeile hinzufügen, damit Spalten Titel haben
  insert_column(" ", c(1:12)) %>% # Zeile hinzufügen, damit Reihen Titel haben
  set_bold(1,everywhere,TRUE) # erste Zeile fett drucken

quick_docx(clim_table, file = "Tabelle_Klima_Bern.docx") # Exportieren der Tabelle als docx Datei

quick_html(clim_table, file = "Tabelle_Klima_Bern.html") # Exportieren der Tabelle als html Datei
