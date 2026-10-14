
# =========================== DANE =============================================
# Niech zbiór nazywa sie, np. dane.
dane <- readxl::read_excel("Dane_cale.xlsx")
str(dane)
View(dane)
rm(list = ls())

# ========================== PAKIETY ===========================================
install.packages("tidyverse")
install.packages("e1071")
install.packages("ineq")
install.packages("kableExtra")
install.packages("gridExtra")

library(tidyverse)
library(e1071)
library(ineq)
library(kableExtra)
library(gridExtra)
# ====================== OBLICZENIE STATYSTYK ==================================

statystyki <- dane %>%
  # Wybieramy tylko zmienne numeryczne, pomijamy kraj
  select(where(is.numeric)) %>%
  # Przekształcamy do długiego formatu
  pivot_longer(cols = everything(), names_to = "Zmienna", values_to = "Wartosc") %>%
  # Grupujemy po zmiennej
  group_by(Zmienna) %>%
  # Obliczamy wszystkie statystyki
  summarise(
    N                   = n(),
    Srednia             = mean(Wartosc),
    Mediana             = median(Wartosc),
    Min                 = min(Wartosc),
    Max                 = max(Wartosc),
    Odch_Std            = sd(Wartosc),
    Wsp_Zm_Proc         = (Odch_Std / Srednia) * 100,
    Skosnosc            = skewness(Wartosc, type = 2), 
    Gini                = ineq(Wartosc, type = "Gini")
  ) %>%
  # Dodajemy klasyfikacje
  mutate(
    Klasa_Asymetrii = case_when(
      Skosnosc < -1.2 ~ "silna lewostronna",     # Poniżej -1.2
      Skosnosc > 1.2  ~ "silna prawostronna",    # Powyżej 1.2 (Twoja zmiana)
      TRUE            ~ "słaba/umiarkowana"      # Wszystko pomiędzy (-1.2 a 1.2)
    ),
    Klasa_Zmiennosci = case_when(
      Wsp_Zm_Proc < 10 ~ "bardzo mała zmienność",
      Wsp_Zm_Proc < 25 ~ "mała zmienność",
      Wsp_Zm_Proc < 50 ~ "umiarkowana zmienność",
      Wsp_Zm_Proc < 75 ~ "duża zmienność",
      TRUE             ~ "bardzo duża zmienność"
    )
  ) %>%
  # Zaokrąglamy wszystko do 2 miejsc po przecinku
  mutate(across(where(is.numeric), ~ round(., 2))) %>%
  # Sortujemy malejąco po zmienności, tak jak chciałeś
  arrange(desc(Wsp_Zm_Proc))

View(statystyki)


# ====================== HISTOGRAMY NA SZYBKO ==================================

# Szybki przegląd graficzny zmiennych w formie hisgoramu 
dane %>%
  select(where(is.numeric)) %>%
  pivot_longer(cols = everything(), names_to = "Zmienna", values_to = "Wartosc") %>%
  ggplot(aes(x = Wartosc)) +
  geom_histogram(bins = 15, fill = "steelblue", color = "white") +
  facet_wrap(~Zmienna, scales = "free") +
  theme_minimal() +
  labs(title = "Rozkłady zmiennych x1-x15")


# ====================== ŁADNA TABLICA W VIEWERZE ==============================

statystyki %>%
  mutate(
    # Kolorowanie ASYMETRII
    Klasa_Asymetrii = cell_spec(
      Klasa_Asymetrii,
      "html", 
      color = case_when(
        Klasa_Asymetrii == "silna lewostronna"     ~ "#B71C1C", # Ciemny Czerwony
        Klasa_Asymetrii == "słaba/umiarkowana"     ~ "#1B5E20", # Ciemny Zielony
        Klasa_Asymetrii == "silna prawostronna"    ~ "#0D47A1"  # Ciemny Niebieski
      ),
      background = case_when(
        Klasa_Asymetrii == "silna lewostronna"     ~ "#FFCDD2", # Jasny Czerwony
        Klasa_Asymetrii == "słaba/umiarkowana"     ~ "#C8E6C9", # Jasny Zielony
        Klasa_Asymetrii == "silna prawostronna"    ~ "#BBDEFB"  # Jasny Niebieski
      ),
      bold = TRUE
    ),
    
    # Kolorowanie ZMIENNOŚCI
    Klasa_Zmiennosci = cell_spec(
      Klasa_Zmiennosci,
      "html",
      color = case_when(
        Klasa_Zmiennosci == "bardzo mała zmienność"  ~ "#1B5E20", # Ciemny Zielony
        Klasa_Zmiennosci == "mała zmienność"         ~ "#388E3C", # Zielony
        Klasa_Zmiennosci == "umiarkowana zmienność"  ~ "#F57C00", # Pomarańczowy
        Klasa_Zmiennosci == "duża zmienność"         ~ "#E64A19", # Ciemny Pomarańczowy
        Klasa_Zmiennosci == "bardzo duża zmienność"  ~ "#B71C1C"  # Ciemny Czerwony
      ),
      background = case_when(
        Klasa_Zmiennosci == "bardzo mała zmienność"  ~ "#C8E6C9", # Jasny Zielony
        Klasa_Zmiennosci == "mała zmienność"         ~ "#A5D6A7", # Jaśniejszy Zielony
        Klasa_Zmiennosci == "umiarkowana zmienność"  ~ "#FFE0B2", # Jasny Pomarańczowy
        Klasa_Zmiennosci == "duża zmienność"         ~ "#FFCCBC", # Jaśniejszy Pomarańczowy
        Klasa_Zmiennosci == "bardzo duża zmienność"  ~ "#FFCDD2"  # Jasny Czerwony
      ),
      bold = TRUE
    )
  ) %>%
  kbl(escape = FALSE, 
      caption = "<b style='font-size:18px; color:black;'>Statystyki opisowe zmiennych</b>",
      align = "c") %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 12
  ) %>%
  row_spec(0, bold = TRUE, color = "white", background = "blue") %>%
  column_spec(1, bold = TRUE, background = "#f0f0f0") %>%
  scroll_box(width = "100%", height = "850px")


# ====================== PRZYGOTOWANIE DANYCH DO WYKRESÓW ======================

# Łączymy oryginalne dane z OBIEMA klasyfikacjami
dane_wykresy <- dane %>%
  select(where(is.numeric)) %>%
  pivot_longer(cols = everything(), names_to = "Zmienna", values_to = "Wartosc") %>%
  left_join(
    statystyki %>% select(Zmienna, Klasa_Asymetrii, Klasa_Zmiennosci, Wsp_Zm_Proc, Skosnosc), 
    by = "Zmienna"
  )

# Sprawdzenie
head(dane_wykresy, 20)

# ====================== HISTOGRAMY WEDŁUG ASYMETRII ===========================

# --- GRUPA 1: SILNA ASYMETRIA LEWOSTRONNA ---
dane_wykresy %>%
  filter(Klasa_Asymetrii == "silna lewostronna") %>%
  ggplot(aes(x = Wartosc)) +
  geom_histogram(bins = 15, fill = "#ef5350", color = "white", alpha = 0.8) +
  facet_wrap(~Zmienna, scales = "free") +
  theme_minimal() +
  labs(
    title = "Grupa 1: Silna asymetria lewostronna (Skośność < -1.2)",
    subtitle = "Większość wartości wysokich, ogon w stronę niskich",
    x = "Wartość", y = "Liczba miast"
  ) +
  theme(strip.text = element_text(size = 11, face = "bold"))


# --- GRUPA 2: ASYMETRIA SŁABA / UMIARKOWANA ---
dane_wykresy %>%
  filter(Klasa_Asymetrii == "słaba/umiarkowana") %>%
  ggplot(aes(x = Wartosc)) +
  geom_histogram(bins = 15, fill = "#66bb6a", color = "white", alpha = 0.8) +
  facet_wrap(~Zmienna, scales = "free") +
  theme_minimal() +
  labs(
    title = "Grupa 2: Asymetria słaba/umiarkowana (-1.2 do 1.2)",
    subtitle = "Rozkłady zbliżone do symetrycznych",
    x = "Wartość", y = "Liczba miast"
  ) +
  theme(strip.text = element_text(size = 11, face = "bold"))

# --- GRUPA 3: SILNA ASYMETRIA PRAWOSTRONNA ---
dane_wykresy %>%
  filter(Klasa_Asymetrii == "silna prawostronna") %>%
  ggplot(aes(x = Wartosc)) +
  geom_histogram(bins = 15, fill = "#42a5f5", color = "white", alpha = 0.8) +
  facet_wrap(~Zmienna, scales = "free") +
  theme_minimal() +
  labs(
    title = "Grupa 3: Silna asymetria prawostronna (Skośność > 1.2)",
    subtitle = "Większość wartości niskich, pojedyncze bardzo wysokie (np. PKB)",
    x = "Wartość", y = "Liczba miast"
  ) +
  theme(strip.text = element_text(size = 11, face = "bold"))

# ====================== HISTOGRAMY WEDŁUG ZMIENNOŚCI ==========================

# --- ZMIENNOŚĆ: BARDZO MAŁA I MAŁA (<25%) ---
dane_wykresy %>%
  filter(Klasa_Zmiennosci %in% c("bardzo mała zmienność", "mała zmienność")) %>%
  ggplot(aes(x = Wartosc)) +
  geom_histogram(bins = 15, fill = "#66bb6a", color = "white", alpha = 0.8) +
  facet_wrap(~Zmienna, scales = "free") +
  theme_minimal() +
  labs(
    title = "Zmienne o małej zmienności (Wsp. Zm. < 25%)",
    subtitle = "Stabilne, jednolite wartości między miastami",
    x = "Wartość", y = "Liczba miast"
  ) +
  theme(strip.text = element_text(size = 11, face = "bold"))


# --- ZMIENNOŚĆ: UMIARKOWANA (25-50%) ---
dane_wykresy %>%
  filter(Klasa_Zmiennosci == "umiarkowana zmienność") %>%
  ggplot(aes(x = Wartosc)) +
  geom_histogram(bins = 15, fill = "#FFA726", color = "white", alpha = 0.8) +
  facet_wrap(~Zmienna, scales = "free") +
  theme_minimal() +
  labs(
    title = "Zmienne o umiarkowanej zmienności (Wsp. Zm. 25-50%)",
    subtitle = "Średnie zróżnicowanie między miastami",
    x = "Wartość", y = "Liczba miast"
  ) +
  theme(strip.text = element_text(size = 11, face = "bold"))


# --- ZMIENNOŚĆ: DUŻA I BARDZO DUŻA (>50%) ---
dane_wykresy %>%
  filter(Klasa_Zmiennosci %in% c("duża zmienność", "bardzo duża zmienność")) %>%
  ggplot(aes(x = Wartosc)) +
  geom_histogram(bins = 15, fill = "#ef5350", color = "white", alpha = 0.8) +
  facet_wrap(~Zmienna, scales = "free") +
  theme_minimal() +
  labs(
    title = "Zmienne o dużej zmienności (Wsp. Zm. > 50%)",
    subtitle = "Silne zróżnicowanie - duże różnice między miastami",
    x = "Wartość", y = "Liczba miast"
  ) +
  theme(strip.text = element_text(size = 11, face = "bold"))


# ====================== WYKRES SYNTETYCZNY: ASYMETRIA vs. ZMIENNOŚĆ ===========

statystyki %>%
  ggplot(aes(x = Skosnosc, y = Wsp_Zm_Proc, color = Klasa_Asymetrii, shape = Klasa_Zmiennosci)) +
  geom_point(size = 4, alpha = 0.8) +
  geom_text(aes(label = Zmienna), vjust = -0.8, size = 3, color = "black") +
  geom_vline(xintercept = c(-1.2, 1.2), linetype = "dashed", color = "gray50") +
  geom_hline(yintercept = c(25, 50), linetype = "dashed", color = "gray50") +
  scale_color_manual(
    values = c(
      "silna lewostronna" = "#ef5350",
      "słaba/umiarkowana" = "#66bb6a",
      "silna prawostronna" = "#42a5f5"
    )
  ) +
  theme_minimal() +
  labs(
    title = "Mapa zmiennych: Asymetria vs. Zmienność",
    subtitle = "Linie przerywane = granice klasyfikacji",
    x = "Skośność (asymetria)",
    y = "Współczynnik zmienności (%)",
    color = "Klasa asymetrii",
    shape = "Klasa zmienności"
  ) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical"  # Legendy wertykalnie (jedna pod drugą)
  )


# ====================== RANKING ZMIENNYCH =====================================

# Ranking po zmienności
statystyki %>%
  ggplot(aes(x = reorder(Zmienna, Wsp_Zm_Proc), y = Wsp_Zm_Proc, fill = Klasa_Zmiennosci)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "bardzo mała zmienność" = "#66bb6a",
      "mała zmienność" = "#9ccc65",
      "umiarkowana zmienność" = "#FFA726",
      "duża zmienność" = "#FF7043",
      "bardzo duża zmienność" = "#ef5350"
    )
  ) +
  theme_minimal() +
  labs(
    title = "Ranking zmiennych według zmienności",
    x = "Zmienna",
    y = "Współczynnik zmienności (%)",
    fill = "Klasa zmienności"
  ) +
  theme(legend.position = "bottom")


# Ranking po asymetrii (wartość bezwzględna)
statystyki %>%
  mutate(Abs_Skosnosc = abs(Skosnosc)) %>%
  ggplot(aes(x = reorder(Zmienna, Abs_Skosnosc), y = Skosnosc, fill = Klasa_Asymetrii)) +
  geom_col() +
  coord_flip() +
  geom_hline(yintercept = c(-1.2, 1.2), linetype = "dashed", color = "gray30") +
  scale_fill_manual(
    values = c(
      "silna lewostronna" = "#ef5350",
      "słaba/umiarkowana" = "#66bb6a",
      "silna prawostronna" = "#42a5f5"
    )
  ) +
  theme_minimal() +
  labs(
    title = "Ranking zmiennych według asymetrii",
    x = "Zmienna",
    y = "Skośność",
    fill = "Klasa asymetrii"
  ) +
  theme(legend.position = "bottom")


################################################################################
################################################################################

# ====================== MACIERZ KORELACJI - HEATMAPA ==========================

install.packages("corrplot")
library(corrplot)

dane %>%
  select(where(is.numeric)) %>%
  cor(use = "complete.obs") %>%
  corrplot(
    method = "color", 
    type = "upper",
    tl.col = "black", 
    tl.srt = 45,
    addCoef.col = "black", 
    number.cex = 0.6,
    col = colorRampPalette(c("#ef5350", "white", "#42a5f5"))(200),
    # CZERWONY = ujemne, NIEBIESKI = dodatnie
    title = "Macierz korelacji zmiennych",
    mar = c(0,0,2,0)
  )

# ==============================================================================
# INNE MODYFIKACJE

# Najpierw wylicz macierz korelacji raz
cor_matrix <- dane %>%
  select(where(is.numeric)) %>%
  cor(use = "complete.obs")

corrplot(
  cor_matrix,
  method = "circle",         # Okręgi (wielkość = siła korelacji)
  type = "upper",
  tl.col = "black",
  tl.srt = 45,
  col = colorRampPalette(c("#ef5350", "white", "#42a5f5"))(200),
  title = "Wariant: Okręgi (wielkość = siła korelacji)",
  mar = c(0,0,2,0)
)


corrplot(
  cor_matrix,
  method = "shade",          # Zacienione kwadraty
  type = "upper",
  tl.col = "black",
  tl.srt = 45,
  col = colorRampPalette(c("#ef5350", "white", "#42a5f5"))(200),
  title = "Wariant: Zacienione",
  mar = c(0,0,2,0)
)

corrplot(
  cor_matrix,
  method = "square",         # Kwadraty (jak color, ale bardziej geometryczne)
  type = "upper",
  tl.col = "black",
  tl.srt = 45,
  addCoef.col = "black",
  number.cex = 0.6,
  col = colorRampPalette(c("#ef5350", "white", "#42a5f5"))(200),
  title = "Wariant: Kwadraty",
  mar = c(0,0,2,0)
)

corrplot(
  cor_matrix,
  method = "color",
  type = "full",             # Pełna macierz (nie tylko górny trójkąt)
  tl.col = "black",
  tl.srt = 45,
  addCoef.col = "black",
  number.cex = 0.5,
  col = colorRampPalette(c("#ef5350", "white", "#42a5f5"))(200),
  title = "Wariant: Pełna macierz",
  mar = c(0,0,2,0)
)

# ====================== KORELACJE - WERSJA GGPLOT =============================

install.packages("reshape2")
library(reshape2)

cor_matrix <- dane %>%
  select(where(is.numeric)) %>%
  cor(use = "complete.obs")

# Przekształcamy do długiego formatu
cor_matrix %>%
  melt() %>%
  ggplot(aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), size = 2.5, color = "black") +
  scale_fill_gradient2(
    low = "#ef5350",      # CZERWONY = ujemne ✅
    mid = "white", 
    high = "#42a5f5",     # NIEBIESKI = dodatnie ✅
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_blank()
  ) +
  labs(
    title = "Macierz korelacji Pearsona",
    fill = "Korelacja"
  )

# ====================== TOP KORELACJE (bez powtórzeń) =========================

cor_matrix_upper <- cor_matrix
cor_matrix_upper[lower.tri(cor_matrix_upper, diag = TRUE)] <- NA

cor_matrix_upper %>%
  melt(na.rm = TRUE) %>%
  mutate(abs_value = abs(value)) %>%
  arrange(desc(abs_value)) %>%
  slice_head(n = 20) %>%
  ggplot(aes(x = reorder(paste(Var1, "-", Var2), abs_value), 
             y = value, 
             fill = value > 0)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(
    values = c("TRUE" = "#42a5f5", "FALSE" = "#ef5350"),  # ✅ POPRAWIONE!
    #          NIEBIESKI = dodatnie    CZERWONY = ujemne
    labels = c("Dodatnia", "Ujemna")
  ) +
  theme_minimal() +
  labs(
    title = "Top 20 najsilniejszych korelacji",
    x = "Para zmiennych",
    y = "Współczynnik korelacji",
    fill = "Typ"
  ) +
  theme(legend.position = "bottom")

# ====================== ŁADNA TABELA KORELACJI ================================

cor_matrix %>%
  as.data.frame() %>%
  rownames_to_column("Zmienna") %>%
  kbl(digits = 2, 
      caption = "<b>Macierz korelacji Pearsona</b>",
      align = "c") %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 11
  ) %>%
  row_spec(0, bold = TRUE, color = "white", background = "blue") %>%
  column_spec(1, bold = TRUE, background = "#f0f0f0")

# ==============================================================================
# ====================== WYKRES ROZRZUTU - PODSTAWOWY ==========================

install.packages("ggrepel")
install.packages("ggpubr")
install.packages("viridis")
library(ggrepel)
library(ggpubr)
library(viridis)

# RÓŻNE PROPOZCYJE DO WYBRANIA

# Wyróżnij top 5 miast (np. wg x2)
dane_highlight <- dane %>%
  mutate(
    Top = ifelse(x2 >= quantile(x2, 0.75), "Top 25%", "Pozostałe")
  )
dane_highlight %>%
  ggplot(aes(x = x1, y = x2)) +
  geom_point(aes(color = Top, size = Top), alpha = 0.8) +
  scale_color_manual(values = c("Top 25%" = "#FF6B6B", "Pozostałe" = "#42a5f5")) +
  scale_size_manual(values = c("Top 25%" = 6, "Pozostałe" = 4)) +
  geom_text_repel(
    aes(label = Miasto), 
    size = 3.5,
    max.overlaps = 30,
    box.padding = 0.6,
    point.padding = 0.5,
    segment.color = "grey60",
    segment.size = 0.3,
    fontface = "bold"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 13, face = "bold"),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "grey90", linewidth = 0.4),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "x1 vs x2",
    x = "x1",
    y = "x2",
    color = NULL,
    size = NULL
  )

#########################

dane %>%
  ggplot(aes(x = x1, y = x2)) +
  geom_point(aes(color = x2), size = 5, alpha = 0.8) +
  scale_color_gradient2(
    low = "#ef5350",
    mid = "#FFA726", 
    high = "#42a5f5",
    midpoint = median(dane$x2, na.rm = TRUE)
  ) +
  geom_text_repel(
    aes(label = Miasto), 
    size = 3.5,
    max.overlaps = 30,
    box.padding = 0.6,
    point.padding = 0.4,
    segment.color = "grey60",
    segment.size = 0.3,
    fontface = "bold"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 13, face = "bold"),
    legend.position = "right",
    panel.grid.major = element_line(color = "grey90", linewidth = 0.4),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "x1 vs x2",
    x = "x1",
    y = "x2",
    color = "Wartość x2"
  )

#########################

dane %>%
  ggplot(aes(x = x1, y = x2, size = x3)) +
  geom_point(alpha = 0.7, color = "#42a5f5") +
  scale_size_continuous(range = c(3, 12)) +
  geom_text_repel(
    aes(label = Miasto), 
    size = 3.5,
    max.overlaps = 30,
    box.padding = 0.7,
    point.padding = 0.6,
    segment.color = "grey50",
    segment.size = 0.3,
    fontface = "bold"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 13, face = "bold"),
    legend.position = "right",
    panel.grid.major = element_line(color = "grey90", linewidth = 0.4),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "x1 vs x2",
    x = "x1",
    y = "x2",
    size = "x3"
  )

#########################

dane %>%
  ggplot(aes(x = x1, y = x2)) +
  geom_point(aes(color = x2), size = 6, alpha = 0.85) +
  scale_color_viridis(option = "plasma") +
  geom_text_repel(
    aes(label = Miasto), 
    size = 3.5,
    max.overlaps = 30,
    box.padding = 0.6,
    point.padding = 0.5,
    segment.color = "grey60",
    segment.size = 0.3,
    fontface = "bold"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 13, face = "bold"),
    legend.position = "right",
    panel.grid.major = element_line(color = "grey92", linewidth = 0.4),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "x1 vs x2",
    x = "x1",
    y = "x2",
    color = "Wartość x2"
  )

#########################

dane %>%
  ggplot(aes(x = x1, y = x2)) +
  geom_point(size = 5.5, color = "#42a5f5", alpha = 0.85) +
  geom_point(size = 2.5, color = "white", alpha = 0.9) +  # Biały środek
  geom_text_repel(
    aes(label = Miasto), 
    size = 3.5,
    max.overlaps = 30,
    box.padding = 0.6,
    point.padding = 0.5,
    segment.color = "grey50",
    segment.size = 0.3,
    fontface = "bold"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 13, face = "bold"),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.4),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "x1 vs x2",
    x = "x1",
    y = "x2"
  )

#########################

dane %>%
  ggplot(aes(x = x1, y = x2)) +
  geom_point(size = 3, alpha = 0.8, shape = 21, fill = "#42a5f5", color = "white", stroke = 1) +
  geom_text_repel(
    aes(label = Miasto), 
    size = 3, 
    max.overlaps = 20,
    box.padding = 0.5,
    point.padding = 0.3,
    segment.color = "gray70",
    segment.size = 0.2,
    min.segment.length = 0
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray95"),
    plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 10)),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  labs(
    title = "x1 vs x2",
    x = "x1",
    y = "x2"
  )

# ==============================================================================
install.packages("corrplot")
library(corrplot)
# Wybierz 4 zmienne i oblicz korelacje
cor_selected <- dane %>%
  select(x1, x2, x3, x5) %>%
  cor(use = "complete.obs")

# Wykres corrplot
corrplot(
  cor_selected,
  method = "color",
  type = "upper",
  tl.col = "black",
  tl.srt = 0,
  tl.cex = 1.2,
  addCoef.col = "black",
  number.cex = 1.2,
  col = colorRampPalette(c("#ef5350", "white", "#42a5f5"))(200),
  title = "Macierz korelacji: x1, x2, x3, x5",
  mar = c(0, 0, 2, 0),
  cl.cex = 1
)

#####################

install.packages("reshape2")
library(reshape2)

cor_selected <- dane %>%
  select(x1, x2, x3, x5) %>%
  cor(use = "complete.obs")

cor_selected %>%
  melt() %>%
  ggplot(aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = round(value, 2)), size = 6, fontface = "bold") +
  scale_fill_gradient2(
    low = "#ef5350",
    mid = "white",
    high = "#42a5f5",
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_blank(),
    axis.text = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    panel.grid = element_blank()
  ) +
  labs(
    title = "Macierz korelacji: x1, x2, x3, x5",
    fill = "Korelacja"
  ) +
  coord_fixed()

########################

# x1 vs x2
p1 <- dane %>%
  ggplot(aes(x = x1, y = x2)) +
  geom_point(size = 4, alpha = 0.7, color = "#42a5f5") +
  geom_text_repel(aes(label = Miasto), size = 2.5, max.overlaps = 15) +
  theme_minimal() +
  labs(title = "x1 vs x2", x = "x1", y = "x2") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# x1 vs x3
p2 <- dane %>%
  ggplot(aes(x = x1, y = x3)) +
  geom_point(size = 4, alpha = 0.7, color = "#66bb6a") +
  geom_text_repel(aes(label = Miasto), size = 2.5, max.overlaps = 15) +
  theme_minimal() +
  labs(title = "x1 vs x3", x = "x1", y = "x3") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# x2 vs x3
p3 <- dane %>%
  ggplot(aes(x = x2, y = x3)) +
  geom_point(size = 4, alpha = 0.7, color = "#FFA726") +
  geom_text_repel(aes(label = Miasto), size = 2.5, max.overlaps = 15) +
  theme_minimal() +
  labs(title = "x2 vs x3", x = "x2", y = "x3") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# x6 vs x12
p4 <- dane %>%
  ggplot(aes(x = x6, y = x12)) +
  geom_point(size = 4, alpha = 0.7, color = "#ef5350") +
  geom_text_repel(aes(label = Miasto), size = 2.5, max.overlaps = 15) +
  theme_minimal() +
  labs(title = "x6 vs x12", x = "x6", y = "x12") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# Połącz w siatkę 2x2
grid.arrange(p1, p2, p3, p4, ncol = 2,
             top = "Wykresy rozrzutu dla wybranych zmiennych")



###################

# x1 vs x2
p1 <- dane %>%
  ggplot(aes(x = x1, y = x2)) +
  geom_point(size = 4.5, alpha = 0.75, color = "#42a5f5") +
  theme_minimal() +
  labs(title = "x1 vs x2", 
       subtitle = paste("r =", round(cor(dane$x1, dane$x2), 3)),
       x = "x1", y = "x2") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 12))

# x1 vs x3
p2 <- dane %>%
  ggplot(aes(x = x1, y = x3)) +
  geom_point(size = 4.5, alpha = 0.75, color = "#66bb6a") +
  theme_minimal() +
  labs(title = "x1 vs x3",
       subtitle = paste("r =", round(cor(dane$x1, dane$x3), 3)),
       x = "x1", y = "x3") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 12))

# x2 vs x3
p3 <- dane %>%
  ggplot(aes(x = x2, y = x3)) +
  geom_point(size = 4.5, alpha = 0.75, color = "#FFA726") +
  theme_minimal() +
  labs(title = "x2 vs x3",
       subtitle = paste("r =", round(cor(dane$x2, dane$x3), 3)),
       x = "x2", y = "x3") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 12))

# x2 vs x5
p4 <- dane %>%
  ggplot(aes(x = x2, y = x5)) +
  geom_point(size = 4.5, alpha = 0.75, color = "#ef5350") +
  theme_minimal() +
  labs(title = "x2 vs x5",
       subtitle = paste("r =", round(cor(dane$x2, dane$x5), 3)),
       x = "x2", y = "x5") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 12))

# Połącz
grid.arrange(p1, p2, p3, p4, ncol = 2,
             top = grid::textGrob("Wykresy rozrzutu dla wybranych zmiennych", 
                                  gp = grid::gpar(fontsize = 16, fontface = "bold")))


# ==============================================================================

# OWOCNEJ ANALIZY I ZABAWY
library(dplyr)
#install.packages('heplots')
library('heplots')
#install.packages('dplyr')
library('dplyr')
#install.packages('corrplot')
library('corrplot')
library(ggplot2)

# 1
corrplot(kor, method = "square", 
         col = colorRampPalette(c("red", "white", "blue"))(200),
         type = "full",  tl.cex = 1,      # upper, lower jeden trójkąt lub full
         tl.col = "black", tl.srt = 45)           # kolor etykiet na osiach i
# kąt nachylenia etykiet



corrplot(kor, method = "color", 
         type = "full", tl.cex = 0.8,                   # cała macierz korelacji i 
         # rozmiar czcionki etykiet
         title = "Macierz korelacji między zmiennymi", 
         mar = c(0, 0, 2, 0))                   # marginesy (dolny, lewy, górny, prawy)




# Wybór kolumn (popyt na szkoły)
X <- dane %>% dplyr::select("x1", "x7","x9","x10","x16")
X

# Wybór kolumn (środowisko publiczne)
Y <- dane %>% dplyr::select("x11","x17", "x21","x22","x24")
Y

korX <- cor(X)
korY <- cor(Y)

dane
kor <- kor <- cor(dane[, -1])
kor
kor <- round(kor,2)

par(mfrow = c(1,2))
corrplot(korX)
corrplot(korY)

par(mfrow = c(1,1))

correlation_matrix <-cor(X,Y)
correlation_matrix <-round(correlation_matrix, 3)
correlation_matrix

# wizualizacja korelacji pomiędzy zmiennymi X i Y
par(fig = c(0.1, 0.7, 0.2, 1), new = TRUE)
corrplot(correlation_matrix, method = 'color',
         type = 'full',
         tl.cex = 1.5,
         title = "Macierz korelacji między zmiennymi zbioru X i Y",
         mar = c(0, 0, 2, 0)
)

# Analiza korespondencji
install.packages("cfa")
library(cfa)
install.packages('CCP')
library("CCP")

install.packages('CCA')
library("CCA")

correl <- matcor(X,Y)
img.matcor(correl)
img.matcor(correl, type = 2)

AK <- cc(X,Y)
AK
View(AK)

AK[1:2]
AK[1:3]
AK[1:4]
AK[1:5]

AK$xcoef
AK$ycoef

AK$cor


#wnioski wysokie wyniki egzaminów ósmoklasisty (x10 na poziomie 0.88) są napędzane 
#przez to, że w mieście mieszka dużo przedsiębiorców (x8) i ilość nowych mieszkań (x7). 
#Z kolei wydatki z budżetu miasta na państwową oświatę (x11) mają w tym układzie 
#znaczenie wręcz marginalne (zaledwie 0.20) 

# Testowanie istotnosci wymiarów kanonicznych

# Test Lambda Wilksa

install.packages('candisc')
library(candisc)
options(scipen = 999) # pokazuje dużo liczb po przecinku
Wilks <- cancor(X,Y)
print(summary(Wilks))

#Analiza wykazała, że pierwsza zmienna kanoniczna jest wysoce istotna statystycznie (p < 0,001) i odpowiada
#za wyjaśnienie aż 81,89% całkowitej wariancji obu zbiorów. 
#Drugi wymiar kanoniczny również okazał się istotny statystycznie (p = 0,0027), 
#dokładając kolejne 15% wyjaśnionej wariancji. 
#Wymiary trzeci i czwarty odrzucono jako nieistotne (p > 0,05).
#wysoka przedsiębiorczość = dobrze zdane egzaminy


# Inne testy

n <- dim(X)[1]
n
p <- length(X)
p
q <- length(Y)
q

rho <- AK$cor
rho

# testowanie istotności wymaiarów kanonicznych
p.asym(rho, n, p, q, tstat ="Wilks")   #nie ma podstaw do odrzucenia h0, są 2 wymiary
p.asym(rho, n, p, q, tstat ="Hotelling") #nie ma podstaw do odrzucenia h0, są 2 wymiary
p.asym(rho, n, p, q, tstat ="Pillai") #tak samo
p.asym(rho, n, p, q, tstat ="Roy") 


dane
X <- as.matrix(dane[,c("x1", "x7","x9","x10","x16")]) # popyt i zamożność 
X

# I zmienna kan. wyjaśnia 32,49% wariancji zmiennych X - odczytne z wyników
# II zmienna.kan. wyjaśnia 6,41% wariancji zmiennych X - odczytane z wyników
# I zmienna kan. wyjaśnia 30,25% wariancji zmiennych Y - odczytne z wyników
# II zmienna.kan. wyjaśnia 4,46% wariancji zmiennych Y - odczytane z wyników


Y <- as.matrix(dane[,c("x11","x17", "x21","x22","x24")]) # Środowisko publiczne 
Y

cc <- cancor(X, Y, set.names = c('Popyt i zamożność', 'Srodowisko publiczne'))

redundancy(cc)

plot(AK$cor, type = 'b')

plt.cc(AK, var.label = T)

plt.var(AK, 1, 2, int = 0.5, var.label = TRUE, Xnames = NULL, Ynames = NULL)

#Zbiór czynników opisujących "Środowisko publiczne" (Zbiór Y) objaśnia aż 40,38% całkowitej 
#wariancji zmiennych z grupy "Popyt i Zamożność" (Zbiór X). 
#W kierunku odwrotnym współczynnik nadmiarowości wyniósł 36,93%. 
#Należy podkreślić, że dominujący udział w objaśnianiu obu zbiorów 
#ma pierwsza zmienna kanoniczna (odpowiednio 32,5% oraz 30,2%), co w pełni pokrywa się z wynikami testu Lambdy Wilksa.
###################################

# 2
library(knitr)
library(kableExtra)

kbl(kor, caption = "Macierz korelacji") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = F) %>%
  column_spec(1, bold = T) # Pogrubienie nazw wierszy



# 3

kbl(kor, 
    caption = "<span style='color: black; font-weight: bold;'>Macierz korelacji</span>",
    align = "c") %>%           # "c" = center (środek) dla WSZYSTKICH kolumn
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
                full_width = F) %>%
  column_spec(1, bold = T, border_right = T) %>% 
  # Pierwsza kolumna (nazwy): pogrubiona + linia oddzielająca
  column_spec(2:ncol(kor)+1, width = "2cm")      
# Pozostałe kolumny: Stała szerokość 2cm (żeby były równe)




# 4
library(ggplot2)
library(dplyr)
install.packages("ggrepel")
library(ggrepel) # Do ładnych, nienachodzących napisów

# 1. PRZYGOTOWANIE DANYCH
# Zbiór X popyt i zamożność 
df_x <- data.frame(
  Dim1 = AK$scores$corr.X.xscores[,1],
  Dim2 = AK$scores$corr.X.xscores[,2],
  Zmienna = rownames(AK$scores$corr.X.xscores),
  Grupa = "Zbiór X Popyt i zamożność"
)



# Zbiór Y Środowisko publiczne
df_y <- data.frame(
  Dim1 = AK$scores$corr.Y.yscores[,1],
  Dim2 = AK$scores$corr.Y.yscores[,2],
  Zmienna = rownames(AK$scores$corr.Y.yscores),
  Grupa = "Zbiór Y Środowisko publiczne"
)



circle_data <- rbind(df_x, df_y)



# Tło (okrąg)
angle <- seq(0, 2 * pi, length.out = 100)
circle_edge <- data.frame(x = cos(angle), y = sin(angle))


# 2. RYSOWANIE WYKRESU Z DUŻYMI CZCIONKAMI (wykres kołowy zmiennych)
ggplot() +
  
  # A. Tło (Okrąg i osie)
  geom_path(data = circle_edge, aes(x = x, y = y), color = "gray60", linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "gray") +
  
  # B. Strzałki (Tworzą legendę - linie)
  geom_segment(data = circle_data,
               aes(x = 0, y = 0, xend = Dim1, yend = Dim2, color = Grupa),
               arrow = arrow(length = unit(0.3, "cm")), size = 1.5) + # size=1.5 -> Grubsze strzałki
  
  # C. Napisy (show.legend = FALSE -> usuwa "a" z legendy)
  geom_text_repel(data = circle_data,
                  aes(x = Dim1, y = Dim2, label = Zmienna, color = Grupa),
                  size = 6,              # Duża czcionka napisów na kole
                  fontface = "bold", 
                  box.padding = 0.5,
                  show.legend = FALSE) + # <--- TO USUWA LITERKI "a" Z LEGENDY
  
  # D. Kolory
  scale_color_manual(values = c("Zbiór X Popyt i zamożność" = "steelblue", 
                                "Zbiór Y Środowisko publiczne" = "firebrick")) +
  
  # E. Wygląd i CZCIONKI (Powiększone)
  coord_fixed() +
  theme_minimal() +
  labs(title = "Wykres kołowy zmiennych",
       x = "1. Zmienna Kanoniczna",
       y = "2. Zmienna Kanoniczna",
       color = "Zbiór danych") +
  theme(
    legend.position = "bottom",
    
    # 1. DUŻY TYTUŁ WYKRESU
    plot.title = element_text(face = "bold", size = 20),
    
    # 2. DUŻE TYTUŁY OSI (X i Y)
    axis.title = element_text(face = "bold", size = 16),
    
    # 3. DUŻE LICZBY NA OSIACH
    axis.text = element_text(size = 14, color = "black"),
    
    # 4. DUŻA LEGENDA (Tytuł i tekst)
    legend.title = element_text(face = "bold", size = 15),
    legend.text = element_text(size = 14),
    
    # Dłuższe linie w legendzie (żeby było widać kolory)
    legend.key.width = unit(2, "cm")
  )

# 5
##############################################################
#install.packages("arm", dependencies = TRUE)
library(arm)



# ŁADUNKI ZAMIENIONE NA DODATNIE
#install.packages("semPlot", type = "binary")
library(CCA)
library(semPlot)
library(lavaan)
#install.packages("arm")
library(arm)




# ==============================================================================
# 1. PRZYGOTOWANIE DANYCH
# ==============================================================================

vars_X <- c("x1", "x7","x9","x10","x16")
vars_Y <- c("x11","x17", "x21","x22","x24")
X <- dane[, vars_X]
Y <- dane[, vars_Y]

# Obliczenia
cca_result <- cc(X, Y)
rho <- cca_result$cor[1]
lx <- cor(X, cca_result$scores$xscores)[, 1]
ly <- cor(Y, cca_result$scores$yscores)[, 1]
cx <- lx * rho
cy <- ly * rho

# --- FORMATOWANIE: 3 MIEJSCA PO PRZECINKU ---
fmt <- function(x) sprintf("%.3f", x)

# --- ZMIANA: WSZYSTKIE WARTOŚCI DODATNIE (abs) ---
# Używamy funkcji abs(), aby usunąć minusy
wartosci <- abs(c(lx, ly, rho, cy, cx))
etykiety_gotowe <- fmt(wartosci)
#do poprawy
# ==============================================================================
# 2. MODEL LAVAAN (Kolejność linii jest kluczowa dla kolorów!)
# ==============================================================================
model_full <- '
  # GRUPA 1: Ładunki X (4 linie)
  CV_Popyt =~ x1 + x5 + x7 + x8 

  # GRUPA 2: Ładunki Y (4 linie)
  CV_Srodowisko =~ x10 + x11 + x13 + x14

  # GRUPA 3: Korelacja (1 linia)
  CV_Popyt ~~ CV_Srodowisko

  # GRUPA 4: Krzyżowe Y (4 linie)
  CV_Popyt =~ x10 + x11 + x13 + x14

  # GRUPA 5: Krzyżowe X (4 linie)
  CV_Srodowisko =~ x1 + x5 + x7 + x8
'
fit <- cfa(model_full, data = dane)
#ewentualnie 1. Wyciągamy tylko potrzebne kolumny i robimy na nich standaryzację (funkcja scale)
dane_do_modelu <- as.data.frame(scale(dane[, c(vars_X, vars_Y)]))

# 2. Odpalamy model Lavaan na wyrównanych danych
fit <- cfa(model_full, data = dane_do_modelu)
# ==============================================================================
# 3. DEFINICJA KOLORÓW (TĘCZA POŁĄCZEŃ)
# ==============================================================================
# Definiujemy kolory dla każdej grupy linii z osobna
kolory_linii <- c(
  rep("firebrick", 4),    # GRUPA 1: Ładunki X -> Ciemnoczerwony
  rep("navy", 4),         # GRUPA 2: Ładunki Y -> Ciemnoniebieski
  "black",                # GRUPA 3: Korelacja -> Czarny
  rep("#FFB3B3", 4),      # GRUPA 4: Krzyżowe Y -> Jasnoróżowy
  rep("#99CCFF", 4)       # GRUPA 5: Krzyżowe X -> Jasnoniebieski
)



# Grubość linii (Główne grube, krzyżowe cieńsze)
grubosc <- c(rep(2, 8), 3, rep(2, 8))



# ==============================================================================
# 4. UKŁAD I RYSOWANIE
# ==============================================================================
uklad <- matrix(c(
  -1.2,  1.5,  -1.2,  0.5,  -1.2, -0.5,  -1.2, -1.5, # X
  1.2,  1.5,   1.2,  0.5,   1.2, -0.5,   1.2, -1.5, # Y
  -0.4, 0,      0.4, 0                               # Środek
), ncol = 2, byrow = TRUE)



# Krzywizna (tylko dla krzyżowych)
krzywizna <- c(rep(0, 9), rep(2.5, 4), rep(2.8, 4))



semPaths(fit,
         layout = uklad,
         whatLabels = "hide",
         edgeLabels = etykiety_gotowe,
         
         # --- CZCIONKA NA STRZAŁKACH ---
         edge.label.cex = 1.2,      # Duża, czytelna czcionka liczb
         edge.label.bg = "white",   # Białe tło
         
         # --- CZYSTOŚĆ ---
         residuals = FALSE,         # Brak szarych strzałek
         exoVar = FALSE,            # Brak wariancji
         
         # --- CZCIONKI W PROSTOKĄTACH ---
         sizeMan = 12,              # Szerokość prostokątów
         sizeMan2 = 5,              # Wysokość prostokątów
         sizeLat = 11,              # Wielkość kółek
         label.cex = 0.8,           # Mniejsza czcionka w środku prostokąta
         
         # --- LINIE I KOLORY ---
         edge.color = kolory_linii, # Twoje kolory (Czerwony, Granatowy, Różowy...)
         edge.width = grubosc,      # Zróżnicowana grubość
         lty = 1, fixedStyle = 1, freeStyle = 1,
         curve = krzywizna,
         
         # --- KOLORY WĘZŁÓW ---
         groups = list(Miasto = c("CV_Popyt", vars_X), Wyniki = c("CV_Srodowisko", vars_Y)),
         color = c("#FF9999", "#99CCFF"), # Tło węzłów (Czerwony / Niebieski)
         
         # --- MARGINESY ---
         mar = c(3, 5, 3, 5),
         rescale = TRUE,
         legend = FALSE,
         border.width = 1.5)






#Topsis
install.packages("topsis")
library(topsis)

dane_ranking <- dane[, c("Miasto", "x1", "x5","x7","x8","x10","x11","x13","x14")]
macierz <- as.matrix(dane_ranking[-1])
wagi <- rep(1,ncol(macierz))
wpływ <- rep("+", ncol(macierz))
Z <- scale(macierz)
wyniki_topsis <- topsis(macierz, wagi, wpływ)

ranking_topsis <- data.frame(Miasto = 
                               dane_ranking$Miasto, TOPSIS = wyniki_topsis$score)
ranking_topsis <- ranking_topsis[order(-ranking_topsis$TOPSIS),]

print(" TOP 10 MIAST - METODA TOPSIS ")
head(ranking_topsis, 10)
#Dla wszystkich
dane_ranking <- dane[, c(dane)]
macierz <- as.matrix(dane_ranking[-1])
wagi <- rep(1,ncol(macierz))
wpływ <- rep("+", ncol(macierz))
Z <- scale(macierz)
wyniki_topsis <- topsis(macierz, wagi, wpływ)

ranking_topsis <- data.frame(Miasto = 
                               dane_ranking$Miasto, TOPSIS = wyniki_topsis$score)
ranking_topsis <- ranking_topsis[order(-ranking_topsis$TOPSIS),]

print(" TOP 10 MIAST - METODA TOPSIS ")
head(ranking_topsis, 10)




#METODA HELLWIGA

# Krok 1: Standaryzacja danych (sprawiedliwe wyrównanie)
Z <- scale(macierz)

# Krok 2: Znalezienie "Miasta Idealnego" (maksimum z każdej kolumny)
wzorzec <- apply(Z, 2, max)

# Krok 3: Obliczenie odległości każdego miasta od ideału
odleglosci <- sqrt(rowSums(sweep(Z, 2, wzorzec)^2))

# Krok 4: Obliczenie wskaźnika Hellwiga (im bliżej 1, tym lepiej)
d0 <- mean(odleglosci) + 2 * sd(odleglosci)
miara_hellwiga <- 1 - (odleglosci / d0)

# Składamy wyniki z nazwami miast i sortujemy
ranking_hellwig <- data.frame(Miasto = dane_ranking$Miasto, Hellwig = miara_hellwiga)
ranking_hellwig <- ranking_hellwig[order(-ranking_hellwig$Hellwig), ]

print("--- TOP 10 MIAST - METODA HELLWIGA ---")
head(ranking_hellwig, 10)

#dla wszystkich
dane_ranking <- dane[, c(dane)]
Z <- scale(macierz)
wzorzec <- apply(Z, 2, max)
odleglosci <- sqrt(rowSums(sweep(Z, 2, wzorzec)^2))
d0 <- mean(odleglosci) + 2 * sd(odleglosci)
miara_hellwiga <- 1 - (odleglosci / d0)
ranking_hellwig <- data.frame(Miasto = dane_ranking$Miasto, Hellwig = miara_hellwiga)
ranking_hellwig <- ranking_hellwig[order(-ranking_hellwig$Hellwig), ]

print("--- TOP 10 MIAST - METODA HELLWIGA ---")
head(ranking_hellwig, 10)


# SELEKCJA ZMIENNYCH DIAGNOSTYCZNYCH (Kryterium statystyczne)


# KROK 0: Przygotowanie pełnej bazy (tylko liczby, bez nazw miast)
# Zakładamy, że w 1. kolumnie masz nazwy miast, więc ją wycinamy [-1]
dane_numeryczne <- dane[, -1] 

print("--- SITO 1: WSPÓŁCZYNNIK ZMIENNOŚCI (V) ---")
# Szukamy zmiennych, które są zbyt nudne (różnice między miastami to mniej niż 10%)
calc_v <- function(x) { (sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)) * 100 }
wsp_zmiennosci <- sapply(dane_numeryczne, calc_v)

# Wyświetlamy wyniki
print(round(wsp_zmiennosci, 2))

# Wskazujemy te do wyrzucenia (V < 10%)
do_wyrzucenia_V <- names(wsp_zmiennosci[abs(wsp_zmiennosci) < 10])
print(paste("Zmienne do usunięcia (zbyt niska zmienność):", paste(do_wyrzucenia_V, collapse = ", ")))


print("--- SITO 2: ODWRÓCONA MACIERZ KORELACJI ---")
# Szukamy zmiennych, które dublują informacje (zbyt silnie skorelowane)
# Najpierw bierzemy tylko te zmienne, które przetrwały Sito 1
dane_po_sicie1 <- dane_numeryczne[, !(names(dane_numeryczne) %in% do_wyrzucenia_V)]

# Liczymy macierz korelacji i ją odwracamy
R <- cor(dane_po_sicie1)
R_inv <- solve(R)

# Bierzemy tylko liczby z przekątnej odwróconej macierzy
przekatna <- diag(R_inv)

# Wyświetlamy wyniki
print(round(przekatna, 2))

# Jeśli wynik na przekątnej jest > 10, to zmienną trzeba wyrzucić
do_wyrzucenia_R <- names(przekatna[przekatna > 10])
print(paste("Zmienne do usunięcia (dublowanie informacji):", paste(do_wyrzucenia_R, collapse = ", ")))


#kompletna analiza hellwig + zmienność + mst
#to chyba zrobić przed hellwig i topssis (chyba)
install.packages(c("ggplot2","dplyr", "tidyr", "gt", "igraph", "ggraph", "scales"))

library(ggplot2)
library(dplyr)
library(tidyr)
library(gt)
library(igraph)
library(ggraph)
library(scales)

# 1. Przygotowanie danych
dane
df <- as.data.frame(dane)
rownames(df) <- df[, 1]
df_numeryczne <- df[, -1]

#tutaj próg 0,5 ale w pracy 0,8
# metoda parametryczna helwiga (eliminacja par skorelowanych)
#ustawienie progu korelacji
PROG_KORELACJI <- 0.8

# ==========================================================
# 1. FUNKCJA ELIMINACJI (HELLWIG)
# ==========================================================

eliminacja_par_viz <- function(data, r_kryt) {
  zmienne  <- colnames(data)
  iteracja <- 0
  historia <- list()
  repeat {
    if (length(zmienne) < 2) break
    iteracja <- iteracja + 1
    R     <- cor(data[, zmienne, drop = FALSE], use = "pairwise.complete.obs")
    R_abs <- abs(R)
    diag(R_abs) <- 0
    max_r <- max(R_abs, na.rm = TRUE)
    if (max_r < r_kryt) {
      historia[[iteracja]] <- list(iteracja = iteracja, zmienne = zmienne, max_r = max_r, 
                                   para_A = NA, para_B = NA, usunieta = NA, suma_A = NA, 
                                   suma_B = NA, koniec = TRUE)
      break
    }
    max_idx <- which(R_abs == max_r, arr.ind = TRUE)[1, ]
    cecha_A <- zmienne[max_idx[1]]
    cecha_B <- zmienne[max_idx[2]]
    pozostale <- setdiff(zmienne, c(cecha_A, cecha_B))
    if (length(pozostale) > 0) {
      suma_A <- sum(abs(cor(data[, cecha_A], data[, pozostale, drop = FALSE], use = "pairwise.complete.obs")), na.rm = TRUE)
      suma_B <- sum(abs(cor(data[, cecha_B], data[, pozostale, drop = FALSE], use = "pairwise.complete.obs")), na.rm = TRUE)
      do_usuniecia <- if (suma_A >= suma_B) cecha_A else cecha_B
    } else {
      suma_A <- NA; suma_B <- NA
      do_usuniecia <- cecha_B
    }
    historia[[iteracja]] <- list(iteracja = iteracja, zmienne = zmienne, max_r = round(max_r, 3),
                                 para_A = cecha_A, para_B = cecha_B, usunieta = do_usuniecia,
                                 suma_A = round(suma_A, 3), suma_B = round(suma_B, 3), koniec = FALSE)
    zmienne <- setdiff(zmienne, do_usuniecia)
  }
  list(zmienne_finalne = zmienne, historia = historia)
}

wynik    <- eliminacja_par_viz(df_numeryczne, r_kryt = PROG_KORELACJI)
historia <- wynik$historia

# ==========================================================
# 2. WIZUALIZACJE KORELACJI
# ==========================================================

# Wykres 1: Macierz przed
R_full <- cor(df_numeryczne, use = "pairwise.complete.obs")
R_df   <- as.data.frame(as.table(R_full)) %>%
  rename(Zmienna1 = Var1, Zmienna2 = Var2, Korelacja = Freq) %>%
  mutate(Przekracza = abs(Korelacja) >= PROG_KORELACJI & Zmienna1 != Zmienna2,
         Etykieta   = ifelse(Zmienna1 != Zmienna2, round(Korelacja, 2), ""))

print(ggplot(R_df, aes(x = Zmienna1, y = Zmienna2, fill = Korelacja)) +
        geom_tile(color = "white") +
        geom_tile(data = filter(R_df, Przekracza), aes(x = Zmienna1, y = Zmienna2), fill = NA, color = "#E74C3C", linewidth = 1.2) +
        geom_text(aes(label = Etykieta), size = 2.5) +
        scale_fill_gradient2(low = "#2980B9", mid = "white", high = "#E74C3C", limits = c(-1, 1)) +
        labs(title = "Macierz korelacji PRZED eliminacją", x = "", y = "") +
        theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1)))

# Wykres 2: Schemat
siatka <- expand.grid(Zmienna = colnames(df_numeryczne), Iteracja = seq_len(length(historia))) %>% mutate(Status = "Aktywna")
for (i in seq_len(length(historia))) {
  h <- historia[[i]]
  if (!is.null(h$usunieta) && !is.na(h$usunieta)) {
    siatka <- siatka %>% mutate(Status = ifelse(Zmienna == h$usunieta & Iteracja >= i, "Usunięta", Status))
    siatka <- siatka %>% mutate(Status = ifelse(Zmienna == h$usunieta & Iteracja == i, "Usuwana", Status))
  }
}
print(ggplot(siatka, aes(x = Iteracja, y = Zmienna, fill = Status)) +
        geom_tile(color = "white") +
        scale_fill_manual(values = c("Aktywna" = "#D5F5E3", "Usuwana" = "#E74C3C", "Usunięta" = "#BDC3C7")) +
        labs(title = "Schemat eliminacji zmiennych", x = "Iteracja", y = "Zmienna") +
        theme_minimal())

# Wykres 3: Max korelacja
historia_df <- bind_rows(lapply(historia, function(h) {
  data.frame(Iteracja = h$iteracja, Max_r = h$max_r, Usunieta = ifelse(is.na(h$usunieta), "KONIEC", h$usunieta),
             Koniec = h$koniec, stringsAsFactors = FALSE)
}))
print(ggplot(historia_df, aes(x = Iteracja, y = Max_r)) +
        geom_hline(yintercept = PROG_KORELACJI, linetype = "dashed", color = "#E74C3C", linewidth = 1) +
        annotate("text", x = max(historia_df$Iteracja), y = PROG_KORELACJI + 0.05, label = paste("Próg r =", PROG_KORELACJI), color = "#E74C3C", fontface = "bold", size = 3.5) +
        geom_col(aes(fill = Max_r >= PROG_KORELACJI), width = 0.6, alpha = 0.85) +
        geom_text(aes(label = round(Max_r, 3)), vjust = -0.5, fontface = "bold", size = 3.5) +
        geom_label(data = filter(historia_df, !Koniec), aes(y = 0.05, label = paste0("↑ Usuń:\n", Usunieta)), fill = "#FADBD8", color = "#C0392B", size = 3, fontface = "bold", label.padding = unit(0.2, "lines")) +
        scale_fill_manual(values = c("TRUE" = "#E74C3C", "FALSE" = "#27AE60"), name = "") +
        scale_x_continuous(breaks = historia_df$Iteracja) +
        labs(title = "Maksymalna korelacja w kolejnych iteracjach", x = "Iteracja", y = "Maksymalne |r|") +
        theme_minimal())

# Wykres 4: Macierz PO eliminacji (PRZYWRÓCONY)
zmienne_fin <- wynik$zmienne_finalne
R_final     <- cor(df_numeryczne[, zmienne_fin, drop = FALSE], use = "pairwise.complete.obs")
R_fin_df    <- as.data.frame(as.table(R_final)) %>% rename(Zmienna1 = Var1, Zmienna2 = Var2, Korelacja = Freq) %>% mutate(Etykieta = ifelse(Zmienna1 != Zmienna2, round(Korelacja, 2), ""))

print(ggplot(R_fin_df, aes(x = Zmienna1, y = Zmienna2, fill = Korelacja)) +
        geom_tile(color = "white") +
        geom_text(aes(label = Etykieta), size = 3) +
        scale_fill_gradient2(low = "#2980B9", mid = "white", high = "#E74C3C", limits = c(-1, 1)) +
        labs(title = "Macierz korelacji PO eliminacji zmiennych", 
             subtitle = paste0("Pozostałe (", length(zmienne_fin), "): ", paste(zmienne_fin, collapse = ", ")), x = "", y = "") +
        theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1)))


# eliminacja cech o niskiej zmienności
cechy_po_korelacji <- wynik$zmienne_finalne
cechy_po_korelacji
df_po_kor <- df_numeryczne[, cechy_po_korelacji, drop = FALSE]

wspolczynniki_zmiennosci <- sapply(df_po_kor,
                                   function(x) (sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)) * 100)
cechy_finalne <- names(wspolczynniki_zmiennosci[wspolczynniki_zmiennosci >= 10])
print(round(wspolczynniki_zmiennosci,2))
cechy_finalne

#KROK 3: Drzewo mst + profile na cechach finalnych

#bierzemy tylko finalne cechy
df_finalne <- df_numeryczne[, cechy_finalne, drop = FALSE]

#macierz odległości euklidesowych
macierz_odleglosci <- dist(scale(df_finalne), method = "euclidean")
graf_pelny <- graph_from_adjacency_matrix(as.matrix(macierz_odleglosci),
                                          mode = "undirected",
                                          weighted = TRUE)
drzewo_mst <- mst(graf_pelny)

#PODZIAŁ STRUKTURALNY NA 4 KLASY (3 MOSTY)

mosty_krawedziowe <- edge_betweenness(drzewo_mst)
wspolczynnik_mostow <- mosty_krawedziowe * E(drzewo_mst)$weight
indeksy_mostow <- order(wspolczynnik_mostow, decreasing = TRUE)[1:3]

mosty_krawedziowe
wspolczynnik_mostow
indeksy_mostow

drzewo_tymczasowe <- delete_edges(drzewo_mst, indeksy_mostow)
klasy <- components(drzewo_tymczasowe)$membership

drzewo_tymczasowe
klasy

#Atrybuty grafu do wizualizacji 
V(drzewo_mst)$grupa <- as.factor(klasy)
V(drzewo_mst)$nazwa <- rownames(df)
E(drzewo_mst)$etykieta_czysta <- as.character(round(E(drzewo_mst)$weight, 2))
E(drzewo_mst)$czy_most <- FALSE
E(drzewo_mst)$czy_most[indeksy_mostow] <- TRUE

#Wykres drzewa

wykres_drzewo <- (ggraph(drzewo_mst, layout = "stress") +
                    geom_edge_link(aes(label = etykieta_czysta, color = czy_most, linetype = czy_most), 
                                   label_size = 3, label_colour = "black", vjust = -0.6, alpha = 0.7, show.legend = FALSE) +
                    geom_node_point(aes(color = grupa), size = 8) +
                    geom_node_label(aes(label = nazwa), repel = TRUE, size = 2.5, fontface = "bold", 
                                    max.overlaps = Inf, segment.color = NA, box.padding = 0.2) +
                    scale_color_brewer(palette = "Set1", name = "Klasa") +
                    scale_edge_color_manual(values = c("gray70", "red")) +
                    scale_edge_linetype_manual(values = c("solid", "dashed")) +
                    theme_void() + labs(title = "Drzewo minimalnego rozpinania (MST)"))

print(wykres_drzewo)


#wykres profili 
# 1. STANDARYZUJEMY DANE, żeby zmieściły się na jednej osi Y
df_finalne_klasy <- as.data.frame(scale(df_finalne)) 
df_finalne_klasy$Klasa <- as.factor(klasy)
profile_srednich <- df_finalne_klasy %>% group_by(Klasa) %>% 
  summarise(across(everything(), mean, na.rm = TRUE))
profile_dlugie <- profile_srednich %>% 
  pivot_longer(cols = -Klasa, names_to = "Cecha", values_to = "Wartosc")
profile_dlugie$Cecha <- factor(profile_dlugie$Cecha, levels = cechy_finalne)

profile_srednich

print(ggplot(profile_dlugie, aes(x = Cecha, y = Wartosc, group = Klasa, 
                                 color = Klasa)) +
        geom_line(linewidth = 1.2) + geom_point(size = 3) +
        geom_text(aes(label = round(Wartosc, 1)), vjust = -1, size = 3, 
                  show.legend = FALSE) +
        scale_color_brewer(palette = "Set1") + theme_minimal() + 
        labs(title = "Profile średnich wartości cech dla klas") +
        theme(legend.position = "bottom", 
              axis.text.x = element_text(angle = 45, hjust = 1)))


# tablica
licznosc <- as.numeric(table(klasy))
profile_t <- profile_srednich %>%
  pivot_longer(cols = -Klasa, names_to = "Cecha", values_to = "Wartosc") %>%
  pivot_wider(names_from = Klasa, values_from = Wartosc, names_prefix = "Klasa_")
profile_t
profile_t$Srednia_krajowa <- sapply(cechy_finalne, 
                                    function(z) mean(df_numeryczne[[z]], na.rm = TRUE))


red_palette <- colorRampPalette(c("#F4A582", "#FFFFFF"))(100)
green_palette <- colorRampPalette(c("#FFFFFF", "#7FBF7B"))(100)

tabela_gt <- profile_t %>% 
  gt() %>%
  # DODANO: Tytuł i podtytuł
  tab_header(
    title = md("**Profile średnich**"), 
    subtitle = "Klasyfikacja: czerwony (poniżej średniej krajowej), zielony (powyżej)"
  ) %>%
  # Formatowanie liczb
  fmt_number(columns = everything(), decimals = 2) %>%
  # Etykiety kolumn
  cols_label(
    Cecha = "Cecha", 
    Klasa_1 = paste0("Klasa 1\n(n=", licznosc[1], ")"), 
    Klasa_2 = paste0("Klasa 2\n(n=", licznosc[2], ")"), 
    Klasa_3 = paste0("Klasa 3\n(n=", licznosc[3], ")"),
    Klasa_4 = paste0("Klasa 4\n(n=", licznosc[4], ")"), 
    Srednia_krajowa = "Średnia krajowa"
  )

# Pętla kolorująca (Twoja logikę pozostawiam bez zmian)
for (i in 1:nrow(profile_t)) {
  avg <- profile_t$Srednia_krajowa[i]
  row_vals <- as.numeric(profile_t[i, 2:5])
  min_val <- min(row_vals, na.rm = TRUE); max_val <- max(row_vals, na.rm = TRUE)
  for (col_idx in 2:5) {
    val <- as.numeric(profile_t[i, col_idx]); if (is.na(val)) next
    color <- "#FFFFFF"
    if (val > avg) { diff <- max_val - avg; if (diff > 0) color <- green_palette[min(round(((val - avg) / diff) * 99) + 1, 100)] }
    else if (val < avg) { diff <- avg - min_val; if (diff > 0) color <- red_palette[min(round(((val - min_val) / diff) * 99) + 1, 100)] }
    tabela_gt <- tabela_gt %>% tab_style(style = cell_fill(color = color), locations = cells_body(columns = col_idx, rows = i))
  }
}

# Stylizacja końcowa: nagłówki + WYŚRODKOWANIE
tabela_gt <- tabela_gt %>%
  tab_style(style = list(cell_fill(color = "#2C3E50"), cell_text(color = "white", weight = "bold")), 
            locations = cells_column_labels()) %>%
  # DODANO: Wyśrodkowanie danych
  cols_align(align = "center", columns = everything()) %>%
  # DODANO: Lewe wyrównanie tylko dla kolumny "Cecha" (dla lepszej czytelności)
  cols_align(align = "left", columns = Cecha)

print(tabela_gt)

cat("\n=== PRZYNALEŻNOŚĆ OBIEKTÓW DO KLAS ===\n")
print(data.frame(Obiekt = names(klasy), Klasa = as.character(klasy)) %>% arrange(Klasa))

# ==============================================================================
# 1. PRZYGOTOWANIE I ODLEGŁOŚCI (Ze standaryzacją!)
# ==============================================================================
library(cluster)
library(dplyr)
library(ggplot2)
library(tidyr)

# Standaryzacja to obowiązek! Inaczej duże kwoty zniszczą klastry.
dane_std <- scale(df_finalne)

# Liczymy odległości (zwykłą i kwadratową dla metody Warda)
odl <- dist(dane_std, method = "euclidean")
odl_kwadrat <- dist(dane_std, method = "euclidean")^2

# ==============================================================================
# 2. SZEŚĆ METOD GRUPOWANIA (hclust)
# ==============================================================================
dend_single   <- hclust(odl, method = "single")
dend_complete <- hclust(odl, method = "complete")
dend_average  <- hclust(odl, method = "average")
dend_centroid <- hclust(odl, method = "centroid")
dend_median   <- hclust(odl, method = "median")
dend_ward     <- hclust(odl_kwadrat, method = "ward.D") # WARD  KWADRAT

# Dodajemy nazwy miast do dendrogramów
dend_single$labels   <- rownames(df_finalne)
dend_complete$labels <- rownames(df_finalne)
dend_average$labels  <- rownames(df_finalne)
dend_centroid$labels <- rownames(df_finalne)
dend_median$labels   <- rownames(df_finalne)
dend_ward$labels     <- rownames(df_finalne)

# ==============================================================================
# 3. OCENA METOD - WSPÓŁCZYNNIK KOFENETYCZNY 
# ==============================================================================
kof_single   <- cor(cophenetic(dend_single), odl)
kof_complete <- cor(cophenetic(dend_complete), odl)
kof_average  <- cor(cophenetic(dend_average), odl)
kof_centroid <- cor(cophenetic(dend_centroid), odl)
kof_median   <- cor(cophenetic(dend_median), odl)
kof_ward     <- cor(cophenetic(dend_ward), odl_kwadrat)

# Zestawienie wyników
kofenetyczna <- data.frame(
  Metoda = c("Najbliższego sąsiada (single)", "Najdalszego sąsiada (complete)", 
             "Średniej grupowej (average)", "Środka ciężkości (centroid)", 
             "Mediany (median)", "Warda (ward.D)"),
  Korelacja = c(kof_single, kof_complete, kof_average, kof_centroid, kof_median, kof_ward)
)
print("--- WSPÓŁCZYNNIK KOFENETYCZNY (Im wyżej, tym lepiej) ---")
print(kofenetyczna[order(-kofenetyczna$Korelacja), ])

# ==============================================================================
# 4. KRYTERIUM MOJENY I DENDROGRAM (Dla najlepszej metody - zazwyczaj Warda)
# ==============================================================================
# Zakładamy, że pracujemy na metodzie Warda (daje najładniejsze, biznesowe klastry)
dend_final <- dend_ward

wys <- c(0, dend_final$height)
a <- 0.7 # Współczynnik z Twojego kodu
Mojena <- mean(wys) + a * sd(wys)

print(paste("Wartość odcięcia (Mojena):", round(Mojena, 2)))

# Liczymy ile klas wyszło z odcięcia Mojeny
liczba_klas <- sum(dend_final$height > Mojena) + 1
grupy <- cutree(dend_final, k = liczba_klas)

# Rysujemy główny dendrogram
plot(dend_final, hang = -1, main = paste("Dendrogram (Metoda Warda) -", liczba_klas, "klasy"), 
     xlab = "Miasta", ylab = "Odległość", sub = "", cex = 0.6)
abline(h = Mojena, col = "red", lwd = 2, lty = 2)
rect.hclust(dend_final, k = liczba_klas, border = "turquoise")


# ==========================================================
# 1. PRZYGOTOWANIE I ODLEGŁOŚCI
# ==========================================================
dane_std <- scale(df_finalne)

odl_zwykla <- dist(dane_std, method = "euclidean")
odl_kwadrat <- odl_zwykla^2 

# ==========================================================
# 2. TWORZENIE DENDROGRAMÓW (4 metody)
# ==========================================================
a_dend <- hclust(odl_kwadrat, method = "ward.D") # TYLKO Ward używa kwadratu
b_dend <- hclust(odl_zwykla, method = "single")  # Reszta na zwykłych
c_dend <- hclust(odl_zwykla, method = "complete")
d_dend <- hclust(odl_zwykla, method = "average")
e_dend <- hclust(odl_zwykla, method = "centroid")
f_dend <- hclust(odl_zwykla, method = "median")
# Podpinamy poprawne nazwy miast 
a_dend$labels <- rownames(df_finalne)
b_dend$labels <- rownames(df_finalne)
c_dend$labels <- rownames(df_finalne)
d_dend$labels <- rownames(df_finalne)
e_dend$labels <- rownames(df_finalne)
f_dend$labels <- rownames(df_finalne)
# ==========================================================
# 3. RYSOWANIE 4 DENDROGRAMÓW OBOK SIEBIE
# ==========================================================
par(mfrow=c(2,2)) # Dzieli ekran na 4 okienka
plot(a_dend, hang=-1, main="Metoda Warda", sub="", xlab="", cex=0.5)
plot(b_dend, hang=-1, main="Najbliższego Sąsiada (Single)", sub="", xlab="", cex=0.5)
plot(c_dend, hang=-1, main="Najdalszego Sąsiada (Complete)", sub="", xlab="", cex=0.5)
plot(d_dend, hang=-1, main="Średniej Grupowej (Average)", sub="", xlab="", cex=0.5)
plot(dend_centroid, hang=-1, main="Środek ciężkości (Centroid)", sub="", xlab="", cex=0.5)
plot(dend_median, hang=-1, main="Mediana (Median)", sub="", xlab="", cex=0.5)
par(mfrow=c(1,1)) 


# ==========================================================
# 4. WSPÓŁCZYNNIK KOFENETYCZNY (NAPRAWIONY!)
# ==========================================================
c1 <- cophenetic(a_dend)  
kof1 <- cor(c1, odl_kwadrat) # Ward porównujemy do kwadratu

c2 <- cophenetic(b_dend)  
kof2 <- cor(c2, odl_zwykla)  # Resztę do zwykłej odległości

c3 <- cophenetic(c_dend) 
kof3 <- cor(c3, odl_zwykla)  

c4 <- cophenetic(d_dend)  
kof4 <- cor(c4, odl_zwykla)  

c5 <- cophenetic(e_dend)  
kof5 <- cor(c5, odl_zwykla)  

c6 <- cophenetic(f_dend)  
kof6 <- cor(c6, odl_zwykla)  

# Zestawienie w ładnej tabelce
kofenetyczna <- data.frame(
  Metoda = c("Ward", "Single", "Complete", "Average", "Centroid", "Median"),
  Wsp_Kofenetyczny = round(c(kof1, kof2, kof3, kof4, kof5, kof6), 6)
)

print("--- OCENA METOD (Im bliżej 1.00, tym lepsza metoda) ---")
print(kofenetyczna[order(-kofenetyczna$Wsp_Kofenetyczny), ])

# tablica
licznosc <- as.numeric(table(klasy))
profile_t <- profile_srednich %>%
  pivot_longer(cols = -Klasa, names_to = "Cecha", values_to = "Wartosc") %>%
  pivot_wider(names_from = Klasa, values_from = Wartosc, names_prefix = "Klasa_")
profile_t
profile_t$Srednia_krajowa <- sapply(zmienne_fin, 
                                    function(z) mean(df_numeryczne[[z]], na.rm = TRUE))


red_palette <- colorRampPalette(c("#F4A582", "#FFFFFF"))(100)
green_palette <- colorRampPalette(c("#FFFFFF", "#7FBF7B"))(100)

tabela_gt <- profile_t %>% 
  gt() %>%
  # DODANO: TytuE i podtytuE
  tab_header(
    title = md("*Profile Erednich*"), 
    subtitle = "Klasyfikacja: czerwony (poniE<ej Eredniej krajowej), zielony (powyE<ej)"
  ) %>%
  # Formatowanie liczb
  fmt_number(columns = everything(), decimals = 2) %>%
  # Etykiety kolumn
  cols_label(
    Cecha = "Cecha", 
    Klasa_1 = paste0("Klasa 1\n(n=", licznosc[1], ")"), 
    Klasa_2 = paste0("Klasa 2\n(n=", licznosc[2], ")"), 
    Klasa_3 = paste0("Klasa 3\n(n=", licznosc[3], ")"),
    Klasa_4 = paste0("Klasa 4\n(n=", licznosc[4], ")"), 
    Srednia_krajowa = "Średnia krajowa"
  )

# ==============================================================================
# 5. PROFILE ŚREDNICH (Zautomatyzowane - bez ręcznego wpisywania nazw)
# ==============================================================================
# Łączymy oryginalne (surowe) dane z numerem grupy, żeby liczby na wykresie miały sens
dane_profile <- df_finalne
dane_profile$Grupa <- as.factor(grupy)

# Liczymy średnie dla każdej grupy i przygotowujemy do wykresu (pivot_longer)
profile_dlugie <- dane_profile %>%
  group_by(Grupa) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  pivot_longer(cols = -Grupa, names_to = "Zmienne", values_to = "Srednia_wartosc")

# Wykres profili na kafelkach (najbardziej czytelny dla surowych danych)
ggplot(data = profile_dlugie, aes(x = Grupa, y = Srednia_wartosc, fill = Grupa)) +
  geom_col() +
  geom_text(aes(label = round(Srednia_wartosc, 1)), vjust = -0.5, size = 3) +
  facet_wrap(~ Zmienne, scales = "free_y") +
  scale_fill_brewer(palette = "Set1") + 
  theme_minimal() +
  labs(title = "Profile Klastrów - Średnie wartości", x = "Skupienie", y = "Wartość") +
  theme(legend.position = "none", strip.text = element_text(face = "bold", size = 10))