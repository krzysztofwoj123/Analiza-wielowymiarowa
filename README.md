# Analiza-wielowymiarowa
Analiza wielowymiarowa i segmentacja rynku miast pod kątem optymalnej lokalizacji szkoły prywatnej.
# Multivariate Analysis: Commercial Location Selection (R / GUS)

Wielowymiarowa analiza porównawcza oraz segmentacja rynku 66 polskich miast na prawach powiatu pod kątem wyboru optymalnej lokalizacji dla prywatnej placówki edukacyjnej.

## Zawartość repozytorium
- `Raport lokalizacja szkoly.pdf` – pełny raport analityczny (39 stron) zawierający metodologię statystyczną, wykresy, rankingi oraz wnioski biznesowe.
- `Analiza_cala.R` – skrypt w języku R realizujący procedury analizy wielowymiarowej.
- `Dane_cale.xlsx` – standaryzowany zbiór 25 cech diagnostycznych pobranych z Banku Danych Lokalnych GUS (2024).

## Metodologia badawcza
1. **Analiza wstępna i selekcja cech:** statystyka opisowa, redukcja zmiennych metodą korelacji Hellwiga oraz weryfikacja współczynnika zmienności (CV).
2. **Analiza powiązań i redukcja wymiarowości:**
   - Analiza korelacji kanonicznej (CCA) – wyodrębnienie syndromu rozwojowego.
   - Analiza głównych składowych (PCA) – dekompozycja wariancji.
3. **Porządkowanie liniowe (Ranking):**
   - Syntetyczna miara rozwoju Hellwiga.
   - Wielokryterialna metoda TOPSIS.
4. **Analiza skupień i segmentacja rynku:**
   - Algorytm k-średnich (k-means) wsparty oceną sylwetkową (Silhouette) i metodą łokcia.
   - Hierarchiczna aglomeracja metodą Warda.
   - Grafowa metoda drzewa minimalnego rozpinania (MST) – identyfikacja specyficznych nisz rynkowych.

## Kluczowe wnioski
- Wyodrębnienie czołówki metropolitalnej o najwyższym potencjale zamożności i popytu (Kraków, Warszawa, Sopot, Gdańsk, Wrocław).
- Zidentyfikowanie alternatywnego segmentu wschodnich ośrodków akademickich (Rzeszów, Lublin, Kielce, Białystok) o bardzo wysokich wskaźnikach edukacyjnych przy niższej konkurencji rynkowej.

---
*Projekt zrealizowany na Uniwersytecie Gdańskim.*  
*Autorzy: Krzysztof Wojtala, Krzysztof Hajderek*
