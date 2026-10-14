# Analiza-wielowymiarowa
*Autorzy: Krzysztof Wojtala, Krzysztof Hajderek*

Wielowymiarowa analiza porównawcza 66 polskich miast na prawach powiatu pod kątem doboru optymalnej lokalizacji dla prywatnej placówki edukacyjnej (dane GUS, 2024).

### 1. Analiza wstępna
- **Zakres:** 66 miast, 25 zmiennych diagnostycznych w 5 obszarach.
- **Metody:** statystyki opisowe, współczynnik Giniego, macierz korelacji Pearsona.
- **Kluczowa relacja:** wynagrodzenia ($x_7$) vs wyniki egzaminu ósmoklasisty ($x_{16}$) – identyfikacja anomalii rynkowych (np. Jastrzębie-Zdrój, Płock).

### 2. Analiza korelacji kanonicznej (CCA)
- **Zbiory:** Popyt i zamożność (X) vs Środowisko publiczne (Y).
- **Wyniki:** pierwsza korelacja kanoniczna $\text{CanR} = 0,904$ ($\text{CanR}^2 = 81,8\%$, $p < 0,001$).
- **Wniosek:** otoczenie publiczne wyjaśnia ponad 43% wariancji potencjału rynkowego i zamożności.

### 3. Analiza głównych składowych (PCA)
- **Zakres:** redukcja wymiarowości na 19 zmiennych.
- **Wyjaśniona wariancja:** 3 składowe = 70,0% wariancji (PC1: 38,1% – rozwój gospodarczy; PC2: 18,8% – oświata i transport; PC3: 13,1% – demografia).

### 4. Dobór cech (Procedura Hellwiga)
- **Eliminacja:** korelacja $|r| > 0,8$ oraz próg zmienności $V < 20\%$.
- **Korekta merytoryczna:** finalny zestaw 12 kluczowych zmiennych do taksonomii.

### 5. Porządkowanie liniowe
- **Metody:** Hellwig oraz TOPSIS (zgodność rang Spearmana: $r_s = 0,972$).
- **Liderzy (Klasa I):** Kraków (lider), Warszawa, Sopot, Gdańsk, Wrocław, Poznań, Katowice, Rzeszów, Gdynia, Opole.

### 6. Analiza skupień (Segmentacja)
- **$k$-średnich:** optymalny podział na $k = 2$ ($S = 0,27$) oraz $k = 3$ (metoda łokcia).
- **Metody hierarchiczne:** metoda Warda (najwyższa spójność grup, wsp. kofenetyczny = 0,648).
- **Drzewo MST:** wyodrębnienie niszy 6 ośrodków (m.in. Rzeszów, Lublin, Kielce) o najwyższym klimacie edukacyjnym przy umiarkowanych kosztach.

---
*Projekt akademicki – Uniwersytet Gdański*  

