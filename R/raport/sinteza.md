# 📝 Sinteza Progresului – Modelare Econometrică Clasică (Student 2)

Până în acest moment, ai parcurs întregul ciclu de **estimare, curățare, diagnosticare și validare** a unui model de regresie liniară multiplă (OLS).

---

## 1. Procesarea Datelor și Tratarea Valorilor Extreme (Outliers)

### Importul datelor

* S-a utilizat fișierul Excel brut, cu parametrul `skip=1` pentru a gestiona antetul și variabilele.

### Identificarea outlierilor

Inițial, modelul prezenta reziduuri foarte mari (Max: **423.63**). Analiza exploratorie a evidențiat două observații extreme:

* **Hong Kong** – Depozite ≈ **548%** din PIB
* **Singapore** – Depozite ≈ **244%** din PIB

Acestea, fiind centre financiare atipice, distorsionau relațiile economice pentru restul țărilor.

### Decizia metodologică

* Eliminarea observațiilor cu `deposits_gdp > 200%`.
* Eșantion final: **n = 132** țări.
* Rezultat: reducerea zgomotului și redescoperirea relațiilor economice reale.

---

## 2. Estimarea Modelului OLS (Pe Date Curățate)

### Specificația modelului

```
Deposits = β0 + β1 * Branches + β2 * ATMs + β3 * Legal + β4 * Regulation + β5 * GDP + ε
```

### Rezultate principale

* **R² ajustat**: creștere de la ~0.15 (cu outliers) la **~0.21** (fără outliers).
* Variabile semnificative:

  * **branches_100k**: p < 0.01, coeficient ≈ +0.31
    → mai multe sucursale → mai multe depozite.
  * **regulation**: p < 0.05, coeficient pozitiv
    → reglementări mai eficiente stimulează economisirea.
* **gdp_pc_ppp** devine nesemnificativ → infrastructura și reglementarea contează mai mult decât nivelul de venit.

---

## 3. Diagnosticarea Modelului

Modelul final (**model_ols_clean**) respectă ipotezele Gauss–Markov:

* **Multicoliniaritate**: VIF < 5 pentru toate variabilele (max ~4).
* **Homoscedasticitate**: Test Breusch–Pagan → p ≈ 0.29
  → nu este necesară corectarea erorilor standard.

---

## 4. Validarea Capacității Predictive (Out-of-sample)

* Set de test: 20% din date, cu `seed(123)`
* **RMSE Train:** ~29.78
* **RMSE Test:** ~25.82

**Concluzie:** Model robust, fără overfitting.

---

# 5. Extinderea Modelului (Cerința 4.a)

Au fost testate relații neliniare și termeni de interacțiune:

### Modificări:

* Transformare logaritmică: `log(gdp_pc_ppp)`
* Interacțiune: `atms_100k * high_freedom`

### Rezultate:

* **R² ajustat** scade de la 0.2106 → **0.2010**
* **AIC** crește de la 1047 → **1050**
* Termenii adiționali: **nesemnificativi (p > 0.05)**

**Decizie:** Modelul extins este respins → modelul simplu este superior și mai robust.

---

# 6. Scenariu de Prognoză pentru România (Cerința 4.b)

### Ipoteza scenariului

> Ceteris paribus, numărul de sucursale la 100.000 de adulți se **dublează** (≈63 → ≈127).

### Rezultate:

* Nivel actual al depozitelor: **34.53% din PIB**
* Nivel prognozat: **≈85.53%**
* Creștere estimată: **+51 puncte procentuale**

### Interpretare:

1. **Efect direct (marginal)** → ~20 pp
   (influența coeficientului branches)
2. **Efect de ”catch-up”** → ~30 pp
   (România pornește semnificativ sub tendința medie a țarilor comparabile.)

---

# 7. Concluzie Finală (Student 2)

Ai finalizat modelarea econometrică clasică.
Modelul optim este cel **liniar OLS**, estimat pe date curate (fără Hong Kong și Singapore).

Acesta confirmă:

* rolul esențial al **infrastructurii fizice** (branches),
* importanța **reglementării eficiente**,
* robustețea relației economice identificate.

Modelul servește ca **bază solidă** pentru comparația ulterioară cu modelele de Machine Learning dezvoltate de Studentul 3.
