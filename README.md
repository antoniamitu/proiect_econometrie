## APLICAȚIA 1: Modele de regresie pe date de tip transversal și introducere în analiza econometrică asistată de machine learning

Folosind date reale referitoare la variabile între care există o legătură logică din punctul de vedere al teoriei economice (modelare la nivel macroeconomic sau microeconomic), se cere construirea și analizarea unor modele econometrice și ML.

Exemple de contexte posibile:

- Modelare la nivel macroeconomic:
  - testarea empirică a curbei lui Phillips
  - testarea empirică a legii lui Okun
  - testarea empirică a curbei lui Laffer
  - determinanți ai sectorului public
  - impactul componentelor de mediu, sociale și de guvernanță (ESG) asupra creșterii economice
- Modelare la nivel microeconomic:
  - folosirea datelor microeconomice din Eurobarometre pentru a dezvolta modele de impact la nivel micro (a se vedea site-ul GESIS)
- Modelare bazată pe indicatori de sustenabilitate și guvernanță:
  - utilizarea obiectivelor de dezvoltare sustenabilă (SDGs)
  - utilizarea indicatorilor ESG la nivel suveran
  - utilizarea indicatorilor sustenabili de governance (SGI)

### 1. Fundamentarea teoretică și contextualizarea aplicației

1. Să se formalizeze contextul analizei pe baza teoriei economice, cu reliefarea stadiului actual al cunoașterii pentru problematica aleasă, prin raportare la cele mai recente 5–10 articole științifice (din ultimii 5 ani).
2. Se va evidenția modul în care relațiile teoretice pot fi traduse în modele empirice, precum și potențialele extinderi prin tehnici de analiză predictivă.

### 2. Analiza exploratorie a datelor

1. Se vor descrie variabilele identificate:
   - definiții
   - unități de măsură
   - surse
   - periodicitate
   - metode de colectare
2. Se vor efectua analize statistice descriptive și analize grafice privind distribuțiile și corelațiile dintre variabile.
3. Se vor documenta toate transformările aplicate datelor:
   - deflaționare
   - logaritmare
   - tratarea valorilor lipsă etc.
4. Setul de date va fi împărțit în:
   - set de antrenare (train)
   - set de testare (test)  
   pentru a permite evaluarea comparativă a performanței modelelor econometrice în afara eșantionului.
5. Opțional, în completarea metodelor tradiționale, se pot aplica și tehnici exploratorii de tip machine learning, precum:
   - identificarea pattern-urilor și grupărilor prin clustering (K-Means, hierarchical clustering)
   - reducerea dimensionalității prin PCA sau t-SNE
   - estimarea relațiilor non-liniare preliminare prin metode de regresie simplă cu kernel sau spline

### 3. Modelare econometrică clasică

1. Se vor identifica principalii determinanți ai fenomenului analizat prin construirea de modele de regresie multiplă, cu interpretarea economică și econometrică a rezultatelor.
2. Se va testa validitatea modelului:
   - semnificația parametrilor
   - indicatorii de bonitate
   - verificarea ipotezelor clasice
   - aplicarea corecțiilor necesare  
   și se va evidenția modelul optim pe baza criteriilor econometrice.
3. Se va evalua capacitatea predictivă în afara eșantionului pe setul de testare, folosind indicatori precum:
   - RMSE
   - MAE
   - MAPE
   - R² ajustat

### 4. Extinderea modelului și scenarii de prognoză

1. Se va îmbunătăți modelul prin:
   - adoptarea unei alte forme funcționale (log-log, polinomială)
   - adăugarea de variabile dummy
   - includerea termenilor de interacțiune
2. Se vor reliefa beneficiile acestor transformări, iar pe modelul optim (validat anterior) se va construi un scenariu de prognoză, cu evidențierea ipotezelor de lucru.

### 5. Regularizare și integrarea tehnicilor ML

1. Se vor aplica tehnici de regularizare (Lasso, Ridge, Elastic Net) pe modelul multifactorial considerat.
2. Se vor realiza comparații între performanțele modelelor ML și cele ale modelului econometric optim, prin indicatori precum:
   - RMSE
   - MAE
   - R² ajustat
3. Se vor discuta diferențele dintre:
   - modelele explicative (orientate pe interpretarea parametrilor)
   - modelele predictive (orientate pe acuratețea predicției)
4. Opțional, se pot explora și alte metode de regresie avansată:
   - Random Forest
   - Gradient Boosting
   - SVR  
   pentru evidențierea diferențelor între abordările explicative și predictive.

### 6. Discuții și validarea rezultatelor

1. Se vor interpreta rezultatele în raport cu ipotezele inițiale și literatura de specialitate.
2. Se vor evidenția convergențele și discrepanțele între:
   - rezultatele econometrice
   - rezultatele generate de metodele ML
3. Se vor discuta limitările studiului (date, metode, bias) și se vor formula direcții pentru cercetări viitoare, inclusiv potențiale extinderi cu modele non-liniare sau de învățare automată.

---

## APLICAȚIA 2: Modele cu date de tip panel

Să se dezvolte un model de regresie pe date de tip panel pentru analiza determinanților fenomenului ales, ce va respecta următoarele specificații tehnice:

1. Se va configura setul de date pentru un eșantion de unități și pentru un interval de timp, creându-se o structură cu date de tip panel adecvată fenomenului analizat.
2. Să se definească modelul și să se specifice variabilele, cu precizarea transformărilor aduse.
3. Să se testeze alegerea tipului de model RE sau FE cu ajutorul testelor specifice.
4. Să se estimeze modelul adecvat obținut din analiza etapei 3.  
   - Opțional: se vor testa ipotezele pe reziduuri și se vor crea scenarii de prognoză.
5. Să se interpreteze rezultatele obținute din punct de vedere econometric și economic, cu testarea ipotezelor modelului etc.
6. Să se creioneze o secțiune de discuții pe baza rezultatelor obținute, cu validarea acestora în raport cu literatura de specialitate:
   - compararea cu ipotezele inițiale
   - semnificația rezultatelor
   - limitări ale studiului
   - sugestii pentru cercetări viitoare

> **Notă:** Toate aplicațiile se realizează în **R**.
