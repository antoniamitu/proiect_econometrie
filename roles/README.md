### 💠Studentul 1 – „Teoreticianul și Analistul Explorator”
**Focus:** fundamentare teoretică, descrierea datelor și concluzii finale.

-➡️ Răspunde de punctele **1.a** și **1.b**: documentează fenomenul analizat, caută și sintetizează cele 5–10 articole științifice relevante și explică mecanismele economice care justifică modelul ales (relația dintre variabila dependentă și explicative).
-➡️ Împreună cu Studentul 3, descrie structura setului de date: prezintă variabilele, sursele, unitățile de măsură și periodicitatea, precum și logica transformărilor aplicate (logaritmări, deflaționare, tratarea valorilor lipsă) pentru punctele **2.a** și **2.c**, dar din perspectivă explicativă în raport.
➡️ Realizează și interpretează analiza descriptivă și exploratorie pentru punctul **2.b**: tabele de descriptivi, distribuții, histograme, scatterplot-uri, matrice de corelații și comentariile economice aferente.
➡️ Coordonează redactarea secțiunii de **Discuții și Concluzii** (punctele **6.a**, **6.b**, **6.c**), integrând rezultatele obținute de Studentul 2 (modele clasice) și Studentul 3 (ML/regularizare); formulează concluziile finale, limitele studiului și posibile direcții de extindere.

---

### 💠Studentul 2 – „Econometricianul Clasic”
**Focus:** modelul de regresie clasic, teste econometrice și prognoză.

➡️ Primește seturile de date **train** și **test** pregătite de Studentul 3 și construiește modelul de regresie multiplă OLS pentru punctele **3.a** și **3.b**: alege specificația de bază, estimează coeficienții, interpretează rezultatele și verifică ipotezele clasice (semnificație statistică, heteroscedasticitate, autocorelare, normalitatea reziduurilor etc.).
➡️ Se ocupă de validarea *out-of-sample* pentru punctul **3.c**: calculează și compară indicatorii de performanță (de exemplu **RMSE, MAE, MAPE, R² ajustat**) pe setul de test, folosind aceleași date ca Studentul 3 pentru modelele de ML.
➡️ Dezvoltă extensiile modelului pentru punctul **4.a**: testează forme funcționale alternative (log-log, modele cu termeni polinomiali, dummy-uri, interacțiuni), selectând varianta econometrică optimă.
➡️ Construiește scenariile de prognoză pentru punctul **4.b**, pe baza modelului considerat optim: definește ipotezele, generează prognozele și interpretează rezultatele în termeni economici.
➡️ Contribuie la secțiunea de discuții (punctul **6**), oferind interpretări despre robustețea și limitele modelului clasic în raport cu modelele de ML.

---

### 💠Studentul 3 – „Data Engineer și ML Specialist”
**Focus:** pregătirea tehnică a datelor, împărțirea train/test și modelele de regularizare & ML.

➡️ Se ocupă de partea tehnică a curățării și transformării datelor pentru punctul **2.c**: importă setul de date brut, tratează valorile lipsă, aplică eventuale transformări (logaritmări, deflaționare, standardizare), documentează pașii făcuți și salvează versiunile finale în `data/processed`.
➡️ Realizează împărțirea în seturi de antrenare și testare pentru punctul **2.d**, asigurându-se că aceeași împărțire este folosită și de Studentul 2, astfel încât performanțele modelelor clasice și ML să fie comparabile.
➡️ Dacă timpul permite, abordează punctul **2.e** (explorare avansată): aplică tehnici precum **PCA** sau **clustering** pentru a identifica eventuale grupări sau structuri latente în date și furnizează graficele și interpretările de bază.
➡️ Este responsabil de modelele de regularizare și Machine Learning pentru punctele **5.a**, **5.b**, **5.c**: implementează **Lasso, Ridge, Elastic Net** (și eventual alte metode, dacă se dorește: *Random Forest, Gradient Boosting* etc.), antrenează modelele pe setul de train, le evaluează pe setul de test și compară performanțele cu modelul clasic al Studentului 2 (prin aceeași metrici RMSE/MAE etc.).
➡️ Contribuie la discuția privind diferența între modele explicative și modele predictive, oferind input pentru secțiunea **5.c** și pentru concluziile de la punctul **6**, în special în ceea ce privește compromisurile între interpretabilitate și acuratețea predicției.
