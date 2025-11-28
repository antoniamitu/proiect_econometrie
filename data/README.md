
➡️ **Y – Outstanding deposits with commercial banks (% of GDP)** (Sursa: World Bank, cod: *deposits_gdp*) – **variabilă dependentă.** Măsoară volumul depozitelor bancare raportat la dimensiunea economiei (PIB). Este un indicator al profunzimii financiare și al încrederii populației și firmelor în sistemul bancar. Un nivel mai ridicat sugerează o utilizare intensă a serviciilor bancare și o mobilizare eficientă a economisirii în economie.

➡️ **X1 – Commercial bank branches per 100,000 adults** (Sursa: World Bank, cod: *branches_100k*) – **variabilă explicativă.** Indicator de infrastructură bancară „tradițională”, care reflectă accesul fizic la servicii bancare. Mai multe sucursale facilitează deschiderea de conturi, consultanță directă și interacțiuni care cresc probabilitatea ca populația să își plaseze economiile în depozite bancare.

➡️ **X2 – ATMs per 100,000 adults** (Sursa: World Bank, cod: *atms_100k*) – **variabilă explicativă.** Reprezintă infrastructura bancară „digitală/modernă”. Un număr ridicat de bancomate indică disponibilitate mare a serviciilor automatizate și confort în utilizarea conturilor bancare. Testează ipoteza că accesul tehnologic poate crește utilizarea depozitelor chiar și în absența interacțiunii cu personalul din sucursale.

➡️ **X3 – Legal System & Property Rights** (Sursa: EFW, Area 2, cod: *legal_rights*) – **variabilă explicativă.** Indicator instituțional care reflectă calitatea sistemului juridic, protecția drepturilor de proprietate și aplicarea contractelor. Un scor mai mare semnalează un mediu în care deponenții se simt mai protejați, ceea ce crește probabilitatea de a-și păstra economiile în bănci și, implicit, nivelul depozitelor ca % din PIB.

➡️ **X4 – Regulation** (Sursa: EFW, Area 5, cod: *regulation*) – **variabilă explicativă.** Măsoară gradul de libertate în mediul de afaceri (birocrație, costuri administrative, rigiditatea reglementărilor). Un scor ridicat indică o reglementare mai flexibilă și procese mai eficiente, care pot facilita activitatea bancară și pot stimula mobilizarea depozitelor.

➡️ **X5 – GDP per capita, PPP (constant 2021)** (Sursa: World Bank, cod: *gdp_pc_ppp*) – **variabilă de control.** Indicator al nivelului real de trai. Țările mai bogate tind să aibă populații cu venituri mai mari, cu o capacitate mai mare de economisire. Introducerea acestei variabile asigură că efectul observat al infrastructurii sau instituțiilor asupra depozitelor bancare nu este doar o consecință a nivelului de dezvoltare economică (evită Omitted Variable Bias).

➡️ **D – High_Freedom** (dummy derivată din scorul general EFW, cod: *high_freedom*) – **variabilă dummy.** High_Freedom = 1 dacă scorul total de libertate economică al țării > mediana eșantionului, 0 altfel. Captează condițiile macroeconomice generale și clima economică în care operează sistemul bancar. Permite testarea ipotezei că infrastructura financiară (sucursale/ATM-uri) are efecte diferite într-un mediu economic liber comparativ cu unul restrictiv (ex.: *ATMs × High_Freedom*).

Dacă vrei, pot transforma această secțiune și într-un fișier **README.md** complet pentru GitHub.
