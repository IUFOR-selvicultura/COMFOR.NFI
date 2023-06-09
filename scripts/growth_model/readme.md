# Mod crecimiento 41
Como input se necesita el fichero de crecimientoTO del script "Depuracion de datos modelos", que es su output, en el que se recogen todas las especies.

Se ha decidido hacer dos modelos distintos para que así sea lo más realista posible ya que las especies son bastante diferentes entre si.
Lo primero es quedarnos con los árboles de la especie 41 y añadir a este nuevo data frame la variable BAL teniendo únicamente en cuenta estos árboles.

Posteriormente, realizamos un análisis en componentes principales que nos permita decidir que variables debemos de incluir en nuestro modelo. Tenemos variables de 4 grupos diferentes (tamaño, competencia, especificidad y densidad), y debemos de mantener al menos una de cada uno de ellos para que así el modelo sea lo más preciso posible. Nos fijamos en los autovalores y nos quedamos con el número de componentes cuyo autovalor sea superior a 1, en este caso 4. Posteriormnete las representamos 2 a 2 y además utilizamos la matriz de correlaciones para decidir las variables. En este caso se deciden vRes , ProxVol , perimetro.x , dbh.x , esbeltez.x , HartB , Dg , G.x , SDI , BALTOTAL , BAL y porcentajeG pero en realidad no nos interesan ni ProxVol ni perimetro.x, ya que son combinacion de dbh, y no va a ser favorable añadirlas en el modelo.
La varible forma también es relevante pues los árboles son muy diferentes entre ellos según la forma que tengan. Nos qudamos con las formas 2 y 5 que son las mayoritarias y añadimos una variable dummy para que se difereciem.

Una vez tenemos las variables posibles del modelo probamos con modelos lineales y mixtos hasta quedarnos con el modelo que más conviene, que es un modelo lineal con solo las variables significativas que resultan ser: 
sqrt(vRes)~dbh.x+esbeltez.x+Dg+(G.x)+SDI+BAL+BALTotal+porcentajeG+AlturaDominante+HartB+dummyF2 haciendose una transformación en la variable respuest para que así los residuales en el test de Kolmogorov asuman normalidad y con un R^2 del 96.72%, es decir, parece un modelo muy completo.

# Mod crecimiento 72
Es muy similar al de la especie 41, pero para la 72

En este caso, en el acp son necesarias 5 componentes, y las variables explicativas que se seleccionan son vRes , ProxVol , Perimetro , dbh.x , esbeltez , HartB , G.x , SDI , BALTOTAL , BAL , porcentajeG , Altura.x y se añade también la variable de forma dummy.

Se prueban modelos lineales y mixtos con alguna transformación, y ninguno de ellos se distribuye normalmente por lo que nos hemos quedado con el más sencillo pero que tiene todas las variables que necesitamos:
vRes~dbh.x+esbeltez.x+HartB+G.x+BAL+BALTotal+Altura.x+dummyF2, con un R^2 del 94.05%
