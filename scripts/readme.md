# Calculos IFN
Este script hace la depuración de los datos para obtener las parcelas divididas en monoespecíficas, pluriespecíficas y plurimodales para poder estudiar la evolución del IFN.
El input que se tiene que proporcionar es el archivo de datos trees que está en el rdata del IFN que se quiere estudiar. Obtendremos varios outputs que serán necsearios para los demás scripts. 
Primero se completan los datos obtenidos del IFN con otras variables que van a ser de interés en el estudio, como puede ser el área basimetrica, el factor de expansión o el dbh entre muchas otras; además de añadir variables para las parcelas.
Las primeras parcelas que se obtienen son las mixtas, es decir, que tengan al menos 2 especies, y que representen al menos un 10% del total. A continuación se obtienen las parcelas monoespecíficas, que son aquellas que no son mixtas. Posteriormente, a patir de las parcelas monoespecíficas, se obtienen las plurimodales, pero anteriormente hay que comprobar si se distribuyen normalmente o no con el test de normalidad, pues solo nos interesan las que no lo hagan. Una vez hecho esto, se procede al test de modalidad, del cuál nos interesaran aquellas parcelas que rechacen la hipótesis, es decir en las que haya más de una moda.

A mayores, en este script aparecen gráficos para el análisis previo de los datos, entre los que podemos encontrar: la distribución de área basimétrica por especies; el número de árboles por calidad, forma u origen.

# Mapa por una CCAA
En este script se pretende representar en el mapa de españa las parcelas corresponidentes a cada uno de los grupos en los que las hemos clasificado anteriormente.
Como input necesitamos de outputs del script Calculos IFN 'of_resultHeightPlurimodal.csv' , 'of_resultDiamPlurimodal.csv', 'of_plotsPluriSP.csv' y 'of_plotsMonoSP.csv' ; y las coordenadas. Además se deberá cambiar la comunidad autónoma que se desee representar. 

Se buscan las coordenadas que corresponden con las parcelas y serán esas las que se representen. Las parcelas plurimodales serán aquellas que rechazen el test de modalidad tanto para diametro como para altura, para tener una mayor seguridad de que la parcela sea plurimodal.

Se obtienen mapas interactivos y mapas no interacticos

# Gráficos evolución
En este script se pretende mostrar la evolución en el número de parcelas de cada uno de los tipos.
Como input necesitamos de outputs del script Calculos IFN 'of_resultHeightPlurimodal.csv' , 'of_resultDiamPlurimodal.csv', 'of_plotsPluriSP.csv' y 'of_plotsMonoSP.csv'

Se van a presentar gráficos tanto para el conjunto de todo España, como por comunidades autónomas. Para el de España se presentaran en barras apiladas, y se muestran 3 barras, una para Inventario Forestal Naciona, y dentro de esas barras, con 3 diferentes colores, que sirven para identificar que inventario es. En el eje y aparece el número de parcelas.

Para el de las comunidades autónomas se utiliza un diagrama de barras múltiples. En este caso se hará un gráfico para cada tipo de parcela, para permitir una mayor comprensión. En el eje x se representan las comunidades autónomas, y el eje y, el número de parcelas que hay. Para cada una de las comunidades hay 3 barras, que cada una hace referencia a uno de los tres inventarios.

# Combinaciones IFN
En este script se obtienen las combinaciones de las parcelas mixtas para cada uno de los inventarios con el fin de poder hacer una representación posterior de su evolución o con el objetivo de obtener el número de rpeticiones de una combinación en concreta y obtener las parcelas en las que se da dicha combinación.

Como input necesitamos la ruta donde se guarden los outputs del script IFNCalculos para leer los datos de los archivos 'of_plotsPluriSP_sps.csv' y 'arboles.csv', y como outputs se obtendran tablas de las combinaciones del IFN y los datos correspondientes a las combinaciones deseadas.

Se obtienen tablas que asocian cada combinación con el número de repeticiones que hay

# Gráficas combinaciones
En este script se representa la evolución de las combinaciones de especies en los diferentes invenatrios.

Como input necesitamos las tablas de las combinaciones que se han guradado en Combinaciones IFN.

Nos quedamos con las combinaciones que aparecen más de 100 veces, ya que si no pueden ser combinaciones poco frecuentes que se hayan dado aleatoriamente para un caso concreto, y enb realidad nos interesan las que de verdad son representativas.
Se obtienen dos tipos de gráficos. Con la función graficoIndividual se obtiene un gráfico de barras para cada uno de los IFN (que se indica cual en la funcion) y así se puede observar visualmente cuantas repeticiones tiene cada combinación.
Por otra parte, se representa un gráfico de barras múltiples para los 3 inventarios a la vez, en el qyue el eje x son las combinaciones, el eje y, el numero de repeticiones. Para cada una de las combinaciones aparecen 3 barras (como máximo), que cada una hace referencia a un inventario. De esta forma se podrá ver más claramente como auemntan o disminuyen las parcelas con dichas combinaciones, además de ver la evolución más clara.

