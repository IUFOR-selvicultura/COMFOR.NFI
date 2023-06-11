#  Calculos IFN

'CalculosIFN.R'

Este script hace la depuración de los datos para obtener las parcelas divididas en monoespecíficas, pluriespecíficas y plurimodales para poder estudiar la evolución del IFN.
El input que se tiene que proporcionar es el archivo de datos trees que está en el rdata del IFN que se quiere estudiar. Obtendremos varios outputs que serán necsearios para los demás scripts.
Primero se completan los datos obtenidos del IFN con otras variables que van a ser de interés en el estudio, como puede ser el área basimetrica, el factor de expansión o el dbh entre muchas otras; además de añadir variables para las parcelas.
Las primeras parcelas que se obtienen son las mixtas, es decir, que tengan al menos 2 especies, y que representen al menos un 10% del total. A continuación se obtienen las parcelas monoespecíficas, que son aquellas que no son mixtas. Posteriormente, a patir de las parcelas monoespecíficas, se obtienen las plurimodales, pero anteriormente hay que comprobar si se distribuyen normalmente o no con el test de normalidad, pues solo nos interesan las que no lo hagan. Una vez hecho esto, se procede al test de modalidad, del cuál nos interesaran aquellas parcelas que rechacen la hipótesis, es decir en las que haya más de una moda.

A mayores, en este script aparecen gráficos para el análisis previo de los datos, entre los que podemos encontrar: la distribución de área basimétrica por especies; el número de árboles por calidad, forma u origen.

Además, [en esta carpeta](../scripts4Report/readme.md), hay otros scripts que permiten la elaboración de mapas y gráficas.