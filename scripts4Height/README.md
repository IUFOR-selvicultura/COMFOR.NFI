# Modelo altura
Se depuran los datos para poder realizar el modelo de altura-diametro. 
En este caso nuestra variable respuesta será la altura, y como variable explicativa tiene que estar el diámetro obligatoriamente.

Solo nos interesan los datos del IFN2, asi que nuestros inputs serán 'combi4172' que recoge las parcelas mixtas con árboles de las especies 41 y 72 y 'parcelasPorcentaje' que recoge las variables de parcelas. Nos quedamos únicamente con los datos de parcelasPorcentaje que corresponden a las parcelas mixtas buscadas

Añadimos variables de árboles y parcela como ProxVol, Dg, SDI, BAL, AlturaDominante, HartB y Hart, y además reescalamos la variable g para que tenga un rango de valores similar.

El ioutput generado es 'alturaDiam', que contiene el data frame resultante de unir los dos inputs con todas las variables necesarias

# Mod Altura E41
Se seleccionan los datos de las especies 41 para obtener el modelo. 
Como input se tiene el archivo 'alturaDiam'.

Añadimos la variable BAL teniendo en cuanta solo la especie 41.

Para seleccionar las variables realizamos un análisis en componentes principales, teniendo en cuenta que el dbh debe de aparecer siempre. Las variables seleccionadas son ProxVol ,perimetro, dbh, esbeltez, HartB, Dg, Altura, SDI, G, AlturaDominante, BALTOTAL, BAL, porcentajeN y porcentajeG, pero no seleccionamos ni Proxvol ni perimetro ya que son combinaciones de otras.
Además añadimos la variable Dummy de forma, eliminanod las formas 3 y 6 que tienen poca representación, y agrupando el 4 y 5 por ser muy parecidas, y por el otro lado tendremos la forma 2.

Para elegir que modelo es el adecuado, añadimos las variables seleccionadas anteriormene, y nso fijamos si son significativas o no, y en su factor de infación de la varainza (que no supere el 10). En primer lugar realizamos un modelo lineal, y se observa como los resiudales no asumen normalidad, por lo que se ha decidido probar con modelos lineale generalizados y mixtos, pero en ellos tampoco se asume normalidad, y por ello nos quedamos con el modelo lineal, que es más sencillo. El modelo elegido es: 
Altura~dbh+esbeltez+HartB+Dg+SDI+AlturaDominante+BALTotal+porcentejeN+porcentajeG+dummyF2
con un R^2 del 75.86%

