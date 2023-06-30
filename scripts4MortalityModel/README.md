# Modelo mortalidad
Script para obtener los datos necesarios para los modelos de mortalidad

Los datos que necesitamos como input son: Del IFN2 "combi4172" y  "parcelasPorcentajes" y del IFN3 "combi4172".

Vamos a trabajar con las variables de parcelas que serán las que nos indiquen si las masas han aumentado o no, y de variables de árbol solo necesitaremos altura y expan para calcular otras variables necesarias como AlturaDominante.

Primero tenemos que indicar si se ha muerto un árbol o no, y para ello nos fijamos en el orden, es decir, que si es distinto de 0 en el orden2 y 0 en el orden3, indica que ese arbol ya no existe. Si está muerto le asignamos un 1 , y si esta vivo un 0.
Los datos de arboles les necesitamos en realidad del IFN2 , por lo que ahora dividimos los datos entre vivos y muertos, para buscar esos arboles en el data frame de combi4172_2 y asignarle el valor 0 o 1 que corresponda. 
Lo que en realidad nos importa es el numoro de árboles que se han muerto en cada una de las parcelas, y para ello vamos a sumar el número de arboles muertos por parcela, y a multiplicarle por el factor de expansión para tener el valor necesraio. 

Posteriormente calculamos otras variables como son Dg, SDI, alturaDominante y Hart Becking.

Los outputs generados son  'mortalidadT0', que contiene las variables de parcelas necesarias y 'arbolesMortalidad' con las variables de árbol

# Mod Mortalidad E41
Como input tenemos los archivos de outputs del modelo mortalidad.

Lo primero que hacemos en este script es seleccionar solo los datos de la especie 41. Luego calculamos las variables altura dominate y hartB para los arboles de dicha especie.

Vamos a realizar un modelo generalizado de poisson. Incluimos inicialente todas las variables, y vamos eliminando las que no son significativas y las que tiene un factor de inflación de varianza superior a 10. Por tanto, finalmente el modelo con el que nos quedamos está formado por  las siguiente variables: Motalidad~N+porcentejeN+Dg+SDI+AlturaDom+HartB41, con un R^2 de 51.31%

# Mod Mortalidad E72
Como input tenemos los archivos de outputs del modelo mortalidad.

Lo primero que hacemos en este script es seleccionar solo los datos de la especie 72. Luego calculamos las variables altura dominate y hartB para los arboles de dicha especie.

Vamos a realizar un modelo generalizado de poisson. Incluimos inicialente todas las variables, y vamos eliminando las que no son significativas y las que tiene un factor de inflación de varianza superior a 10. Por tanto, finalmente el modelo con el que nos quedamos está formado por  las siguiente variables: Motalidad~G+N+porcentajeG+porcentejeN+Dg+AlturaDom+AlturaDom72+HartB72, con un R^2 de 54.07%

