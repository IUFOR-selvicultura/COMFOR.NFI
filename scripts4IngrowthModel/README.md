# Datos incorporacion
Script para obtener los datos necesarios para los modelos de mortalidad

Los datos que necesitamos como input son: Del IFN2 "combi4172" y "parcelasPorcentajes" y del IFN3 "arboles_3S".

Vamos a trabajar con las variables de parcelas que serán las que nos indiquen si las masas han aumentado o no, y de variables de árbol solo necesitaremos altura y expan para calcular otras variables necesarias como AlturaDominante.

Primero tenemos que indicar si se ha incorporado un árbol o no, y para ello nos fijamos en el orden, es decir, que si es distinto de 0 en el orden3 y 0 en el orden2, indica que ese arbol es nuevo. Si se ha incorporado le asignamos un 1 , y si ya estaba un 0. Lo que en realidad nos importa es el numero de árboles que se han incorporado en cada una de las parcelas, y para ello vamos a sumar el número de arboles incoprorados por parcela, y a multiplicarle por el factor de expansión para tener el valor necesraio.

Posteriormente calculamos otras variables como son Dg, SDI, alturaDominante y Hart Becking.

Los outputs generados son 'mortalidadT0', que contiene las variables de parcelas necesarias y 'arbolesMortalidad' con las variables de árbol

# Mod Incorporacion E41
Como input tenemos los archivos de outputs del modelo mortalidad.

Lo primero que hacemos en este script es seleccionar solo los datos de la especie 41. Luego calculamos las variables altura dominate y hartB para los arboles de dicha especie. Eliminamos aquellas parcelas que estaban en el IFN3 pero no en el IFN2, ya que solo nos interesa la evolucion

Vamos a realizar un modelo generalizado de poisson. Incluimos inicialente todas las variables, y vamos eliminando las que no son significativas y las que tiene un factor de inflación de varianza superior a 10. Por tanto, finalmente el modelo con el que nos quedamos está formado por las siguiente variables: Incorporacion~G+N+porcentajeG+porcentejeN+Dg+AlturaDom+AlturaDom41+HartB41, con un R^2 de 19.34%

# Mod Incorporación E72
Como input tenemos los archivos de outputs del modelo mortalidad.

Lo primero que hacemos en este script es seleccionar solo los datos de la especie 41. Luego calculamos las variables altura dominate y hartB para los arboles de dicha especie. Eliminamos aquellas parcelas que estaban en el IFN3 pero no en el IFN2, ya que solo nos interesa la evolucion

Vamos a realizar un modelo generalizado de poisson. Incluimos inicialente todas las variables, y vamos eliminando las que no son significativas y las que tiene un factor de inflación de varianza superior a 10. Por tanto, finalmente el modelo con el que nos quedamos está formado por las siguiente variables: Incorporacion~G+porcentajeG+porcentejeN+Dg+AlturaDom+HartB+AlturaDom72+HartB72, con un R^2 de 33.06%
