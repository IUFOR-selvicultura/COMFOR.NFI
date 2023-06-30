# Modelo mortalidad
Script para obtener los datos necesarios para los modelos de mortalidad

Los datos que necesitamos como input son: Del IFN2 "combi4172" y  "parcelasPorcentajes" y del IFN3 "combi4172".

Vamos a trabajar con las variables de parcelas que serán las que nos indiquen si las masas han aumentado o no, y de variables de árbol solo necesitaremos altura y expan para calcular otras variables necesarias como AlturaDominante.

Primero tenemos que indicar si se ha muerto un árbol o no, y para ello nos fijamos en el orden, es decir, que si es distinto de 0 en el orden2 y 0 en el orden3, indica que ese arbol ya no existe. Si está muerto le asignamos un 1 , y si esta vivo un 0.
Los datos de arboles les necesitamos en realidad del IFN2 , por lo que ahora dividimos los datos entre vivos y muertos, para buscar esos arboles en el data frame de combi4172_2 y asignarle el valor 0 o 1 que corresponda. 
Lo que en eralidad no simporta es el numoer de a´rboles que se han muerto en cada una de las 

