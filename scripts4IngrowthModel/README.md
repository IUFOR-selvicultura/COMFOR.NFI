# Datos incorporacion

# Mod Incorporacion E41
Como input tenemos los archivos de outputs del modelo mortalidad.

Lo primero que hacemos en este script es seleccionar solo los datos de la especie 41. Luego calculamos las variables altura dominate y hartB para los arboles de dicha especie. Eliminamos aquellas parcelas que estaban en el IFN3 pero no en el IFN2, ya que solo nos interesa la evolucion

Vamos a realizar un modelo generalizado de poisson. Incluimos inicialente todas las variables, y vamos eliminando las que no son significativas y las que tiene un factor de inflación de varianza superior a 10. Por tanto, finalmente el modelo con el que nos quedamos está formado por las siguiente variables: Incorporacion~G+N+porcentajeG+porcentejeN+Dg+AlturaDom+AlturaDom41+HartB41, con un R^2 de 19.34%

# Mod Incorporación E72
Como input tenemos los archivos de outputs del modelo mortalidad.

Lo primero que hacemos en este script es seleccionar solo los datos de la especie 41. Luego calculamos las variables altura dominate y hartB para los arboles de dicha especie. Eliminamos aquellas parcelas que estaban en el IFN3 pero no en el IFN2, ya que solo nos interesa la evolucion

Vamos a realizar un modelo generalizado de poisson. Incluimos inicialente todas las variables, y vamos eliminando las que no son significativas y las que tiene un factor de inflación de varianza superior a 10. Por tanto, finalmente el modelo con el que nos quedamos está formado por las siguiente variables: Incorporacion~G+porcentajeG+porcentejeN+Dg+AlturaDom+HartB+AlturaDom72+HartB72, con un R^2 de 33.06%
