# Modelo altura
Se depuran los datos para poder realizar el modelo de altura-diametro. 
En este caso nuestra variable respuesta será la altura, y como variable explicativa tiene que estar el diámetro obligatoriamente.

Solo nos interesan los datos del IFN2, asi que nuestros inputs serán 'combi4172' que recoge las parcelas mixtas con árboles de las especies 41 y 72 y 'parcelasPorcentaje' que recoge las variables de parcelas. Nos quedamos únicamente con los datos de parcelasPorcentaje que corresponden a las parcelas mixtas buscadas

Añadimos variables de árboles y parcela como ProxVol, Dg, SDI, BAL, AlturaDominante, HartB y Hart, y además reescalamos la variable g para que tenga un rango de valores similar.

El ioutput generado es 'alturaDiam', que contiene el data frame resultante de unir los dos inputs con todas las variables necesarias

# Mod Altura E41
