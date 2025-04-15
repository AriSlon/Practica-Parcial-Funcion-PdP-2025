# Consigna TP1: Intro a Funcional

## Objetivos

- **Definir funciones** en Haskell, trabajando con tipos básicos.
- **Abstraer** y evitar repetición de lógica. ¡Vale definir todas las funciones que te parezcan útiles más allá de las que se piden!
- Trabajar adecuadamente con **expresiones booleanas**, **pattern matching** y **guardas**, eligiendo las herramientas más adecuadas para los distintos problemas.
- Desarrollar de forma **iterativa** usando **tests** automáticos :test_tube:

Recordá que si lo necesitás podés ayudarte con la [guía de lenguajes](https://docs.google.com/document/d/e/2PACX-1vTlLkakSbp6ubcIq00PU4-Z96tg8CUSc8bO793_uftmiGjfkSn7Ug-F_y0-ieIWG6aWfuoHLJrRL8Fd/pub).

> :bulb: Tené en cuenta que podés **subir soluciones intermedias** tantas veces como quieras. De hecho es una buena práctica subir tus cambios luego de aquellos avances que consideres relevantes.
> 
> Si algo no te funciona y necesitás ayuda para destrabarte, podés subir una solución intermedia y crear un **issue de consulta** en GitHub comentando qué es lo que no te está saliendo para que veamos lo que tenés y te demos una mano. No es la idea que te frustres :heart:

## Estaciones del año :sunny: :fallen_leaf: :snowflake: :blossom:

Queremos armar un programa que nos ayude a determinar qué estación del año se corresponde con cada mes.

A cada mes lo representaremos con un número entre el 1 (enero) y el 12 (diciembre). Cualquier otro número no se consideraría como un mes para nuestro programa.

A su vez sabemos que cada 3 meses hay cambios de estación, y en esos meses se da una mezcla de estaciones, como podría ser el caso de marzo, que cuando arranca es verano, pero cuando termina es otoño.

A continuación se detallan las funciones mínimas a desarrollar para resolver este problema.

> :exclamation: **Atención:** sólo se detallan las funciones que seguro hay que desarrollar, que son las que se usan desde las pruebas. Esto no significa que sean las únicas funciones que puedan ser útiles para resolver el problema, ¡todo lo contrario!
>
> Recordá que un objetivo de este trabajo es **abstraer** :rainbow:

### Saber si un número es un mes

Definir la función `esMes :: Number -> Bool` que dado un número retorne `True` sólo si el valor recibido se encuentra entre 1 y 12.

A continuación se muestran algunos ejemplos de uso:

```haskell
> esMes 8
True
> esMes 18
False
```

> :test_tube: Al correr `stack test` vas a encontrar los resultados de las pruebas para esta función agrupados bajo el título **Es mes** en el grupo de pruebas del TP1.

### Mes anterior y siguiente

- Definir la función `mesAnterior :: Number -> Number` que dado un número **que sea un mes válido**, retorne el número del mes anterior.
- De forma análoga, definir la función `mesSiguiente :: Number -> Number` para obtener el número del mes siguiente al recibido por parámetro.

:bulb: Tener en cuenta que el **dominio** de ambas funciones debería estar **acotado** a los números que sean meses: deberían ser **funciones parciales**. O sea que es esperable que **fallen** si se las usa con un número que no sea un mes válido.

A continuación se muestran algunos ejemplos de uso válidos:

```haskell
> mesAnterior 5
4
> mesAnterior 12
11
> mesAnterior 1
12
> mesSiguiente 5
6
> mesSiguiente 12
1
> mesSiguiente 1
2
```

> :test_tube: Al correr `stack test` vas a encontrar los resultados de las pruebas para estas funciones agrupados bajo el título **Mes anterior y siguiente** en el grupo de pruebas del TP1.

### Determinar la estación de un mes

Finalmente queremos cumplir con objetivo principal que es conocer cuál es la estación para cada mes considerando que:

- Sabemos que en enero (1) es `"verano"`, que en abril (4) es `"otonio"`, que en julio (7) es `"invierno"` y en octubre (10) es `"primavera"`.

- Para los meses en los cuales **hay cambio de estación** la respuesta debería combinar la **estación del mes anterior** con la **estación del mes siguiente**, separadas por `"/"`. Por ejemplo, para marzo (3) la respuesta debería ser `"verano/otonio"`.
  
  Los meses en los que **hay cambio de estación** son aquellos que, además de cumplir con ser meses, son múltiplos de 3.

- **Para los otros meses**, debería responder **lo mismo que para el mes anterior**, como podría ser el caso de febrero (2), que debería ser `"verano"` debido a que esa es la estación del mes de enero (1).

Se pide definir la función `estacion :: Number -> String`, considerando nuevamente que el **dominio** debería estar acotado a los números **que sean meses**, por lo que para otros valores es esperable que falle.

A continuación se muestran algunos ejemplos de uso con las respuestas esperadas:

```haskell
> estacion 8
"invierno"
> estacion 10
"primavera"
> estacion 9
"invierno/primavera"
```

> :bulb: Al definir la función `estacion`, asegurate de ***generalizar*** la lógica todo lo que puedas, usando las funciones `esMes`, `mesAnterior` y `mesSiguiente` que definiste anteriormente. El problema está expresado en esos términos intencionalmente para guiarte :wink:
>
> :warning: Para los meses 1, 4, 7 y 10, la respuesta no puede deducirse a partir de otra información, por lo que es correcto explicitar qué estación corresponde para cada uno de ellos. Sin embargo, para los otros meses **deberías poder calcular la estación sin tener que explicitar caso por caso cuál debería ser la respuesta**.


> :bulb: **Ayuda:** para concatenar strings en Haskell se puede usar la función `(++)`, como se muestra en este ejemplo:
> ```haskell
> > "para" ++ "digmas"
> "paradigmas"
> ```

> :test_tube: Al correr `stack test` vas a encontrar los resultados de las pruebas para esta función agrupados bajo el título **Estaciones** en el grupo de pruebas del TP1.

## Checkpoint de autorevisión :mag::broom::repeat:

Si estás leyendo esto y todas las pruebas están pasando: :tada: **felicitaciones** :tada: tu solución está cumpliendo con la funcionalidad esperada.

> :exclamation: Si no lo hiciste todavía, asegurate de subir tus cambios de la misma forma que se explicó en el TP0, usando los comandos:
> - `git add .` seguido de `git commit -m "mensaje del commit"` (o alternativamente `git commit -am "mensaje del commit"` que combina ambas operaciones), para versionar tus cambios localmente,
> - y luego `git push` para que se suban al repo remoto.

Sin embargo, todavía hay algo más para hacer antes de dar por terminado el trabajo. No olvides que el desarrollo es un proceso iterativo, nadie pretende que quede todo impecable a la primera pasada :smile:

1. :mag: Revisá la solución en busca de cosas mejorables.
2. :broom: Si encontrás alguna mejora posible, realizala, volvé a correr las pruebas, y si todo está ok, volvé a subir la solución.
3. :repeat: Repetí los pasos anteriores hasta que estés conforme con el resultado.

Prestá especial atención a los siguientes *code smells* que puede que estén presentes en tu solución:

- Comparaciones entre booleanos:
  - `expresion == True`: si una expresión booleana ya retorna True, no hace falta compararla con True para obtener True :wink:
  - `expresion == False`: en vez de hacer una comparación con False, podrías **negar** la expresión booleana.
- Guardas innecesarias: en [este artículo](https://wiki.uqbar.org/wiki/articles/funciones-por-partes.html) hay varios ejemplos de malos usos de guardas. Revisá que estés usando las herramientas más apropiadas en cada problema para que tu solución sea simple y elegante :monocle_face:
  
  > Prestá especial atención a los siguientes ejemplos:
  > - **Ejemplo 1:** guardas innecesarias al retornar booleanos
  > - **Ejemplo 3:** Uso de guardas cuando podría usarse pattern matching directamente sobre el parámetro de la función.
  > 
  > 💡 Recordá que se pueden combinar definiciones basadas en pattern matching y guardas para una misma función.

Otros aspectos importantes para revisar:
- **Abstracciones:** ¿identificaste alguna idea importante que se pudiera extraer como una función separada? ¿hay algún subproblema independiente que haya quedado mezclado con el resto de la lógica pedida?
- **Expresividad:** ¿los nombres elegidos para variables y funciones son claros y representativos?
- **Reutilización:** ¿aprovechaste las funciones desarrolladas en partes anteriores en la resolución de los siguientes problemas?

  > :bangbang: Si tu solución para la función `estacion` consta de 12 definiciones, donde para cada mes se explicita cuál debería ser la respuesta, o si se define con guardas mediante condiciones triviales que simplemente agrupan los valores para los cuales se espera que la respuesta sea la misma (por ejemplo, con condiciones como `mes == 1 || mes == 2`), buscá otra forma de plantearla, ya que será considerada **inválida** a pesar de que las pruebas pasen.

## Entrega de TP

Una vez que hayas subido tu solución para el trabajo práctico, recordá crear un tag para que se corran las pruebas en el servidor de GitHub, como se explicaba en el TP0 (usando un nuevo nombre de versión).
