# Consigna TP2: Composición, Aplicación Parcial y Orden Superior

## Objetivos

- Trabajar con tipos de datos compuestos.
- Usar **orden superior, composición y aplicación parcial** para resolver problemas complejos a partir de funciones más chicas.
- Elegir las operaciones de listas más apropiadas para cada problema.

## Contenido Audiovisual :arrow_forward:

Tenemos un programa que cuenta con el siguiente modelo de datos:

```haskell
data Video = Video {
  titulo :: String,
  minutos :: Number,
  hashtags :: [String]
} deriving (Show, Eq)

data Playlist = Playlist {
  descripcion :: String,
  videos :: [Video]
} deriving (Show, Eq)
```

A partir de este modelo, queremos hacer un programa que permita analizar la información de los videos que tenemos.

> :bulb: Recordá que podés ayudarte con la [guía de lenguajes](https://docs.google.com/document/d/e/2PACX-1vTlLkakSbp6ubcIq00PU4-Z96tg8CUSc8bO793_uftmiGjfkSn7Ug-F_y0-ieIWG6aWfuoHLJrRL8Fd/pub), que sirve como ayuda memoria de las operaciones comunes para trabajar con listas.

### Hashtags :hash:

Inicialmente queremos analizar el contenido de las playlists armadas en base a los hashtags que tienen sus videos. Para ello se pide definir las siguientes funciones:

- `tieneHashtag :: String -> Video -> Bool` que se cumpla para un video si el hashtag indicado se encuentra entre los propios.
- `minutosTotalesConHashtag :: String -> Playlist -> Number` que retorne la suma de minutos de aquellos videos de la playlist que tengan el hashtag indicado.

Por ejemplo, si se tiene una playlist `clasesPdeP` con los siguientes videos:
- `Video "Clase 1" 20 ["#intro", "#declaratividad"]`
- `Video "Clase 2" 60 ["#intro", "#funcional"]`
- `Video "Clase 3" 45 ["#funcional", "#data", "#tipos"]`

El comportamiento esperado para `minutosTotalesConHashtag` sería:

```haskell
> minutosTotalesConHashtag "#intro" clasesPdeP
80

> minutosTotalesConHashtag "#funcional" clasesPdeP
105
```

> :bulb: Procurá usar la función `tieneHashtag` para definir `minutosTotalesConHashtag`.

> Al correr `stack test` vas a encontrar los resultados de las pruebas para las 3 funciones pedidas, agrupados bajo el título **Hashtags** en el grupo de pruebas del TP2.

### Videos recomendados :ballot_box_with_check:

Queremos aprovechar lo desarrollado previamente para saber qué videos podrían ser recomendables para una playlist.

Para ello se pide resolver las siguientes funciones:

- `estanRelacionados :: Video -> Video -> Bool` que se cumpla cuando el primer video tenga alguno de los hashtags del segundo.
- `recomendable :: Video -> Playlist -> Bool` que se cumpla cuando el video en cuestión **esté relacionado con al menos dos** de los videos que conforman la playlist.

El comportamiento esperado para `estanRelacionados` sería:

```haskell
> estanRelacionados (Video "Clase 2" 60 ["#intro", "#funcional"]) (Video "Clase 3" 45 ["#funcional", "#data", "#tipos"])
True

> estanRelacionados (Video "Clase 1" 20 ["#intro", "#declaratividad"]) (Video "Clase 3" 45 ["#funcional", "#data", "#tipos"])
False
```
Ya que el video de la `"Clase 2"` tiene el hashtag `"#funcional"` (por lo tanto está relacionado con el de la `"Clase 3"`), mientras que el video de la `"Clase 1"` no está relacionado porque no tiene el hashtag `"#funcional"`, ni `"#data"`, ni `"#tipos"`.

Retomando el ejemplo anterior de la playlist `clasesPdeP`, se consideraría **recomendable** un video con el hashtag `"#funcional"`, por estar relacionado con los videos de `"Clase 2"` y `"Clase 3"`.

> Al correr `stack test` vas a encontrar los resultados de las pruebas bajo el título **Videos recomendados** en el grupo de pruebas del TP2.

## Checkpoint de autorevisión :mag::broom::repeat:

Si estás leyendo esto y todas las pruebas están pasando: :tada: **felicitaciones** :tada: tu solución está cumpliendo con la funcionalidad esperada. 

Sin embargo, todavía hay algo más para hacer antes de dar por terminado el trabajo. No olvides que el desarrollo es un proceso iterativo, nadie pretende que quede todo impecable a la primera pasada :smile:

> :bulb: Si no lo hiciste todavía, recordá que es una buena práctica subir tus cambios en esta instancia usando los comandos:
> - `git add .` seguido de `git commit -m "mensaje del commit"` (o alternativamente `git commit -am "mensaje del commit"` que combina ambas operaciones), para versionar tus cambios localmente,
> - y luego `git push` para que se suban al repo remoto.

Aspectos importantes para revisar:

- **Pattern matching vs. funciones de acceso**: Para estos problemas te sugerimos que **no uses pattern matching** para descomponer los datas. Es preferible trabajar con un nivel de abstracción más elevado, desentendiéndote de la forma de los datos.

- **¿Tenés composiciones de funciones interesantes?**: Asegurate de trabajar con composición de funciones en vez de aplicaciones anidadas. En especial revisá tu solución para `minutosTotalesConHashtag` y `recomendable` que pueden pensarse como secuencias de operaciones no triviales.

  - Si bien no es necesario que esas funciones se definan con [notación point-free](http://wiki.uqbar.org/wiki/articles/notacion-point-free.html) (omitiendo el último parámetro), recomendamos que lo hagas, para forzarte a explotar el uso de composición al máximo.

  - :bulb: Recordá que las funciones de acceso de los datas también pueden componerse como cualquier otra.

- **¿Estás aprovechando bien la aplicación parcial?:**  
  - Evitá definir **funciones auxiliares** sólo para sortear problemas que podían resolverse de forma sencilla, aplicando parcialmente una funciones preexistentes.
  - De forma análoga, evitá el uso de **expresiones lambdas** para los casos en los cuales la aplicación parcial alcanza para obtener la función que necesitás parametrizar.

- **¿Estás usando las operaciones de listas más adecuadas para cada problema?**: Si bien hay más de una forma de cumplir con la funcionalidad pedida, vamos a buscar que usen las operaciones que mejor se ajusten a la necesidad.

  Revisá las siguientes equivalencias a ver si encontrás algún patrón en común con tu solución que te permita simplificarla:
    - `any (== valor)` es equivalente a `elem valor`
    - `not.null.filter condicion` es equivalente a `any condicion`
    - Mapeos con funciones que retornan Bool:
      - `any (== True).map condicion` es equivalente a `any condicion`
      - `all (== True).map condicion` es equivalente a `all condicion`
      - `any (== False).map condicion` es equivalente a `any (not.condicion)`
      - `all (== False).map condicion` es equivalente a `all (not.condicion)`

## Entrega de TP

Una vez que hayas subido tu solución para el trabajo práctico, recordá crear un tag para que se corran las pruebas en el servidor de GitHub, como se explicaba en el TP0 (usando un nuevo nombre de versión).
