-------------------------------------------------------------------------------
-- Student's name:  Antonio J.
-- Student's group: Galán Herrera
--
-- Data Structures. Grado en Informática. UMA.
-------------------------------------------------------------------------------

module DataStructures.Graph.EulerianCycle(isEulerian, eulerianCycle) where

import DataStructures.Graph.Graph
import Data.List

--H.1)
isEulerian :: Eq a => Graph a -> Bool
isEulerian g = and [ even $ length (successors g ver) | ver <- vertices g ]

-- «ver <- vertices g» devuelve una lista con todos los vertices del grafo.
-- «length (successors g ver)» devuelve la longitud de los sucesores de cada vertice.
-- «even» devuelve True si su argumento es par. «and» devuelve False si hay algun False.



-- H.2)
remove :: (Eq a) => Graph a -> (a,a) -> Graph a
remove g (v,u) | aisladoV && aisladoU = deleteVertex (deleteVertex g' v) u
               | aisladoV  = deleteVertex g' v
               | aisladoU  = deleteVertex g' u
               | otherwise = g'
    where
        g'       = deleteEdge g (v,u)
        aisladoV = degree g' v == 0
        aisladoU = degree g' u == 0

-- Primero debe borrarse la arista «(v,u)» de «g», eso es seguro, produciendo «g'».
-- Tras eso, «v»/«u» puede quedarse aislado, entonces debe comprobarse y si es así,
-- debe eliminarse dicho vertice del grafo resultante sin la arista «g'».



-- H.3)
extractCycle :: (Eq a) => Graph a -> a -> (Graph a, Path a)
extractCycle g v0 = aux g v0 [v0]
    where
        aux g v xs | u == v0   = (g', u:xs)
                   | otherwise = aux g' u (u:xs) 
            where
                u  = head $ successors g v
                g' = remove g (v,u)

-- Para este tipo de funciones mas complejas de lo normal, sera necesario crear
-- una funcion auxiliar con argumentos personalizados. Como la funcion «extractCycle»
-- devuelve tambien un camino (lista), se creará una funcion auxiliar donde el tercer
-- argumento es una lista, cuyo primer elemento es «v0» («aux v0 [v0]»).

-- «g'» será el grafo una vez se haya completado un paso del algoritmo voraz.
-- «u» es el primer sucesor del vertice que se analiza «v». Se escoge el primer
-- argumento de la lista de sucesores porque simplemente, da igual, ya que lo
-- unico necesario es generar un camino parcial.



-- H.4)
connectCycles :: (Eq a) => Path a -> Path a -> Path a
connectCycles [] ciclo = ciclo
connectCycles (x:xs) (y:ys) = if x == y then y:ys++xs else x:connectCycles xs (y:ys)

-- Lo unico importante a tener en cuenta es que el segundo camino ya incluye al vertice
-- por el que se busca, así que el elemento «x» que es el que se evalua de la primera
-- lista (camino) no debe incluirse en la concatenacion, porque ya esta en la lista
-- (camino) del segundo argumento «(y:ys)».



-- H.5)
vertexInCommon :: Eq a => Graph a -> Path a -> a
vertexInCommon g []     = error "No hay vertices comunes"
vertexInCommon g (c:cs) = if c `elem` vertices g then c else vertexInCommon g cs

-- Sea «(c:cs)» la lista que representa un camino parcial, si su primer elemento es
-- uno de los vertices del grafo, entonces lo devuelve. En caso contrario, vuelve a
-- llamar a la funcion, pero con el resto de vertices del camino parcial.
-- En el caso de no encontrarse ningun vertice, seria un error como que el camino
-- parcial no corresponde al grafo «g».



-- H.6) 
eulerianCycle :: Eq a => Graph a -> Path a
eulerianCycle g | not $ isEulerian g = error "Grafo no euleriano."
                | otherwise          = aux g (head $ vertices g) []
    where
        aux g v xs | isEmpty g = xs
                   | otherwise = aux g' v' xs'
            where
                (g', ciclo) = extractCycle g v
                xs'         = connectCycles xs ciclo
                v'          = vertexInCommon g' xs'

-- Para este tipo de funciones mas complejas de lo normal, sera necesario crear
-- una funcion auxiliar con argumentos personalizados. En este caso, interesa
-- mantener las modificaciones de un grafo «g», un vertice para analizar «v» y
-- un camino que se irá modificando hasta ser el camino euleriano «xs».
-- Al descomponerse el enunciado, queda bastante claro que la ejecuccion solo
-- tiene lugar si el grafo es euleriano (lineas 88 y 89), de manera que si lo es
-- ejecute la funcion auxiliar que, a su vez, se sabe que sera recursiva hasta que
-- el grafo del primer argumento «g» sea vacio.

-- Una vez conseguido el esquema principal (por llamarlo de alguna forma) ya solo
-- queda traducir el algoritmo descrito en este ultimo apartado:

--    En la linea 95 se extrae un nuevo ciclo parcial para el primer vertice de la lista
--    de vertices del grafo «g». Esta linea se encargara de sacar un nuevo ciclo parcial
--    para cada uno de los vertices del grafo, obteniendo poco a poco el camino euleriano.
--    Tambien se extrae el nuevo grafo sin el primer vertice «g'» y sin vertices aislados,
--    este grafo que se ira reduciendo hasta estar vacio, lo que finalizara el algoritmo.

--    En la linea 96 se obtiene «xs'», que es la combinacion del ciclo parcial de cada vertice
--    de «g» con el camino parcial «xs», argumento en el que se irá modificando el camino final.

--    En la linea 97 se obtiene «v'», el primer vertice de la lista de vertices comunes entre
--    el nuevo grafo modificado «g'» y el camino parcial «xs'». Así se obtiene el vertice
--    para la siguiente iteracion de «aux», ya que hay que asegurarse de que no sea un vertice
--    que haya sido eliminado en el paso anterior por quedarse aislado.
--    Tambien tiene que pertenecer al camino parcial, para que si fuese necesario durante el
--    algoritmo se tuviese que añadir un ciclo parcial desde ese vertice.
