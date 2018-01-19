module WellBalanced where
import DataStructures.Stack.LinearStack as S                        -- "as S" para que en la línea 20
import Test.QuickCheck                                              -- genere una pila vacía correctamente


---------------------------------------
-- Ejercicio 1
---------------------------------------

wellBalanced :: String -> Bool
wellBalanced xs = wellBalanced' xs S.empty

wellBalanced' :: String -> Stack Char -> Bool
wellBalanced' [] s = isEmpty s                                         -- Caso base: ¿la pila está vacía?
wellBalanced' (x:xs) s | apertura     = wellBalanced' xs (push x s)    -- ¿Es una apertura? Introducir en la pila.
                       | cierre && p' = wellBalanced' xs (pop s)       -- ¿Es un cierre y su apertura está en la pila? Sacar de la pila.
                       | otherwise    = wellBalanced' xs s             -- No es signo de apertura ni de cierre.
    where
        apertura = x == '(' || x == '[' || x == '{'                    -- Signos de apertura.
        cierre   = x == ')' || x == ']' || x == '}'                    -- Signos de cierre.
        p        = (top s, x)                                          -- (último valor de la pila, signo de cierre).
        p'       = p == ('(',')') || p == ('[',']') || p == ('{','}')  -- Parejas de signos de apertura y de cierre.


    -- Idea (mía): Cada vez que se vaya a analizar un signo de cierre, también será
    -- necesario comprobar si su pareja (signo de apertura) está en la pila. Si se
    -- hiciese la función sin identificar parejas bastaría que con que hubiese el
    -- mismo número de signos de apertura que de cierre (aunque fuesen distintos)
    -- para que el resultado fuera una cadena balanceada, cuando podría no serlo.
    --
    -- Para identificar si la pareja del signo de cierre está en la pila he usado
    -- una variable "p" de tipo tupla, con el último elemento de la pila y el signo
    -- que se va a comprobar (signo de cierre). Además, creé otra variable "p'" que
    -- contiene todas las parejas de signos, de forma que comprobando si "p'" resulta
    -- verdadero en algún caso, significará que la pila conserva la pareja del signo
    -- de cierre introducido. Evitando así que se saque de la pila un signo de
    -- apertura con un signo de cierre que no es su pareja.