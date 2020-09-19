module InFix (evaluateInFix) where
import DataStructures.Stack.LinearStack
import Expression


---------------------------------------
-- Ejercicio 4
---------------------------------------

evaluateInFix :: Expression -> Integer       -- Necesaria una función auxiliar, aquí se
evaluateInFix expr = aux expr empty empty    -- crean los datos (expresión y 2 pilas vacías).

                                             
aux :: Expression -> Stack Integer -> Stack Item -> Integer
aux [] sData sOp         = top sData                        -- Caso base.
aux (Add:expr) sData sOp = aux expr sData (push Add sOp)
aux (Dif:expr) sData sOp = aux expr sData (push Dif sOp)
aux (Mul:expr) sData sOp = aux expr sData (push Mul sOp)
aux (Value x:expr) sData sOp = aux expr (push x sData) sOp
aux (LeftP:expr) sData sOp   = aux expr sData sOp
aux (RightP:expr) sData sOp  = aux expr (push res sD'') sOp'
    where
        a1   = top sData   -- Argumento 1.
        sD'  = pop sData   -- Pila sData sin el "Argumento 1".
        a2   = top sD'     -- Argumento 2.
        sD'' = pop sD'     -- Pila sData sin "a1" y "a2".
        op   = top sOp     -- Operación.
        sOp' = pop sOp     -- Pila sOp sin la última operación.
        res  = value op a2 a1    -- "Resultado" de la operación realizada.
                                 -- Los argumentos los coge al revés (a2, a1).