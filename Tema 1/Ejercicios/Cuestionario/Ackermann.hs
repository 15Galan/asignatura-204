ackermann :: (Integer,Integer) -> Integer
ackermann (m,n) | m == 0          = n+1
                | m > 0 && n == 0 = ackermann(m-1,1)
                | m > 0 && n > 0  = ackermann(m-1,ackermann(m,n-1))
                | m < 0 && n >= 0 = error "El valor m no está en el dominio."
                | m >= 0 && n < 0 = error "El valor n no está en el dominio"
                | m < 0 && n < 0  = error "Los valores no están en el dominio de la función."
