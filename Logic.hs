module Logic (JugadorActivo(JuegaBlanco, JuegaNegro, GanoBlanco, GanoNegro, FinJuego), Casilla,Tablero,puedoMoverPeonNegro,moverFicha,tableroInicial,esMiFicha, jugadorActivo, puedoMover, mover,obtenerCasilla, imprimirTablero) where
import Prelude

data Casilla = Invalida | Vacia | Negra | Blanca | DamaNegra | DamaBlanca
	deriving (Eq, Enum)

data JugadorActivo = JuegaBlanco | JuegaNegro | GanoBlanco | GanoNegro | FinJuego
	deriving (Eq, Enum, Show)

-- Posiciones de casillas determinadas por: (columna, fila)
data Tablero = Tablero {casillas :: [[Casilla]], activo :: JugadorActivo}
	deriving (Eq, Show)

notImplemented :: String -> a
notImplemented s = error $ " [" ++ s ++ "] Not Yet Implemented!"

instance Show Casilla where
	show Vacia = "."
	show Negra = "x"
	show Blanca = "o"
	show DamaNegra = "N"
	show DamaBlanca = "B"
	show Invalida = " "

-- devuelve el estado inicial del tablero
tableroInicial :: Tablero
tableroInicial = Tablero [
		[Negra, Invalida, Negra, Invalida, Negra, Invalida, Negra, Invalida],
		[Invalida, Negra, Invalida, Negra, Invalida, Negra, Invalida, Negra],
		[Negra, Invalida, Negra, Invalida, Negra, Invalida, Negra, Invalida],

		[Invalida, Vacia, Invalida, Vacia, Invalida, Vacia, Invalida, Vacia],
		[Vacia, Invalida, Vacia, Invalida, Vacia, Invalida, Vacia, Invalida],

		[Invalida, Blanca, Invalida, Blanca, Invalida, Blanca, Invalida, Blanca],
		[Blanca, Invalida, Blanca, Invalida, Blanca, Invalida, Blanca, Invalida],
		[Invalida, Blanca, Invalida, Blanca, Invalida, Blanca, Invalida, Blanca]
	] JuegaNegro



-- devuelve el jugador al que le toca jugar
jugadorActivo :: Tablero -> JugadorActivo
jugadorActivo t = activo t

obtenerCasilla :: Tablero -> (Int, Int) -> Casilla
obtenerCasilla t (b,a) = ((casillas t) !! (b-1)) !! (a-1)

esCasillaVacia :: Tablero -> (Int, Int) -> Bool
esCasillaVacia t (a, b) = if(esPosicionCorrecta (a,b)) then (obtenerCasilla t (a, b)) == Vacia else False


esCasillaEnemiga :: Tablero -> (Int,Int) -> Bool
esCasillaEnemiga t (m,n) = if (esPosicionCorrecta (m,n)) then (esCasillaEnemigaAux t (m,n)) else False
esCasillaEnemigaAux :: Tablero -> (Int, Int) -> Bool
esCasillaEnemigaAux t (a, b)
 | activo t == JuegaBlanco = (obtenerCasilla t (a, b)) == Negra || (obtenerCasilla t (a, b)) == DamaNegra
 | activo t == JuegaNegro = (obtenerCasilla t (a, b)) == Blanca || (obtenerCasilla t (a, b)) == DamaBlanca
 | otherwise = False

jugadorCasillero :: Tablero -> (Int, Int) -> JugadorActivo
jugadorCasillero t (a, b)
 | casilla == Negra = JuegaNegro
 | casilla == DamaNegra = JuegaNegro
 | otherwise = JuegaBlanco
 where casilla = (obtenerCasilla t (a,b))

--------------------------------------------------------------------------------------------------------
--IMPRESION DEL TABLERO--
imprimirFila :: [Casilla] -> String
imprimirFila [] = ""
imprimirFila (x:xs) = (show x) ++ "|" ++ (imprimirFila xs)

imprimirFilas :: [[Casilla]] -> [String]
imprimirFilas [] = [""]
imprimirFilas (x:xs) = ("|"++(show (8 - (length xs)))++"|" ++ (imprimirFila x)) : imprimirFilas xs
---
-- imprime el tablero
--	efectua la impresion del tablero tal cual se ve en la figura
--
--	 |_|1|2|3|4|5|6|7|8|
--	 |1|x| |x| |x| |x| |
--	 |2| |x| |x| |x| |x|
--	 |3|x| |x| |x| |x| |
--	 |4| |.| |.| |.| |.|
--	 |5|.| |.| |.| |.| |
--	 |6| |o| |o| |o| |o|
--	 |7|o| |o| |o| |o| |
--	 |8| |o| |o| |o| |o|


-- Cada linea se devuelve en un String diferente de la lista de retorno
imprimirTablero :: Tablero -> [String]
imprimirTablero t = "|_|1|2|3|4|5|6|7|8|" : imprimirFilas (casillas t)
------------------------------------------------------------------------------------------------------------

-- devuelve true si esta permitida la jugada y false en otro caso
-- las coordenas se representan como (1-8 filas, 1-8 columnas)
-- 	el segundo parametro son las coordenadas de origen
--  el tercer parametro son las coordenadas de destino
-- 	Se valida la jugada, en caso de error, los siguientes mensajes se deberan retornar junto al False
--			Error, las coordenadas no son correctas
--			Error, la ficha origen no es del jugador activo
--			Error, el juego acabo
--			Error, la jugada no esta permitida
-- 		En otro caso de devolvera
--			(True, "Jugada Permitida")
juegaBlanco :: Tablero -> Bool
juegaBlanco t = if(activo t)== JuegaBlanco then True else False

juegaNegro :: Tablero -> Bool
juegaNegro t = if (activo t)==JuegaNegro then True else False

esAliado :: Tablero -> (Int, Int) -> Bool
esAliado t casilla = not (esCasillaEnemiga t casilla)

puedoMoverPeonNegro :: Tablero -> (Int,Int)-> (Int, Int) -> (Bool,String)
puedoMoverPeonNegro t (i,j) (m,n)
	|esPosicionCorrecta (m,n) && esMiFicha t (i,j) =
		--Atrapando los casos de los extremos
	--Si la ficha se encuentra en el extremo izquierdo del tablero
					--Solo puede avanzar en su diagonal izquierda, si esta es vacia puede avanzar
	if (j==1) then 
				if (esCasillaVacia t (i+1, j+1)) && ((m,n)== (i+1, j+1))then (True,"Jugada Permitida")	
						--Si su diagonal inmediata derecha es enemiga, y su segunda diagonal inmediata derecha es vacia avanza y come
				else 
				   	if (esCasillaEnemiga t (i+1, j+1)) && (esCasillaVacia t (i+2, j+2)) && ((m, n) == (i+2, j+2)) then (True,"Jugada Permitida")
				   	else (False,"Jugada Invalida")
	else
		if(j==8) then 
				if (esCasillaVacia t (i+1,j-1)) && ((m,n)==(i+1,j-1))then (True,"Jugda Permitida")
    										
    			else
    			  							--Si su diagonal inmediata izquierda es enemiga y ademas la seungda diagonal imediata es vacia come y avanza
    			  	if( (esCasillaEnemiga t (i+1,j-1)) && (esCasillaVacia t (i+2,j-2)) && ((m,n)==(i+2,j-2)) ) then (True,"Jugada Permitida")
    			  									
    			  	else (False,"Jugada Invalida")
    	
    	else
			
				if(  ((obtenerCasilla t (i+1,j+1)) == (obtenerCasilla t (i+1,j-1))) && (esCasillaVacia t (i+1,j+1)) )then
					    if ((m,n)==(i+1,j+1) || (m,n)==(i+1,j-1)) then (True,"Jugada Permitida")
					    else (False,"Jugada Invalida")
						--Si la diagonal abajoderecha NO es enemiga, y la digonal imediata izquierda es enemidga, y la segunda
						--diagonal inmediata izquierd es vacia entonces se tiene que mover a esta ultima
				else 
					if(esCasillaEnemiga t (i+1,j+1)) && (esCasillaVacia t (i+2,j+2)) &&(esCasillaEnemiga t (i+1,j-1)) && (esCasillaVacia t (i+2,j-2)) then
								if (m,n)==(i+2,j+2) ||(m,n)==(i+2,j-2) then (True,"Jugaada Permitida")
								else
									(False,"Jugada Invalida")
							else
								if ((esCasillaEnemiga t (i+1,j-1)) && (esCasillaVacia t (i+2,j-2))) then
									if ((m,n)==(i+2,j-2)) then (True,"Jugada Permitida")--En este punto se tiene que comer la ficha ubicada en (i+1,j-1)
													--Idem caso anterior, solo que intercambiando derecha invertido
									else (False,"Jugada Invalida") 											     	 
								else 
									if ((esCasillaEnemiga t (i+1,j+1)) && (esCasillaVacia t (i+2,j+2))) then
										if ((m,n)==(i+2,j+2)) then (True,"Jugada Permitida")--En este punto se tiene que comer la ficha ubicada en (i+1,j-1)
													--Idem caso anterior, solo que intercambiando derecha invertido
										else (False,"Jugada Invalida")
									
							
					
							
					else
						if(not(esCasillaVacia t (i+1,j+1))) && (esCasillaVacia t (i+1,j-1)) then
							if(m,n)==(i+1,j-1) then (True,"Jugada Permitida")
							else (False,"Jugada Invalida6")
						else
							if(not(esCasillaVacia t (i+1,j-1))) && (esCasillaVacia t (i+1,j+1))then
								if(m,n)==(i+1,j+1) then (True,"Jugada Permitida")
								else (False,"Jugada Invalida")
							else (False,"Jugada Invalida")
	|(esPosicionCorrecta (m,n)) && ((m,n) == (i,j)) && esMiFicha t (i,j)= (True,"Jugada Permitida")
	|otherwise= (False,"Jugada Invalida")

puedoMoverPeonBlanco :: Tablero -> (Int,Int) -> (Int,Int) ->(Bool,String)
puedoMoverPeonBlanco t (i,j) (m,n)
	|esPosicionCorrecta (m,n) && esMiFicha t (i,j) =
	if (j==1) then 
				if (esCasillaVacia t (i-1, j+1)) && ((m,n)== (i-1, j+1))then (True,"Jugada Permitida")	
						--Si su diagonal inmediata derecha es enemiga, y su segunda diagonal inmediata derecha es vacia avanza y come
				else 
				   	if (esCasillaEnemiga t (i-1, j+1)) && (esCasillaVacia t (i-2, j+2)) && ((m, n) == (i-2, j+2)) then (True,"Jugada Permitida")
				   	else (False,"Jugada Invalida")
	else
		if(j==8) then 
				if (esCasillaVacia t (i-1,j-1)) && ((m,n)==(i-1,j-1))then (True,"Jugda Permitida")
    										
    			else
    			  							--Si su diagonal inmediata izquierda es enemiga y ademas la seungda diagonal imediata es vacia come y avanza
    			  	if( (esCasillaEnemiga t (i-1,j-1)) && (esCasillaVacia t (i-2,j-2)) && ((m,n)==(i-2,j-2)) ) then (True,"Jugada Permitida")
    			  									
    			  	else (False,"Jugada Invalida")
    	
    	else

		--Si las diagonales mas cercanas son ambas vacias, entonces puede ir a cualquiera de ellas



			
				if(  ((obtenerCasilla t (i-1,j+1)) == (obtenerCasilla t (i-1,j-1))) && (esCasillaVacia t (i-1,j+1)) )then
					    if ((m,n)==(i-1,j+1) || (m,n)==(i-1,j-1)) then (True,"Jugada Permitida")
					    else (False,"Jugada Invalida")
						--Si la diagonal abajoderecha NO es enemiga, y la digonal imediata izquierda es enemidga, y la segunda
						--diagonal inmediata izquierd es vacia entonces se tiene que mover a esta ultima
				else 
					if(esCasillaEnemiga t (i-1,j+1)) && (esCasillaVacia t (i-2,j+2)) &&(esCasillaEnemiga t (i-1,j-1)) && (esCasillaVacia t (i-21,j-2)) then
								if (m,n)==(i-2,j+2) ||(m,n)==(i-2,j-2) then (True,"Jugaada Permitida")
								else
									(False,"Jugada Invalida5")
							else
								if ((esCasillaEnemiga t (i-1,j-1)) && (esCasillaVacia t (i-2,j-2))) then
									if ((m,n)==(i-2,j-2)) then (True,"Jugada Permitida")--En este punto se tiene que comer la ficha ubicada en (i-1,j-1)
													--Idem caso anterior, solo que intercambiando derecha invertido
									else (False,"Jugada Invalida4") 											     	 
								else 
									if ((esCasillaEnemiga t (i-1,j+1)) && (esCasillaVacia t (i-2,j+2))) then
										if ((m,n)==(i-2,j+2)) then (True,"Jugada Permitida")--En este punto se tiene que comer la ficha ubicada en (i-1,j-1)
													--Idem caso anterior, solo que intercambiando derecha invertido
										else (False,"Jugada Invalida2")
									
							
					
							
					else
						if(not(esCasillaVacia t (i-1,j+1))) && (esCasillaVacia t (i-1,j-1)) then
							if(m,n)==(i-1,j-1) then (True,"Jugada Permitida")
							else (False,"Jugada Invalida6")
						else
							if(not(esCasillaVacia t (i-1,j-1))) && (esCasillaVacia t (i-1,j+1))then
								if(m,n)==(i-1,j+1) then (True,"Jugada Permitida")
								else (False,"Jugada Invalida7")
							else (False,"Jugada Invalida")

					
				
	--Escoge las mismas coordenadas de origen y destino y ambas son coordenadas validas
	|(esPosicionCorrecta (m,n)) && ((m,n) == (i,j)) && esMiFicha t (i,j)= (True,"Jugada Permitida")
--Cualquier otro caso, tamañana
	|otherwise= (False,"Jugada Invalida1")


--Decide si una dama puede comer o no
puedoMoverDama :: Tablero -> (Int,Int) -> (Int,Int)-> (Bool,String)
puedoMoverDama t (i,j) (m,n) 
	|(esPosicionCorrecta (m,n)) && (esMiFicha t (i,j)) = 
		---Alguna de  cuatro primeras casillas que rodean a la dama es vacia y las segundas son NO-vacias. (No tiene posibilidad de comer)
		
		if(length (posicionesQueComen t(i,j))/=0) then
			if(any((m,n)==) (posicionesQueComen t (i,j))) then (True,"Jugada Permitida")
			else (False,"Jugada Invalida")	
				--if (not(esCasillaEnemiga t arribaDer))&& (not(esCasillaEnemiga t arribaIzq)) && (not(esCasillaEnemiga t abajoIzq))&&(not(esCasillaEnemiga t abajoDer))then
		else
			if(((m,n)==arribaDer)||((m,n)==arribaIzq)||((m,n)==abajoDer)||((m,n)==abajoIzq))&&(esCasillaVacia t (m,n)) then (True,"Jugada Permitida")
			else
				(False,"Jugada Invalida2")
				--else (False,"Jugada Invalida 3")
	
			
			

	|otherwise = (False,"Jugada Invalida-Fuera de rango")
	where 
		arribaDer = (i-1,j+1)
		arribaDer2 = (i-2,j+2)
		arribaIzq = (i-1,j-1)
		arribaIzq2 = (i-2,j-2)
		abajoDer = (i+1,j+1)
		abajoDer2 = (i+2,j+2)
		abajoIzq = (i+1,j-1)
		abajoIzq2 = (i+2,j-2)


--Recorre todo el tablero insertando en una lista las coordenas de las fichas del jugador activo
--que tienen posibilidad de comer
quePiezasPuedenComer :: Tablero -> (Int,Int) ->[(Int,Int)]
quePiezasPuedenComer t (i,j)
	|j>8 = []
	|otherwise = 
		if(i==9) then (quePiezasPuedenComer t (1,j+1))
		else
			if(esPeonNegro t (i,j))&&(esMiFicha t (i,j)) then (puedeComerPeonNegro t (i,j)) ++ (quePiezasPuedenComer t (i+1,j))
			else
				if(esPeonBlanco t (i,j))&& (esMiFicha t (i,j)) then (puedeComerPeonBlanco t (i,j))++(quePiezasPuedenComer t (i+1,j))
				else
					if(esDama t (i,j))&&(esMiFicha t (i,j)) then (posicionesQueComen2 t (i,j))++(quePiezasPuedenComer t (i+1,j))
					else []++(quePiezasPuedenComer t (i+1,j))

--Devuelve las corrdenas de un peon con posibilidades de comer en  una lista
puedeComerPeonNegro :: Tablero -> (Int,Int)-> [(Int,Int)]
puedeComerPeonNegro t (i,j) =
	if(esPeonNegro t (i,j))then
				if ((esCasillaEnemiga t (i+1,j+1))&& (esCasillaVacia t (i+2,j+2)))||((esCasillaEnemiga t (i+1,j-1)) && (esCasillaVacia t (i+2,j-2))) then (i,j):[]
				else
					[]
	else
		[]

--Devuelv las coordenadas de un peon con posibilidades de comer en una lista
puedeComerPeonBlanco :: Tablero -> (Int,Int)->[(Int,Int)]
puedeComerPeonBlanco t (i,j) =
	if(esPeonBlanco t (i,j))then
		if (esCasillaEnemiga t (i-1,j+1))&& (esCasillaVacia t (i-2,j+2)) ||(esCasillaEnemiga t (i-1,j-1)) && (esCasillaVacia t (i-2,j-2)) then (i,j):[]
		else 
			[]
	else []


--Las funciones que terminan en 2 devuelven una lista con la posicion donde se ubica la pieza que tiene posibilidad de comer
--Las que no terminan en 2 devuelven una lista con la ubicacion a hacia donde se tiene que mover la pieza
posicionesQueComen2 :: Tablero -> (Int,Int)-> [(Int,Int)]
posicionesQueComen2 t orig = (podraComerArribaDer2 t orig)++(podraComerArribaIzq2 t orig)++(podraComerAbajoDer2 t orig)++(podraComerAbajoIzq2 t orig)++[]
podraComerArribaDer2 :: Tablero -> (Int,Int) -> [(Int,Int)]
podraComerArribaDer2 t (i,j) = if(esCasillaEnemiga t arribaDer) && (esCasillaVacia t arribaDer2) then (i,j):[]
							  else []
	where 
		arribaDer = (i-1,j+1)
		arribaDer2 = (i-2,j+2)


podraComerArribaIzq2 :: Tablero -> (Int,Int) -> [(Int,Int)]
podraComerArribaIzq2 t (i,j) = if(esCasillaEnemiga t arribaIzq) && (esCasillaVacia t arribaIzq2) then (i,j):[]
							  else []
	
	where
		arribaIzq = (i-1,j-1)
		arribaIzq2 = (i-2,j-2)


podraComerAbajoDer2 :: Tablero -> (Int,Int)-> [(Int,Int)]
podraComerAbajoDer2 t (i,j) = if(esCasillaEnemiga t abajoDer) && (esCasillaVacia t abajoDer2) then (i,j):[]
							 else [] 

	where
		abajoDer = (i+1,j+1)
		abajoDer2 = (i+2,j+2)


podraComerAbajoIzq2 :: Tablero -> (Int,Int)-> [(Int,Int)]
podraComerAbajoIzq2 t (i,j) = if(esCasillaEnemiga t abajoIzq) && (esCasillaVacia t abajoIzq2) then (i,j):[]
							 else []

	where
		abajoIzq = (i+1,j-1)
		abajoIzq2 = (i+2,j-2)

--Estas cuatro funciones determinan si una ficha tiene posibilidad de comer en la direccion indicada
--Si puede comer cada funcion retorna la posición final de destino donde se debe colocar la ficha 
--para que se concrete la accion de "comer"
podraComerArribaDer :: Tablero -> (Int,Int) -> [(Int,Int)]
podraComerArribaDer t (i,j) = if(esCasillaEnemiga t arribaDer) && (esCasillaVacia t arribaDer2) then arribaDer2:[]
							  else []
	where 
		arribaDer = (i-1,j+1)
		arribaDer2 = (i-2,j+2) 
podraComerArribaIzq :: Tablero -> (Int,Int) -> [(Int,Int)]
podraComerArribaIzq t (i,j) = if(esCasillaEnemiga t arribaIzq) && (esCasillaVacia t arribaIzq2) then arribaIzq2:[]
							  else []
	
	where
		arribaIzq = (i-1,j-1)
		arribaIzq2 = (i-2,j-2)
podraComerAbajoDer :: Tablero -> (Int,Int)-> [(Int,Int)]
podraComerAbajoDer t (i,j) = if(esCasillaEnemiga t abajoDer) && (esCasillaVacia t abajoDer2) then abajoDer2:[]
							 else [] 

	where
		abajoDer = (i+1,j+1)
		abajoDer2 = (i+2,j+2)

podraComerAbajoIzq :: Tablero -> (Int,Int)-> [(Int,Int)]
podraComerAbajoIzq t (i,j) = if(esCasillaEnemiga t abajoIzq) && (esCasillaVacia t abajoIzq2) then abajoIzq2:[]
							 else []

	where
		abajoIzq = (i+1,j-1)
		abajoIzq2 = (i+2,j-2) 
---Juntando las funciones auxiliares : podraComerArribaDer, podraComerArribaIzq, podraComerAbajoDer, podraComerAbajoIzq
---Inserta en una unica lista todas los pisiciones del jugador activo que pueden comer
posicionesQueComen :: Tablero -> (Int,Int)-> [(Int,Int)]
posicionesQueComen t orig = (podraComerArribaDer t orig)++(podraComerArribaIzq t orig)++(podraComerAbajoDer t orig)++(podraComerAbajoIzq t orig)++[]


---------------------------------------------
--Esta funcion se aplica si el jugador activo es el jugadorNegro
--Decide si un peon del jugador activo tiene posibilidad de movimiento
podraMoverPeonNegro :: Tablero -> (Int,Int)-> Bool
podraMoverPeonNegro t (i,j)=
	if((esCasillaVacia t abajoDer)||(esCasillaEnemiga t abajoDer) && (esCasillaVacia t abajoDer2)||
		(esCasillaVacia t abajoIzq)|| (esCasillaEnemiga t abajoIzq) && (esCasillaVacia t abajoIzq)) then True
	else False

	where
		abajoDer = (i+1,j+1)
		abajoDer2 = (i+2,j+2)
		abajoIzq = (i+1,j-1)
		abajoIzq2 = (i+2,j-2)


--Esta funcion se aplica si el jugaodr activo es el JugadorBlanco
--Decide si un pen del jugador tiene posibilidad de moviemiento
podraMoverPeonBlanco :: Tablero -> (Int,Int)-> Bool
podraMoverPeonBlanco t (i,j)=
	if((esCasillaVacia t arribaDer)||(esCasillaEnemiga t arribaDer) && (esCasillaVacia t arribaDer2)||
			(esCasillaVacia t arribaIzq)|| (esCasillaEnemiga t arribaIzq) && (esCasillaVacia t arribaIzq2)) then True
	else False
	where
		arribaDer = (i-1,j+1)
		arribaDer2 = (i-2,j+2)
		arribaIzq = (i-1,j-1)
		arribaIzq2 = (i-2,j-2)

	
	
       		
--Devuelve true si la dama del jugador activo tiene algún movimiento posible
podraMoverDama :: Tablero -> (Int,Int) -> Bool
podraMoverDama t (i,j) = 
	if((esCasillaVacia t arribaDer)||(esCasillaEnemiga t arribaDer) && (esCasillaVacia t arribaDer2)||
			(esCasillaVacia t arribaIzq)|| (esCasillaEnemiga t arribaIzq) && (esCasillaVacia t arribaIzq)||
			(esCasillaVacia t abajoDer)||(esCasillaEnemiga t abajoDer) && (esCasillaVacia t abajoDer2)||
		(esCasillaVacia t abajoIzq)|| (esCasillaEnemiga t abajoIzq) && (esCasillaVacia t abajoIzq)) then True
	else False
	where 
		arribaDer = (i-1,j+1)
		arribaDer2 = (i-2,j+2)
		arribaIzq = (i-1,j-1)
		arribaIzq2 = (i-2,j-2)
		abajoDer = (i+1,j+1)
		abajoDer2 = (i+2,j+2)
		abajoIzq = (i+1,j-1)
		abajoIzq2 = (i+2,j-2)
		

--Decide que piezas del jugador activo tienen posibilidad de realizar algún movimiento
quePiezasPuedenMover :: Tablero -> (Int,Int) -> [(Int,Int)]
quePiezasPuedenMover t (i,j)
	|j>8 = []
	|otherwise = 
		if(i==9) then (quePiezasPuedenMover t (1,j+1))
		
		else
			if(esPeonNegro t (i,j))&&(esMiFicha t (i,j)) then 
				if (podraMoverPeonNegro t (i,j)) then  ((i,j):([]++(quePiezasPuedenMover t (i+1,j)))) 
				else (quePiezasPuedenMover t (i+1,j)) 
			else
				if(esPeonBlanco t (i,j))&& (esMiFicha t (i,j)) then 
					if(podraMoverPeonBlanco t (i,j)) then ((i,j):([]++(quePiezasPuedenMover t (i+1,j)))) 
					else (quePiezasPuedenMover t (i+1,j)) 
				else
					if(esDama t (i,j))&&(esMiFicha t (i,j)) then 
						if (podraMoverDama t (i,j)) then ([]++(quePiezasPuedenMover t (i+1,j)))
						else quePiezasPuedenMover t (i+1,j)
					else []++(quePiezasPuedenMover t (i+1,j))
				

----------------------------------------------


--Esta funcion determina si la ficha escogida puede mover, sea dama o peon
--Aquí si se intenta mover una ficha con posiblidad de comer y no come devuelve false
--Si se intenta mover una ficha sin posibilida de comer cuando hay otra propia con posibilidad de comer
--devuevle false
puedoMover :: Tablero -> (Int, Int) -> (Int, Int) -> (Bool, String)
puedoMover t (i,j) (m,n)
	|(juegaNegro t) && esPeon t (i,j) =
		if(length (quePiezasPuedenComer t (1,1))/=0) then 
			if(any ((i,j)==) (quePiezasPuedenComer t (1,1))) then (puedoMoverPeonNegro t (i,j) (m,n))
			else
				(False,"Jugada Invalida")
		else puedoMoverPeonNegro t (i,j) (m,n)

		
	|(juegaNegro t) && esDama t (i,j) = 
		if(length (quePiezasPuedenComer t (1,1))/=0) then 
			if(any ((i,j)==) (quePiezasPuedenComer t (1,1))) then (puedoMoverDama t (i,j) (m,n))
			else
				(False,"Jugada Invalida")
		else puedoMoverDama t (i,j) (m,n)


	|(juegaBlanco t) && esDama t (i,j) =
		if(length (quePiezasPuedenComer t (1,1))/=0) then 
			if(any ((i,j)==) (quePiezasPuedenComer t (1,1))) then (puedoMoverDama t (i,j) (m,n))
			else
				(False,"Jugada Invalida")
		else puedoMoverDama t (i,j) (m,n)
	|(juegaBlanco t) && esPeon t (i,j) =
		if(length (quePiezasPuedenComer t (1,1))/=0) then 
			if(any ((i,j)==) (quePiezasPuedenComer t (1,1))) then (puedoMoverPeonBlanco t (i,j) (m,n))
			else
				(False,"Jugada Invalida")
		else puedoMoverPeonBlanco t (i,j) (m,n)


	|otherwise = (False,"Jugada Invalida")


--devuelve true si el juego termino 
--false en caso contrario
termino :: Tablero -> Bool
termino t
	|((length (terminoAux t (1,1)))==0) = True
	|((length (quePiezasPuedenMover (cambiarTurnoSimple t) (1,1)))==0) = True
	|otherwise = False


--Decide si el jugador activo del tablero se quedo  sin movimientos
meQuedeSinMovimientos :: Tablero -> Bool
meQuedeSinMovimientos t
	|((length (quePiezasPuedenMover t (1,1)))==0) = True
	|otherwise = False 


--Recorre el tablero insertando en una lista las coordenas de las fichas enemigas del jugador activo del tablero
terminoAux :: Tablero -> (Int,Int)-> [(Int,Int)]
terminoAux t (i,j)
	|j>8 = []
	|otherwise = 
		if(i==9) then terminoAux t (1,j+1)
		else
			if(esCasillaEnemiga t (i,j)) then (i,j):(terminoAux t (i+1,j))
			else terminoAux t (i+1,j)





--------------------------------FUNCIONES AUXILIARES (puedeMover)------------------------------------------

--Dado un tablero, y una coordenada determina
--Devuelve true si la ficha ubicada en la coordenada pertenece al jugador activo del tablero
--Devuelve false en cualquier caso que no entre en el primero
esMiFicha:: Tablero -> (Int,Int)-> Bool
esMiFicha t (i,j)
	|(activo t)==JuegaBlanco = if ((obtenerCasilla t (i,j))==Blanca) || ((obtenerCasilla t (i,j))==DamaBlanca)
		then True
		else False
	|(activo t)==JuegaNegro = if ((obtenerCasilla t (i,j))==Negra) || ((obtenerCasilla t (i,j))==DamaNegra)
		then True
		else False
	|otherwise= False


--Dado una coordenada determina si esta cae dentro de la matriz (1,1) - (8,8)
--Duevuelve true en caso afirmativo
--Devuelve false en caso contrario
esPosicionCorrecta :: (Int, Int) -> Bool
esPosicionCorrecta (a, b) = (a > 0) && (a < 9) && (b > 0) && (b < 9)

--Dado un tablero y una coordenada
--Devuelve true si la ficha ubicada en esa coordenada es una Dama
--Devuelve false en caso contrario
esDama :: Tablero -> (Int, Int) -> Bool
esDama t (a, b) = (obtenerCasilla t (a, b) == DamaNegra) || (obtenerCasilla t (a, b) == DamaBlanca)

--Dado un tablero y una coordenada
--Devuelve true si la ficha ubicada en esa coordenada es un peon
--Devuelve false en caso contrario
esPeon :: Tablero -> (Int, Int) -> Bool
esPeon t (a, b) =if(esPosicionCorrecta (a,b)) then not (esDama t (a, b)) else False

--decide si una ficha es un peonNegro
esPeonNegro :: Tablero -> (Int,Int)->Bool
esPeonNegro t (a,b) =if(esPosicionCorrecta (a,b)) then  ((show (obtenerCasilla t (a,b)))=="x")&& (esPeon t (a,b)) else False

--Decide si una ficha es un PeonBlanco
esPeonBlanco :: Tablero -> (Int,Int)->Bool
esPeonBlanco t (a,b) = if(esPosicionCorrecta (a,b))then ((show (obtenerCasilla t (a,b)))=="o")&&(esPeon t (a,b)) else False

--decide si una ficha es DamaBlanca
esDamaBlanca :: Tablero -> (Int,Int)->Bool
esDamaBlanca t (a,b) = if(esPosicionCorrecta (a,b)) then ((show (obtenerCasilla t (a,b)))=="B")&&(esDama t (a,b)) else False

--Decide si una ficha es DamaNegra
esDamaNegra :: Tablero -> (Int,Int)->Bool
esDamaNegra t (a,b) =if(esPosicionCorrecta (a,b)) then ((show (obtenerCasilla t (a,b)))=="esDama")&&(esDama t (a,b)) else False

------------------------------------------------------------------------------

--Estas cuatro funciones dada una posicion retornan la posicion de destino correspondiente
casillaArribaIzq :: (Int, Int) -> (Int, Int)
casillaArribaIzq (a, b) = (a - 1, b + 1)

casillaArribaDer :: (Int, Int) -> (Int, Int)
casillaArribaDer (a, b) = (a + 1, b + 1)

casillaAbajoIzq :: (Int, Int) -> (Int, Int)
casillaAbajoIzq (a, b) = (a - 1, b - 1)

casillaAbajoDer :: (Int, Int) -> (Int, Int)
casillaAbajoDer (a, b) = (a + 1, b - 1)



----------------------------------------------------------------------------------------------------------------
--Dado un tablero, devulve el mismo tablero con el jugador activo contrario
--Previo a cambiar el truno decide si el jugador activo tiene algunas ficha con 
--posibilidades de "comer" a otra ficha, de ser así no cambia el jugador activo
--caso contrario cambia el jugador activo del tablero.
cambiarTurno :: Tablero -> Tablero
cambiarTurno tab = 

	if((length (quePiezasPuedenComer tab (1,1)))/=0) then tab
	else
		if ((activo tab) == JuegaBlanco) then (Tablero (casillas tab) JuegaNegro) 
		else (Tablero (casillas tab) JuegaBlanco)


--Cambia el turno en el tablero
cambiarTurnoSimple :: Tablero -> Tablero
cambiarTurnoSimple t
	|((jugadorActivo t)==JuegaNegro) = Tablero (casillas t) JuegaBlanco
	|((jugadorActivo t)==JuegaBlanco) = Tablero (casillas t) JuegaNegro
	|otherwise = t


---------------------------------------------------------------------------------------------------------------------------------------

-- Efectua el movimiento, suponiendo que la invocacion con los mismos parametros de la funcion anterior dio (True, _)
-- Decide si el juego ha terminado y cambia el turno en el tableo
mover :: Tablero -> (Int, Int) -> (Int, Int) -> Tablero
mover t (i,j) (m,n)=
	if fst (puedoMover t (i,j) (m,n)) then
		if((abs(i-m))>1)&&((abs(j-n))>1)then 

			if (termino (Tablero (casillas (moverFicha t (i,j) (m,n))) (jugadorActivo t))) then 
				if(jugadorActivo t == JuegaNegro) then (Tablero (casillas (moverFicha t (i,j) (m,n))) (GanoNegro))
				else (Tablero (casillas (moverFicha t (i,j) (m,n))) (GanoBlanco))
			else
				if(meQuedeSinMovimientos (Tablero (casillas (moverFicha t (i,j) (m,n))) (jugadorActivo t))) then
						if(jugadorActivo t ==JuegaNegro) then Tablero (casillas (moverFicha t (i,j) (m,n))) (GanoBlanco)
						else
							Tablero (casillas (moverFicha t (i,j) (m,n))) (GanoNegro)
				else
					if(meQuedeSinMovimientos (Tablero (casillas (moverFicha t (i,j) (m,n))) (jugadorActivo t))) then
						if(jugadorActivo t ==JuegaNegro) then Tablero (casillas (moverFicha t (i,j) (m,n))) (GanoBlanco)
						else
							Tablero (casillas (moverFicha t (i,j) (m,n))) (GanoNegro)
					else
						cambiarTurno (Tablero (casillas (moverFicha t (i,j) (m,n))) (jugadorActivo t))

		else


			(cambiarTurnoSimple (Tablero (casillas (moverFicha t (i,j) (m,n))) (jugadorActivo t)))

		
	else
		t
	


---- FUNCIONES AUXILIARES (MOVER)-------------------------------------------------------------------------------------------------------

---Retorna el elemento ubicado en una posicion dada en una lista
elemAtPos :: [a] -> Int -> a
elemAtPos xs b = xs !! b


--Función que modifica una lista dado un indice y un nuevo elemento
changeElemAtPosLista :: [a] -> Int -> a -> [a]
changeElemAtPosLista xs idx elem = (modificarElem (xs,[]) idx elem)


--Función auxiliar para funcion: ChangeElemAtPosLista
modificarElem :: ([a],[a]) -> Int -> a -> [a]
modificarElem (x:xs,bs) idx elem = if (idx == 1) then  (reverse bs)++(elem:xs)else  (modificarElem (xs,x:bs) (idx-1) elem)

--Función auxiliar para changeElemAtPos2
auxChange :: [[a]] -> Int -> Int -> a -> [[a]]
auxChange xs i j elem = (changeElemAtPosLista xs i (changeElemAtPosLista (elemAtPos xs (i-1)) j elem))

--Dada una matriz, una coordenada perteneciente a la matriz, y un elemento
--Reemplaza el elemento ubicado en la coordenada indicada por el nuevo elemento
changeElemAtPosMat :: [[a]] -> (Int,Int) -> a -> [[a]]
changeElemAtPosMat xs (i,j) elem = auxChange xs i j elem




--Dado un tablero, una coordenada de origen y una coordenada de destino
--Remplaza la ficha ubicada en la coordenada de destino por una ficha igual a la ficha ubicada en la coordenada de origen
--La ficha ubicada en el origen es remplazada por un casillero vacio
{-
moverFicha :: Tablero -> (Int, Int) -> (Int, Int) -> Tablero
moverFicha t orig dest = Tablero   (changeElemAtPosMat ((changeElemAtPosMat (casillas t) dest (obtenerCasilla t orig))) orig Vacia)   (activo t)

-}	
{- Vacia la casilla en la posicion dada para una lista de casillas. -}
colocarFichaAux :: [Casilla] -> Casilla -> Int -> [Casilla]
colocarFichaAux columna casilla b = (take b columna) ++ [casilla] ++ (drop (b + 1) columna)

colocarFicha :: [[Casilla]] -> Casilla -> (Int, Int) -> [[Casilla]]
colocarFicha casillas casilla (i, j) =
	(take (i - 1) casillas) ++ [colocarFichaAux columna casilla (j - 1)] ++ (drop (i) casillas)
	where columna = casillas !! (i - 1)
-------------------------------------------------------------------------------------------------------------------

--Función importante 
--Dado un tablero, una coordenada de origne y una coordenada de destino
--En la ubicacion de destino se inserta la ficha ubicada en la posicion de origen
--En la ubicacion de origen y en toda la trayectoria que describe el movimiento
--desde el origen hasta (no inclusive) el destino todas las fichas son cambiadas por 
--fichas vacias
moverFicha :: Tablero -> (Int,Int) -> (Int,Int) -> Tablero 
moverFicha t (i,j) (m,n) = 

	if((jugadorActivo t)==(JuegaNegro))then
		if(m==8)then Tablero (changeElemAtPosMat (casillas (vaciarCasillas t (i,j) (m,n))) ((m,n)) (DamaNegra)) (jugadorActivo t)
		else
			Tablero (changeElemAtPosMat (casillas (vaciarCasillas t (i,j) (m,n))) ((m,n)) (obtenerCasilla t (i,j))) (jugadorActivo t) 
	else
		if((jugadorActivo t)==(JuegaBlanco))then
				if(m==1)then Tablero (changeElemAtPosMat (casillas (vaciarCasillas t (i,j) (m,n))) ((m,n)) (DamaBlanca)) (jugadorActivo t)
				else
					Tablero (changeElemAtPosMat (casillas (vaciarCasillas t (i,j) (m,n))) ((m,n)) (obtenerCasilla t (i,j))) (jugadorActivo t)	
		else
					Tablero (changeElemAtPosMat (casillas (vaciarCasillas t (i,j) (m,n))) ((m,n)) (obtenerCasilla t (i,j))) (jugadorActivo t)
	

----------------------------------------SOLUCIÓN AL PROBLEMA: Comer enemigos-----------------------------------
obtenerSigno :: Int -> Int
obtenerSigno a
 | a > 0 = 1
 | a < 0 = -1
 | otherwise = 0

{- Mueve una posición hacia destino. -}
moverHaciaDestino :: (Int, Int) -> (Int, Int) -> (Int, Int)
moverHaciaDestino (a, b) (c, d) = (a + (obtenerSigno despV), b + (obtenerSigno despH))
  where despV = (c - a)
        despH = (d - b)

{- Vacia la casilla en la posicion dada para una lista de casillas. -}
vaciarCasillaAux :: [Casilla] -> Int -> [Casilla]
vaciarCasillaAux columna b = (take b columna) ++ [Vacia] ++ (drop (b + 1) columna)

{-
Vacia una casilla de una fila

-}

vaciarCasilla :: [[Casilla]] -> (Int, Int) -> [[Casilla]]
vaciarCasilla casillas (i, j) =
	(take (i - 1) casillas) ++ [vaciarCasillaAux columna (j - 1)] ++ (drop (i) casillas)
	where columna = casillas !! (i - 1)



{-
Vacia las casillas de un tablero, que se ubican dentro de la diagonal 
formada por origen y destino, las coordeas de orig y destino
tambien son remplazas por casilleros vacios.

-}

vaciarCasillas :: Tablero -> (Int, Int) -> (Int, Int) -> Tablero
vaciarCasillas t origen destino
 | origen == destino = tableroPostVaciar
 | otherwise = (vaciarCasillas tableroPostVaciar (moverHaciaDestino origen destino) destino)
 where tableroPostVaciar = Tablero (vaciarCasilla (casillas t) origen) (activo t)


---REGLAS DEL JUEGO ---
{-
*Comienzan las blancas 
*Los peones se mueven únicamente en diagonal, de a un casillero y hacia delante.
*Existe un movimiento en el que un peon avanza dos casilleros, esto se da cuando 
"come" un rival y sucede cuando uno de los primeros casilleros diagonles a la ficha
es un enemigo y su consecutivo es un casillero vacio
*Si en peon llega hasta la primer fila del enemigo entonces este peon se convierte en dama
*Una dama se puede mover en las cuatro diagonales que la rodean, y "come" de la misma manera
que un peon. La dama se puede mover tanto hacia delante como hacia atras
*Pierde el primer jugador que se quede sin fichas
*Pirde el primer jugador que se quede sin movimientos 



-}