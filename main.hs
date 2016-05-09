module Main where
import Prelude
import Logic (JugadorActivo(JuegaBlanco, JuegaNegro, GanoBlanco, GanoNegro, FinJuego), Tablero, tableroInicial,moverFicha,esMiFicha,obtenerCasilla,puedoMoverPeonNegro,jugadorActivo, puedoMover, mover, imprimirTablero)

data Jugador = Blanco {nombre :: String} | Negro {nombre :: String}
	deriving (Eq)

data Juego = Juego {tablero :: Tablero, jugadorBlanco :: Jugador, jugadorNegro :: Jugador}
	deriving (Eq)

-- getString :: IO String
-- getString = readLn

getInt :: IO Int
getInt = readLn


--
-- JUGADOR
--

-- instancia de Show
instance Show Jugador where
	-- show j = notImplemented "Show Jugador"
	show (Blanco n) = "Jugador: " ++ n ++ "(b)"
	show (Negro n) = "Jugador: " ++ n ++ "(n)"

readWhite = do { putStrLn "Ingrese el nombre del jugador Blanco: ";
	n <- getLine;
	return (Blanco n) }
readBlack = do { putStrLn "Ingrese el nombre del jugador Negro: ";
	n <- getLine;
	return (Negro n) }
readCoord str = do { putStrLn ("Ingrese las coordenadas de " ++ str ++ ": ");
	i <- getInt;
	j <- getInt;
	return (i, j) }

tryPlay board c1 c2 = if v then do { return (mover board c1 c2) } else do { putStrLn m; return board }
	where
		(v, m) = puedoMover board c1 c2

printBoard = putStrLn . (concatMap (++ "\n")) . imprimirTablero

playAndPrint j = do
	{
		c1 <- readCoord "origen";
		c2 <- readCoord "destino";
		b2 <- tryPlay (tablero j) c1 c2;
		printBoard b2;
		loopGame (Juego b2 (jugadorBlanco j) (jugadorNegro j))
	}



loopGame j = case (jugadorActivo (tablero j)) of
			JuegaBlanco -> do { putStrLn (show (jugadorBlanco j)); playAndPrint j }
			JuegaNegro -> do { putStrLn (show (jugadorNegro j)); playAndPrint j }
			GanoBlanco -> do { putStrLn ("El juego acabo, gano " ++ (nombre (jugadorBlanco j))); main }
			GanoNegro -> do { putStrLn ("El juego acabo, gano " ++ (nombre (jugadorNegro j))); main }
			FinJuego -> do { putStrLn "El juego acabo, puede comenzar otra partida"; main }

startGame = do
	{
		putStrLn "Inicia juego de Damas:";
		putStrLn "======================";
		putStrLn "";
		putStrLn "|_|1|2|3|4|5|6|7|8|";
	    putStrLn "|1|x| |x| |x| |x| |";
	    putStrLn "|2| |x| |x| |x| |x|";
	 	putStrLn "|3|x| |x| |x| |x| |";
	 	putStrLn "|4| |.| |.| |.| |.|";
	 	putStrLn "|5|.| |.| |.| |.| |";
	 	putStrLn "|6| |o| |o| |o| |o|";
	 	putStrLn "|7|o| |o| |o| |o| |";
	 	putStrLn "|8| |o| |o| |o| |o|";
		putStrLn "";
		putStrLn "";
		w <- readWhite;
		b <- readBlack;
		loopGame (Juego tableroInicial w b)
	}

main = do
	{ putStrLn (concatMap (++ "\n")  ["==========================================", "|          Damas ProgFun - 2015          |", "==========================================", "Escriba:", "  i- iniciar", "  s- salir", "==========================================" ]);
		c <- getLine;
		-- putStrLn c;
		-- putStrLn "====";
		case c of
			('i':_) -> startGame;
			('s':_) -> return ();
			_ -> main;
	}
