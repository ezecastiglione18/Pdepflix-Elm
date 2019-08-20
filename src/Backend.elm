module Backend exposing(..)
import Models exposing(Movie, Preferences)

-- **************
-- Requerimiento: filtrar películas por su título a medida que se escribe en el buscador;
-- **************

filtrarPeliculasPorPalabrasClave : String -> List Movie -> List Movie
filtrarPeliculasPorPalabrasClave palabras = List.filter(esIgualAlTexto palabras)

esIgualAlTexto : String -> Movie -> Bool
esIgualAlTexto texto pelicula = any (tieneParteDelTexto texto) [pelicula.title]

tieneParteDelTexto : String -> Bool
tieneParteDelTexto texto = String.contains (toUpper texto)

--QUE HACER CON LA FUNCION DE ABAJO?
--peliculaTienePalabrasClave palabras pelicula = String.contains "Toy" pelicula.title

-- **************
-- Requerimiento: visualizar las películas según el género elegido en un selector;
-- **************

filtrarPeliculasPorGenero : String -> List Movie -> List Movie
filtrarPeliculasPorGenero genero = List.filter (coincideConGenero genero)

coincideConGenero : String -> Movie -> Bool
coincideConGenero genero pelicula = contains (genero) [pelicula.genre]

-- **************
-- Requerimiento: filtrar las películas que sean aptas para menores de edad,
--                usando un checkbox;
-- **************

filtrarPeliculasPorMenoresDeEdad : Bool -> List Movie -> List Movie
filtrarPeliculasPorMenoresDeEdad mostrarSoloMenores = if (mostrarSoloMenores == True) then List.filter(esAptaParaMenores)

esAptaParaMenores : Movie -> Bool
esAptaParaMenores pelicula = pelicula.forKids


-- **************
-- Requerimiento: ordenar las películas por su rating;
-- **************

ordenarPeliculasPorRating : List Movie -> List Movie
ordenarPeliculasPorRating = List.sortBy([pelicula.rating])<<map [pelicula.rating]

-- **************
-- Requerimiento: dar like a una película
-- **************

darLikeAPelicula : Int -> List Movie -> List Movie
darLikeAPelicula id = map (likearPelicula id)

likearPelicula : Int -> Movie -> Movie
likearPelicula id pelicula = if idMatchs id pelicula then darleLike pelicula

idMatchs : Int -> Movie -> Bool
idMatchs id pelicula = id == pelicula.id

--NOTA: Me fije en el tp de Currify que hay un idMatch que se fija si una pelicula coincide con un id

    darleLike : Movie -> Movie
    darleLike pelicula = {pelicula | liked = True, likes = likes pelicula + 1}

-- **************
-- Requerimiento: cargar preferencias a través de un popup modal,
--                calcular índice de coincidencia de cada película y
--                mostrarlo junto a la misma;
-- **************

calcularPorcentajeDeCoincidencia : Preferences -> List Movie -> List Movie 
calcularPorcentajeDeCoincidencia preferencias = map(calcularPorcentajePorPelicula preferencias) 

calcularPorcentajePorPelicula : Preferences -> Movie -> Int
calcularPorcentajePorPelicula preferencias pelicula = if contains (preferencias.favoriteActor) [pelicula.actors] then (sumarPorcentaje pelicula 50)
															else if (preferencias.genre) == [pelicula.genre] then (sumarPorcentaje pelicula 60)
																else if (esIgualAlTexto (preferencias.keywords) [pelicula.title]) then (sumarPorcentaje pelicula 20)
																
sumarPorcentaje : Movie -> Int -> Movie
sumarPorcentaje valor pelicula = {pelicula | matchPercentage = matchPercentage pelicula + valor}
																