module Backend exposing(..)
import Models exposing(Movie, Preferences)

completaAca = identity

-- **************
-- Requerimiento: filtrar películas por su título a medida que se escribe en el buscador;
-- **************
filtrarPeliculasPorPalabrasClave : String -> List Movie -> List Movie
filtrarPeliculasPorPalabrasClave palabras = List.filter(esIgualAlTexto palabras)

esIgualAlTexto : String -> Movie -> Bool
esIgualAlTexto texto pelicula = tieneParteDelTexto texto pelicula.title

tieneParteDelTexto : String -> String -> Bool
tieneParteDelTexto texto tituloPelicula = String.contains(String.toUpper texto) tituloPelicula

-- **************
-- Requerimiento: visualizar las películas según el género elegido en un selector;
-- **************

filtrarPeliculasPorGenero : String -> List Movie -> List Movie
filtrarPeliculasPorGenero genero = List.filter(coincideConGenero genero)

coincideConGenero : String -> Movie -> Bool
coincideConGenero genero pelicula = List.member (genero) pelicula.genre

-- **************
-- Requerimiento: filtrar las películas que sean aptas para menores de edad,
--                usando un checkbox;
-- **************

filtrarPeliculasPorMenoresDeEdad : Bool -> List Movie -> List Movie
filtrarPeliculasPorMenoresDeEdad mostrarSoloMenores = completaAca

-- **************
-- Requerimiento: ordenar las películas por su rating;
-- **************

ordenarPeliculasPorRating : List Movie -> List Movie
ordenarPeliculasPorRating = List.sortBy(.rating)

-- **************
-- Requerimiento: dar like a una película
-- **************

darLikeAPelicula : Int -> List Movie -> List Movie
darLikeAPelicula id = List.map(likearPelicula id)

likearPelicula : Int -> Movie -> Movie
likearPelicula id pelicula = if id == pelicula.id then darleLike pelicula

darleLike : Movie -> Movie
darleLike pelicula = {pelicula | likes = likes pelicula + 1}

-- **************
-- Requerimiento: cargar preferencias a través de un popup modal,
--                calcular índice de coincidencia de cada película y
--                mostrarlo junto a la misma;
-- **************

calcularPorcentajeDeCoincidencia : Preferences -> List Movie -> List Movie
calcularPorcentajeDeCoincidencia preferencias = List.map(calcularPorcentajePorPelicula preferencias) 

calcularPorcentajePorPelicula : Preferences -> Movie -> Int
calcularPorcentajePorPelicula preferencias pelicula = (actorFavorito preferencias pelicula)<<(generoFavorito preferencias pelicula)<<(palabrasClave preferencias pelicula)

actorFavorito : Preferences -> Movie -> Movie
actorFavorito preferencias pelicula = if List.member (preferencias.favoriteActor) pelicula.actors then (sumarPorcentaje pelicula 50)

generoFavorito : Preferences -> Movie -> Movie
generoFavorito preferencias pelicula = if (preferencias.genre) == pelicula.genre then (sumarPorcentaje pelicula 60)

palabrasClave : Preferencias -> Movie -> Movie
palabrasClave preferencias pelicula = if (esIgualAlTexto (preferencias.keywords) pelicula.title) then (sumarPorcentaje pelicula 20)
																
sumarPorcentaje : Movie -> Int -> Movie
sumarPorcentaje valor pelicula = {pelicula | matchPercentage = matchPercentage pelicula + valor}	