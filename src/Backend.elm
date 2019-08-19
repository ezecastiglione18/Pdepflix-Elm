module Backend exposing(..)
import Models exposing(Movie, Preferences)

completaAca = identity --QUE TENGO QUE COMPLETAR ACA????????

-- **************
-- Requerimiento: filtrar películas por su título a medida que se escribe en el buscador;
-- **************

filtrarPeliculasPorPalabrasClave : String -> List Movie -> List Movie
filtrarPeliculasPorPalabrasClave palabras = List.filter (esIgualAlTexto palabras)

esIgualAlTexto : String -> Movie -> Bool
esIgualAlTexto texto pelicula = any (tieneParteDelTexto texto) [pelicula.title]

tieneParteDelTexto : String -> Bool
tieneParteDelTexto text = contains (toUpper text)

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
filtrarPeliculasPorMenoresDeEdad mostrarSoloMenores = filter (mostrarSoloMenores)

mostrarSoloMenores : Movie -> Bool
mostrarSoloMenores pelicula = pelicula.forKids == True

--NOTA: No entiendo la parte del checkbox. Que tengo que hacer con el checkbox?

-- **************
-- Requerimiento: ordenar las películas por su rating;
-- **************

ordenarPeliculasPorRating : List Movie -> List Movie
ordenarPeliculasPorRating = List.sort(map [pelicula.rating])

-- **************
-- Requerimiento: dar like a una película
-- **************

darLikeAPelicula : Int -> List Movie -> List Movie
darLikeAPelicula id = map (likearPelicula id)

likearPelicula : Int -> Movie -> Movie
likearPelicula id pelicula = if idMatchs id pelicula then darleLike pelicula

    idMatchs : Int -> Movie -> Bool
    idMatchs id pelicula = id == [pelicula.id]

--NOTA: Me fije en el tp de Currify que hay un idMatch que se fija si una pelicula coincide con un id

    darleLike : Movie -> Movie
    darleLike pelicula = {pelicula | liked = True}

-- **************
-- Requerimiento: cargar preferencias a través de un popup modal,
--                calcular índice de coincidencia de cada película y
--                mostrarlo junto a la misma;
-- **************

calcularPorcentajeDeCoincidencia : Preferences -> List Movie -> List Movie
calcularPorcentajeDeCoincidencia preferencias = completaAca
