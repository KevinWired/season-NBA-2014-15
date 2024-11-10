{-|

    Date: Oct 14, 2021
    
    Autor: Kevin Santillan
    
    File: nba.hs
    
    Last modify: Nov 10, 2024
    
-}

type Player = (String, Int, Int, Int, Int, Int, Int, Int)
-- <NAME, GAMES, MIN, PTS, FGM, FGA, PM3, PA3>

-- Proyecciones
name :: Player -> String
name (player, games, minutes, pts, fgm, fga, pm3, pa3) = player

cant_games :: Player -> Int
cant_games (player, games, minutes, pts, fgm, fga, pm3, pa3) = games

cant_min :: Player -> Int 
cant_min (player, games, minutes, pts, fgm, fga, pm3, pa3) = minutes

cant_pts :: Player -> Int 
cant_pts (player, games, minutes, pts, fgm, fga, pm3, pa3) = pts

cant_fgm :: Player -> Int
cant_fgm (player, games, minutes, pts, fgm, fga, pm3, pa3) = fgm

cant_fga :: Player -> Int
cant_fga (player, games, minutes, pts, fgm, fga, pm3, pa3) = fga

cant_pm3 :: Player -> Int
cant_pm3 (player, games, minutes, pts, fgm, fga, pm3, pa3) = pm3

cant_pa3 :: Player -> Int
cant_pa3 (player, games, minutes, pts, fgm, fga, pm3, pa3) = pa3

-- fin Proyecciones

-- Funciones 
-- 1) Funcion tipo fold (dada la lista de jugadores, cuenta la cantidad de estos)
totalPlayers :: [Player] -> Int
totalPlayers [] = 0
totalPlayers (x:xs) = 1 + totalPlayers xs 

-- 2) funcion tipo fold (calcula la sumatoria de todos los puntos de todos los jugadores)
totalPts :: [Player] -> Int
totalPts [] = 0
totalPts (x:xs) = cant_pts x + totalPts (xs) 

-- funcion auxiliar1 (calcula los puntos logrados de un jugador promedio de la NBA)
ptsAverage :: [Player] -> Int
ptsAverage xs  =  div (totalPts xs)(totalPlayers xs) 

-- 3) funcion tipo filter (Muestra la lista de jugadores con puntos mayores a la media)
leaders :: [Player] -> [Player]
leaders [] = []
leaders (x:xs) | (cant_pts x > 718) = x : leaders xs 
               | otherwise = leaders xs 

-- 4) funcion combinacion entre map y fitler (el filtro extrae los jugadores con puntaje mayor a la media, map nos lleva al nombre de los jugadores que cumplen tal condicion)
leadersMap :: [Player] -> [String]
leadersMap [] = []
leadersMap (x:xs) | (cant_pts x > 718) = (name x) : leadersMap xs 
                  | otherwise = leadersMap xs 

-- 5) funcion tipo filter (dada la lista, el filtro extrae, y da una nueva lista donde se encuentra el maximo anotador de triples)
king3 :: [Player] -> [Player]
king3 [x1] = [x1]
king3 (x1:x2:xs)
                   | (cant_pm3 x1 > cant_pm3 x2) = king3 (x1:xs) 
                   | otherwise = king3 (x2:xs)

--  funcion de analisis1 (devuelve la eficacia de un jugador respecto a los tiros triples)
tripleStat :: Player -> Int
tripleStat (player, games, minutes, pts, fgm, fga, pm3, pa3) = div (pm3 * 100)  pa3
-- fin Funciones

--15 nov
rey3 :: [Player] -> String
rey3 [x1] = name x1
rey3 (x1:x2:xs) | (cant_pm3 x1 > cant_pm3 x2) = rey3 (x1:xs)
                | otherwise = rey3 (x2:xs) 

player :: [Player]
player = [("James Jarden" ,81, 2981, 2217, 647, 1470, 208,555), ("Luis Scola",81, 1661, 763, 300, 642, 5, 20), ("Manu Ginobili", 70, 1587, 738, 251, 589, 89, 258), ("Stephen Curry", 80, 2618, 1900, 653, 1341, 286, 646),("Andrew Wiggins", 82, 2969, 1387, 497, 1137, 39, 126), ("Draymond Green", 79, 2490, 921, 339, 765, 111, 329), ("Kobe Bryant", 35, 1207, 782, 266, 713, 54, 184), ("Trevor Ariza", 82, 2930, 1048, 366, 910, 194, 555), ("Damian Lillard", 82, 2925, 1720, 590, 1360, 196, 572), ("Jimmy Butler", 65, 2513, 1301, 421, 912, 73, 193),  ("Jhon Wall", 79, 2820, 1387, 519, 1166, 65, 217), ("Kalin Lucas", 1, 6, 0, 0, 0, 0, 0), ("Kevin Love", 75, 2532, 1228, 413, 952, 144, 392), ("Anthony Davis", 68, 2455, 1656, 642, 1199, 1, 12), ("DeMarcus Cousins", 59, 2013, 1421, 498, 1066, 2, 8), ("Kevin Durant", 27, 913, 686, 238, 467, 64, 159), ("Paul Millsap", 73, 2390, 1218, 443, 930, 77, 216), ("Timofey Mozgov", 81, 2046, 785, 314, 566, 2, 6), ("Caron Butler", 78, 1623, 460, 161, 396, 83, 219), ("Allen Crabbe", 51, 683, 168, 63, 153, 30, 85), ("Russell Westbrook", 67, 2302, 1886, 627, 1471, 86, 288),("Alonzo Gee", 54, 661, 241, 87, 186, 13, 35), ("Lance Stephenson", 61, 1573, 501, 207, 550, 18, 105), ("Amir Johnson", 75, 1979, 694, 298, 519, 19, 46), ("Anderson Varejao", 26, 636, 255, 111, 200, 2, 33), ("Andre Dawkins", 4, 22, 3, 1, 6, 1, 6), ("Nik Stauskas", 73, 1127, 319, 108, 296, 48, 149), ("LeBron James", 69, 2493, 1743, 624, 1279, 120, 339), ("Pau Gasol", 78, 2681, 1446, 570, 1153, 12, 26), ("Willie Green", 52, 951, 306, 118, 306, 42, 121), ("Terrence Ross", 82, 2092, 807, 309, 753, 145, 390), ("Zach LaVine", 77, 1902, 778, 286, 677, 57, 167), ("Tristan Thompson", 82, 2194, 693, 267, 488, 159, 248), ("CJ Watson", 57, 1422, 570, 181, 417, 70, 175), ("Will Bynum", 7, 67, 22, 10, 31, 9, 2), ("Will Cherry", 8, 69, 15, 5, 19, 2, 9), ("Paul George", 6, 91, 53, 18, 49, 9, 22), ("Avery Bradley", 77, 2428, 1071, 434, 1013, 124, 352), ("Chris Paul", 82, 2857, 1564, 568, 1170, 139, 349), ("Tony Allen", 63, 1648, 539, 225, 455, 10, 29), ("Kawhi Leonard", 64, 2033, 1057, 394, 822, 67, 192), ("Zoran Dragic", 16, 75, 28, 11, 30, 3, 14), ("Aaron Gordon", 47, 797, 243, 93, 208, 13, 48), ("Adreian Payne", 32, 740, 213, 91, 220, 1, 9), ("Andre Dawkins", 4, 22, 3, 1, 6, 1, 6), ("Andre Miller", 81, 1253, 355, 145, 290, 7, 34), ("Brandon Knight", 63, 2035, 1070, 375, 888, 125, 321), ("Xavier Henry", 9, 86, 20, 3, 13, 14, 24), ("Quincy Acy", 68, 1287, 398, 152, 331, 18, 60), ("Ricky Rubio", 22, 692, 226, 78, 219, 13, 51), ("Rajon Rondo", 68, 2019, 608, 275, 645, 27, 86), ("Jarnell Stokes", 19, 126, 57, 21, 37, 15, 28), ("JJ Barea", 77, 1362, 580, 225, 536, 54, 167), ("Henry Walker", 24, 628, 175, 58, 168, 45, 132), ("Elliot Williams", 13, 119, 37, 14, 37, 8, 18), ("Dwyane Wade", 62, 1971, 1331, 509, 1084, 29, 102), ("Cory Joseph", 79, 1444, 535, 207, 411, 16, 44), ("Alec Burks", 27, 899, 374, 121, 300, 26, 68), ("Brandon Bass", 82, 1929, 866, 344, 683, 9, 32), ("Francisco Garcia", 14, 200, 45, 17, 63, 10, 45), ("Gary Neal", 54, 1192, 543, 188, 503, 50, 164), ("Jeremy Lin", 74, 1907, 832, 277, 654, 65, 176), ("Joe Harris", 51, 493, 136, 48, 120, 31, 84), ("Jose Calderon", 42, 1270, 382, 147, 354, 59, 142), ("Klay Thompson", 77, 2455, 1668, 602, 1299, 239, 545), ("Mirza Teletovic", 40, 892, 339, 122, 319, 62, 193)]
main = putStrLn "Hello World"
