import Week40Exercise1
--Solar System
 
--Exercise 1
--a)
numberOfMoons :: PlanetarySystem -> Integer
numberOfMoons (PlanetarySystem _ planets) = sum $ map (fromIntegral . length . moons) planets

--b)
atLeastOneMoon :: PlanetarySystem -> [Planet] 
atLeastOneMoon (PlanetarySystem _ planets) = 
              [ x |
              x <- planets, hasMoon x 
              ]
              where
                hasMoon :: Planet -> Bool
                hasMoon planet = if length (moons planet)==0 then False else True

