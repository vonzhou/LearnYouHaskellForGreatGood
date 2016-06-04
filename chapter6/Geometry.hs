module Geometry
( sphereVolume
,sphereArea
,cubeVolume
,cubeArea
,cuboidArea
,cuboidVolume
) where 

sphereVolume :: Float -> Float 
sphereVolume radius = (4.0 / 3.0 ) * pi * (radius ^ 3)

sphereArea :: Float -> Float
sphereArea radius = 4 * pi * (radius ^ 2)

cubeVolume :: Float -> Float
cubeVolume side = cuboidVolume side side side

cubeArea :: Float -> Float
cubeArea side = cuboidArea side side side

cuboidVolume :: Float -> Float -> Float -> Float
cuboidVolume a b c = rectArea a b * c

cuboidArea :: Float -> Float -> Float -> Float
cuboidArea a b c = rectArea a b * 2 + rectArea a c * 2 + rectArea c b * 2

-- Not export this function
rectArea :: Float -> Float -> Float
rectArea a b = a * b


