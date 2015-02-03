module Geometry.Cuboid (volume, area) where
    
    volume :: Float -> Float -> Float
    volume x y z = rectangleArea x y * z

    area :: Float -> Float -> Float
    area a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2

    rectangleArea :: Float -> Float -> Float
    rectangleArea a b = a * b