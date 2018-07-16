module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

year :: Float -> Float 
year seconds = seconds / 31557600

ageOn :: Planet -> Float -> Float
ageOn Mercury seconds = year seconds / 0.2408467
ageOn Venus seconds = year seconds / 0.61519726
ageOn Earth seconds = year seconds
ageOn Mars seconds = year seconds / 1.8808158
ageOn Jupiter seconds = year seconds / 11.862615
ageOn Saturn seconds = year seconds / 29.447498
ageOn Uranus seconds = year seconds / 84.016846
ageOn Neptune seconds = year seconds / 164.79132

